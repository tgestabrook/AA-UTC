###############################
# AAUTC Machine Learning Script
# Author: Thomas Estabrook
# 1/17/2022
# Updated: 1/17/2022
###############################

library(sf)
library(randomForest)
library(tidyverse)
library(e1071)
library(caret)
library(nnet)

# # Initial information
# seg_method <- "pixel"       # segmentation method (e.g. LiDAR, pixel, buffered pixel)
# location <- "BPW"           # location (e.g. BPW, full_city)
# genus_threshold <- 40       # number below which genera are lumped into "other" category
# today <- Sys.Date()         # current date
# set.seed(1)                 # set a random seed for reproducibility
# setwd('G:/Shared drives/A2_UTC Drive/R_scripts/Model_Inputs')                     # set working directory
# 
# # Load in the csv with tree data
# # Should have a column with Genus, and columns for each predictor variable
# trees <- read_sf("training_testing_Clip_resample5ft_withPixelVal.shp") %>% 
#   mutate(tree_id = row_number()) %>% # add a new unique ID for the dataset
#   st_drop_geometry() %>%
#   drop_na()
# Use this to list all columns not being used as predictor variables
# dropcols <- c("tree_id", "Join_Count", "TARGET_FID", "Plnt_Cd", "Cmmn_Nm", "Source", "Prjct_C", "train")

 # # Initial information
 # seg_method <- "lidRvarsm2"       # segmentation method (e.g. LiDAR, pixel, buffered pixel)
 # location <- "BPW"           # location (e.g. BPW, full_city)
 # genus_threshold <- 40       # number below which genera are lumped into "other" category
 # today <- Sys.Date()         # current date
 # set.seed(1)                 # set a random seed for reproducibility
 # setwd('G:/Shared drives/A2_UTC Drive/R_scripts/Model_Inputs')                     # set working directory
 # 
 # # Load in the csv with tree data
 # # Should have a column with Genus, and columns for each predictor variable
 # trees <- read.csv("Les_BPW_LidR_seg_var_smooth2_finalZS.csv") %>% 
 #   mutate(tree_id = row_number()) %>% # add a new unique ID for the dataset
 #   drop_na() %>%
 #   filter(Genus != "")
 # 
 # head(trees)
 # colnames(trees)
 # 
 # #Use this to list all columns not being used as predictor variables
 # dropcols <- c("X", "Id", "MAJORITY", "MINORITY", "VARIETY", "tree_id")

# # Initial information
# seg_method <- "lidR30sm"       # segmentation method (e.g. LiDAR, pixel, buffered pixel)
# location <- "BPW"           # location (e.g. BPW, full_city)
# genus_threshold <- 40       # number below which genera are lumped into "other" category
# today <- Sys.Date()         # current date
# set.seed(1)                 # set a random seed for reproducibility
# setwd('G:/Shared drives/A2_UTC Drive/R_scripts/Model_Inputs')                     # set working directory
# 
# # Load in the csv with tree data
# # Should have a column with Genus, and columns for each predictor variable
# trees <- read.csv("Les_BPW_LidR_seg_30_smooth_finalZS.csv") %>% 
#   mutate(tree_id = row_number()) %>% # add a new unique ID for the dataset
#   drop_na() %>%
#   filter(Genus != "")
# 
# head(trees)
# colnames(trees)
# 
# #Use this to list all columns not being used as predictor variables
# dropcols <- c("X", "Id", "MAJORITY", "MINORITY", "VARIETY", "tree_id")

# Initial information
seg_method <- "lidR30"       # segmentation method (e.g. LiDAR, pixel, buffered pixel)
location <- "BPW"           # location (e.g. BPW, full_city)
genus_threshold <- 40       # number below which genera are lumped into "other" category
today <- Sys.Date()         # current date
set.seed(1)                 # set a random seed for reproducibility
setwd('G:/Shared drives/A2_UTC Drive/R_scripts/Model_Inputs')                     # set working directory

# Load in the csv with tree data
# Should have a column with Genus, and columns for each predictor variable
trees <- read.csv("Les_BPW_LidR_seg_30_finalZS.csv") %>% 
  mutate(tree_id = row_number()) %>% # add a new unique ID for the dataset
  drop_na() %>%
  filter(Genus != "")

head(trees)
colnames(trees)

#Use this to list all columns not being used as predictor variables
dropcols <- c("X", "Id", "MAJORITY", "MINORITY", "VARIETY", "tree_id")

############################################################################################################
# Get counts of tree genera
genuscounts <- trees %>%
  group_by(Genus) %>%
  summarise(count = n())

# Filter out underrepresented genuses and recode to "Other"
other_genera <- genuscounts$Genus[genuscounts$count < genus_threshold] 
trees$Genus[trees$Genus %in% other_genera] <- "Other"

# Randomly take half of each genus for training and testing
training_trees <- trees %>%
  group_by(Genus) %>%
  slice_sample(prop = 0.5)

testing_trees <- trees[!(trees$tree_id %in% training_trees$tree_id),]

# Remove unneeded columns
training_trees <- training_trees %>%
  select(!all_of(dropcols))

testing_trees <- testing_trees %>%
  select(!all_of(dropcols))

# Turn the Genus column into a factor
training_trees$Genus <- as.factor(training_trees$Genus)
testing_trees$Genus <- as.factor(testing_trees$Genus)

# Run a random forest model
rf_mod <- randomForest(Genus ~ ., data = training_trees, 
                       xtest = testing_trees[,!names(testing_trees) == "Genus"],
                       ytest = testing_trees$Genus)
rf_pred <- rf_mod$test$predicted
rf_conf <- confusionMatrix(data = rf_pred, reference = testing_trees$Genus)

# Run a SVR model
svm_mod <- svm(Genus ~ ., data = training_trees)
svm_pred <- predict(svm_mod, testing_trees)
svm_conf <- confusionMatrix(data = svm_pred, reference = testing_trees$Genus)

# Run multinomial logistic regression
reg_mod <- multinom(Genus ~ ., data = training_trees)
reg_pred <- predict(reg_mod, testing_trees)
reg_conf <- confusionMatrix(data = reg_pred, reference = testing_trees$Genus)


# Save outputs
setwd('G:/Shared drives/A2_UTC Drive/R_scripts/Model_Outputs')    
filename <- paste0(seg_method, "_", location, "_", genus_threshold, "_", today)
save(rf_mod, file = paste0("rf_mod_", filename))
save(svm_mod, file = paste0("svm_mod_", filename))
save(reg_mod, file = paste0("reg_mod_", filename))

sink(file = paste0("summary_", filename, ".txt"))
print("Random Forest:")
rf_conf$overall[1:2]
print("Support Vector:")
svm_conf$overall[1:2]
print("Multinomial Regression:")
reg_conf$overall[1:2]

print("Random Forest:")
rf_conf

print("Support Vector:")
svm_conf

print("Multinomial Regression:")
reg_conf
sink(file=NULL)













