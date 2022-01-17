library(sf)
library(randomForest)
library(tidyverse)

dataset_name <- ""
segmentation_method <- ""



# Load in dataset with labeled trees and predictor variables
trees <- read_sf("G:/Shared drives/A2_UTC Drive/ClippedImagery_BPW_Les/training_testing_Clip_resample5ft_withPixelVal.shp")

# Figure out which genera to filter out
genuscounts <- trees %>%
  group_by(Genus) %>%
  summarise(count = n(), mean_dbh = sum(DBH)/n())

# Filter out underrepresented genuses and recode to "Other"
other_genera <- genuscounts$Genus[genuscounts$count < 40] 
trees$Genus[trees$Genus %in% other_genera] <- "Other"

training_trees <- trees %>%
  filter(train == 'train') %>%
  select(!c("Join_Count", "TARGET_FID", "Plnt_Cd", "Cmmn_Nm", "Source", "Prjct_C", "train")) %>%
  st_drop_geometry() %>%
  drop_na()

testing_trees <- trees %>%
  filter(train == 'test', Genus != "ASIMI") %>%
  select(!c("Join_Count", "TARGET_FID", "Plnt_Cd", "Cmmn_Nm", "Source", "Prjct_C", "train")) %>%
  st_drop_geometry() %>%
  drop_na()

training_trees$Genus <- as.factor(training_trees$Genus)
testing_trees$Genus <- as.factor(testing_trees$Genus)

rf_test <- randomForest(Genus ~ ., data = training_trees, 
                        xtest = testing_trees[,-2],
                        ytest = testing_trees$Genus)
rf_test











