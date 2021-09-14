library(tidyverse)
library(stringr)
library(sf)

tree_codes <- read.csv("C:/Users/User/OneDrive - Umich/School/Capstone Project/Data//CodedTrainingData/GenusSpecies_PlantCode_All_Data.csv")

colnames(tree_codes)[1] <- "Common_Name"
tree_codes$Project_Code[tree_codes$Project_Code == ""] <- NA
tree_codes$Plant_Code[tree_codes$Plant_Code == ""] <- NA
tree_codes$Genus[tree_codes$Genus == ""] <- NA
tree_codes <- tree_codes %>%
  mutate(Project_Code = coalesce(Project_Code, Plant_Code)) %>%
  mutate(Common_Name = str_to_title(Common_Name)) %>%
  mutate(Common_Genus = word(Botanical, 1))

Genus_codes <- tree_codes[word(tree_codes$Botanical, 2) == 'sp.', ] %>%
  mutate(Genus = coalesce(Genus, Plant_Code)) %>%
  select(Common_Genus, Genus)

tree_codes <- tree_codes %>%
  select(-Genus) %>%
  left_join(Genus_codes)

missing_genus <- tree_codes[is.na(tree_codes$Genus), ] %>%
  group_by(Common_Genus) %>%
  summarise(count = n()) %>%
  mutate(test_code = str_to_upper(str_sub(Common_Genus, 1,5)))

dupes <- missing_genus$test_code[missing_genus$test_code %in% tree_codes$Genus]
missing_genus$test_code[7] <- "CERCI2"

genus_extension <- missing_genus %>%
  mutate(Genus = test_code, Plant_Code = test_code) %>%
  mutate(Common_Name = paste(Common_Genus, "sp.")) %>%
  mutate(Botanical = paste(Common_Genus, "sp.")) %>%
  select(Common_Name, Botanical, Plant_Code, Common_Genus, Genus)

tree_codes <- bind_rows(tree_codes, genus_extension)

Genus_codes <- tree_codes[word(tree_codes$Botanical, 2) == 'sp.', ] %>%
  mutate(Genus = coalesce(Genus, Plant_Code)) %>%
  select(Common_Genus, Genus)

tree_codes <- tree_codes %>%
  select(-Genus) %>%
  left_join(Genus_codes)

# campus_trees <- read.csv("C:/Users/User/OneDrive - Umich/School/Capstone Project/Data//CodedTrainingData/Campus_trees_2.csv")
# campus_ID <- unique(campus_trees$d_SpeciesID)
# campus_ID <- str_to_title(campus_ID)
# 
# length(campus_ID)
# 
# matched <- campus_ID[(campus_ID %in% tree_codes$Common_Name)]
# needing_match <- campus_ID[!(campus_ID %in% tree_codes$Common_Name)]
# tree_codes$Campus_Name <- ifelse(tree_codes$Common_Name %in% matched, tree_codes$Common_Name, NA)
# 
# campus_key <- tree_codes %>%
#   select(Common_Name, Campus_Name) %>%
#   drop_na() %>%
#   add_row(Campus_Name = needing_match, Common_Name = "")
  
write.csv(tree_codes, "C:/Users/User/OneDrive - Umich/School/Capstone Project/Data//CodedTrainingData/GenusSpecies_PlantCode_All_Data2.csv")

genus_counts <- tree_codes %>%
  group_by(Genus) %>%
  summarise(count = n()) %>%
  mutate(percent = count / nrow(tree_codes) * 100)
