
#packages and read in files 
library(tidyverse)
library(dplyr)
library(ggpubr)
path="Figures"

datOldRedCap<-read.csv("Data/OLDPREMAPPreScreenin-SupplementDataToUpda_DATA_2025-05-16_1218.csv")

datNewRedCap<-read.csv("Data/PREMAPIntegration-PrescreenDataAll_DATA_2025-05-23_1003.csv") 

datNewRedCap %>% select(
"psychosisdx1",                             
"psychosisdx2" ,                             
"psychosisdx3"  ,                            
 "psychosisdx4"  ,                            
 "psychosisdx5"   ,                           
 "psychosisdx6"    ,                          
 "psychosisdx7"     ,                         
 "psychosisdx8"      ,                        
 "psychosisdx9"       ,                       
 "psychosisdx10"     ) %>% summary()
 

#found duplicates within each df and between the 2 when using "firstname", "lastname", "email", "dob" - for now I have just removed these
#record_ids duplicate also - some are the same person, others are not. 

#remove prefix from variable col names in old data and align col names for varialbes we want to use to match acorss new and old datasets
##please not we cannot use record_id due to insufficient understanding of duplicate IDs and missing IDS
oldRedCap_newNames<-datOldRedCap %>%
  rename_with(~ str_replace(., "^pscreen_", "")) %>%
  separate(name, into = c("firstname", "lastname"), sep = " ", extra = "merge")

#some duplicates remain when using name, email and dob - removing these for now
oldRedCap_newNames_noDups <- oldRedCap_newNames[!duplicated(oldRedCap_newNames[c("firstname", "lastname", "email", "dob")]), ] #11 duplicate rows removed
datNewRedCap_noDups <- datNewRedCap[!duplicated(datNewRedCap[c("firstname", "lastname", "email", "dob")]), ] #38 duplicate rows removed

#merging new and old datasets using all 4 cols and adding .old to any columns that exist in both 
merged_data_left <- left_join(datNewRedCap_noDups, oldRedCap_newNames_noDups, by = c("firstname", "lastname", "email", "dob"), suffix = c("", ".old"))

write.csv(merged_data_left, "Outputs/merged_data_redcapNewAndOld.csv")



