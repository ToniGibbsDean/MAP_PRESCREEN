library(tidyverse)
library(dplyr)
library(ggpubr)
path="Figures"

######## Next script 

#making new column that merges the birthcontrol data 0/1 from old and new data - i.e., where missing it should add in the data where appropirate - still have 56 people missing
#but have more data overall. Will remove these 56 as further investigation showed they have none to v limited other data 
coalesceDat_bc <- merged_data_left %>%
  mutate(
    birthControl_oldANDnewData = coalesce(birth_control, birth_control.old)
  ) %>%
  select(-ends_with(".old")) %>%
  filter(!is.na(birthControl_oldANDnewData)) #remove 56 people who still have NAs - and seem to have no other data now at 1380

#psychosis dx also v important and info for this may be stored in old. 

psycohsisdx_cleaning<- coalesceDat_bc %>% 
    #select(c(record_id, dx_10:dx_17)) %>% 
    mutate(across(c(dx_10:dx_13, dx_15:dx_17), ~ replace_na(.,  0))) %>%
    mutate(across(c(psychosisdx1:psychosisdx10), ~ replace_na(.,  0))) %>% 
    mutate(psychosisDXOldRC = as.numeric(rowSums(across(c(dx_10:dx_17))) > 0)) %>% 
    mutate(nPsychosisDx = rowSums(across(c(  psychosisDXOldRC,
                                             psychosisdx1,
                                             psychosisdx2,
                                             psychosisdx3,
                                             psychosisdx4,
                                             psychosisdx5,
                                             psychosisdx6,
                                             psychosisdx7,
                                             psychosisdx8,
                                             psychosisdx9,
                                             psychosisdx10)))) %>%
    mutate(psychosisDXyesNo = nPsychosisDx > 0 ) %>%
    #mutate(ageOfPsychosisDx_oldRC = rowSums(across(psychosisDXOldRC)) #- needs finishing - integrate old and new data for ages

moodDXintegration <- psycohsisdx_cleaning %>%
    mutate(across(c(dx_19:dx_23), ~ replace_na(.,  0))) %>%
    mutate(across(c(mooddx1:mooddx6), ~ replace_na(.,  0))) %>% 
    mutate(moodDXOldRC = as.numeric(rowSums(across(c(dx_19:dx_23))) > 0)) %>% 
    mutate(nMoodDx = rowSums(across(c(mooddx1:mooddx6)))) %>%
    mutate(moodDXyesNo = nMoodDx > 0) #%>%
    #mutate(ageOfMoodDx_oldRC = rowSums(across(psychosisDXOldRC)) 
    #select(c(dx_19:dx_23, mooddx1:mooddx6, moodDXOldRC, nMoodDx, moodDXyesNo)) 


    
    
    :dx_13, dx_15:dx_17, psychosisdx1:psychosisdx10)))) %>%
    mutate(psychosisDXyesNo =rowSums(c_across(c(dx_10:dx_13, dx_15:dx_17, psychosisdx1:psychosisdx10))) > 0 )



    mutate(ageOfPsychosisDx = rowSums(across(c(dx_10:dx_17)))) %>%
    mutate(psychosisDXOldRC = rowSums(across(c(dx_10:dx_17))) > 0) %>% 
    mutate(psychosisDXNewdRC = rowSums(across(c(psychosisdx1:psychosisdx10))) > 0) %>% 
    

summary()


psycohsisdx_cleraning$psychosisDxfromOld


oldRedCap_newNames %>%
  count(firstname, lastname, email, dob) %>%
  filter(n > 1)







merged_data_left %>% select(birth_control) %>% mutate(as.factor(birth_control)) %>% summary()
merged_data_left  %>% select(birth_control.old) %>% mutate(as.factor(birth_control.old)) %>% summary()



coalesceDat_bc %>% select(birthControl_oldANDnewData) %>% mutate(as.factor(birthControl_oldANDnewData)) %>% summary()

NA_BC <- coalesceDat_bc %>% filter(is.na(birthControl_oldANDnewData))

write.csv(NA_BC, "Outputs/birthcontrol_NAdatfromOLDandNEWredCapDat.csv")

NA_BC_justNewdat<-read.csv("Outputs/bcDetailsDat.csv")

overlapping_ids <- inner_join(NA_BC, NA_BC_justNewdat, by = "record_id") %>%
  select(record_id) %>%
  mutate(repeated = duplicated(record_id) | duplicated(record_id, fromLast = TRUE)) %>% summary()





PBQ <- merged_data_left %>% dplyr::select(record_id,
    pqb1,pqb2,pqb3,pqb4,pqb5,pqb6,pqb7,pqb8,pqb9,pqb10,pqb11,
            pqb12,pqb13,pqb14,pqb15,pqb16,pqb17,pqb18,pqb19,pqb20,pqb21,
            pqb1_yes,pqb2_yes,pqb3_yes,pqb4_yes,pqb5_yes,pqb6_yes,pqb7_yes,
            pqb8_yes,pqb9_yes,pqb10_yes,pqb11_yes,
            pqb12_yes,pqb13_yes,pqb14_yes,pqb15_yes,pqb16_yes,pqb17_yes,pqb18_yes,
            pqb19_yes,pqb20_yes,pqb21_yes)

missDat <- merged_data_left %>% 
mutate(birth_sex = factor(case_when(is.na(birth_sex) ~ "PreferNotToSay",
    birth_sex == 0 ~ "PreferNotToSay",
    TRUE ~ "female"))) %>%  
mutate(ethnic_v1 = factor(case_when(is.na(ethnic_v1) ~ "PreferNotToSay",
    ethnic_v1 == 3 ~ "PreferNotToSay",
    ethnic_v1 == 2 ~ "NotHispanicOrLatino",
    ethnic_v1 == 1 ~ "HispanicOrLatino"))) %>%    
mutate(residence_country = factor(case_when(is.na(residence_country) ~ "notUSA",
    residence_country > 0 ~ "notUSA",
    residence_country == 0 ~ "USA"))) %>% 
#mutate(residence_state = factor(case_when(is.na(residence_state) ~ 52,
#                               TRUE ~ residence_state))) %>% 
#levels = c(1:51, 52), labels = c(state.name, "Other"))) %>%   
mutate(thyroidscreen = factor(case_when(is.na(thyroidscreen) ~ 2,
    TRUE ~ thyroidscreen))) %>%  #select(thyroidscreen) %>% summary() 
mutate(thyroidYesNo = factor(case_when(thyroidscreen == 1 ~ "Yes",
    thyroidscreen == 2 ~ "No",
    TRUE ~ "Yes"))) %>% 
mutate(eatingDisYesNo = factor(case_when(is.na(anascreen) ~ "No",
    TRUE ~ "Yes"))) %>%
mutate(SUDYesNo = factor(case_when(is.na(sudscreen) ~ "No",
    TRUE ~ "Yes"))) %>%
mutate(pregnancy = factor(case_when(is.na(pregnancy) ~ "PreferNotToSay",
    pregnancy == 0 ~ "notPregnant",
    TRUE ~ "pregnant"))) %>%  
select(
    record_id,
    dob,
    race___1,
    race___2,
    race___3,
    race___4,
    race___5,
    race___8,
    race___9,
    birth_sex,
    ethnic_v1,
    residence_country,
    pregnancy,
    hyster1___1,
    hyster1___2,
    hyster1___3,
    hyster1___4,
    hyster1___5,
    hyster1___6,
    hyster1___7,
    hyster5,
    hyster2,
    thyroidscreen,
    thyroidYesNo,
    eatingDisYesNo,
    SUDYesNo,
    prescreendate,
    lastperiod) %>%
as_tibble() %>%
filter(complete.cases(.)) #still 1069! Yay



merged_data_right <- dim(right_join(datNewRedCap_noDups, oldRedCap_newNames_noDups, by = c("firstname", "lastname", "email", "dob"), suffix = c(".new", ".old")))
merged_data_left <- dim(left_join(datNewRedCap_noDups, oldRedCap_newNames_noDups, by = c("firstname", "lastname", "email", "dob"), suffix = c(".new", ".old")))
merged_data_inner <- dim(inner_join(datNewRedCap_noDups, oldRedCap_newNames_noDups, by = c("firstname", "lastname", "email", "dob"), suffix = c(".new", ".old")))


duplicatesNewDat <- datNewRedCap %>%
  count(firstname, lastname, email, dob) %>%
  filter(n > 1)

duplicatesOldDat <- oldRedCap_newNames %>%
  count(firstname, lastname, email, dob) %>%
  filter(n > 1)

merged_data <- full_join(datNewRedCap, oldRedCap_newNames, by = c("firstname", "lastname", "email", "dob"), suffix = c("", ".old"))


oldRedCap_newNames[622,] %>% select(name, dob) #81 Esther Jiang 1968-05-30
oldRedCap_newNames[769,] %>% select(name, dob) #Jessica Classey 1975-11-30
datNewRedCap[769,] %>% select(firstname, lastname, dob) #Esther    Jiang 1968-05-30


overlapping_ids <- inner_join(oldRedCap_newNames, datNewRedCap, by = "record_id") %>%
  select(record_id) %>%
  mutate(repeated = duplicated(record_id) | duplicated(record_id, fromLast = TRUE)) %>% summary()

df1_only <- anti_join(datOldRedCap, datNewRedCap, by = "record_id") %>%
  select(record_id)

# record_ids in df2 but not in df1
df2_only <- anti_join(datNewRedCap, datOldRedCap, by = "record_id") %>%
  select(record_id)

#THE IDS in df1_only are weird - seems not useable so am going to remove 
datOldRedCap_filtered <- datOldRedCap %>%
  filter(!record_id %in% df1_only$record_id)



old_overlap <- datOldRedCap %>%
  filter(record_id %in% datNewRedCap$record_id)
patched_data <- datNewRedCap %>%
  left_join(old_overlap, by = "record_id", suffix = c("", "_old"))




# joining the df's but prefer the new redcap data?
joined_data <- full_join(datOldRedCap_filtered, datNewRedCap, by = "record_id", suffix = c("_old", "_new"))


joined_data <- bind_rows(
  # Only the rows from datOldRedCap with overlapping IDs
  datOldRedCap_filtered %>%
    filter(record_id %in% overlapping_ids$record_id),

  # All rows from datNewRedCap (includes overlapping and df2-only)
  datNewRedCap
) %>%
  distinct(record_id, .keep_all = TRUE)