# Script to connect the two datasets

library(tidyverse)


# 1)  initial data loading ---------------------------------------------------------------
#new data 
# load and remove records without names or times as failed data collections
n<-read.csv("Data/PREMAPIntegration-PrescreenDataAll_DATA_2025-05-23_1003.csv") %>% 
    as_tibble%>% 
    filter(timestamp!="" & lastname!="") 
# old data
# remove those without a name, all lack other data and are marked as tests or failed data entries
o<-read.csv("Data/OLDPREMAPPreScreenin-SupplementDataToUpda_DATA_2025-05-16_1218.csv") %>% 
    as_tibble %>%
    filter(pscreen_name!="")


# 2) remove incorrect old data from new data frae ---------------------------------------------
# filter out everthing from before the new dataset - these were merged in an error-prone way
# this also removes 4 rows that do not have prescreendate values, but all have timestamp values 
#showing they are from the old dataset
n<-n %>%
    mutate(prescreendate=as_date(prescreendate))%>% 
    filter(!(prescreendate < "2024-11-12") |is.na(prescreendate)) %>%
    arrange(prescreendate)


# 3) remove multiple record collection events per person ------------------------------------------------------
# remove multiple records fro mthe sam person (using emails as they must be unique)
# always favoring the last record on assumption that multiple collection efforts reflect
# the need to repeat

# remove second occurances on people in new data collection effort
n<-n[!duplicated(n$email ),]

#remove second occurance of people in old data collection effort
o<-o[  !(duplicated(o$pscreen_email) & o$pscreen_email!="") ,]

#remove people from old data collection effort who subsequnelt came and had data collected in the new effort
o<-o[ ! o$pscreen_email %in% 
    n$email,]


# 4) remove anyone from @yale.edu domain; these are tests ------------------------------------------------------
n<-n[!str_detect(n$email, "@yale.edu"),]
o<-o[!str_detect(o$pscreen_email, "@yale.edu"),]



# 5) align columns between old and new dataframes --------------------------------------------------------------------
cols <- c(
  #"record_id",
  "prescreendate", "name",
  "dob",          "email",         "phone",         "birth_sex",
  "race___1",     "race___2",      "race___3",      "race___4",      "race___5",
  "race___8",     "race___9",      "ethnic_v1",     "english",       
  #"birthcity",  "residence_country", 
  "residence_state", "residence_zipcode",
  "lastperiod",   "noperiod",      "regularperiods","irregularperiods","birth_control",
  "bc_details",   "othermeds",     "pregnancy",
  paste0("hyster1___", 1:7),  # expands to hyster1___1 … hyster1___7
  "hyster4",      "hyster5",      "hyster2",       "hyster3",
  "seizure",      "seizure2",     "seizure3",
  paste0("head", c("",2,5,3,4)),  # head, head2, head5, head3, head4
  "physical_dx",  "surgery",      "surgery2",
  paste0("exclusion_dx1___", c(1,2,3,6,8)),
  "thyroidscreen",
  #"anascreen", # removed as it doesn't seem to match a column in old
  "sudscreen",
  paste0("mooddx", 1:6),
  paste0("psychosisdx", 1:10),
  "adhddx",
  paste0("age_mooddx", 1:6),
  paste0("age_psychosisdx", 1:10),
  "age_adhddx",
  # pqb and pqb_yes pairs
  unlist(lapply(1:21, function(i) c(paste0("pqb", i), paste0("pqb", i, "_yes")))),
  "repro96"
)

# get cols in new desired columns
n_selected<-n %>%
  mutate(name=paste0(firstname, " ", middleinitial, " ", lastname)) %>% 
  select(all_of(cols)) %>%
  mutate(
    dob=as_date(dob),
    lastperiod=as_date(lastperiod))

# rename and reorganise old cols to match new
attach(o)
o_reformatted<-data.frame(
    "prescreendate"= as_date(pscreen_date),
    "name" =pscreen_name,
    "dob" = as_date(pscreen_dob),
    "email" = pscreen_email,
    "phone" = pscreen_phone,
    "birth_sex"= scree_birth_sex,
    "race___1"=NA,
    "race___2"=NA,
    "race___3"=NA,
    "race___4"=NA,
    "race___5"=NA,
    "race___8"=NA,
    "race___9"=NA,
    "ethnic_v1"=NA,
    "english"= pscreen_english,
    "residence_state"=pscreen_us_location,
    "residence_zipcode"=pscree_zipcode,
    "lastperiod"=as_date(pscreen_last_period_date),
    "noperiod"=pscreen_period,
    "regularperiods"=pscreen_regular_periods,
    "irregularperiods"=pscreen_irregular,
    "birth_control"=pscreen_birth_control,
    "bc_details"=pscreen_bc_details,
    "othermeds"= paste0(pscreen_current_meds,  pscreen_medication_list),
    "pregnancy"=pscreen_pregnancy,
    "hyster1___1"=str_detect(prescreen_hyster_type, "1"),
    "hyster1___2"=str_detect(prescreen_hyster_type, "2"),
    "hyster1___3"=str_detect(prescreen_hyster_type, "3"),
    "hyster1___4"=str_detect(prescreen_hyster_type, "4"),
    "hyster1___5"=str_detect(prescreen_hyster_type, "5"),
    "hyster1___6"=str_detect(prescreen_hyster_type, "6"), # always false as 6 not an option in old
    "hyster1___7"=str_detect(prescreen_hyster_type, "7"), # always false as 7 not an option in old
    "hyster4"=NA,
    "hyster5"=NA,
    "hyster2"=prescreen_hyster_date, #recorded as age not date
    "hyster3"=pscreen_hysterreason,
    "seizure"=pscreen_seizure,
    "seizure2"=pscreen_seizuredisorder_2,
    "seizure3"=pscreen_seizuredisorder_3,
    "head" = pscreen_head,
    "head2" = pscreen_head2,
    "head5" = pscreen_head5,
    "head3" = pscreen_head3,
    "head4" = pscreen_head4,
    "physical_dx" = pscreen_physical_dx,
    "surgery" = surgery,
    "surgery2" = surgeries_type,
    "exclusion_dx1___1"=str_detect(pscreen_med_diagnosis, "1"),
    "exclusion_dx1___2"=str_detect(pscreen_med_diagnosis, "2"),
    "exclusion_dx1___3"=str_detect(pscreen_med_diagnosis, "3"),
    # left out 4 - steroids data not colleced in new
    # left out 5 - chemo data not collected in new
    "exclusion_dx1___6"=str_detect(pscreen_med_diagnosis, "6"),
    "exclusion_dx1___8"=str_detect(pscreen_med_diagnosis, "8"), # always false as 8 wasn't an option in old
    "thyroidscreen"=pscreen_thyroid1,
    "sudscreen" = pscreen_dx_51,
    "mooddx1" = pscreen_dx_19,
    "mooddx2" = pscreen_dx_20,
    "mooddx5" = NA,
    "mooddx3" = pscreen_dx_25,
    "mooddx4" = pscreen_dx_21,
    "mooddx6" = NA,
    "psychosisdx1" = pscreen_dx_10,
    "psychosisdx2" = pscreen_dx_12,
    "psychosisdx3" = pscreen_dx_13,
    "psychosisdx4" = pscreen_dx_14,
    "psychosisdx5" = pscreen_dx_15,
    "psychosisdx6" = pscreen_dx_11,
    "psychosisdx7" = pscreen_dx_11,
    "psychosisdx8" = pscreen_dx_11,
    "psychosisdx9" = pscreen_dx_18,
    "psychosisdx10" = pscreen_dx_16,
    "adhddx" = pscreen_dx_53,
    "age_mooddx1" = pscreen_dx_19_v2,
    "age_mooddx2" = pscreen_dx_20_v2,
    "age_mooddx3" = pscreen_dx_25_v2,
    "age_mooddx4" = pscreen_dx_21_v2,
    "age_mooddx5" = NA,
    "age_mooddx6" = pscreen_dx_25_v2,
    "age_psychosisdx1" = pscreen_dx_10_v2,
    "age_psychosisdx2" = pscreen_dx_12_v2,
    "age_psychosisdx3" = pscreen_dx_13_v2,
    "age_psychosisdx4" = pscreen_dx_14_v2,
    "age_psychosisdx5" = pscreen_dx_15_v2,
    "age_psychosisdx6" = pscreen_dx_11_v2,
    "age_psychosisdx7" = pscreen_dx_11_v2,
    "age_psychosisdx8" = pscreen_dx_11_v2,
    "age_psychosisdx9" = pscreen_dx_18_v2,
    "age_psychosisdx10" = pscreen_dx_16_v2,
    "age_adhddx" = pscreen_dx_53,
    "pqb1" = pscreen_pqb1,
    "pqb1_yes" = pscreen_pqb1_yes,
    "pqb2" = pscreen_pqb2,
    "pqb2_yes" = pscreen_pqb2_yes,
    "pqb3" = pscreen_pqb3,
    "pqb3_yes" = pscreen_pqb3_yes,
    "pqb4" = pscreen_pqb4,
    "pqb4_yes" = pscreen_pqb4_yes,
    "pqb5" = pscreen_pqb5,
    "pqb5_yes" = pscreen_pqb5_yes,
    "pqb6" = pscreen_pqb6,
    "pqb6_yes" = pscreen_pqb6_yes,
    "pqb7" = pscreen_pqb7,
    "pqb7_yes" = pscreen_pqb7_yes,
    "pqb8" = pscreen_pqb8,
    "pqb8_yes" = pscreen_pqb8_yes,
    "pqb9" = pscreen_pqb9,
    "pqb9_yes" = pscreen_pqb9_yes,
    "pqb10" = pscreen_pqb10,
    "pqb10_yes" = pscreen_pqb10_yes,
    "pqb11" = pscreen_pqb11,
    "pqb11_yes" = pscreen_pqb11_yes,
    "pqb12" = pscreen_pqb12,
    "pqb12_yes" = pscreen_pqb12_yes,
    "pqb13" = pscreen_pqb13,
    "pqb13_yes" = pscreen_pqb13_yes,
    "pqb14" = pscreen_pqb14,
    "pqb14_yes" = pscreen_pqb14_yes,
    "pqb15" = pscreen_pqb15,
    "pqb15_yes" = pscreen_pqb15_yes,
    "pqb16" = pscreen_pqb16,
    "pqb16_yes" = pscreen_pqb16_yes,
    "pqb17" = pscreen_pqb17,
    "pqb17_yes" = pscreen_pqb17_yes,
    "pqb18" = pscreen_pqb18,
    "pqb18_yes" = pscreen_pqb18_yes,
    "pqb19" = pscreen_pqb19,
    "pqb19_yes" = pscreen_pqb19_yes,
    "pqb20" = pscreen_pqb20,
    "pqb20_yes" = pscreen_pqb20_yes,
    "pqb21" = pscreen_pqb21,
    "pqb21_yes" = pscreen_pqb21_yes,
    "repro96" = NA
) %>% 
    as_tibble 

# 6) rbind old and new  --------------------------------------------------------------------------
new_df<-rbind(
    cbind(o_reformatted, "DataCollection"="old"),
    cbind(n_selected, "DataCollection"="new")
) %>% as_tibble

new_df %>% write.csv("Outputs/joinedCorrected_df.csv")
saveRDS(new_df, "Outputs/joinedCorrected_df.rds")