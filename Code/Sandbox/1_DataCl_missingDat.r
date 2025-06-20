#packages and read in files 
library(tidyverse)
library(dplyr)
library(ggpubr)
path="Figures"

dat<-read.csv("Outputs/merged_data_redcapNewAndOld.csv") #1436  397

###########################################################################################
#creating new cols using old and new redcap data to ensure data completion on key variables
###########################################################################################

### birth control ###

#making new column that merges the birthcontrol data 0/1 from old and new data - i.e., where missing it should add in the data where appropirate - still have 56 people missing
#but have more data overall. Will remove these 56 as further investigation showed they have none to v limited other data 
coalesceDat_bc <- dat %>%
  mutate(
    birthControl_oldANDnewData = coalesce(birth_control, birth_control.old)
  ) %>%
  #select(-ends_with(".old")) %>%
  filter(!is.na(birthControl_oldANDnewData)) #remove 56 people who still have NAs - and seem to have no other data now at 1380
#1380  398

### psychosis dx ###

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
    mutate(psychosisDXyesNo = nPsychosisDx > 0 ) #%>%
    #mutate(ageOfPsychosisDx_oldRC = rowSums(across(psychosisDXOldRC)) #- needs finishing - integrate old and new data for ages
    #1380  401

### mood dx ###

moodDXintegration <- psycohsisdx_cleaning %>%
    mutate(across(c(dx_19:dx_23), ~ replace_na(.,  0))) %>%
    mutate(across(c(mooddx1:mooddx6), ~ replace_na(.,  0))) %>% 
    mutate(moodDXOldRC = as.numeric(rowSums(across(c(dx_19:dx_23))) > 0)) %>% 
    mutate(nMoodDx = rowSums(across(c(mooddx1:mooddx6)))) %>%
    mutate(moodDXyesNo = nMoodDx > 0) #%>%
    #mutate(ageOfMoodDx_oldRC = rowSums(across(psychosisDXOldRC)) 
    #select(c(dx_19:dx_23, mooddx1:mooddx6, moodDXOldRC, nMoodDx, moodDXyesNo)) 

data<-moodDXintegration

###########################################################################################
# re-naming NAs where NA=0 or prefer not to say etc.,
###########################################################################################

dim(missDat)

irreg<- data %>% 
select(prescreendate, 
       noperiod, #both
       period, #just old?
       regular_periods, #old
       regularperiods, #new
       lastperiod, #new (date)
       last_period_date, #old (date) seems a lot missing possibly contingent
       length_period_days_v2, 
       period_increase, 
       irregular)  %>%
mutate(noperiod=as_factor(noperiod)) %>%
mutate(period=as_factor(period)) %>%
mutate(prescreendate_parsed = parse_date_time(prescreendate, orders = c("mdy", "dmy", "ymd"))) %>%
 mutate(
    lastPeriod_parsed = parse_date_time(lastperiod, orders = c("mdy", "dmy", "ymd"))) %>%
 mutate(REMOVE_noLastPeriodDate = is.na(lastPeriod_parsed)) %>% #summary() there are 30 people without this info 
mutate(postMenoNatural = factor(case_when(
    interval(lastPeriod_parsed, prescreendate_parsed) > years(1) ~ lastPeriod_parsed,
    TRUE ~ as.Date(NA)
  )))#%>%
#select(noperiod, postMenoNatural) %>%
#filter(noperiod==0 & !is.na(postMenoNatural))


checking <- irreg %>%
select(noperiod, period) %>%
summary()

filter(!is.na(last_period_date))

mergining_periodRelatedCols_oldAndNew <- irreg %>% 
  #select(regular_periods, regularperiods) %>%
  mutate(regularPeriodYesNo = factor(case_when(regular_periods == 1 | regularperiods == 1 ~ "Yes",
                                        regular_periods == 0 | regularperiods == 0 ~ "No"))) %>%
  mutate(stillHavePeriod = factor(case_when(noperiod == 0 | period == 0 ~ "No",
                                            noperiod == 1 | period == 1 ~ "Yes"))) %>%
  mutate(lastPeriod_parsed = parse_date_time(lastperiod, orders = c("mdy", "dmy", "ymd"))) %>%
  mutate(last_period_date = parse_date_time(last_period_date, orders = c("mdy", "dmy", "ymd"))) %>%
  mutate(lastPeriodData = coalesce(lastPeriod_parsed, last_period_date))


  mutate(lastPeriodData = case_when(!is.na(lastperiod) | !is.na(last_period_date ~ 
  filter(regularPeriodYesNo == 0 & stillHavePeriod == 0)



filter(is.na(regularPeriodYesNo))

irreg %>% filter(noperiod == )

irreg %>% select(regular_periods)  as.factor(regular_periods))

missDat <- data %>% 
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
    lastperiod,
    birthControl_oldANDnewData,
    nPsychosisDx,
    psychosisDXyesNo,
    nMoodDx,
    moodDXyesNo
    ) %>%
as_tibble() %>%
filter(complete.cases(.)) #still 1380! Yay

###########################################################################################
#compile data where NAs are meaningful and can be added post complete case?
###########################################################################################

### PQB data ###
pqb <- data %>% 
    select(starts_with("pqb"), record_id) 

# Identify columns ending with '.old'
old_cols <- names(pqb)[endsWith(names(pqb), ".old")]

for (old_col in old_cols) {
  # Determine the corresponding new column name
  new_col <- str_remove(old_col, "\\.old$")
  
  # If the new column exists, coalesce the values
  if (new_col %in% names(pqb)) {
    pqb[[new_col]] <- coalesce(pqb[[new_col]], pqb[[old_col]])
  } else {
    # If the new column doesn't exist, rename the old column
    names(pqb)[names(pqb) == old_col] <- new_col
  }
}

# Remove the '.old' columns
pqb <- pqb %>% select(-ends_with(".old"), record_id) # leaves around 35-37 people with NAs in their PQB data but differs by pqb number 

# checking the data of those missing PQB scores
t<-pqb %>% filter(is.na(pqb3)) %>% select(record_id)
filtered_data <- data[data$record_id %in% t$record_id, ] # seems they are missing all other data too so fine to be removed

pqb_number_cols <- pqb %>%
  select(matches("^pqb\\d+$"), record_id)

t <- pqb_number_cols %>%
  filter(rowSums(is.na(select(., starts_with("pqb")))) > 0.5 * ncol(select(., starts_with("pqb")))) %>%
  select(record_id)

filtered_data <- data[data$record_id %in% t$record_id, ] #they dont have pqb data (new or old projects) but they do have some other data

#write.csv(filtered_data, "Outputs/missingPQBdata.csv")

#remove people missing pqb data (and lots of other data) -- MOVING THIS TO EXCLUDE SCRIPT IN CASE OTHER DATA FOR THESE P'S IS NEEDED instead will create col to make this easier
#pqb_compcase <- pqb %>% filter(!.$record_id %in% t$record_id) #leaves 1343
#add in col that will make it easier to remove those missing PQB data
pqb_cldat <- pqb %>% 
mutate(REMOVEmissingPQDdat = pqb$record_id %in% t$record_id)

cmpltDat<- missDat %>% left_join(pqb_cldat) #1380

write.csv(cmpltDat, "Outputs/datacl_missingDat.csv")


