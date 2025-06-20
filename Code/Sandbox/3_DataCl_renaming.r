#packages and read in files 
library(tidyverse)
library(dplyr)
library(ggpubr)
library(lubridate)
path="Figures"

cmpltDat <- read.csv("Outputs/datacl_missingDat.csv")

## age ##
#there are 47 people either nissing a DOB or a date of study: 11 for dob and 29 for prescreened date 
# then 7 have 0's because the DOB is in 2025
# the rest of these peoples data is relatively intact
cmpltDat <- cmpltDat %>%
  mutate(
    dob_parsed = parse_date_time(dob, orders = c("mdy", "dmy", "ymd")),
    prescreendate_parsed = parse_date_time(prescreendate, orders = c("mdy", "dmy", "ymd")),
    age = floor(time_length(interval(dob_parsed, prescreendate_parsed), "years"))
  ) #%>% #summary()

ageNA <- cmpltDat %>% filter(age==0 | is.na(age))
dobNA <- cmpltDat %>% filter(age==0 | is.na(dob_parsed)) %>% select(record_id)

# I think we have to flag remove we anyone with a 0 or no DOB because theres no way of working backwards
# for the rest we could use todays date as at least a rough estimate... 

noDOBflagged <- cmpltDat %>%
    mutate(REMOVE_ageMissing = cmpltDat$record_id %in% dobNA$record_id)

datWithAgeEsts %>%  filter(is.na(prescreendate_parsed))

datWithAgeEsts <- noDOBflagged %>%
  mutate(
    prescreendate_parsed = if_else(is.na(prescreendate_parsed), today(), prescreendate_parsed)) %>%
  mutate(
    age = floor(time_length(interval(dob_parsed, prescreendate_parsed), "years"))
  )

#datWithAgeEsts %>% filter(is.na(age))


#1, partial hysterectomy
#2, Full/total hysterectomy
#3, oophorectomy
#4, Salpingectomy
#6, other surgery or procedure
#5, Yes but unknown which
#7, I have not had any of these procedures

#dependant on "yes" to 1-5/6 above
#hyster5  = age at ovarian or uterine procedure (numbers 4 or 6 must be endorsed to answer this one)
#hyster2 =age at hysterectomy/oopherectomy (number 1,2,3 must be endoresed to answer this)
#hyster4  = describe the ovarian or uterine procedure (hyster 6 must be endorsed)

renamDat <- noDOBflagged %>% 
mutate(HystCats = case_when(
    hyster1___7 == 1 ~ "None",
    (hyster1___6 | hyster1___5) == 1 ~ "Unknown surgery",
    hyster1___3 == 1 ~ "Oopherectomy",
    hyster1___2 == 1 ~ "Hysterectomy",
    hyster1___1 == 1 ~ "PartialHysterectomy",
    hyster1___4 == 1 ~ "Salpingectomy")) %>% #summary()
mutate(HystCats = factor(HystCats, levels = c("None", 
                                              "Unknown surgery",
                                              "Salpingectomy",
                                              "PartialHysterectomy", 
                                              "Hysterectomy",
                                              "Oopherectomy"), ordered=TRUE)) %>%
mutate( #dealing with natural language issue e.g., mutliple ages input into col
    hyster5_clean = str_extract(hyster5, "\\d+"),
    hyster2_clean = str_extract(hyster2, "\\d+"),
    hyster5_num = as.numeric(hyster5_clean), #note there is one 2015 in here that needs changing + out of 169 people with unknown surgey or salp - 73 people did not leave the age of the surgery
    hyster2_ageAtHystorOop = as.numeric(hyster2_clean) #note there is one 2015 in here that needs changing + out of 150 people with hyst/ooph, 6 people did not leave an age
  ) %>%
  mutate( # removes peopple that had no intervention but put an age (currently 4)
    remove_flag = (!is.na(hyster2_ageAtHystorOop) | !is.na(hyster5_num)) & HystCats == "None",
    REMOVEhysterAgeIncorrect = replace_na(remove_flag, FALSE)
  )

#decided to remove the 6 for hyster 2 and not use hyster 5
onlyoophetc <- renamDat %>% filter(HystCats %in% c("PartialHysterectomy", "Hysterectomy", "Oopherectomy")) #150 people
x<-onlyoophetc %>% select(record_id, hyster5_num, hyster2_ageAtHystorOop) %>% filter(is.na(hyster2_ageAtHystorOop)) %>% select(record_id) # 6 people missing ages (hyster2_ageAtHystorOop)

#adds in REMOVE col for missing age for hysterectomy 
renamDat_missingHystAge <- renamDat %>%
    mutate(REMOVE_missingHysterAge =  renamDat$record_id %in% x$record_id) 



### last period ###

lastperiod_missing <- renamDat_missingHystAge %>% 
 mutate(
    lastPeriod_parsed = parse_date_time(lastperiod, orders = c("mdy", "dmy", "ymd"))) %>%
 mutate(REMOVE_noLastPeriodDate = is.na(lastPeriod_parsed)) #%>% #summary() there are 30 people without this info 



### calc cols ###

#THIS IS WHERE IM UP TO - THIS CALC IS INTRODUCING NAS FOR SOME REASON 
#1380
dim(summaryDat) 

summaryDat <- lastperiod_missing %>%
    filter(!REMOVEmissingPQDdat == TRUE) %>% #1344
    filter(!REMOVEhysterAgeIncorrect == TRUE) %>% #1340
    filter(!REMOVE_missingHysterAge == TRUE) %>% #1334
    filter(!REMOVE_ageMissing == TRUE) %>% #1318
    filter(!REMOVE_noLastPeriodDate == TRUE) %>% #1291
    mutate(across(
            c("pqb1", "pqb2", "pqb3", "pqb4", "pqb5", 
            "pqb6", "pqb7", "pqb8", "pqb9", "pqb10",
            "pqb11", "pqb12", "pqb13", "pqb14", "pqb15",
            "pqb16", "pqb17", "pqb18", "pqb19", "pqb20",
            "pqb21"),
            ~ replace_na(., 0))) %>%
    mutate(totalPqb = rowSums(across(c("pqb1", "pqb2", "pqb3", "pqb4", "pqb5", 
                                       "pqb6", "pqb7", "pqb8", "pqb9", "pqb10",
                                       "pqb11", "pqb12", "pqb13", "pqb14", "pqb15",
                                       "pqb16", "pqb17", "pqb18", "pqb19", "pqb20",
                                       "pqb21")))) %>% 
    mutate(totalDistressPqb = rowSums(across(c("pqb1_yes", "pqb2_yes", "pqb3_yes", "pqb4_yes", "pqb5_yes", 
                                       "pqb6_yes", "pqb7_yes", "pqb8_yes", "pqb9_yes", "pqb10_yes",
                                       "pqb11_yes", "pqb12_yes", "pqb13_yes", "pqb14_yes", "pqb15_yes",
                                       "pqb16_yes", "pqb17_yes", "pqb18_yes", "pqb19_yes", "pqb20_yes",
                                       "pqb21_yes")))) %>%
    mutate(ageFirstInt = min(hyster5_num, hyster2_ageAtHystorOop, na.rm=TRUE)) %>%
    mutate(ageFirstInt = case_when(is.infinite(ageFirstInt) ~ NA, .default = ageFirstInt)) %>%
    mutate(race = factor(case_when(
        race___1 == 1 ~ "race___1",
        race___2 == 1 ~ "race___2",
        race___3 == 1 ~ "race___3",
        race___4 == 1 ~ "race___4",
        race___5 == 1 ~ "race___5",
        race___8 == 1 ~ "race___8",
        race___9 == 1 ~ "race___9",
    TRUE ~ NA_character_
  ))) %>%  
  mutate(postMenoNatural = case_when(
      !is.na(lastPeriod_parsed) ~ (coalesce(prescreendate_parsed, today()) - lastPeriod_parsed) > 365,
      TRUE ~ NA))

   summaryDat %>%
  mutate(postMenoNatural = factor(case_when(
    interval(lastPeriod_parsed, prescreendate_parsed) > years(1) ~ lastPeriod_parsed,
    TRUE ~ as.Date(NA)
  ))) %>%
  as_tibble() %>% summary()

 x<-summaryDat %>%
  mutate(
    postMenoNatural = case_when(
      !is.na(lastPeriod_parsed) ~ (coalesce(prescreendate_parsed, today()) - lastPeriod_parsed) > 365,
      TRUE ~ NA
    )
  ) %>%
  filter(postMenoNatural) %>%
  select()

x %>% filter(is.na(postMenoNatural))

    mutate(postMenoNatural = lastPeriod_parsed )
    as_tibble()

reduSumDat <- summaryDat %>%
    select(as.factor(record_id),
            race,
            age,
            totalPqb,
            totalDistressPqb,
            HystCats,
            ageFirstInt,
            nMoodDx,
            moodDXyesNo,
            nPsychosisDx,
            psychosisDXyesNo,
            birthControl_oldANDnewData,
            )



############################### END

summaryDat %>% 
    select(c("pqb1_yes", "pqb2_yes", "pqb3_yes", "pqb4_yes", "pqb5_yes", 
                                       "pqb6_yes", "pqb7_yes", "pqb8_yes", "pqb9_yes", "pqb10_yes",
                                       "pqb11_yes", "pqb12_yes", "pqb13_yes", "pqb14_yes", "pqb15_yes",
                                       "pqb16_yes", "pqb17_yes", "pqb18_yes", "pqb19_yes", "pqb20_yes",
                                       "pqb21_yes")) # , "totalDistressPqb")) %>% 
    filter(is.na(totalPqb))


    mutate(totalDistressPqb = rowSums(across(c("pqb1_yes":"pqb21_yes")))) %>%
    mutate(multiHyst = rowSums(across(starts_with("hyster1___")))) %>%
    mutate(AgeFirstInt = min(hyster5_num, hyster2_ageAtHystorOop, na.rm=TRUE)) %>%
    mutate(AgeFirstInt = case_when(is.infinite(AgeFirstInt) ~ NA, .default = AgeFirstInt)) %>%
    as_tibble()

write.csv(summaryDat, "Outputs/tempTibble.csv")


summaryDat[summaryDat$totalPqb=="NA" ,]




#calc code



cmpltDat <- renamDat %>%
  mutate(
    dob_parsed = parse_date_time(dob, orders = c("mdy", "dmy", "ymd")),
    prescreendate_parsed = parse_date_time(prescreendate, orders = c("mdy", "dmy", "ymd")),
    age = floor(time_length(interval(dob_parsed, prescreendate_parsed), "years"))
  )

noprod<-  renamDat %>% filter(!HystCats %in% c( "None"))

onlyoophetc <- renamDat %>% filter(HystCats %in% c("PartialHysterectomy", "Hysterectomy", "Oopherectomy")) #150 people
onlyoophetc %>% select(hyster5_num, hyster2_ageAtHystorOop) %>% summary() # 6 people missing ages (hyster2_ageAtHystorOop)

onlyOthersurgs <- renamDat %>% filter(HystCats %in% c("Unknown surgery", "Salpingectomy")) #169 people
onlyOthersurgs %>% select(hyster5_num, hyster2_ageAtHystorOop) %>% summary() # 73 people missing ages (hyster5_num)

test<- cmpltDat %>% 
  mutate(HystCats = case_when(
    hyster1___7 == 1 & hyster1___6 == 0 ~ "None", 
    hyster1___3 == 1 ~ "Oopherectomy",
    hyster1___4 == 1 ~ "Salpingectomy",
    hyster1___2 == 1 ~ "Hysterectomy",
    hyster1___1 == 1 ~ "PartialHysterectomy",
    TRUE ~ "Other"
  ))

investigatingOther <- test %>% filter(HystCats=="Other") %>% select(HystCats,  hyster1___7,hyster1___6, hyster1___3, hyster1___4, hyster1___2, hyster1___1)

cmpltDat %>% select(hyster1___7,hyster1___6, hyster1___3, hyster1___4, hyster1___2, hyster1___1) %>% summary()

mutate(HystCats = factor(HystCats, levels = c("None", "PartialHysterectomy", "Hysterectomy", "Salpingectomy","Oopherectomy"), ordered=TRUE)) %>%
mutate( #dealing with natural language issue
    hyster5_clean = str_extract(hyster5, "\\d+"),
    hyster2_clean = str_extract(hyster2, "\\d+"),
    hyster5_num = as.numeric(hyster5_clean),
    hyster2_ageAtHystorOop = as.numeric(hyster2_clean)
  ) %>%
  mutate( # removes peopple that had no intervention but put an age (currently 3 so N now 1066)
    remove_flag = (!is.na(hyster2_ageAtHystorOop) | !is.na(hyster5_num)) & HystCats == "None",
    remove_flag = replace_na(remove_flag, FALSE)
  )

sum_dat <- renamDat %>%

