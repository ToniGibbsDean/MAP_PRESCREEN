library(tidyverse)
library(dplyr)
library(ggpubr)
path="Figures"
x<-readRDS("Outputs/joinedCorrected_df.rds")
a<-read.csv("Outputs/joinedCorrected_df.csv")

### #birth_sex ###

birthsex <- a %>%
    #filter(!birth_sex==0) %>%
    mutate(birth_sex = factor(case_when(is.na(birth_sex) ~ "PreferNotToSay/other/NA",
                                        birth_sex == 2 ~ "PreferNotToSay/other",
                                        birth_sex == 0 ~ "male",
                                        TRUE ~ "female"))) %>% 
    filter(!birth_sex=="male") #still not right - male still showing up no matter which way around i do the filtering 

#birth_sex - NAs=138 (New missing = 133; old missing = 5); male=2; preferNotToSay =2 --> missing have grouped with 2, Other/prefer not to say; one person answers 0 (male - have groups with 2)
#EXCLUDED 2 for being male 

### age ###

age <- birthsex %>%
  mutate(
    dob_parsed = parse_date_time(dob, orders = c("mdy", "dmy", "ymd")),
    prescreendate_parsed = parse_date_time(prescreendate, orders = c("mdy", "dmy", "ymd")),
    age = floor(time_length(interval(dob_parsed, prescreendate_parsed), "years"))
  ) #%>% #summary()

ageNA <- age %>% filter(age==0 | is.na(age)) %>% select(age, dob,prescreendate,DataCollection )

# There are 21 people where this age calc goes awry - 13 of these is because the dob and prescreen date is the same, the other are iether missing
#the dob (4), or the prescreen data (4)
#to rectify - could go back and include: "age" from old data - may resolve the 7 people from old with 0 as age; could also include time stamp to rectify the missing prescreen dates 

#HAVE NOT FILTERED THESE PEOPLE OUT YET IN CASE SOME DATA CAN BE SUPPLEMENTED AS SUCH AS FURTHER CALCS USING PRESCREEN DATE MAY STILL SHOW MISSING


### state ###

# is that NAs could be people outside of the country - cant tell as varialbe from old project "pscreen_country" not included and data from
# old project on the state variable is contigent on that qu. Same for new project - variable this is contingent on is:  residence_country 
# could just assume that the 42 people with NA have answered that they dont actually live in the US and make col US vs Not US using this
# actually the the states would be hard to inc in an analysis anyway because its so skewed with lots of 1's 

country <- age %>%
mutate(country_US = case_when(is.na(residence_state) ~ "NotUSA",
                              TRUE ~ "USA")) %>%
mutate(country_US = as_factor(country_US)) %>%
filter(!country_US == "NotUSA")

### zipcode ###

# no NAs - some inconsistentcies - v skewed so  could not use as a factor
zipcode <- country %>%
mutate(zipcode = as_factor(residence_zipcode))


### last period ###
lastperiod <- zipcode %>%
 mutate(lastperiod_parsed = parse_date_time(lastperiod, orders = c("mdy", "dmy", "ymd")),
        lastperiod_daysSince = floor(time_length(interval(lastperiod_parsed, prescreendate_parsed), "days")))

lastperiod_filtered %>% select(lastperiod_daysSince) %>% summary()

#### this code shows that there are like 15 people where the date they gave for their last period is forward in time
lastperiod %>%
  filter(lastperiod_daysSince < 0 | lastperiod_daysSince > 20000) %>%
  select(lastperiod, prescreendate_parsed, lastperiod_parsed, lastperiod_daysSince, DataCollection)


x<-lastperiod %>%
  mutate(
    # Fix obvious year typos
    lastperiod_parsed_fixed = case_when(
      year(lastperiod_parsed) < 1920 ~ update(lastperiod_parsed, year = year(lastperiod_parsed) + 800),  # e.g., 1124 → 1924 or 2024
      year(lastperiod_parsed) > 2035 ~ update(lastperiod_parsed, year = year(lastperiod_parsed) - 1000), # e.g., 3023 → 2023
      TRUE ~ lastperiod_parsed
    ),
    
    # Recalculate days since using corrected date
    lastperiod_daysSince = floor(time_length(interval(lastperiod_parsed_fixed, prescreendate_parsed), "days"))
  )

  x %>%
  filter(lastperiod_daysSince < 0 | lastperiod_daysSince > 20000) %>%
  select(lastperiod, prescreendate_parsed, lastperiod_parsed, lastperiod_daysSince)

lastperiod_filtered <- lastperiod %>%
filter(!is.na(lastperiod_daysSince)) %>% 
filter(!lastperiod_daysSince < 0 | lastperiod_daysSince > 20000) %>%
mutate(menopausalStatus = lastperiod_daysSince>365) #1325


## there are 4 people getting nas becuase prescreen date is missing, and 36 because theres no last period date
#this variable is mandatory and not contingent - it is unclear how NAs exist. 
# not really other info to use to supplement this so I think they will have to be removed 

### birthcontrol ###

birthcontrol <- lastperiod_filtered %>%
filter(!is.na(birth_control)) %>%
mutate(birth_control = factor(case_when(birth_control == 0 ~ "No",
                                       birth_control == 1 ~ "Yes"))) #%>% select(birth_control) %>% group_by(birth_control) %>% summarise(n())
#ISSUE with this variable is that for OLD data birthcontrol question does not include HRT prompt - old n=737, new n=598

lastperiod_filtered %>% filter(birth_control=="Yes") %>% group_by(bc_details) %>% summarise(n())

### no period ### unusable
# mandatory in new but not in old - not contingent on other questions 
# questions slightly different: old= do you still have a period; new = has it been over one year since your last period (both not contingent); 

birthcontrol %>%
#select(noperiod, DataCollection) %>% 
group_by(noperiod) %>%
filter(DataCollection=="old") %>% filter(is.na(noperiod)) %>%
summarise(n()) 

#write.csv(NAnoperiod, "Outputs/NAnoperiod.csv") # people have a good amoutn of other data 

# 324 missing (NA) - Old data contains 322 of the NAs
# I think this variable is unusable - will confirm

### regular period ### unusuable

birthcontrol %>% 
select(regularperiods, DataCollection) %>%
group_by(regularperiods) %>%
filter(DataCollection=="old") %>%
summarise(n())

# 515 NAs split evening across new and old projects - also contingent on noperiod, which itself has a large amount of unexplained misisng, meaning the NAs are hard to intepret
# I dont think we can use this data

### head injusy/seizre ###
 
 #head unusable
birthcontrol %>% 
select(head, DataCollection) %>%
group_by(head) %>%
filter(DataCollection=="old") %>%
summarise(n())#total Nas are 636 - most come from old data (621) - not related to any other field - I think unusable 


seizure <- birthcontrol %>%
mutate(seizure = factor(case_when(seizure == 1 ~ "Yes", 
                                  seizure == 0 ~ "No"))) #%>% 

### excludsionary med dx's

exclDx <- seizure %>% 
mutate(Endocrine_or_thyroid_Disease = factor(exclusion_dx1___1),
       PCOS = factor(exclusion_dx1___2),
       MEN_IorII = factor(exclusion_dx1___3),
       EatingDisorder = factor(exclusion_dx1___6),
       SubstanceUseDisorder = factor(exclusion_dx1___8)) 

### pregnancy 

pregnancy <- exclDx %>% 
filter(!is.na(pregnancy)) %>%
mutate(pregnant = factor(case_when(pregnancy == 1 ~ "Yes",
                                   pregnancy == 0 ~ "No")))

### PQB total ###

PQBtotal <- pregnancy %>% 
mutate(totalPqb = rowSums(across(c("pqb1", "pqb2", "pqb3", "pqb4", "pqb5", 
                                       "pqb6", "pqb7", "pqb8", "pqb9", "pqb10",
                                       "pqb11", "pqb12", "pqb13", "pqb14", "pqb15",
                                       "pqb16", "pqb17", "pqb18", "pqb19", "pqb20",
                                       "pqb21")))) %>%
filter(!is.na(totalPqb))

 #NEED TO ADD IN PQB DISTRESS- BUT THESE HAVE NECESSARY NAs - SO WILL ADD AT THE END

###hysterectomy ###

psycohsisdx_cleaning <- PQBtotal %>% 
mutate(HystCats = factor(case_when(
    hyster1___7 == 1 ~ "None",
    hyster1___3 == 1 ~ "Oopherectomy",
    hyster1___2 == 1 ~ "FullHysterectomy",
    hyster1___1 == 1 ~ "PartialHysterectomy",
    hyster1___4 == 1 ~ "Salpingectomy",
    hyster1___6 == 1 ~ "OtherGyneSurgery",
    hyster1___5 == 1 ~ "Unknown",
    .default = "None"))) %>% # select(HystCats) %>% summary()
#HAVE GROUPED NAS HERE select(HystCats) %>% summary()
mutate(HystCats = factor(HystCats, levels = c("None", 
                                              "Unknown",
                                              "OtherGyneSurgery",
                                              "Salpingectomy",
                                              "PartialHysterectomy", 
                                              "FullHysterectomy",
                                              "Oopherectomy"), ordered=TRUE)) #%>%
#mutate( #dealing with natural language issue e.g., mutliple ages input into col
    #hyster5_clean = str_extract(hyster5, "\\d+"),
 #   hyster2_clean = str_extract(hyster2, "\\d+"),
    #hyster5_num = as.numeric(hyster5_clean), #note there is one 2015 in here that needs changing + out of 169 people with unknown surgey or salp - 73 people did not leave the age of the surgery
    #hyster2_ageAtHystorOop = as.numeric(hyster2_clean)) %>%
    #filter(!hyster2_ageAtHystorOop > 100)     %>% #note there is one 2015 in here that needs changing + out of 150 people with hyst/ooph, 6 people did not leave an age
  #mutate( # removes peopple that had no intervention but put an age (currently 4)
    #remove_flag = (!is.na(hyster2_ageAtHystorOop) & HystCats == "None")
  #) #%>% select(HystCats, hyster2_clean, hyster2_ageAtHystorOop, remove_flag) %>% group_by(remove_flag) %>% summarise(n())

#hyster %>% select(hyster2_ageAtHystorOop, HystCats) %>% filter(HystCats=="None") %>% summary() 

#hyster %>% select(HystCats) %>% filter(!is.na(HystCats)) %>%   summary() 

##NB NOT including age at hysterectomy because its not mandatory, (is contingent) - even if people have selected a surgery, they may not have input an age
#this lack of consistency led to a lot of NAs - therefore on this pass Im not including it. 

### psychosis dx

psycohsisdx_cleaning<- hyster %>% 
    select(c(record_id, dx_10:dx_17)) %>% 
    mutate(across(c(dx_10:dx_13, dx_15:dx_17), ~ replace_na(.,  0))) %>%
   mutate(across(c(psychosisdx1:psychosisdx10), ~ replace_na(.,  0))) %>% 
  mutate(psychosisDXOldRC = as.numeric(rowSums(across(c(dx_10:dx_17))) > 0)) %>% 
    mutate(nPsychosisDx = rowSums(across(c(  
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

### mood dx ###
###Old code
moodDXintegration <- psycohsisdx_cleaning %>%
    #mutate(across(c(dx_19:dx_23), ~ replace_na(.,  0))) %>%
    mutate(mooddx3 = as.numeric(mooddx3)) %>%
     mutate(mooddx6 = as.numeric(mooddx6)) %>%
    mutate(across(c(mooddx1:mooddx6), ~ replace_na(.,  0))) %>% 
    #mutate(moodDXOldRC = as.numeric(rowSums(across(c(dx_19:dx_23))) > 0)) %>% 
    mutate(nMoodDx = rowSums(across(c(mooddx1:mooddx6)))) %>%
    mutate(moodDXyesNo = nMoodDx > 0) 


###new code in progress
c<-  psycohsisdx_cleaning %>% 
select(mooddx1:mooddx6,DataCollection) %>% #group_by(DataCollection) %>% summarise(n())
filter(DataCollection=="old") %>% summary()

o<- read.csv("Data/OLDPREMAPPreScreenin-SupplementDataToUpda_DATA_2025-05-16_1218.csv") %>% 
    as_tibble %>%
    filter(pscreen_name!="") %>%
    select(pscreen_dx_25, pscreen_dx_51,
pscreen_dx_19,
pscreen_dx_20,
pscreen_dx_25,
pscreen_dx_21)

##lookinng at the old data for mooddx3 i.e., pscreen_dx_25,51,19,,20,21 etc, the data is sometimes an integrer and other times
#characters - so for those that are characters the as.numeric is throwing an error - so will change the relevant cols here - I have
#surpressed the warning for as.numeric function because there isnt a NA argument option, and it automatically puts non coercible data
#as NAs - so its work and the warnings are irrelevant 

psycohsisdx_cleaning %>% 
 filter(DataCollection=="old") %>%



#DIM 719
oldMoodDat<- psycohsisdx_cleaning %>% 
 filter(DataCollection=="old") %>%
 #select(mooddx1:mooddx6) %>% 
 #mutate(mooddx3 = as.integer(mooddx3)) %>% summary()
 mutate("BPDI_age" = as.numeric(mooddx1)) %>%
 mutate("BPDII_age" = as.numeric(mooddx2)) %>%
 mutate("MDD_Dep_otherDep_age" = suppressWarnings(as.numeric(mooddx3))) %>% 
 mutate("CyclothymicDep_age" = as.numeric(mooddx4)) %>%
 mutate("BPD_related_onlyNewRedCap_age" = as.numeric(mooddx5)) %>%
 mutate("Other_dep_onlyNewRedCap_age" = as.numeric(mooddx6)) %>% #summary()
 mutate("Schizophrenia_Spectrum_Disorder_age" = as.numeric(psychosisdx1)) %>%
 mutate("Delusional_Disorder_age" = as.numeric(psychosisdx2)) %>%
 mutate("Brief_Psychotic_Disorder_age" = as.numeric(psychosisdx3)) %>% 
 mutate("Schizophreniform_Disorder_age" = as.numeric(psychosisdx4)) %>%
 mutate("Schizoaffective_Disorder_age" = as.numeric(psychosisdx5)) %>%
 mutate(psychosisPD_combined_Age = as.numeric(coalesce(psychosisdx6, psychosisdx7, psychosisdx8))) %>% 
 mutate("catatonia_age" = as.numeric(psychosisdx9)) %>% 
 mutate("OtherPsychosis_age" = as.numeric(psychosisdx10)) %>% #summary()

 #add a 1 in mooddx1 when there is a number in this column (rather than the age)
 mutate(BPDI = factor(case_when(as.numeric(mooddx1) >= 1 ~ 1,
                                       is.na(mooddx1) ~ 0,
                                       TRUE ~ 0))) %>%
 mutate(BPDII = factor(case_when(as.numeric(mooddx2) >= 1 ~ 1,
                                       is.na(mooddx2) ~ 0,
                                       TRUE ~ 0))) %>%
 mutate(MDD_Dep_otherDep = factor(suppressWarnings(case_when(as.numeric(mooddx3) >= 1 ~ 1,
                                       is.na(mooddx3) ~ 0,
                                       TRUE ~ 0)))) %>% 
 mutate(CyclothymicDep = factor(case_when(as.numeric(mooddx4) >= 1 ~ 1,
                                       is.na(mooddx4) ~ 0,
                                       TRUE ~ 0))) %>%
 mutate(BPD_related_onlyNewRedCap = factor(case_when(as.numeric(mooddx5) >= 1 ~ 1,
                                       is.na(mooddx5) ~ 0,
                                       TRUE ~ 0))) %>%
 mutate(Other_dep_onlyNewRedCap = factor(case_when(as.numeric(mooddx6) >= 1 ~ 1,
                                       is.na(mooddx6) ~ 0,
                                       TRUE ~ 0))) %>%

 mutate(Schizophrenia_Spectrum_Disorder = factor(case_when(as.numeric(psychosisdx1) >= 1 ~ 1,
                                       is.na(psychosisdx1) ~ 0,
                                       TRUE ~ 0))) %>%
 mutate(Delusional_Disorder = factor(case_when(as.numeric(psychosisdx2) >= 1 ~ 1,
                                       is.na(psychosisdx2) ~ 0,
                                       TRUE ~ 0))) %>%
 mutate(Brief_Psychotic_Disorder = factor(suppressWarnings(case_when(as.numeric(psychosisdx3) >= 1 ~ 1,
                                       is.na(psychosisdx3) ~ 0,
                                       TRUE ~ 0)))) %>% 
 mutate(Schizophreniform_Disorder = factor(case_when(as.numeric(psychosisdx4) >= 1 ~ 1,
                                       is.na(psychosisdx4) ~ 0,
                                       TRUE ~ 0))) %>%
 mutate(Schizoaffective_Disorder = factor(case_when(as.numeric(psychosisdx5) >= 1 ~ 1,
                                       is.na(psychosisdx5) ~ 0,
                                       TRUE ~ 0))) %>%
 mutate(psychosisPD_combined = factor(pmax(psychosisdx6, psychosisdx7, psychosisdx8, na.rm = TRUE))) %>%

mutate(catatonia = factor(case_when(as.numeric(psychosisdx9) >= 1 ~ 1,
                                       is.na(psychosisdx9) ~ 0,
                                       TRUE ~ 0))) %>%
mutate(OtherPsychosis = factor(case_when(as.numeric(psychosisdx10) >= 1 ~ 1,
                                       is.na(psychosisdx10) ~ 0,
                                       TRUE ~ 0)))# %>% summary()



####YOU ARE HERE TRYING TO FIGURE OUT THE NEW DATA MOOD DX I) AGE AND II) YES/NO
newcheck <- psycohsisdx_cleaning %>% #dim540
 filter(DataCollection=="new") %>%
 #select(mooddx1:mooddx6, age_mooddx1:age_mooddx6) %>% #summary()
 mutate(BPDI = factor(case_when(mooddx1 == 0 | mooddx1 == 3 ~ 0,
                            mooddx1 == 1 ~ 1,
                                       TRUE ~ 0))) %>%
 mutate(BPDII = factor(case_when(mooddx2 == 0 | mooddx1 == 3 ~ 0,
                            mooddx2 == 1 ~ 1,
                                       TRUE ~ 0))) %>%
 mutate(MDD_Dep_otherDep = factor(case_when(mooddx3 == 0 | mooddx1 == 3 ~ 0,
                            mooddx3 == 1 ~ 1,
                                       TRUE ~ 0))) %>%
 mutate(CyclothymicDep = factor(case_when(mooddx4 == 0 | mooddx1 == 3 ~ 0,
                            mooddx4 == 1 ~ 1,
                                       TRUE ~ 0))) %>%
 mutate(BPD_related_onlyNewRedCap = factor(case_when(mooddx5 == 0 | mooddx1 == 3 ~ 0,
                            mooddx5 == 1 ~ 1,
                                       TRUE ~ 0))) %>%
 mutate(Other_dep_onlyNewRedCap = factor(case_when(mooddx6 == 0 | mooddx1 == 3 ~ 0,
                            mooddx6 == 1 ~ 1,
                                       TRUE ~ 0))) %>% #summary()
 mutate("BPDI_age" = as.numeric(age_mooddx1)) %>%
 mutate("BPDII_age" = as.numeric(age_mooddx2)) %>% #group_by(BPDII_age) %>% summarise(n()) %>% print(n=100)
 mutate("MDD_Dep_otherDep_age" = suppressWarnings(as.numeric(age_mooddx3))) %>% 
 mutate("CyclothymicDep_age" = as.numeric(age_mooddx4)) %>%
 mutate("BPD_related_onlyNewRedCap_age" = as.numeric(age_mooddx5)) %>%
 mutate("Other_dep_onlyNewRedCap_age" = as.numeric(age_mooddx6)) %>% #summary()
#filter(BPDII_age <= 100 | is.na(BPDII_age)) %>% summary()
  filter(
    (BPDII_age <= 100 | is.na(BPDII_age)) &
    (MDD_Dep_otherDep_age <= 100 | is.na(MDD_Dep_otherDep_age)) &
    (Other_dep_onlyNewRedCap_age <= 100 | is.na(Other_dep_onlyNewRedCap_age))
  ) %>% #%>% summary() 
 
 mutate(Schizophrenia_Spectrum_Disorder = factor(case_when(psychosisdx1 == 0 | psychosisdx1 == 3 ~ 0,
                            psychosisdx1 == 1 ~ 1,
                                       TRUE ~ 0))) %>%
 mutate(Delusional_Disorder = factor(case_when(psychosisdx2 == 0 | psychosisdx2 == 3 ~ 0,
                            psychosisdx2 == 1 ~ 1,
                                       TRUE ~ 0))) %>%
 mutate(Brief_Psychotic_Disorder = factor(case_when(psychosisdx3 == 0 | psychosisdx3 == 3 ~ 0,
                            psychosisdx3 == 1 ~ 1,
                                       TRUE ~ 0))) %>%
 mutate(Schizophreniform_Disorder = factor(case_when(psychosisdx4 == 0 | psychosisdx4 == 3 ~ 0,
                            psychosisdx4 == 1 ~ 1,
                                       TRUE ~ 0))) %>%
 mutate(Schizoaffective_Disorder = factor(case_when(psychosisdx5 == 0 | psychosisdx5 == 3 ~ 0,
                            psychosisdx5 == 1 ~ 1,
                                       TRUE ~ 0))) %>%
 mutate(psychosisPD_coalesced = as.numeric(coalesce(psychosisdx6, psychosisdx7, psychosisdx8))) %>% 
 mutate(psychosisPD_combined = factor(case_when(psychosisPD_coalesced == 0 | psychosisPD_coalesced == 3 ~ 0,
                            psychosisPD_coalesced == 1 ~ 1,
                                       TRUE ~ 0))) %>% #summary()
 mutate(catatonia = factor(case_when(psychosisdx9 == 0 | psychosisdx9 == 3 ~ 0,
                            psychosisdx9 == 1 ~ 1,
                                       TRUE ~ 0))) %>% #summary()
 mutate(OtherPsychosis = factor(case_when(psychosisdx10 == 0 | psychosisdx10 == 3 ~ 0,
                            psychosisdx10 >= 1 ~ 1,
                                       TRUE ~ 0))) %>% #summary()

 mutate("Schizophrenia_Spectrum_Disorder_age" = as.numeric(age_psychosisdx1)) %>%
 mutate("Delusional_Disorder_age" = as.numeric(age_psychosisdx2)) %>% #group_by(BPDII_age) %>% summarise(n()) %>% print(n=100)
 mutate("Brief_Psychotic_Disorder_age" = suppressWarnings(as.numeric(age_psychosisdx3))) %>% 
 mutate("Schizophreniform_Disorder_age" = as.numeric(age_psychosisdx4)) %>%
 mutate("Schizoaffective_Disorder_age" = as.numeric(age_psychosisdx5)) %>%
  mutate("psychosisPD_combined_Age" = as.numeric(coalesce(age_psychosisdx6, age_psychosisdx7, age_psychosisdx8))) %>% 
 mutate("catatonia_age" = as.numeric(age_psychosisdx9)) %>%
 mutate("OtherPsychosis_age" = as.numeric(age_psychosisdx10)) %>% #summary()
   filter(
    (Brief_Psychotic_Disorder_age <= 100 | is.na(Brief_Psychotic_Disorder_age)) &
    (OtherPsychosis_age >= 100 | is.na(OtherPsychosis_age))) #dim538

#DIM540                      
                            
#JOIN VERTICALLY and add summary cols
allMoodDat <- bind_rows(oldMoodDat, newcheck) %>%
  select(-starts_with("mooddx"), -starts_with("age_mooddx")) %>%
  select(-starts_with("psychosisdx"), -starts_with("age_psychosisdx")) %>%
  mutate(moodDXyesNo = rowSums(across(c(BPDI, BPDII, MDD_Dep_otherDep, CyclothymicDep, Other_dep_onlyNewRedCap, BPD_related_onlyNewRedCap)) == 1, na.rm = TRUE) > 0) %>%
  mutate(psychosisDXyesNo = rowSums(across(c(Schizophrenia_Spectrum_Disorder, Delusional_Disorder,Brief_Psychotic_Disorder, Schizophreniform_Disorder,Schizoaffective_Disorder,psychosisPD_combined, catatonia,OtherPsychosis )) == 1, na.rm = TRUE) > 0)

write.csv(allMoodDat, "Outputs/allMoodDatOLDCODETEST")

###SOMETHING GOING WRONG GETTING WARNINGS

summarydat <- moodDXintegration %>%
select(DataCollection,
       age, #2 NAs
       birth_sex,
       country_US,
       zipcode,
       lastperiod_daysSince,
       menopausalStatus,
       Endocrine_or_thyroid_Disease,
       PCOS,
       MEN_IorII,
       EatingDisorder,
       SubstanceUseDisorder,
       birth_control,
       seizure,
       pregnant,
        totalPqb,
        HystCats,
        #hyster2_ageAtHystorOop,
        #remove_flag,
        psychosisDXyesNo,
        moodDXyesNo,
        BPDI,
        BPDII,
        MDD_Dep_otherDep,
        CyclothymicDep,
        BPD_related_onlyNewRedCap,
        Other_dep_onlyNewRedCap,
        Schizophrenia_Spectrum_Disorder,
        Delusional_Disorder,
        Brief_Psychotic_Disorder,
        Schizophreniform_Disorder,
        Schizoaffective_Disorder,
        psychosisPD_combined,
        catatonia,
        OtherPsychosis
       ) %>%

filter(complete.cases(.))

summaryDatWithNAdat <- 

write.csv(summarydat, "Outputs/summarydatincomplete.csv")

## variables with NAs - pqb distress, hysterage - also hyster age super incomplete because some people give ages when no surgery and surgery with no ages 
## meno stage