
#packages and read in files 
library(tidyverse)
library(dplyr)
library(ggpubr)
path="Figures"

#HRT<-read.csv("Data/bcDetails_with_HRT_excluding_BirthControl.csv")
allMedsDat<-read.csv("Outputs/medsData.csv")

datraw<-read.csv("Data/PREMAPIntegration_DATA_2025-01-28_1610.csv") 
alldat<-datraw[-1, ]
data <- alldat[alldat %>% select(starts_with("psychosis")) %>% rowSums(na.rm=TRUE) == 0,]


#########################
#investigating missing data 
#########################
# step 1: identify and deal with missing data 
    #birth_sex - NAs=missing have grouped with 2, Other/prefer not to say; one person answers 0 (male - have groups with 2)
    #ethnic_v1 - 773 people are NA, so I have grouped them with option 3,Unknown/prefer not to say
    #residence_country - 5 PEOPLE ID that they are from outside US, then 408 have NA's - have split into USA and other/prefer not to say
    #residence_state - 179 people say other, and 118 say NAs so have just group these all into 52 - Not applicalbe -NEEDS WORK
    #noperiod - 863 have NA, with 129 0 and 132 1 - after looking at redcap this qu is not required, so I think this would be true missing data unfortunately so left asNA
    #regularperiods - 471 have NA, with 492 0 and 161 1 - after looking at redcap this qu is not required, so I think this would be true missing data unfortunately so left as NA
    #birth_control - n97 NAs - this question is required on redcap - the NAs are not systematic - the question does not appear to be contingent on any other questions?
        #birth_control - looking at the rest of the data for people with NA in this col I think they may have not completed the screening
    #pregnancy - 100 have NA, with 1022- 0 and 2 - 1 - after looking at redcap this qu is not required AND not contingent on any other branching logic, so I think this would be true missing data unfortunately so left as NA. Changed to pregnant, not pregnant, prefer not to say however not clear if 1 or 0 related to pregnant vs not - but only 2 people for 1 - so have assumed this is pregnant - please correct me if wrong!
    #thyroidscreen - thyroidscreen variable NAs are everyone that did not tick "Endocrine or thyroid disease..." in the question "Do you have any of the following" - this i now a required question so technically could include missing data where someone could still have the disease. But I have taken all NAs in thyroidscreen and assigned to a new factor: 2 - this corresponds to NO THYROID DISEASE; then 0 and 1 will correspond to whether or not they are recieving treatment for the thyoird disease they do have - although form the meta data its unclear which way around this is. I also created a col for thyroid disease vs no thyroid disease using this information 
    #anascreen - is coming form the same grouped question as thyroid screeen - I just went ahead a made a column for yes/no eating disorder - theres only 1s and NAs here, so I cant tell whether the n10 that endose eating disorders have then picked in remission or not in remission. 
    #sudscreen - as with thyroid screen, here there are 3 options 0,1,NA - NAs clearly mean sud not endorsed, but its unclear is 0 or 1 are remirssion or no remission. As such Ive just made a new column for SUD vs n SUD
    #mooddx1-6 - systematic missingness n206; question is required  - query that these people were never ask
    #psychosis
    #sub<-alldat[alldat$mooddx1=="NA"]
    #hyster5 and hyster2 - ages for ovarian procedures and ages for ooph or hyst -- there are 3 people where there are ages when there is no procedure indicated ("none" in hystCats) - Im also unclear on this distinction between hhyster 2 and 5 

data %>% select(lastperiod) %>% mutate(as.factor(birth_control)) %>% summary()
NA_BC <- data %>% filter(is.na(birth_control))
write.csv(NA_BC, "Outputs/birthcontrol_NAdat.csv")

data %>% select(bc_details) %>% mutate(as.factor(bc_details)) %>% summary()
data %>% select(birth_control) %>% mutate(as.factor(birth_control)) %>% summary()

data %>% select(redcap_event_name) %>% mutate(as.factor(redcap_event_name)) %>% summary()



x<-data %>% select(birth_control) %>% mutate(as.factor(birth_control)) %>% print(n=1000)
##################################################
#dealing with missing data to maximise avail dat
##################################################

PBQ<-data %>% select(record_id,
    pqb1,pqb2,pqb3,pqb4,pqb5,pqb6,pqb7,pqb8,pqb9,pqb10,pqb11,
            pqb12,pqb13,pqb14,pqb15,pqb16,pqb17,pqb18,pqb19,pqb20,pqb21,
            pqb1_yes,pqb2_yes,pqb3_yes,pqb4_yes,pqb5_yes,pqb6_yes,pqb7_yes,
            pqb8_yes,pqb9_yes,pqb10_yes,pqb11_yes,
            pqb12_yes,pqb13_yes,pqb14_yes,pqb15_yes,pqb16_yes,pqb17_yes,pqb18_yes,
            pqb19_yes,pqb20_yes,pqb21_yes)

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
    lastperiod) %>%
as_tibble() %>%
filter(complete.cases(.)) #still 1069! Yay

cmpltDat<- missDat %>% left_join(PBQ) #1069


##########################
#renaming and formating for ease/variable needs
##########################

renamDat <-cmpltDat %>% 
mutate(HystCats = case_when(
    hyster1___7 == 1 & hyster1___6==0 ~ "None",
    hyster1___3 == 1 ~ "Oopherectomy",
    hyster1___4 == 1 ~ "Salpingectomy",
    hyster1___2 == 1 ~ "Hysterectomy",
    hyster1___1 == 1 ~ "PartialHysterectomy",
    .default = NA)) %>%
mutate(HystCats = factor(HystCats, levels = c("None", "PartialHysterectomy", "Hysterectomy", "Salpingectomy","Oopherectomy"), ordered=TRUE)) %>%
mutate( #dealing with natural language issue
    hyster5_clean = str_extract(hyster5, "\\d+"),
    hyster2_clean = str_extract(hyster2, "\\d+"),
    hyster5_num = as.numeric(hyster5_clean),
    hyster2_num = as.numeric(hyster2_clean)
  ) %>%
  mutate( # removes peopple that had no intervention but put an age (currently 3 so N now 1066)
    remove_flag = (!is.na(hyster2_num) | !is.na(hyster5_num)) & HystCats == "None",
    remove_flag = replace_na(remove_flag, FALSE)
  )# %>%  
#rename(., "ageAtOtherOvarianProcedure" = "hyster5") %>%
#rename(., "ageAtHystOrOoph" = "hyster2") %>%

##########################
#Removing/filtering
##########################           
#hyster 2 and 5 issues: age at procedures 
## issue 1: free text - some people inputting multiple numbers or written text
### solution: taking the first value indicated - however we may want to remove these entierly

#psychosis dx removed uptop 

cldat <- renamDat %>% 
  filter(!remove_flag) %>%
  select(-remove_flag) %>%
  filter(!pregnancy=="pregnant") 

##########################
#summarising inc. calc cols and filtering out 
##########################


sumDat <- cldat %>%
mutate(totalPqb = rowSums(across(c("pqb1":"pqb21")))) %>%
    mutate(totalDistressPqb = rowSums(across(c("pqb1_yes":"pqb21_yes")))) %>%
    mutate(multiHyst = rowSums(across(starts_with("hyster1___")))) %>% 
rowwise() %>%
    mutate(AgeFirstInt = min(hyster5, hyster2, na.rm=TRUE)) %>%
    mutate(AgeFirstInt = case_when(is.infinite(AgeFirstInt) ~ NA, .default = AgeFirstInt)) %>%
    mutate(age = as.numeric((mdy(prescreendate) - mdy(dob)) / 365)) %>%
    ungroup() 

# others to include: last period - days since, then >12mths yes/no
# some way of IDing perimenopause
# was surgery before menopause  - yes/no
    #lastperiod - date/age of surgery 


### if the women had the surgery pre age 50 and/or the menopause she is unlikely to have been given HRT

##########additional cols that need adding:
###are they in menopause? - use lastperiod col date - meonpause defined as no period for 12 months and right age range? 
###was surgery before menopause  - yes/no
    #lastperiod - date/age of surgery 

###are they in perimenopause - #
    #regular period yes/no 
    # irregularperiods col - currently free text - would need to decide what would constitute perimenopause

### how does the perimenopause relate to surgery timing

###was mood dx onset before menopause, perimenoause, or surgery?
###mood disroder of anykind - yes/no
###add in all race but need to make into one column with value for everyone
###could inclduye residence_state as over 1000 entries
###need to add in other medication - again FREE TEXT NEEDS SORTING OUT
###thyroidscreen column - can discern if people have a thyroid disease and whether they're recieving treatment
    #here NAs do not constitue missing data but are the answer No

###there are also anorexia and SUD - where NAs are not missing but indicate no disorder
    #n is v low for both, plus would only want to look at currently active


######################## potentially need removing???
#exclusion_dx2___4 et.,c if a 1 does this mean they should be removed? 