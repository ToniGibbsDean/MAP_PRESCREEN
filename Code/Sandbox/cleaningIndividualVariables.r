#packages and read in files 
library(tidyverse)
library(dplyr)
library(ggpubr)
path="Figures"
x<-readRDS("Outputs/joinedCorrected_df.rds")
a<-read.csv("Outputs/joinedCorrected_df.csv")

###### Variables needed for model
#*note that now data has been combined "old project" col headings have been changed to correspond with "new project" col headings (where possible)

#age: dob; prescreendate
#birth_sex: birth_sex
#state: residence_state
#zipcode: residence_zipcode
#Reproductive stage: lastperiod, birthcontrol, bc_details, noperiod, irregularperiod, regularperiod
#time since last period: lastperiod
#Hormone therapy:
#Hormone therapy type:
#anyGyneSurgery: old data: pscreen_hysterectomy + new data any hyster__1:6
#hysterectomy_type: hyster__1, hyster__2, hyster__3, hyster__4, hyster__5 (+ non corresponding options only in new project: hyster1__6, hyster__7)
#age of hysterectomy: hyster 2
#head injury: head, seizure 
#Excl_med_condits: exclusion_dx1___1-8
#mood_dx: mood_dx1, mood_dx2, mood_dx3, mood_dx4, mood_dx5, mood_dx6
#psychosis_dx: psychosisdx1:10
#pqb total: pqb1-21
#pqb disrtress: pqb1_yes: pqb21_yes

##SOME VARIABLES MISSING NEED ADDING TO SCRIPT BEFORE:a
# pregnancy - completely matches acorss new and old 

###

### #birth_sex ###

#birth_sex - NAs=138 (New missing = 133; old missing = 5); male=2; preferNotToSay =2 --> missing have grouped with 2, Other/prefer not to say; one person answers 0 (male - have groups with 2)
#EXCLUDED 2 for being male 

x %>% select(birth_sex,DataCollection) %>% group_by(as_factor(birth_sex)) %>% 
#filter(DataCollection=="old") %>% 
#filter(!birth_sex==0) %>%
summarise(n())

birthsex <- a %>%
    #filter(!birth_sex==0) %>%
    mutate(birth_sex = factor(case_when(is.na(birth_sex) ~ "PreferNotToSay/other",
                                        birth_sex == 2 ~ "PreferNotToSay/other",
                                        birth_sex == 0 ~ "male",
                                        TRUE ~ "female"))) %>% 
    filter(!birth_sex=="male") #still not right - male still showing up no matter which way around i do the filtering 


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

age %>% select(residence_state,DataCollection) %>% 
group_by(as_factor(residence_state)) %>% 
#filter(DataCollection=="old") %>% split evenly 
summarise(n()) %>%
print(n=100)

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

country %>% select(residence_zipcode, DataCollection) %>% 
group_by(as_factor(residence_zipcode)) %>% 
#filter(DataCollection=="old") %>% split evenly 
summarise(n()) %>%
print(n=2000)

zipcode <- country %>%
mutate(zipcode = as_factor(residence_zipcode))

# no NAs - some inconsistentcies - v skewed so  could not use as a factor


### repr stage cols: lastperiod, birthcontrol, bc_details, noperiod, irregularperiod, regularperiod

# last period
lastperiod <- zipcode %>%
 mutate(lastperiod_parsed = parse_date_time(lastperiod, orders = c("mdy", "dmy", "ymd")),
        lastperiod_daysSince = floor(time_length(interval(lastperiod_parsed, prescreendate_parsed), "days")))

lastperiod %>%
select(lastperiod_daysSince, DataCollection) %>% 
group_by(lastperiod_daysSince) %>%
summarise(n()) %>% 
print(n=1000) #NA=40

## there are 4 people getting nas becuase prescreen date is missing, and 36 because theres no last period date
#this variable is mandatory and not contingent - it is unclear how NAs exist. 
# not really other info to use to supplement this so I think they will have to be removed 
lastperiod %>% 
filter(is.na(lastperiod_daysSince)) %>% 
select(lastperiod_parsed, prescreendate_parsed,lastperiod_daysSince,DataCollection )

lastperiod_filtered <- lastperiod %>%
filter(!is.na(lastperiod_daysSince))

### birthcontrol ###

lastperiod_filtered %>%
select(birth_control, DataCollection) %>% 
group_by(birth_control) %>%
summarise(n()) 

birthcontrol <- lastperiod_filtered %>%
filter(!is.na(birth_control)) %>%
mutate(birth_control = factor(case_when(birth_control == 0 ~ "No",
                                       birth_control == 1 ~ "Yes"))) #%>% select(birth_control) %>% group_by(birth_control) %>% summarise(n())

birthcontrol %>% 
select(birth_control, DataCollection) %>%
group_by(DataCollection) %>%
summarise(n())

#ISSUE with this variable is that for OLD data birthcontrol question does not include HRT prompt - old n=737, new n=598


### birth control details ##

# again, missing the prompt - but in process of being parsed in a different script due to natural language issues 

birthcontrol %>% 
select(bc_details, birth_control, DataCollection) %>%
group_by(bc_details) %>%
#reframe(n(), DataCollection) %>% #across both old and new proejcts
filter(bc_details=="" & birth_control == "Yes") 

#so 4 people say they are on birth control but then dont say what it is - will leave in but in the script handing this natural lanuage we
# need to make sure the the 1123 people that say they are not on birth control are indicated as such


### no period ###
# mandatory in new but not in old - not contingent on other questions 
# questions slightly different: old= do you still have a period; new = has it been over one year since your last period (both not contingent); 

NAnoperiod <- birthcontrol %>%
#select(noperiod, DataCollection) %>% 
filter(DataCollection=="yes") %>% #filter(is.na(noperiod)) %>%
group_by(noperiod) %>%
summarise(n()) 

write.csv(NAnoperiod, "Outputs/NAnoperiod.csv") # people have a good amoutn of other data 

# 324 missing (NA) - Old data contains 322 of the NAs
# I think this variable is unusable - will confirm

### regular period ###

birthcontrol %>% 
select(regularperiods, DataCollection,noperiod) %>%
group_by(regularperiods) %>%
filter(DataCollection=="old") %>%
filter(regularperiods==1|regularperiods==0 & noperiod==1) %>%
summarise(n())

birthcontrol %>% 
select(regularperiods, DataCollection) %>%
group_by(regularperiods) %>%
filter(DataCollection=="old") %>%
summarise(n())



# 515 NAs split evening across new and old projects - also contingent on noperiod, which itself has a large amount of unexplained misisng, meaning the NAs are hard to intepret
# I dont think we can use this data

### irregular period ###

birthcontrol %>% 
select(irregularperiods, DataCollection,noperiod, regularperiods) %>%
filter(DataCollection=="old") %>%
filter((!irregularperiods=="") & noperiod == 1) %>%
group_by(noperiod) %>%
summarise(n())


birthcontrol %>% 
select(irregularperiods, DataCollection,noperiod, regularperiods) %>%
#ilter(DataCollection=="old") %>%
filter((!irregularperiods=="") & regularperiods == 1) %>%
group_by(irregularperiods) %>%
summarise(n())

birthcontrol %>% 
select(irregularperiods, DataCollection,noperiod) %>%
group_by(irregularperiods) %>%
filter(DataCollection=="old") %>%
filter(irregularperiods==1|irregularperiods==0 & noperiod==1) %>%
summarise(n())
#1=yes it has been over a year


### head injury :seizure and head

birthcontrol %>% 
select(head, DataCollection) %>%
group_by(head) %>%
filter(DataCollection=="old") %>%
summarise(n())#total Nas are 636 - most come from old data (621) - not related to any other field - I think unusable 

birthcontrol %>% 
select(seizure, DataCollection) %>%
group_by(seizure) %>%
filter(DataCollection=="new") %>%
summarise(n()) # 12 NAs - all from new

seizure <- birthcontrol %>%
mutate(seizure = factor(case_when(seizure == 1 ~ "Yes", 
                                  seizure == 0 ~ "No"))) #%>% 
#select(seizure, DataCollection) %>%
#group_by(seizure) %>%
#summarise(n()) #57 people had seizures currently are left in
  

### excludsionary med dx's

seizure %>% 
select(exclusion_dx1___1:exclusion_dx1___8, DataCollection) %>% summary()
#group_by(exclusion_dx1___8) %>%
#filter(DataCollection=="old") %>%
summarise(n())

exclDx <- seizure %>% 
mutate(Endocrine_or_thyroid_Disease = factor(exclusion_dx1___1),
       PCOS = factor(exclusion_dx1___2),
       MEN_IorII = factor(exclusion_dx1___3),
       EatingDisorder = factor(exclusion_dx1___6),
       SubstanceUseDisorder = factor(exclusion_dx1___8)) 
# cannot use as only an option in new data
#missing chemotherapy and current steriods as these were only an option in old data 

exclDx %>%
select(Endocrine_or_thyroid_Disease,PCOS,  MEN_IorII,EatingDisorder, SubstanceUseDisorder ) %>%
summary()




### pregnancy 

exclDx %>% 
select(pregnancy, DataCollection) %>%
group_by(pregnancy) %>%
#filter(DataCollection=="new") %>%
summarise(n())

pregnancy <- exclDx %>% 
filter(!is.na(pregnancy)) %>%
mutate(pregnant = factor(case_when(pregnancy == 1 ~ "Yes",
                                   pregnancy == 0 ~ "No")))

pregnancy %>% select(pregnant) %>% group_by(pregnant) %>% summarise(n())

### PQB total ###

PQBtotal <- pregnancy %>% 
#select("pqb1", "pqb2", "pqb3", "pqb4", "pqb5", 
 #           "pqb6", "pqb7", "pqb8", "pqb9", "pqb10",
  #          "pqb11", "pqb12", "pqb13", "pqb14", "pqb15",
   #         "pqb16", "pqb17", "pqb18", "pqb19", "pqb20",
    #        "pqb21", DataCollection) %>%  #33-36 NAS - might not be fully overlapping 
#filter(!if_any(pqb1:pqb21, is.na)) %>%
mutate(totalPqb = rowSums(across(c("pqb1", "pqb2", "pqb3", "pqb4", "pqb5", 
                                       "pqb6", "pqb7", "pqb8", "pqb9", "pqb10",
                                       "pqb11", "pqb12", "pqb13", "pqb14", "pqb15",
                                       "pqb16", "pqb17", "pqb18", "pqb19", "pqb20",
                                       "pqb21")))) %>%
filter(!is.na(totalPqb))

 #NEED TO ADD IN PQB DISTRESS- BUT THESE HAVE NECESSARY NAs - SO WILL ADD AT THE END

################################################################################################
# not complete 
################################################################################################
### hysterectomy yes no

PQBtotal$hyster2

PQBtotal %>% 
filter(DataCollection=="old") %>%
mutate(HystCats = factor(case_when(
    hyster1___7 == 1 ~ "None",
    hyster1___3 == 1 ~ "Oopherectomy",
    hyster1___2 == 1 ~ "FullHysterectomy",
    hyster1___1 == 1 ~ "PartialHysterectomy",
    hyster1___4 == 1 ~ "Salpingectomy",
    hyster1___6 == 1 ~ "Other surgery",
    hyster1___5 == 1 ~ "Unknown surgery",
    TRUE ~ "NA"))) %>% 
filter(!HystCats=="None"| !HystCats=="NA") %>%
filter(hyster2=="") %>%
group_by(hyster2) %>%
summarise(n())

hyster <- PQBtotal %>% 
mutate(HystCats = factor(case_when(
    hyster1___7 == 1 ~ "None",
    hyster1___3 == 1 ~ "Oopherectomy",
    hyster1___2 == 1 ~ "FullHysterectomy",
    hyster1___1 == 1 ~ "PartialHysterectomy",
    hyster1___4 == 1 ~ "Salpingectomy",
    hyster1___6 == 1 ~ "Other surgery",
    hyster1___5 == 1 ~ "Unknown surgery",
    TRUE ~ "None"))) %>%  #HAVE GROUPED NAS HERE select(HystCats) %>% summary()
mutate(HystCats = factor(HystCats, levels = c("None", 
                                              "Unknown surgery",
                                              "Salpingectomy",
                                              "PartialHysterectomy", 
                                              "Hysterectomy",
                                              "Oopherectomy"), ordered=TRUE)) %>%
mutate( #dealing with natural language issue e.g., mutliple ages input into col
    #hyster5_clean = str_extract(hyster5, "\\d+"),
    hyster2_clean = str_extract(hyster2, "\\d+"),
    #hyster5_num = as.numeric(hyster5_clean), #note there is one 2015 in here that needs changing + out of 169 people with unknown surgey or salp - 73 people did not leave the age of the surgery
    hyster2_ageAtHystorOop = as.numeric(hyster2_clean) #note there is one 2015 in here that needs changing + out of 150 people with hyst/ooph, 6 people did not leave an age
  ) %>%
  mutate( # removes peopple that had no intervention but put an age (currently 4)
    remove_flag = (!is.na(hyster2_ageAtHystorOop) & HystCats == "None")
  ) #%>% select(HystCats, hyster2_clean, hyster2_ageAtHystorOop, remove_flag) %>% group_by(remove_flag) %>% summarise(n())

PQBtotal %>% 
select(hyster1___1, hyster1___2, hyster1___3, hyster1___4, hyster1___5, hyster1___6, hyster1___7, DataCollection) %>%
group_by(hyster1___6) %>%
#filter(DataCollection=="new") %>%
summarise(n())

### NEED TO IGNORE 7 - DO NOT PULL THROUGH TO SUMMARY

### hyterectomy type 

### hyst2: age of intervention 

exclDx %>%
select(hyster2,  DataCollection) %>%
group_by(hyster2) %>%
#filter(DataCollection=="old") %>%
summarise(n())

exclDx %>% 
mutate(anyHyst = factor(case_when(hyster1___1 == 1 | hyster1___2 == 1 | hyster1___3 == 1 ~ "Yes",
                                  hyster1___1 == 0 | hyster1___2 == 0 | hyster1___3 == 0 ~ "No"))) %>%
select(anyHyst, hyster2) %>%
#filter(anyHyst == "Yes" & hyster2 == "") # 6
filter(anyHyst == "No" & !hyster2 == "") # 3


### mood dx
#seems nearly all of the old data is missing - possibly Ive lined up the options incorrectly? Some have _v2 I don’t know
exclDx %>% 
select(mooddx1, DataCollection) %>%
group_by(mooddx1) %>%
filter(DataCollection=="old") %>%
summarise(n())

# new has 0/1/3; old has ages
# decision to interpret NAs from old data as 0's i.e., not endorsed - issue that cant tell is NA missing or NA not endorsed 
# also need to work out age of dx - captured differently in new data 

### psychosis dx
# agin all old prject datamissing - issue with combining 
exclDx %>% 
select(psychosisdx2, DataCollection) %>%
group_by(psychosisdx2) %>%
filter(DataCollection=="new") %>%
summarise(n())




psycohsisdx_cleaning<- hyster %>% 
    #select(c(record_id, dx_10:dx_17)) %>% 
    #mutate(across(c(dx_10:dx_13, dx_15:dx_17), ~ replace_na(.,  0))) %>%
    mutate(across(c(psychosisdx1:psychosisdx10), ~ replace_na(.,  0))) %>% 
    #mutate(psychosisDXOldRC = as.numeric(rowSums(across(c(dx_10:dx_17))) > 0)) %>% 
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
    #mutate(ageOfPsychosisDx_oldRC = rowSums(across(psychosisDXOldRC)) #- needs finishing - integrate old and new data for ages
  
    # NEED TO ADD AGE OF PSYCHOSIS - I THINK FOR NEW DATA ITS THE COL + _V2; AND FOR OLD ITS THE ANSWER TO THE QUESITON ITSELF

   psycohsisdx_cleaning %>%
   filter(DataCollection=="new") %>%
   filter(psychosisDXyesNo==1 & age_psychosisdx10=="")

### mood dx ###



moodDXintegration <- psycohsisdx_cleaning %>%
    #mutate(across(c(dx_19:dx_23), ~ replace_na(.,  0))) %>%
    mutate(mooddx3 = as.numeric(mooddx3)) %>%
     mutate(mooddx6 = as.numeric(mooddx6)) %>%
    mutate(across(c(mooddx1:mooddx6), ~ replace_na(.,  0))) %>% 
    #mutate(moodDXOldRC = as.numeric(rowSums(across(c(dx_19:dx_23))) > 0)) %>% 
    mutate(nMoodDx = rowSums(across(c(mooddx1:mooddx6)))) %>%
    mutate(moodDXyesNo = nMoodDx > 0) 
    
    ####GETTING WARNINGS###
    # NEED TO ADD AGE OF PSYCHOSIS - I THINK FOR NEW DATA ITS THE COL + _V2; AND FOR OLD ITS THE ANSWER TO THE QUESITON ITSELF


moodDXintegration %>%
filter(DataCollection=="old") %>%
filter(moodDXyesNo==1 & age_mooddx6=="")

# Create a list to store the objects
my_objects <- list(a, birthsex, age, country, zipcode, lastperiod_filtered, birthcontrol, seizure)

# Create an empty data frame to store the results
dim_table <- data.frame(object_name = character(), rows = integer(), columns = integer(), stringsAsFactors = FALSE)

# Loop through each object
for (i in seq_along(my_objects)) {
  # Get the dimensions of the current object
  dimensions <- dim(my_objects[[i]])

  # Add the dimensions to the table
  new_row <- data.frame(object_name = paste0("Object", i), rows = dimensions[1], columns = dimensions[2])
  dim_table <- rbind(dim_table, new_row)
}

# Print the table
print(dim_table)

#1335 --> 1321
summarydat <- moodDXintegration %>%
select(DataCollection,
       age,
       birth_sex,
       country_US,
       lastperiod_daysSince,
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
        remove_flag,
        psychosisDXyesNo,
        moodDXyesNo
        #HT,
        #HTType,
        #IndirectHT
       ) %>%

filter(complete.cases(.))

saveRDS(summarydat, "Outputs/summarydatincomplete.RDS")

## variables with NAs - pqb distress, hysterage - also hyster age super incomplete because some people give ages when no surgery and surgery with no ages 
