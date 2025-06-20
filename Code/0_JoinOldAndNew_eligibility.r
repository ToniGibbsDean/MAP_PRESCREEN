
library(tidyverse)
library(gt)
library(dplyr)
library(tidyr)
library(gtsummary)



# 1)  initial data loading ---------------------------------------------------------------
#new data 
# load and remove records without names or times as failed data collections
n<-read.csv("Data/PREMAPIntegration-EligibilityData_DATA_2025-06-17_1206.csv") %>% 
    as_tibble %>% 
    filter(lastname!="") 
# old data
# remove those without a name, all lack other data and are marked as tests or failed data entries
o<-read.csv("Data/OLDPREMAPPreScreenin-EligibilityData_DATA_2025-06-17_1208.csv") %>% 
    as_tibble %>%
    filter(pscreen_name!="")

# 2) remove incorrect old data from new data frae ---------------------------------------------
# filter out everthing from before the new dataset - these were merged in an error-prone way
# this also removes 4 rows that do not have prescreendate values, but all have timestamp values 
#showing they are from the old dataset
n<-n %>%
    mutate(prescreendate=as_date(prescreendate))%>% 
    filter(!(prescreendate < "2024-11-12") | is.na(prescreendate)) %>%
    arrange(prescreendate)

n<-n[!duplicated(n$email ),]

#remove second occurance of people in old data collection effort
o<-o[  !(duplicated(o$pscreen_email) & o$pscreen_email!="") ,]

#remove people from old data collection effort who subsequnelt came and had data collected in the new effort
o<-o[ ! o$pscreen_email %in% 
    n$email,]


# 4) remove anyone from @yale.edu domain; these are tests ------------------------------------------------------
n<-n[!str_detect(n$email, "@yale.edu"),]
o<-o[!str_detect(o$pscreen_email, "@yale.edu"),]


cols <- c(
      "prescreendate", 
      "name",
      "email", 
    paste0("elig_determination___", 1:8))

n_selected<-n %>%
  mutate(name=paste0(firstname, " ", middleinitial, " ", lastname)) %>% 
  select(all_of(cols)) 

o_reformatted <- o %>%
  transmute(
    prescreendate = pscreen_date,
    name = pscreen_name,
    email = pscreen_email,
    elig_determination___1,
    elig_determination___2,
    elig_determination___3,
    elig_determination___4,
    elig_determination___5,
    elig_determination___6,
    elig_determination___7,
    elig_determination___8
  )


new_df <- rbind(
  cbind(n_selected, DataCollection = "old"),
  cbind(o_reformatted, DataCollection = "new")
) %>%
  rename(
    Ineligible = elig_determination___1,
    MAP_psychosis = elig_determination___2,
    MAD_depression = elig_determination___3,
    CHR_group = elig_determination___4,
    HC_group = elig_determination___5,
    Future_eligible = elig_determination___6,
    Full_hysterectomy_screen_only = elig_determination___7,
    Withdrawn = elig_determination___8
  ) %>%
  mutate(
    across(
      c(
        Ineligible,
        MAP_psychosis,
        MAD_depression,
        CHR_group,
        HC_group,
        Future_eligible,
        Full_hysterectomy_screen_only,
        Withdrawn
      ),
      ~ as.factor(.x)
    ),
    Neligible = rowSums(across(
      c(
        MAP_psychosis,
        MAD_depression,
        CHR_group,
        HC_group,
        Full_hysterectomy_screen_only,
      ),
      ~ as.numeric(as.character(.x))
    ), na.rm = TRUE),
    eligible = (Neligible >= 1)
  ) %>%
  as_tibble()

elig_summary <- new_df %>%
  pivot_longer(
    cols = c(
      Ineligible,
      MAP_psychosis,
      MAD_depression,
      CHR_group,
      HC_group,
      Future_eligible,
      Full_hysterectomy_screen_only,
      Withdrawn
    ),
    names_to = "Eligibility_Criterion",
    values_to = "Present"
  ) %>%
  mutate(Present = as.numeric(as.character(Present))) %>%  # convert factor to numeric
  group_by(Eligibility_Criterion) %>%
  summarise(
    N_present = sum(Present, na.rm = TRUE),
    Total = n(),
    Percent_present = round(100 * N_present / Total, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(Percent_present))


eligibilityDat <- new_df %>%
  mutate(
    Ineligible = as.numeric(as.character(Ineligible)),
    Withdrawn = as.numeric(as.character(Withdrawn)),
    Neligible = as.numeric(Neligible),
    Status = case_when(
      Ineligible == 1 ~ "Ineligible",
      Withdrawn == 1 ~ "Withdrawn",
      Neligible > 0 ~ "Eligible",
      TRUE ~ "No Designation"
    )
  )
  

eligibility_sampleCharTab <- eligibilityDat %>%
  select(DataCollection,
 "MAD_depression",               
"CHR_group",
 "HC_group" ,                    
"Future_eligible",
"Full_hysterectomy_screen_only",
"Withdrawn",
"DataCollection"  ,             
 "Neligible",
 "eligible", 
  Ineligible, 
  Status   
) %>%
  tbl_summary(missing = "ifany") %>%
  bold_labels()



saveRDS(eligibilityDat, "Outputs/joined_eligibilityDat.rds")
write.csv(eligibilityDat, "Outputs/joined_eligibilityDat.csv")
gtsave(as_gt(eligibility_sampleCharTab), "Outputs/eligibility_sampleCharTab.pdf")







### issue that n "ineligible" indicated on the questionnaire is 400 and something, whereas 
# the number of people with 0's in every column - also indicating not eligible for each category, 
# is over 1000 --> indicates that there a lot of people without a designation 

# catching anomolous dat
anomolous1 <- new_df %>%
filter(Ineligible == 1 & Neligible == 1) %>% summary()
#"Tanya  M Rodriguez"

anomolous2 <- new_df %>%
filter(Ineligible == 0 & Neligible == 0) %>% 
filter(Withdrawn == 0) %>% 
group_by(DataCollection) %>%
summarise(n())
#summary()
# 713 not indicated as ineligible, but then no other category indicated; excluded withdrawn (n=20)
new_df %>%
group_by(DataCollection) %>%
summarise(n())






new_df %>%
group_by(MissingEligibility) %>%
summarise(n())


new_df <- new_df %>%
  mutate(
    Ineligible_num = as.numeric(as.character(Ineligible)),
    Withdrawn_num = as.numeric(as.character(Withdrawn)),
    MissingEligibility = ifelse(
      is.na(Ineligible_num) & Neligible == 0 & Withdrawn_num == 0,
      TRUE, FALSE
    )
  )


