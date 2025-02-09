    
#Effect of hysterectomies of several varieties on PQ-B score. These include partial, total, salpingectomy, and oophorectomy.

#packages and read in files 
library(tidyverse)
library(dplyr)
library(ggpubr)
path="Figures"

datraw<-read.csv("Data/PREMAPIntegration_DATA_2025-01-28_1610.csv") 
alldat<-datraw[-1, ]

#only included with at least 1000 records for each variable 
names(which(colSums(!is.na(alldat)) > 1000))

#remove any psychosis dx
dat <- alldat[alldat %>% select(starts_with("psychosis")) %>% rowSums(na.rm=TRUE) == 0,]

#create summary df to answer question
PQ_sums <- dat %>% 
    as_tibble() %>%
    mutate(record_id=as.factor(record_id)) %>%
    #group_by(record_id) %>%
    select( pqb1,pqb2,pqb3,pqb4,pqb5,pqb6,pqb7,pqb8,pqb9,pqb10,pqb11,
            pqb12,pqb13,pqb14,pqb15,pqb16,pqb17,pqb18,pqb19,pqb20,pqb21,
            pqb1_yes,pqb2_yes,pqb3_yes,pqb4_yes,pqb5_yes,pqb6_yes,pqb7_yes,
            pqb8_yes,pqb9_yes,pqb10_yes,pqb11_yes,
            pqb12_yes,pqb13_yes,pqb14_yes,pqb15_yes,pqb16_yes,pqb17_yes,pqb18_yes,
            pqb19_yes,pqb20_yes,pqb21_yes,
            hyster1___1,
            hyster1___2,
            hyster1___3,
            hyster1___4,
            hyster1___5,
            hyster1___6,
            hyster1___7,
            hyster5,
            hyster2,
            dob,
            race___1,
            race___2,
            race___3,
            race___4,
            race___5,
            race___8,
            race___9, #unclear how much missing
            irregularperiods,
            birth_control,
            pregnancy,
            othermeds,
            birthcity,
            prescreendate) %>%
    mutate(across(c("pqb1_yes":"pqb21_yes"), ~ replace_na(.,  0))) %>%
    filter(complete.cases(.)) %>%
    mutate(totalPqb = rowSums(across(c("pqb1":"pqb21")))) %>%
    mutate(totalDistressPqb = rowSums(across(c("pqb1_yes":"pqb21_yes")))) %>%
    mutate(multiHyst = rowSums(across(starts_with("hyster1___")))) %>% 
    mutate(HystCats = case_when(
                                hyster1___7 == 1 & hyster1___6==0 ~ "None",
                                hyster1___1 == 1 ~ "PartialHysterectomy",
                                hyster1___2 == 1 ~ "Hysterectomy",
                                hyster1___4 == 1 ~ "Salpingectomy",
                                hyster1___3 == 1 ~ "Oopherectomy",
                                .default = NA)) %>%
    #mutate(HystCats = case_when( multiHyst > 1 ~ "multiHyst", 
     #                          .default = HystCats)) %>%
    mutate(hyster5 = as.numeric(hyster5), hyster2 = as.numeric(hyster2)) %>%
    rowwise() %>%
    mutate(AgeFirstInt = min(hyster5, hyster2, na.rm=TRUE)) %>%
    mutate(AgeFirstInt = case_when(is.infinite(AgeFirstInt) ~ NA, .default = AgeFirstInt)) %>%
    mutate(age = as.numeric((mdy(prescreendate) - mdy(dob)) / 365)) %>%
    ungroup() %>%
    select(totalPqb,
           totalDistressPqb, 
           HystCats, 
           #AgeFirstInt, not enough data
           age, 
           birth_control, 
           pregnancy)

#produced tibble has 944 rows

cl.dat <- PQ_sums %>% filter(complete.cases(.)) %>% 
    mutate(HystCats = factor(HystCats, levels = c("None", "PartialHysterectomy", "Hysterectomy", "Salpingectomy","Oopherectomy"), ordered=TRUE),
           birth_control = as.logical(birth_control),
           pregnancy =  as.logical(pregnancy))

saveRDS(cl.dat, "Outputs/largeModDF.RDS")
            