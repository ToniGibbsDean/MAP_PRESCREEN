    
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
    filter(complete.cases(.)) %>%
    mutate(totalPqb = rowSums(across(starts_with("pqb")))) %>%
    mutate(multiHyst = rowSums(across(starts_with("hyster1___")))) %>% 
    mutate(HystCats = case_when(hyster1___3 == 1 & hyster1___2 == 1 ~ "oophfull",
                                hyster1___4 == 1 & hyster1___2 == 1 ~ "salpifull",
                                #hyster1___4 == 1 ~ "salpiHyst",
                                hyster1___4 == 1 ~ "salpifull",
                                hyster1___3 == 1 ~ "oophfull",
                                hyster1___5 == 1 ~ "otherHyst",
                                hyster1___6 == 1 ~ "otherUnknownHyst",
                                hyster1___7 == 1 ~ "none", 
                                hyster1___1 == 1 ~ "partialHyst",
                                hyster1___2 == 1 ~ "fullHyst",
                                .default = NA)) %>%``
    #mutate(HystCats = case_when( multiHyst > 1 ~ "multiHyst", 
     #                           .default = HystCats)) %>%
    mutate(hyster5 = as.numeric(hyster5), hyster2 = as.numeric(hyster2)) %>%
    rowwise() %>%
    mutate(AgeFirstInt = min(hyster5, hyster2, na.rm=TRUE)) %>%
    mutate(AgeFirstInt = case_when(is.infinite(AgeFirstInt) ~ NA, .default = AgeFirstInt)) %>%
    mutate(age = as.numeric((mdy(prescreendate) - mdy(dob)) / 365)) %>%
    ungroup() %>%
    select(totalPqb, 
           HystCats, 
           #AgeFirstInt, not enough data
           age, 
           birth_control, 
           pregnancy)

cl.dat <- PQ_sums %>% filter(complete.cases(.)) %>% 
    mutate(HystCats = as.factor(HystCats),
           birth_control = as.logical(birth_control),
           pregnancy =  as.logical(pregnancy))

saveRDS(cl.dat, "Outputs/largeModDF.RDS")
            
