
library(GGally)
library(MuMIn)
library(tidyverse)

path="Figures"

datraw<-read.csv("Data/PREMAPIntegration_DATA_2025-01-28_1610.csv") 
alldat<-datraw[-1, ]

d<-readRDS("Outputs/largeModDF.RDS")

redu <- alldat %>% 
select(,191:327) %>% 
filter(reproductive_history_questionnaire_complete==2) %>%
select(repro1, repro14, repro19) %>%
as_tibble() %>%
mutate(across(c("repro1":"repro19"), ~ replace_na(.,  0))) %>%
filter(!repro19 > 1000) %>%
mutate(NA1 = repro1 ==0) %>%
mutate(NA14 = repro14 ==0) %>%
mutate(NA19 = repro19 ==0) %>%
mutate(NA1419 = c(repro14==0 & repro19==0)) %>%
mutate(NA11419 = repro1 == 0 & repro14==0 & repro19==0) #%>% summary()

x<-ggpairs(redu)

ex_NAs <- redu %>% 
filter(!repro19 > 1000) %>%
filter(!repro1==0) %>%
filter(!repro14 ==0) %>%
filter(!repro19 ==0) %>%
select(repro1,repro14,  repro19) %>%
mutate(men_repro14 = (repro14-repro1)) %>%
mutate(men_repro19 = (repro19-repro1))
#mutate(NA1419 = c(repro14==0 & repro19==0)) %>%
#mutate(NA11419 = repro1 == 0 & repro14==0 & repro19==0) 

y<-ggpairs(ex_NAs)

redu$reproductive_history_questionnaire_complete

as.tibble(redu)

#n115 without NAs
redu %>% 
filter(!repro19 > 1000) %>%
filter(!repro1==0) %>%
filter(!repro14 ==0) %>%
filter(!repro19 ==0) %>%
mutate(men_repro14 = (repro14-repro1)) %>%
mutate(men_repro19 = (repro19-repro1))



ages_pq <- alldat %>% 
#select(,191:327) %>% 
filter(reproductive_history_questionnaire_complete==2) %>%
as_tibble() %>%
mutate(across(c("repro1":"repro19"), ~ replace_na(.,  0))) %>%
filter(!repro19 > 1000) %>%
filter(!repro1==0) %>%
filter(!repro14 ==0) %>%
filter(!repro19 ==0) %>%
select(repro1, repro14, repro19, pqb1,pqb2,pqb3,pqb4,pqb5,pqb6,pqb7,pqb8,pqb9,pqb10,pqb11,
            pqb12,pqb13,pqb14,pqb15,pqb16,pqb17,pqb18,pqb19,pqb20,pqb21,
            pqb1_yes,pqb2_yes,pqb3_yes,pqb4_yes,pqb5_yes,pqb6_yes,pqb7_yes,
            pqb8_yes,pqb9_yes,pqb10_yes,pqb11_yes,
            pqb12_yes,pqb13_yes,pqb14_yes,pqb15_yes,pqb16_yes,pqb17_yes,pqb18_yes,
            pqb19_yes,pqb20_yes,pqb21_yes) %>%
mutate(across(c("pqb1_yes":"pqb21_yes"), ~ replace_na(.,  0))) %>%
filter(complete.cases(.)) %>%
mutate(totalPqb = rowSums(across(c("pqb1":"pqb21")))) %>%
mutate(totalDistressPqb = rowSums(across(c("pqb1_yes":"pqb21_yes")))) %>%
mutate(men_repro14 = (repro14-repro1)) %>%
mutate(men_repro19 = (repro19-repro1)) %>%
rename(ageAtMenarche = repro1) %>%
rename(menoAge14 = repro14) %>%
rename(menoAge19 = repro19) %>%
rename("menoAge14-Menarche" = men_repro14) %>%
rename("menoAge19-Menarche" = men_repro19) %>%
select(ageAtMenarche, menoAge14, menoAge19, "menoAge14-Menarche", "menoAge19-Menarche", totalPqb, totalDistressPqb)

z<-ggpairs(ages_pq)

ggsave(z, file=file.path(path, "ageCorrelations.pdf"))




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
                                hyster1___3 == 1 ~ "Oopherectomy",
                                hyster1___4 == 1 ~ "Salpingectomy",
                                hyster1___2 == 1 ~ "Hysterectomy",
                                hyster1___1 == 1 ~ "PartialHysterectomy",
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