
library(GGally)
library(MuMIn)
library(tidyverse)

path="Figures"

datraw<-read.csv("Data/PREMAPIntegration_DATA_2025-01-28_1610.csv") 
alldat<-datraw[-1, ]

d<-readRDS("Outputs/largeModDF.RDS")

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

ggsave(z, file=file.path(path, "ppt/reproQuData_Correlations.pdf"))


