    
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
    mutate(HystCats = case_when(hyster1___1 == 1 ~ "partialHyst",
                                hyster1___2 == 1 ~ "fullHyst", 
                                hyster1___3 == 1 ~ "oophHyst",  
                                hyster1___4 == 1 ~ "salpiHyst",
                                hyster1___5 == 1 ~ "otherHyst",
                                hyster1___6 == 1 ~ "otherUnknownHyst",
                                hyster1___7 == 1 ~ "none", 
                                .default = NA)) %>%
    mutate(HystCats = case_when( multiHyst > 1 ~ "multiHyst", 
                                .default = HystCats)) %>%
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
            




as_date(PQ_sums$dob)
str(mdy(PQ_sums$dob))

(mdy(PQ_sums$prescreendate) - mdy(PQ_sums$dob)) / 365

multiHyst <- PQ_sums %>% select(hyster1___1,
            hyster1___2,
            hyster1___3,
            hyster1___4,
            hyster1___5,
            hyster1___6,
            hyster1___7, 
            multiHyst) 

            str(multiHyst)

PQ_sums[multiHyst,] %>% select(hyster1___1,
            hyster1___2,
            hyster1___3,
            hyster1___4,
            hyster1___5,
            hyster1___6,
            hyster1___7) %>% print(n=50)


table(PQ_sums$HystCats)

table(multiHyst)



PQ_sums %>% select( birth_control,
            pregnancy) %>% summary()




 PQ_sums %>% select(starts_with("race"))


sum(dat %>% select(starts_with("psychosis")) %>% rowSums(na.rm=TRUE) > 0)



#summary() #118 NAs out of 1118 record_ID rows --> 1003 records left
drop_na() %>%
group_by(record_id) %>%
#summarise(totalpqb = sum(pqb1,pqb2,pqb3,pqb4,pqb5,pqb6,pqb7,pqb8,pqb9,pqb10,pqb11,
        #pqb12,pqb13,pqb14,pqb15,pqb16,pqb17,pqb18,pqb19,pqb20,pqb21))
mutate(totalPqb = sum(pqb1,pqb2,pqb3,pqb4,pqb5,pqb6,pqb7,pqb8,pqb9,pqb10,pqb11,
        pqb12,pqb13,pqb14,pqb15,pqb16,pqb17,pqb18,pqb19,pqb20,pqb21)) %>% #summary()
dplyr::select(totalPqb,   
        hyster1___1,
        hyster1___2,
        hyster1___3,
        hyster1___4,
        hyster1___5,
        hyster1___6,
        hyster1___7) %>% 
mutate(anyHyst = case_when(hyster1___1 | hyster1___2 |
                        hyster1___3 | hyster1___4 |
                        hyster1___5 | hyster1___6 >= 1 ~ TRUE, 
                        #is.na() ~ "NA",
                        .default = FALSE)) %>% #summary()
select(anyHyst, totalPqb) %>% #summary()
mutate(logTotalPqb = log(abs(totalPqb+1))) 
                 
#check for normality of continuous variable 

hist(PQ_sums$totalPqb, freq = TRUE)
hist(PQ_sums$logTotalPqb, freq = TRUE)
ggqqplot(PQ_sums$logTotalPqb)
shapiro.test(PQ_sums$logTotalPqb) #non normal even when logged

# stats
wilcox.test(totalPqb ~ anyHyst, PQ_sums)

# plots 
plot1 <- PQ_sums %>% 
    ggplot(aes(y=logTotalPqb, x=anyHyst), group=anyHyst) +
    geom_boxplot(aes(fill=anyHyst)) +
    geom_point(position = "jitter") +
    theme_classic()

boxplot_simpleHystresult_PQBvsAnyHyst <- PQ_sums %>% 
    ggplot(aes(y=totalPqb, x=anyHyst), group=anyHyst) +
    geom_boxplot(aes(fill=anyHyst)) +
    geom_point(position = "jitter") +
    theme_classic()

violinPlot_simpleHystresult_PQBvsAnyHyst <- PQ_sums %>% 
    ggplot(aes(y=totalPqb, x=anyHyst), group=anyHyst) +
    geom_violin(aes(fill=anyHyst)) +
    geom_point(position = "jitter") +
    theme_classic()

#save plots
ggsave(boxplot_simpleHystresult_PQBvsAnyHyst, file=file.path(path, "boxplot_simpleHystresult_PQBvsAnyHyst.pdf"))
ggsave(violinPlot_simpleHystresult_PQBvsAnyHyst, file=file.path(path, "violinPlot_simpleHystresult_PQBvsAnyHyst.pdf"))



