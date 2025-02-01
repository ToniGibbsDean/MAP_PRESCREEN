    
#Effect of hysterectomies of several varieties on PQ-B score. These include partial, total, salpingectomy, and oophorectomy.

#packages and read in files 
library(tidyverse)
library(dplyr)
library(ggpubr)
path="Figures"

datraw<-read.csv("Data/PREMAPIntegration_DATA_2025-01-28_1610.csv")
dat<-datraw[-1, ]

#create summary df to answer question
PQ_sums <- dat %>% 
mutate(record_id=as.factor(record_id)) %>%
group_by(record_id) %>%
select( pqb1,pqb2,pqb3,pqb4,pqb5,pqb6,pqb7,pqb8,pqb9,pqb10,pqb11,
        pqb12,pqb13,pqb14,pqb15,pqb16,pqb17,pqb18,pqb19,pqb20,pqb21,
        hyster1___1,
        hyster1___2,
        hyster1___3,
        hyster1___4,
        hyster1___5,
        hyster1___6,
        hyster1___7) %>% 
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



