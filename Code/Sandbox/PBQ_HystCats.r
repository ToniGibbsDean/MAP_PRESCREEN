    
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
        hyster1___6) %>%
mutate(anyHyst = case_when(hyster1___1 | hyster1___2 |
                hyster1___3 | hyster1___4 |
                hyster1___5 | hyster1___6 >= 1 ~ TRUE, 
                #is.na() ~ "NA",
                .default = FALSE)) #summary()

long_df <- PQ_sums %>% 
pivot_longer(starts_with("hyster"), names_to = "hystCat", values_to = "hystValue") %>%
mutate(hystCat=as.factor(hystCat)) %>%
filter(hystCat %in% c("hyster1___1", "hyster1___2")) %>% 
as_tibble() 

PQBxHystCat<-long_df %>%  
mutate(hystCat=as.factor(hystCat)) %>%
filter(!hystValue==0) %>%
group_by(hystCat) %>%
summarise(totalNHysts=sum(hystValue), 
          meanPqb=mean(totalPqb),
          sumPqb=sum(totalPqb),
          medianpqb=median(totalPqb),
          SDPqb=sd(totalPqb),
          SEPqb=SDPqb/sqrt(sumPqb))

aggregate(totalPqb ~ hystCat, data = long_df, function(x) c(mean = mean(x), sd = sd(x), min = min(x), max = max(x)))



long_df %>% ggplot(aes(x=totalPqb, y=hystValue, colour=hystCat)) +
geom_smooth() + theme_classic()

z<-PQBxHystCat %>% 
ggplot(aes(y=meanPqb, group=hystCat)) +
geom_boxplot(aes(x=hystCat, colour=hystCat)) +
theme_classic()

x<-lm(totalPqb ~  hystCat, long_df)
summary(x)
x<-aov(totalPqb ~  hystCat, long_df)
summary(x)

long_df %>% ggplot(aes(x=totalPqb, y=totalPqb, group=hystCat)) +
geom_smooth() + theme_classic()

test<-long_df %>%  
mutate(hystCat=as.factor(hystCat)) %>%
group_by(hystCat) %>%
summarise(totalPqb=mean(totalPqb), totalhystValue = hystValue)

test<-long_df %>%  
mutate(hystCat=as.factor(hystCat)) %>%
group_by(hystCat) %>%
summarise(totalNHysts=sum(hystValue), totalPqb=sum(totalPqb))




z<-long_df %>% 
ggplot(aes(y=hystValue, group=hystCat)) +
geom_boxplot(aes(x=hystCat, colour=hystCat)) +
theme_classic()


x<-aov(meanPqb ~  hystCat, PQBxHystCat)
summary(x)



summarise(sumHystValue=sum(hystValue), sumPqbTotal=sum(totalPqb))

#mutate(logTotalPqb = log(abs(totalPqb+1)))

x<-lm(totalhystValue ~  hystCat + totalPqb, test)
summary(x)

x<-lm(totalPqb ~  totalhystValue + hystCat, test)
summary(x)



c<-long_df %>% group_by(hystcat)
x<-aov(totalPqb ~  hystCat, long_df)
summary(x)

TukeyHSD(x, conf.level=.95)
plot(TukeyHSD(x, conf.level=.95), las = 2)