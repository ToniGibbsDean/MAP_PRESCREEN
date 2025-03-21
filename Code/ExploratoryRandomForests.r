    
#exploration of random forest concept

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

x<- dat %>% select(birth_control, bc_details) %>%  filter(birth_control==1)

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
            prescreendate,
            repro1, repro14, repro19) %>%
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
           pregnancy,
           repro1, repro14, repro19)

#produced tibble has 944 rows

d <- PQ_sums %>% filter(complete.cases(.)) %>% 
    mutate(HystCats = factor(HystCats, levels = c("None", "PartialHysterectomy", "Hysterectomy", "Salpingectomy","Oopherectomy"), ordered=TRUE),
           birth_control = as.logical(birth_control),
           pregnancy =  as.logical(pregnancy))

library(randomForest)

d<-d %>%
    mutate(
        totalPqb=c(scale(totalPqb, center=TRUE, scale=TRUE)),
        totalDistressPqb=c(scale(totalDistressPqb, center=TRUE, scale=TRUE)))


bootstrap_l<-list()
varImp_l<-list()
for (i in 1:1000) {
    set.seed(i)
    trainIndices<-sample(nrow(d), nrow(d)*0.8)
    dTrain<-d[trainIndices,]
    dTest<-d[-trainIndices,]

    rfTotal<-randomForest(totalPqb~. , data=select(dTrain, -totalDistressPqb))
    rfDistress<-randomForest(totalDistressPqb~. , data=select(dTrain, -totalPqb))
    imp_df<-data.frame(importance(rfTotal), importance(rfDistress)) %>% as.data.frame() %>% rownames_to_column("var")
    names(imp_df)<-c("var", "total", "distress" )
    imp_df$bootstrapReplicate<-i
    varImp_l[[i]]<-imp_df

    dTest$predTotal<-predict(rfTotal, dTest)
    dTest$predDistress<-predict(rfDistress, dTest)
    dTest$bootstrapReplicate<-i
    bootstrap_l[[i]]<-dTest
    print(paste0(i, " complete!"))
}

# dealing with varImp_l, looking at variable importance\
df<-bind_rows(varImp_l) %>%
        filter(var!="pregnancy")

df_summary<-df %>%
    group_by(var) %>%
    summarise_all(mean) %>%
    mutate(totalDominant=total>distress)

df <-left_join(df, select(df_summary, var, totalDominant), by="var")

ggplot(df, aes(x=total, y=distress, group=var,  color=totalDominant))+
    geom_point(alpha=0.2)+
    stat_ellipse()+
    theme_classic()+
    geom_abline()+
    geom_label(data=df_summary, aes(label=var, x=total, y=distress))




# dealing with bootstrap_l, looking at predictive power
df<-bind_rows(bootstrap_l)

    (cor(df$totalPqb, df$pred))^2
    plot(df$totalPqb, df$pred)

lapply(bootstrap_l, FUN=function(item){cor(item$totalPqb, item$pred)}) %>% unlist %>% summary
