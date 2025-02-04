

d<-readRDS("Outputs/largeModDF.RDS")
library(GGally)
library(MuMIn)
library(tidyverse)

path="Figures"

#looking at relationhsips - only inc. hystcats and age
ggpairs(d)

################################################################################
#Create modelling dataframes 
################################################################################

#newly defined categories 
moddat <- d %>%
    mutate(anyHysts = !HystCats=="none") %>%
    mutate(logPqb = log(totalPqb+0.001)) %>%
    mutate(hystMag = case_when( HystCats == "none" ~ "none",
                                HystCats == "fullHyst" ~ "fullHyst",
                                HystCats == "partialHyst" ~ "partialHyst",
                                HystCats == "oophfull" ~ "oophfull", 
                                HystCats == "salpifull" ~ "salpifull", 
                                .default = "other")) %>%
    mutate(hystMag = factor(hystMag, level=c("fullHyst", "salpifull","partialHyst", "oophfull",  "other", "none"), ordered=TRUE)) 


#secondary modelling df - not including the largest category "none"
moddat2 <- d %>%
    mutate(anyHysts = !HystCats=="none") %>%
    mutate(logPqb = log(totalPqb+0.001)) %>%
    mutate(hystMag = case_when( HystCats == "none" ~ "none",
                                HystCats == "fullHyst" ~ "fullHyst",
                                HystCats == "partialHyst" ~ "partialHyst",
                                HystCats == "oophfull" ~ "oophfull", 
                                HystCats == "salpifull" ~ "salpifull", 
                                .default = "other")) %>%
    filter(!hystMag=="none") %>%
    filter(!hystMag=="other")

################################################################################
#Multiple comparison approach using newly defined categories 
################################################################################

########### modat ####################
m5 <- lm(totalPqb ~ anyHysts + age, moddat)
m6 <- lm(totalPqb ~ anyHysts*age, moddat)
m7 <- lm(totalPqb ~ anyHysts, moddat)

m8 <- lm(totalPqb ~ hystMag, moddat)
m9 <- lm(totalPqb ~ hystMag + age, moddat)
m10 <- lm(totalPqb ~ hystMag*age, moddat)

model.sel(m5,m6,m7,m8,m9,m10)

summary(m6)
summary(m8)
summary(m7)
summary(m9)

########### modat2 ####################
m8_2 <- lm(totalPqb ~ hystMag, moddat2)
m9_2 <- lm(totalPqb ~ hystMag + age, moddat2) #winner
m10_2 <- lm(totalPqb ~ hystMag*age, moddat2)

model.sel(m8_2,m9_2,m10_2)

summary(m9_2) #only age is significant
summary(m8_2) # none signficant

################################################################################
# more basic aov approach using Tukey - no covariates 
################################################################################

########### modat ####################
aov<-aov(totalPqb ~ hystMag, moddat) #significant p=.031
summary(aov)
TukeyHSD(aov, which="hystMag") # only significant relationship is other-none; the biggest categories 

########### modat2 ####################
aov<-aov(totalPqb ~ hystMag, moddat2) # removing none makes it none significant p.447
summary(aov)
TukeyHSD(aov, which="hystMag") # obviously all non-sig

################################################################################
# more basic aov approach using Tukey - with covariate age
################################################################################

########### modat ####################
aov<-aov(totalPqb ~ hystMag*age, moddat) #hystMag p=.025; age p<.001
summary(aov)
TukeyHSD(aov, which="hystMag") # only significant relationship is other-none; the biggest categories 

########### modat2 ####################
aov<-aov(totalPqb ~ hystMag*age, moddat2) # only age is significant p<.001
summary(aov)
TukeyHSD(aov, which="hystMag") # obviously all non-sig

################################################################################
# ordering - order the factors none < partial < full etc., 
################################################################################

#######################
#plots
#######################

PqbXageXhystCats_plot<-moddat %>% ggplot(aes(y=totalPqb, x=age, colour=HystCats)) +
    geom_point() +
    geom_smooth(method="lm", se=FALSE) +
    theme_classic() +
    facet_wrap(~HystCats)

PqbXageXanyHysts_plot<-moddat %>% ggplot(aes(y=totalPqb, x=age, colour=anyHysts)) +
    #geom_point() +
    geom_smooth(method="lm") +
    theme_classic() 


boxplot_simpleHystresult_PQBvsAnyHyst <- moddat %>% 
    ggplot(aes(y=totalPqb, x=anyHysts), group=anyHysts) +
    geom_boxplot(aes(fill=anyHysts)) +
    geom_point(position = "jitter") +
    theme_classic()

boxplot_simpleHystresult_PQBvshystMag_noNone <- moddat2 %>% 
    mutate(hystMag = factor(hystMag, level=c("fullHyst", "salpifull","partialHyst", "oophfull",  "other", "none"), ordered=TRUE)) %>%
    ggplot(aes(y=totalPqb, x=hystMag), group=hystMag) +
    geom_boxplot(aes(fill=hystMag)) +
    geom_point(position = "jitter") +
    theme_classic()

boxplot_simpleHystresult_PQBvshystMag_noNone <- moddat %>% 
    mutate(hystMag = factor(hystMag, level=c("fullHyst", "salpifull","partialHyst", "oophfull",  "other", "none"), ordered=TRUE)) %>%
    ggplot(aes(y=totalPqb, x=hystMag), group=hystMag) +
    geom_boxplot(aes(fill=hystMag)) +
    geom_point(position = "jitter") +
    theme_classic()
###

PqbXageXhystCats_plot<-moddat %>% ggplot(aes(y=totalPqb, x=age, colour=hystMag)) +
    geom_point() +
    geom_smooth(method="lm", se=FALSE) +
    theme_classic() +
    facet_wrap(~hystMag)

PqbXageXanyHysts_plot<-moddat %>% ggplot(aes(y=totalPqb, x=age, colour=hystMag)) +
    #geom_point() +
    geom_smooth(method="lm") +
    theme_classic() 


boxplot_simpleHystresult_PQBvsAnyHyst <- moddat %>% 
    ggplot(aes(y=totalPqb, x=hystMag), group=hystMag) +
    geom_boxplot(aes(fill=hystMag)) +
    geom_point(position = "jitter") +
    theme_classic()

#save plots
ggsave(PqbXageXhystCats_plot, file=file.path(path, "PqbXageXhystCats_plot.pdf"))
ggsave(PqbXageXanyHysts_plot, file=file.path(path, "PqbXageXanyHysts_plot.pdf"))
ggsave(boxplot_simpleHystresult_PQBvsAnyHyst, file=file.path(path, "PqbXageXanyHysts_BOXplot.pdf"))










#######################
#mod without other
######################
magnDat <- moddat %>%  
    filter(!hystMag=="other") %>%
    mutate(hystMag = factor(hystMag, level=c("none", "partialHyst", "fullHyst"), ordered=TRUE))

m1 <- lm(totalPqb ~ HystCats, magnDat)
m2 <- lm(totalPqb ~ age, magnDat)
m3 <- lm(totalPqb ~ HystCats + age, magnDat)
m4 <- lm(totalPqb ~ HystCats*age, magnDat)
m5 <- lm(totalPqb ~ anyHysts + age, magnDat)
m6 <- lm(totalPqb ~ anyHysts*age, magnDat)
m7 <- lm(totalPqb ~ anyHysts, magnDat)

m8 <- lm(totalPqb ~ hystMag, magnDat)
m9 <- lm(totalPqb ~ hystMag + age, magnDat)
m10 <- lm(totalPqb ~ hystMag*age, magnDat)

model.sel(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)

plot(m6, which=2)

plot(density(log(moddat$totalPqb)))

summary(m6)

m6 <- lm(totalPqb ~ anyHysts*age, magnDat)
# stats

#people younger with intervention i.e., before mid50s - increases pqb
#the earlier you have the intervention the higher the pqb

moddat %>% group_by(HystCats) %>% summarise(n())

moddat %>% 
    filter(!HystCats %in% c("oophHyst", "salpiHyst")) %>%
    ggplot(aes(y=totalPqb, x=age, colour=HystCats)) +
    geom_smooth(method="lm", se=FALSE) +
    theme_classic() 

moddat %>% ggplot(aes(x=age, y=totalPqb))+
geom_hex()

moddat %>% summarise(sd(age))


