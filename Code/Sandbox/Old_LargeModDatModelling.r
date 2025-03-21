

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
    mutate(anyHysts = !HystCats=="None") %>%
    mutate(logPqb = log(totalPqb+0.001))

################################################################################
#Multiple comparison approach using newly defined categories - total pqb
################################################################################

########### modat ####################
m5 <- lm(totalPqb ~ anyHysts + age, moddat)
m6 <- lm(totalPqb ~ anyHysts*age, moddat)
m7 <- lm(totalPqb ~ anyHysts, moddat)
m5hrt <- lm(totalPqb ~ anyHysts*HRT + age, moddat)
#m6hrt <- lm(totalPqb ~ anyHysts*HRT*age, moddat)
m7hrt <- lm(totalPqb ~ anyHysts + HRT, moddat)
m5hrtm <- lm(totalPqb ~ anyHysts + HRT + age, moddat)
m6hrtm <- lm(totalPqb ~ anyHysts*age + HRT, moddat)
m7hrtm <- lm(totalPqb ~ anyHysts + HRT, moddat)

#Is total PQB predicted by hysterectomy group when defined ordinally (increasing in effect to overaries)
m8 <- lm(totalPqb ~ as.numeric(HystCats), moddat)
m9 <- lm(totalPqb ~ as.numeric(HystCats) + age, moddat)
m10 <- lm(totalPqb ~ as.numeric(HystCats)*age, moddat)
m8hrt <- lm(totalPqb ~ as.numeric(HystCats)*HRT, moddat)
m9hrt <- lm(totalPqb ~ as.numeric(HystCats)*HRT + age, moddat)
#m10hrt <- lm(totalPqb ~ as.numeric(HystCats)*age*HRT, moddat)
m8hrtm <- lm(totalPqb ~ as.numeric(HystCats)+HRT, moddat)
m9hrtm <- lm(totalPqb ~ as.numeric(HystCats)+HRT + age, moddat)
m10hrtm <- lm(totalPqb ~ as.numeric(HystCats)*age + HRT, moddat)

#m12 <- aov(totalPqb ~ as.factor(as.character(HystCats)), moddat)
#m13 <- aov(totalPqb ~ as.factor(as.character(HystCats)) + age, moddat)
#m14 <- aov(totalPqb ~ as.factor(as.character(HystCats)) *age, moddat)
#m12hrt <- aov(totalPqb ~ as.factor(as.character(HystCats))*HRT, moddat)
#m13hrt <- aov(totalPqb ~ as.factor(as.character(HystCats))*HRT + age, moddat)
#m14hrt <- aov(totalPqb ~ as.factor(as.character(HystCats))*HRT *age, moddat)
#m12hrtm <- aov(totalPqb ~ as.factor(as.character(HystCats)) + HRT, moddat)
#m13hrtm <- aov(totalPqb ~ as.factor(as.character(HystCats)) + HRT + age, moddat)
#m14hrtm <- aov(totalPqb ~ as.factor(as.character(HystCats)) + HRT *age, moddat)

msAnyHysts <- model.sel(m5,m6,m7, m5hrt,m7hrt, m5hrtm, m6hrtm, m7hrtm)
#modaveAnyHysts <- model.avg(msAnyHysts, subset = delta < 2)
#summary(modaveAnyHysts)
summary(m6hrtm)

msOrdinalHystCats<-model.sel(m8, m9, m10, m8hrt, m9hrt, m8hrtm, m9hrtm, m10hrtm)
modaveOrdinalHystCats<- model.avg(msOrdinalHystCats, subset = delta < 2)
summary(modaveOrdinalHystCats)

msAllCombined<-model.sel(
    m5,m6,m7, m5hrt,m7hrt, m5hrtm, m6hrtm, m7hrtm,
    m8,m9,m10, m8hrt,m9hrt, m8hrtm,m9hrtm,m10hrtm)
modavemsAllCombined<- model.avg(msAllCombined, subset = delta < 2)
summary(modavemsAllCombined)





#############################################################################
# plots
#############################################################################

AnyIntervetionBoxplot <- moddat %>% 
    ggplot(aes(y=totalPqb, x=anyHysts)) +
    geom_boxplot(aes(fill=anyHysts)) +
    geom_point(position = "jitter") +
    theme_classic()

AnyIntervetionBoxplot_HRT <- moddat %>% 
    ggplot(aes(y=totalPqb, x=anyHysts, fill = as.factor(HRT))) +
    geom_boxplot() +
    geom_point(aes(color=as.factor(HRT)), position = "jitter") +
    theme_classic()

HystCatsBoxplot <- moddat %>% 
    ggplot(aes(y=totalPqb, x=HystCats)) +
    geom_boxplot(aes(fill=HystCats)) +
    geom_point(position = "jitter") +
    theme_classic()

HystCatsBoxplot_HRT <- moddat %>% 
    ggplot(aes(y=totalPqb, x=HystCats, fill = as.factor(HRT))) +
    geom_boxplot() +
    geom_point(aes(color=as.factor(HRT)), position = "jitter") +
    theme_classic()


PQBxAgeInt_Ordinal <- moddat %>% 
    ggplot(aes(y=age, x=totalPqb)) +
    geom_point(alpha=0.3) +
    geom_smooth(aes(color=HystCats), method="lm", se=F)+
    theme_classic()

PQBxAgeInt_Binary<-moddat %>% 
    ggplot(aes(y=totalPqb, x=age, color=anyHysts)) +
    geom_point() +
    geom_smooth(method="lm", se=F)+
    theme_classic()







#ms<-model.sel(m12, m13, m14, m12hrt, m13hrt, m14hrt, m12hrtm, m13hrtm, m14hrtm)

ms<-model.sel(m5,m6,m7,m8,m9,m10, m8hrt,m9hrt,m10hrt , m12, m13, m14)

ms1<-model.sel(m5,m6,m7,m8,m9,m10)

model<-model.avg(ms1, subset = delta < 3)

summary(model)


ggplot(moddat, aes(x=HystCats, y=totalPqb))+
    geom_boxplot(aes(fill=as.factor(HRT)))+
    geom_jitter()+
    facet_wrap(~HRT)



ms2<-model.sel(m8,m9,m10, m11, m12, m13)
summary(m10)
model<-model.avg(ms2, subset = weight > 0.1)
summary(model)

model.sel(m8,m9,m10)

model.sel(m11,m12,m13)

summary(m10)
summary(m6)
summary(m12)
TukeyHSD(m12, which="HystCats")


model.avg()

####mod average incl HRT
msAnyhyst <- model.sel(m5,m6,m7, m5hrt,m6hrt,m7hrt, m5hrtm, m6hrtm, m7hrtm)

msOrdCats <- model.sel(m8,m9,m10, m8hrt,m9hrt,m10hrt, m8hrtm,m9hrtm,m10hrtm)

modelAnyHyst<-model.avg(msAll, subset = delta < 2)
modelOrdCats<-model.avg(msAll, subset = delta < 2)
####mod average incl HRT

summary(modelAnyHyst)
summary(modelOrdCats)

SurIntGroupsBoxplot<-moddat %>% 
    ggplot(aes(y=totalPqb, x=HystCats), group=HystCats) +
    geom_boxplot(aes(fill=HystCats)) +
    geom_point(position = "jitter") +
    theme_classic()

AnyIntervetionBoxplot<-moddat %>% 
    ggplot(aes(y=totalPqb, x=anyHysts), group=anyHysts) +
    geom_boxplot(aes(fill=anyHysts)) +
    geom_point(position = "jitter") +
    theme_classic()

PQBxAgeInt_Ordinal<-moddat %>% 
    ggplot(aes(y=age, x=totalPqb)) +
    geom_point(alpha=0.3) +
    geom_smooth(aes(color=HystCats), method="lm", se=F)+
    theme_classic()

PQBxAgeInt_Binary<-moddat %>% 
    ggplot(aes(y=totalPqb, x=age, color=anyHysts)) +
    geom_point() +
    geom_smooth(method="lm", se=F)+
    theme_classic()

hystcats_HRT <- moddat %>%
    ggplot()

ggsave(SurIntGroupsBoxplot, file=file.path(path, "SurIntGroupsBoxplot.pdf"))
ggsave(AnyIntervetionBoxplot, file=file.path(path, "AnyIntervetionBoxplot.pdf"))
ggsave(PQBxAgeInt_Ordinal, file=file.path(path, "PQBxAgeInt_Ordinal.pdf"))
ggsave(PQBxAgeInt_Binary, file=file.path(path, "PQBxAgeInt_Binary.pdf"))


################################################################################
##Multiple comparison approach using newly defined categories - total DISTRESS pqb
################################################################################

########### modat ####################
m5 <- lm(totalDistressPqb ~ anyHysts + age, moddat)
m6 <- lm(totalDistressPqb ~ anyHysts*age, moddat)
m7 <- lm(totalDistressPqb ~ anyHysts, moddat)

m8 <- lm(totalDistressPqb ~ as.numeric(HystCats), moddat)
m9 <- lm(totalDistressPqb ~ as.numeric(HystCats) + age, moddat)
m10 <- lm(totalDistressPqb ~ as.numeric(HystCats)*age, moddat)

m11 <- aov(totalDistressPqb ~ as.factor(as.character(HystCats)), moddat)
m12 <- aov(totalDistressPqb ~ as.factor(as.character(HystCats)) + age, moddat)
m13 <- aov(totalDistressPqb ~ as.factor(as.character(HystCats)) *age, moddat)


ms1<-model.sel(m5,m6,m7,m8,m9,m10, m11, m12, m13)

model<-model.avg(ms1, subset = weight > 0.1)

summary(model)

ms2<-model.sel(m8,m9,m10, m11, m12, m13)
summary(m10)
model<-model.avg(ms2, subset = weight > 0.1)

summary(model)


summary(m10)
summary(m6)
summary(m12)
TukeyHSD(m12, which="HystCats")

model.avg()

moddat %>% 
    ggplot(aes(y=totalPqb, x=HystCats), group=HystCats) +
    geom_boxplot(aes(fill=HystCats)) +
    geom_point(position = "jitter") +
    theme_classic()

moddat %>% 
    ggplot(aes(y=totalPqb, x=age, color=HystCats)) +
    geom_point() +
    geom_smooth(method="lm")+
    theme_classic()


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


