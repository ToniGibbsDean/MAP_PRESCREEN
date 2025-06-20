

d<-readRDS("Outputs/largeModDF.RDS")
d<-readRDS("Outputs/summarydatincomplete.RDS")
library(GGally)
library(MuMIn)
library(tidyverse)
library(patchwork)

path="Figures"

#looking at relationhsips - only inc. hystcats and age
ggpairs(d) #1155

################################################################################
# exclude
################################################################################

excDat <- d %>%
filter(!psychosisDXyesNo=="Yes") %>%
filter(!seizure=="Yes") %>%
filter(!Endocrine_or_thyroid_Disease==1) %>%
filter(!PCOS==1) %>%
filter(!MEN_IorII==1)%>%
filter(country_US=="USA") #963

################################################################################
#Create modelling dataframes 
################################################################################

#newly defined categories 
moddat <- d %>%
    mutate(anyHysts = !HystCats=="None") %>%
    mutate(logPqb = log(totalPqb+0.001)) %>%
    mutate(HRT = as.factor(HRT))

################################################################################
#Multi-model comparison 
################################################################################

#mod set 1: hysterectomy category definied as binary (any hyst procedure vs no procedure)
m1 <- lm(totalPqb ~ anyHysts, moddat)
m2 <- lm(totalPqb ~ anyHysts + age + HRT, moddat)
m3 <- lm(totalPqb ~ anyHysts*age + HRT, moddat) #by far the best model (weight=0.801)
m4 <- lm(totalPqb ~ anyHysts*HRT + age, moddat)

ModSelAnyHysts <- model.sel(m1, m2, m3, m4)
summary(m3)

#mod set 2: hysterectomy category defined as ordinal category increasing in severity
m5 <- lm(totalPqb ~ as.numeric(HystCats), moddat)
m6 <- lm(totalPqb ~ as.numeric(HystCats) + age + HRT, moddat)
m7 <- lm(totalPqb ~ as.numeric(HystCats)*age + HRT, moddat) #best mod (weight=0.632)
m8 <- lm(totalPqb ~ as.numeric(HystCats)*HRT + age, moddat)

ModSelOrdHystCats <- model.sel(m5, m6, m7, m8)
summary(m7)

################################################################################
#plots
################################################################################

##########################################
#mod set 1 plots
##########################################
#p1 Having a surgical procedure of any kind predicted increased PQB total scores
#anyHysts vs PQB score boxplot showing differences in means

AnyIntervetionXpqb_Boxplot <- moddat %>% 
    ggplot(aes(y=totalPqb, x=anyHysts)) +
    geom_boxplot(aes(fill=anyHysts)) +
    geom_point(alpha=0.4, position = "jitter") +
    theme_classic() +
    scale_fill_manual(values=c("orange", "#098909"), guide="none") +
    scale_x_discrete(labels = c('No procedure','Had procedure')) 

ggsave(AnyIntervetionXpqb_Boxplot, file=file.path(path, "ppt/PqbXageXhystCats_plot.pdf"))

#p2 Younger age predicted increased PQB (β = -0.19, SE = 0.04, p < .001).
#continous line/point age vs pqb - should see negative relationship 

ageXPQB <- moddat %>%
    ggplot(aes(x=totalPqb, y=age)) +
    geom_point() +
    geom_smooth(method='lm') +
    theme_classic() +
    facet_wrap(~HRT)

ggsave(ageXPQB, file=file.path(path, "ppt/ageXPQB.pdf"))

#p3 Not being on HRT predicted increased PQB (β = -1.72, SE = 0.76, p = .024).
#boxplot - binary HRT variable; y axis pqb

HRTxPQB_boxplot <- moddat %>% 
    ggplot(aes(x=HRT, y=totalPqb)) +
    geom_boxplot(aes(fill=HRT)) +
    geom_point(alpha=0.4, position = "jitter") +
    theme_classic() +
    scale_x_discrete(labels = c('Not taking','Taking')) +
    scale_fill_discrete(guide="none")

ggsave(HRTxPQB_boxplot, file=file.path(path, "ppt/HRTxPQB_boxplot.pdf"))

#p4 Significant interaction between age and having any kind of surgical procedure
#x=pqb, y=age; group=surgical proceedure - bare in mind model is treatting varialbe as ordinal - continuous
# so may have to use colour or something to indicate

agexPQBxAnyhyst <- moddat %>%
    ggplot(aes(y=totalPqb, x=age)) +
    geom_point(alpha=0.4) +
    geom_smooth(aes(color=anyHysts), se=F, method='lm') +
    theme_classic() +
    scale_color_manual(values=c("orange", "#098909")) #+
    #facet_grid(~HRT)

ggsave(agexPQBxAnyhyst, file=file.path(path, "ppt/agexPQBxAnyhyst.pdf"))

joinedBoxplots<-HRTxPQB_boxplot | AnyIntervetionXpqb_Boxplot
ggsave(joinedBoxplots, file=file.path(path, "ppt/joinedBoxplots.pdf"))

joinedBoxplotsAndLinePlot<- (HRTxPQB_boxplot | AnyIntervetionXpqb_Boxplot) / agexPQBxAnyhyst
ggsave(joinedBoxplotsAndLinePlot, file=file.path(path, "ppt/joinedBoxplotsAndLinePlot.pdf"))


##########################################
#mod set 2 plots
##########################################

#p1 As procedure severity increased, PQB total scores also increased significantly (β = 4.79, SE = 2.37, p = .043).
#x=pqb, y=hysterectomy??

ordHystxPQB <- moddat %>%
    ggplot(aes(y=totalPqb, x=HystCats)) +
    geom_boxplot(aes(fill=HRT)) +
    theme_classic() +
    scale_fill_manual(values=c("orange", "#098909"), labels = c('Not taking','Taking')) +
    scale_x_discrete(labels = c('None','Partial hysterectomy',"full hysterectomy", "salpingectomy", "oophorectomy")) 

ordHystxPQB_facetHRT <- moddat %>%
    ggplot(aes(y=totalPqb, x=HystCats)) +
    geom_boxplot(aes(fill=HRT)) +
    theme_classic() +
    scale_fill_manual(values=c("orange", "#098909"), labels = c('Not taking','Taking')) +
    scale_x_discrete(labels = c('None','Partial hysterectomy',"full hysterectomy", "salpingectomy", "oophorectomy")) +
    facet_wrap(~HRT)

ggsave(ordHystxPQB, file=file.path(path, "ppt/ordHystxPQB.pdf"))
ggsave(ordHystxPQB_facetHRT, file=file.path(path, "ppt/ordHystxPQB_facetHRT.pdf"))