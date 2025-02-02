

d<-readRDS("Outputs/largeModDF.RDS")
library(GGally)
library(MuMIn)
library(tidyverse)

path="Figures"

#looking at relationhsips - only inc. hystcats and age
ggpairs(d)

moddat <- d %>%
    mutate(anyHysts = !HystCats=="none") %>%
    mutate(logPqb = log(totalPqb+0.001)) %>%
    mutate(hystMag = case_when(HystCats == "fullHyst" ~ "fullHyst",
                               HystCats == "partialHyst" ~ "partialHyst",
                               HystCats == "none" ~ "none", 
                               .default = "other"))

#modelling
m1 <- lm(totalPqb ~ HystCats, moddat)
m2 <- lm(totalPqb ~ age, moddat)
m3 <- lm(totalPqb ~ HystCats + age, moddat)
m4 <- lm(totalPqb ~ HystCats*age, moddat)
m5 <- lm(totalPqb ~ anyHysts + age, moddat)
m6 <- lm(totalPqb ~ anyHysts*age, moddat)
m7 <- lm(totalPqb ~ anyHysts, moddat)

m8 <- lm(totalPqb ~ hystMag, moddat)
m9 <- lm(totalPqb ~ hystMag + age, moddat)
m10 <- lm(totalPqb ~ hystMag*age, moddat)

model.sel(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)

summary(m6)

wilcox.test(totalPqb ~ anyHysts + age, moddat)

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


