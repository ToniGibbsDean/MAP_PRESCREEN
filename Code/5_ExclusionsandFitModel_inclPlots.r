

#d<-readRDS("Outputs/largeModDF.RDS")
#d<-readRDS("Outputs/summarydatincomplete.RDS")
library(GGally)
library(MuMIn)
library(tidyverse)
library(patchwork)
library(purrr)
library(rlang)
library(dplyr)
library(gtsummary)
library(gt)

path="Figures"

d<-readRDS("Outputs/summarydf.RDS")

################################################################################
# exclusions 
################################################################################
track_filters <- function(data, ...) {
  conditions <- enquos(...)
  names(conditions) <- sapply(conditions, rlang::as_label)

  out <- tibble(Step = "Original", RowsRemaining = nrow(data), RowsRemoved = 0)
  current_data <- data

  for (i in seq_along(conditions)) {
    step_label <- names(conditions)[i]
    before <- nrow(current_data)
    current_data <- filter(current_data, !!conditions[[i]])
    after <- nrow(current_data)
    out <- bind_rows(out, tibble(
      Step = step_label,
      RowsRemaining = after,
      RowsRemoved = before - after
    ))
  }

  list(filtered_data = current_data, tracking_table = out)
}

result <- track_filters(
  d,
  birth_sex == "female",
  country_US == "USA",
  !is.na(birth_control),
  !is.na(pregnant),
  pregnant != "Yes",
  !is.na(totalPqb),
  EatingDisorder == 0,
  SubstanceUseDisorder == 0,
  seizure == 0,
  Endocrine_or_thyroid_Disease == 0,
  psychosisDXyesNo == 0,
  MEN_IorII == 0
)

# Extract both elements
d_filtered <- result$filtered_data #923
trackingExclusions <- result$tracking_table

write.csv(trackingExclusions, "Outputs/trackingExclusions.csv")

sampleCharTab <- d_filtered %>%
  select(DataCollection, age, birth_sex, 
        menopausalStatus, HystCats, lastperiod_daysSince, birth_control, 
        PCOS, Endocrine_or_thyroid_Disease, EatingDisorder, SubstanceUseDisorder, seizure,
        psychosisDXyesNo, moodDXyesNo, totalPqb) %>%
  tbl_summary(missing = "ifany") %>%
  bold_labels()

  gtsave(sampleCharTab, filename = "sample_characteristics.html")
gtsave(as_gt(sampleCharTab), "Outputs/table1.pdf")
  ##### YOU ARE HERE #####


################################################################################
#Create modelling dataframes 
################################################################################

#newly defined categories 
moddat <- d %>%
    mutate(anyHysts = !HystCats=="None") %>%
    mutate(logPqb = log(totalPqb+0.001)) %>%
    mutate(HRT = as.factor(HRT))

################################################################################
#looking at relationhsips - only inc. hystcats and age
################################################################################
ggpairs(d)

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