
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

################################################################################
# sample overview
################################################################################

sampleCharTab <- d_filtered %>%
  select(DataCollection, age, birth_sex, 
        menopausalStatus, HystCats, ageAtHyst, lastperiod_daysSince, birth_control, 
        PCOS, Endocrine_or_thyroid_Disease, EatingDisorder, SubstanceUseDisorder, seizure,
        psychosisDXyesNo, moodDXyesNo, totalPqb,
        HT, bc_type, iud_type, hormone_therapy_category, local_global_category) %>%
  tbl_summary(missing = "ifany") %>%
  bold_labels()

gtsave(as_gt(sampleCharTab), "Outputs/sampleCharTab_paper.pdf")

################################################################################
# relationships 
################################################################################
corrs<-d_filtered %>%
select(HystCats, age, ageAtHyst, lastperiod_daysSince, totalPqb, HT)

ggpairs(corrs)

################################################################################
# modelling
################################################################################

#menostatus, hystcats, HT
#dayssince period (logged)
#age
#age at hysterectomy 

M0  <- lm(totalPqb ~ 1, d_filtered) # (null)
M1	<- lm(totalPqb ~ HystCats + menopausalStatus + age, d_filtered)
M2  <- lm(totalPqb ~ HystCats * menopausalStatus + age, d_filtered)
M3	<- lm(totalPqb ~ HystCats * HRT + menopausalStatus + age, d_filtered)
M4	<- lm(totalPqb ~ HystCats * menopausalStatus + HT * age, d_filtered)
M5	<- lm(totalPqb ~ HystCats * menopausalStatus + HT * age + log1p(lastperiod_daysSince), d_filtered)
M6	<- lm(totalPqb ~ HystCats * menopausalStatus + HT * age + menopausalStatus * log1p(lastperiod_daysSince), d_filtered)
M7	<- lm(totalPqb ~ HystCats * menopausalStatus + HT * age + menopausalStatus * log1p(lastperiod_daysSince) + ageAtHyst, d_filtered)
M8	<- lm(totalPqb ~ HystCats * menopausalStatus + HT * age + menopausalStatus * log1p(lastperiod_daysSince) + HystCats * ageAtHyst, d_filtered)
M9	<- lm(totalPqb ~ HystCats * menopausalStatus + HT * menopausalStatus + age + ageAtHyst + menopausalStatus * log1p(lastperiod_daysSince), d_filtered)