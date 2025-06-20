    
#Effect of hysterectomies of several varieties on PQ-B score. These include partial, total, salpingectomy, and oophorectomy.

#packages and read in files
library(tidyverse)
library(dplyr)
library(ggpubr)
library(janitor)
path="Figures"

HRT<-read.csv("Data/bcDetails_with_HRT_excluding_BirthControl.csv")

datDic_raw <- read.csv("Data/PREMAPIntegration_DataDictionary_2025-01-2.csv")
datDicsub <- datDic_raw[c(1, 2, 6)]  %>%
  janitor::clean_names()

datraw <- read.csv("Data/PREMAPIntegration_DATA_2025-01-28_1610.csv")
alldat <- datraw[-1, ]

vars_in_alldat <- tibble(variable = names(alldat))
var_metadata <- vars_in_alldat %>%
left_join(datDicsub, by = c("variable" = "variable_field_name"))

incColsDat <- alldat %>%
  summarise(across(
    everything(),
    list(n_NAs = ~sum(is.na(.)), n_non_NAs = sum(!is.na(.)))
  )) %>%
  pivot_longer(
    everything(),
    names_to = c("variable", ".value"),
    names_sep = "_n_"
  ) %>%
  print(n = 350)

incColsDat <- incColsDat %>%
  rowwise() %>%
  mutate(
    # Count number of unique non-NA values
    n_unique = n_distinct(na.omit(alldat[[variable]])),

    # Pull unique non-NA values
    unique_vals = {
      vals <- unique(na.omit(alldat[[variable]]))

      # Truncate if more than 10
      formatted_vals <- if (length(vals) <= 10) {
        paste(vals, collapse = ", ")
      } else {
        paste(c(vals[1:10], "..."), collapse = ", ")
      }

      # Append "NA" if the variable has any missing
      if (any(is.na(alldat[[variable]]))) {
        paste(formatted_vals, "NA", sep = ", ")
      } else {
        formatted_vals
      }
    },
    moreMissingThanNot = NAs > non_NAs
    # Mark if this variable is mostly missing
    #mostly_missing = n_NAs > n_non_NAs
  ) %>%
  ungroup()

incColsDat$formName <- var_metadata$form_name
incColsDat$choices_calculations_or_slider_labels <- var_metadata$choices_calculations_or_slider_labels


write.csv(incColsDat, "Outputs/metaDataFile.csv")


###was surgery beore menopause

###was mood dx onset before menopause or surgery


