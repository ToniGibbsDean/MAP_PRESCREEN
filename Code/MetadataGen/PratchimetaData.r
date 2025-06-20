    
#Effect of hysterectomies of several varieties on PQ-B score. These include partial, total, salpingectomy, and oophorectomy.

#packages and read in files
library(tidyverse)
library(dplyr)
library(ggpubr)
library(janitor)
path="Figures"

HRT<-read.csv("Data/bcDetails_with_HRT_excluding_BirthControl.csv")

datDic_raw <- read.csv("Data/PREMAPIntegration_DataDictionary_2025-01-2.csv") %>%
  janitor::clean_names()
#datDicsub <- datDic_raw[c(1, 2, 6)]  %>%
  #janitor::clean_names()

datraw <- read.csv("Data/PREMAPIntegration-Prachi4152025_DATA_2025-04-15_1332 (1).csv", row.names=NULL)
alldat <- as.data.frame(datraw)

vars_in_alldat <- tibble(variable = names(alldat))
var_metadata <- vars_in_alldat %>%
left_join(datDic_raw, by = c("variable" = "variable_field_name"))

incColsDat1 <- alldat %>%
  summarise(across(
    everything(),
    list(
      n_NAs = ~sum(is.na(.)),
      n_non_NAs = ~sum(!is.na(.))
    )
  )) %>%
  pivot_longer(
    everything(),
    names_to = c("variable", ".value"),
    names_sep = "_n_"
  )

incColsDat <- incColsDat1 %>%
  left_join(var_metadata %>% select(variable = field_type, field_label, choices_calculations_or_slider_labels), by = "variable") %>%
  rowwise() %>%
  mutate(
    n_unique = n_distinct(na.omit(alldat[[variable]])),
    unique_vals = {
      vals <- unique(na.omit(alldat[[variable]]))
      formatted_vals <- if (length(vals) <= 10) {
        paste(vals, collapse = ", ")
      } else {
        paste(c(vals[1:10], "..."), collapse = ", ")
      }
      if (any(is.na(alldat[[variable]]))) {
        paste(formatted_vals, "NA", sep = ", ")
      } else {
        formatted_vals
      }
    },
    moreMissingThanNot = NAs > non_NAs
  ) %>%
  ungroup()

incColsDat$formName <- var_metadata$form_name
incColsDat$choices_calculations_or_slider_labels <- var_metadata$choices_calculations_or_slider_labels
incColsDat$field_type <-var_metadata$field_type

x<-as_tibble(incColsDat)

write.csv(incColsDat, "Outputs/Prachi4152025_metaDataFile.csv")


###was surgery beore menopause

###was mood dx onset before menopause or surgery


