library(tidyverse)
library(dplyr)
library(ggpubr)
library(dplyr)
library(purrr)
library(rlang)
path="Figures"

x<-readRDS("Outputs/medsCalcs.RDS")

datcl <- x %>%
  mutate(
    birth_sex = factor(case_when(
      is.na(birth_sex) ~ "PreferNotToSay/other/NA",
      birth_sex == 2 ~ "PreferNotToSay/other",
      birth_sex == 0 ~ "male",
      TRUE ~ "female"
    )),
    
    dob_parsed = parse_date_time(dob, orders = c("mdy", "dmy", "ymd")),
    prescreendate_parsed = parse_date_time(prescreendate, orders = c("mdy", "dmy", "ymd")),
    age = floor(time_length(interval(dob_parsed, prescreendate_parsed), "years")),
    
    country_US = as_factor(case_when(
      is.na(residence_state) ~ "NotUSA",
      TRUE ~ "USA"
    )),
    
    zipcode = as_factor(residence_zipcode),
    
    lastperiod_parsed = parse_date_time(lastperiod, orders = c("mdy", "dmy", "ymd")),
    lastperiod_daysSince = floor(time_length(interval(lastperiod_parsed, prescreendate_parsed), "days")),
    menopausalStatus = lastperiod_daysSince > 365,
    
    birth_control = factor(case_when(
      birth_control == 0 ~ "No",
      birth_control == 1 ~ "Yes"
    )),
    
    Endocrine_or_thyroid_Disease = factor(exclusion_dx1___1),
    PCOS = factor(exclusion_dx1___2),
    MEN_IorII = factor(exclusion_dx1___3),
    EatingDisorder = factor(exclusion_dx1___6),
    SubstanceUseDisorder = factor(exclusion_dx1___8),
    
    pregnant = factor(case_when(
      pregnancy == 1 ~ "Yes",
      pregnancy == 0 ~ "No"
    )),
    
    totalPqb = rowSums(across(c(
      "pqb1", "pqb2", "pqb3", "pqb4", "pqb5", 
      "pqb6", "pqb7", "pqb8", "pqb9", "pqb10",
      "pqb11", "pqb12", "pqb13", "pqb14", "pqb15",
      "pqb16", "pqb17", "pqb18", "pqb19", "pqb20",
      "pqb21"
    ))),
    
    HystCats = factor(case_when(
      hyster1___7 == 1 ~ "None",
      hyster1___3 == 1 ~ "Oopherectomy",
      hyster1___2 == 1 ~ "FullHysterectomy",
      hyster1___1 == 1 ~ "PartialHysterectomy",
      hyster1___4 == 1 ~ "Salpingectomy",
      hyster1___6 == 1 ~ "OtherGyneSurgery",
      hyster1___5 == 1 ~ "Unknown",
      .default = "None"
    )),
    
    HystCats = factor(HystCats, levels = c(
      "None", "Unknown", "OtherGyneSurgery",
      "Salpingectomy", "PartialHysterectomy",
      "FullHysterectomy", "Oopherectomy"
    ), ordered = TRUE),
    
  hyster2_clean = str_extract(hyster2, "\\d+"),
  ageAtHyst = as.numeric(hyster2_clean),
  ageAtHyst = ifelse(ageAtHyst > 100, NA, ageAtHyst))  # remove impossible values


# Mood and psychosis dx data - must be processed in old and new redcap projects seperately 

oldMoodDat <- datcl %>% 
  filter(DataCollection == "old") %>%
  mutate(
    BPDI_age = as.numeric(mooddx1),
    BPDII_age = as.numeric(mooddx2),
    MDD_Dep_otherDep_age = suppressWarnings(as.numeric(mooddx3)),
    CyclothymicDep_age = as.numeric(mooddx4),
    BPD_related_onlyNewRedCap_age = as.numeric(mooddx5),
    Other_dep_onlyNewRedCap_age = as.numeric(mooddx6),
    Schizophrenia_Spectrum_Disorder_age = as.numeric(psychosisdx1),
    Delusional_Disorder_age = as.numeric(psychosisdx2),
    Brief_Psychotic_Disorder_age = as.numeric(psychosisdx3),
    Schizophreniform_Disorder_age = as.numeric(psychosisdx4),
    Schizoaffective_Disorder_age = as.numeric(psychosisdx5),
    psychosisPD_combined_Age = as.numeric(coalesce(psychosisdx6, psychosisdx7, psychosisdx8)),
    catatonia_age = as.numeric(psychosisdx9),
    OtherPsychosis_age = as.numeric(psychosisdx10),

    BPDI = factor(case_when(as.numeric(mooddx1) >= 1 ~ 1, is.na(mooddx1) ~ 0, TRUE ~ 0)),
    BPDII = factor(case_when(as.numeric(mooddx2) >= 1 ~ 1, is.na(mooddx2) ~ 0, TRUE ~ 0)),
    MDD_Dep_otherDep = factor(suppressWarnings(case_when(as.numeric(mooddx3) >= 1 ~ 1, is.na(mooddx3) ~ 0, TRUE ~ 0))),
    CyclothymicDep = factor(case_when(as.numeric(mooddx4) >= 1 ~ 1, is.na(mooddx4) ~ 0, TRUE ~ 0)),
    BPD_related_onlyNewRedCap = factor(case_when(as.numeric(mooddx5) >= 1 ~ 1, is.na(mooddx5) ~ 0, TRUE ~ 0)),
    Other_dep_onlyNewRedCap = factor(case_when(as.numeric(mooddx6) >= 1 ~ 1, is.na(mooddx6) ~ 0, TRUE ~ 0)),

    Schizophrenia_Spectrum_Disorder = factor(case_when(as.numeric(psychosisdx1) >= 1 ~ 1, is.na(psychosisdx1) ~ 0, TRUE ~ 0)),
    Delusional_Disorder = factor(case_when(as.numeric(psychosisdx2) >= 1 ~ 1, is.na(psychosisdx2) ~ 0, TRUE ~ 0)),
    Brief_Psychotic_Disorder = factor(suppressWarnings(case_when(as.numeric(psychosisdx3) >= 1 ~ 1, is.na(psychosisdx3) ~ 0, TRUE ~ 0))),
    Schizophreniform_Disorder = factor(case_when(as.numeric(psychosisdx4) >= 1 ~ 1, is.na(psychosisdx4) ~ 0, TRUE ~ 0)),
    Schizoaffective_Disorder = factor(case_when(as.numeric(psychosisdx5) >= 1 ~ 1, is.na(psychosisdx5) ~ 0, TRUE ~ 0)),
    
    psychosisPD_coalesced = as.numeric(coalesce(psychosisdx6, psychosisdx7, psychosisdx8)),
    psychosisPD_combined = factor(case_when(psychosisPD_coalesced == 0 |  psychosisPD_coalesced >=3 ~ 0, psychosisPD_coalesced == 1 ~ 1, TRUE ~ 0)),

    catatonia = factor(case_when(as.numeric(psychosisdx9) >= 1 ~ 1, is.na(psychosisdx9) ~ 0, TRUE ~ 0)),
    OtherPsychosis = factor(case_when(as.numeric(psychosisdx10) >= 1 ~ 1, is.na(psychosisdx10) ~ 0, TRUE ~ 0))
  )

newcheck <- datcl %>% 
  filter(DataCollection == "new") %>%
  mutate(
    BPDI = factor(case_when(mooddx1 == 0 | mooddx1 == 3 ~ 0, mooddx1 == 1 ~ 1, TRUE ~ 0)),
    BPDII = factor(case_when(mooddx2 == 0 | mooddx1 == 3 ~ 0, mooddx2 == 1 ~ 1, TRUE ~ 0)),
    MDD_Dep_otherDep = factor(case_when(mooddx3 == 0 | mooddx1 == 3 ~ 0, mooddx3 == 1 ~ 1, TRUE ~ 0)),
    CyclothymicDep = factor(case_when(mooddx4 == 0 | mooddx1 == 3 ~ 0, mooddx4 == 1 ~ 1, TRUE ~ 0)),
    BPD_related_onlyNewRedCap = factor(case_when(mooddx5 == 0 | mooddx1 == 3 ~ 0, mooddx5 == 1 ~ 1, TRUE ~ 0)),
    Other_dep_onlyNewRedCap = factor(case_when(mooddx6 == 0 | mooddx1 == 3 ~ 0, mooddx6 == 1 ~ 1, TRUE ~ 0)),

    BPDI_age = as.numeric(age_mooddx1),
    BPDII_age = as.numeric(age_mooddx2),
    MDD_Dep_otherDep_age = suppressWarnings(as.numeric(age_mooddx3)),
    CyclothymicDep_age = as.numeric(age_mooddx4),
    BPD_related_onlyNewRedCap_age = as.numeric(age_mooddx5),
    Other_dep_onlyNewRedCap_age = as.numeric(age_mooddx6)
  ) %>%
  ##filter(
    #(BPDII_age <= 100 | is.na(BPDII_age)),
    #(MDD_Dep_otherDep_age <= 100 | is.na(MDD_Dep_otherDep_age)),
    #(Other_dep_onlyNewRedCap_age <= 100 | is.na(Other_dep_onlyNewRedCap_age))
  #) %>%
  mutate(
    Schizophrenia_Spectrum_Disorder = factor(case_when(psychosisdx1 == 0 | psychosisdx1 == 3 ~ 0, psychosisdx1 == 1 ~ 1, TRUE ~ 0)),
    Delusional_Disorder = factor(case_when(psychosisdx2 == 0 | psychosisdx2 == 3 ~ 0, psychosisdx2 == 1 ~ 1, TRUE ~ 0)),
    Brief_Psychotic_Disorder = factor(case_when(psychosisdx3 == 0 | psychosisdx3 == 3 ~ 0, psychosisdx3 == 1 ~ 1, TRUE ~ 0)),
    Schizophreniform_Disorder = factor(case_when(psychosisdx4 == 0 | psychosisdx4 == 3 ~ 0, psychosisdx4 == 1 ~ 1, TRUE ~ 0)),
    Schizoaffective_Disorder = factor(case_when(psychosisdx5 == 0 | psychosisdx5 == 3 ~ 0, psychosisdx5 == 1 ~ 1, TRUE ~ 0)),

    psychosisPD_coalesced = as.numeric(coalesce(psychosisdx6, psychosisdx7, psychosisdx8)),
    psychosisPD_combined = factor(case_when(psychosisPD_coalesced == 0 | psychosisPD_coalesced >=3  ~ 0, psychosisPD_coalesced == 1 ~ 1, TRUE ~ 0)),

    catatonia = factor(case_when(psychosisdx9 == 0 | psychosisdx9 == 3 ~ 0, psychosisdx9 == 1 ~ 1, TRUE ~ 0)),
    OtherPsychosis = factor(case_when(psychosisdx10 == 0 | psychosisdx10 == 3 ~ 0, psychosisdx10 >= 1 ~ 1, TRUE ~ 0)),

    Schizophrenia_Spectrum_Disorder_age = as.numeric(age_psychosisdx1),
    Delusional_Disorder_age = as.numeric(age_psychosisdx2),
    Brief_Psychotic_Disorder_age = suppressWarnings(as.numeric(age_psychosisdx3)),
    Schizophreniform_Disorder_age = as.numeric(age_psychosisdx4),
    Schizoaffective_Disorder_age = as.numeric(age_psychosisdx5),
    psychosisPD_combined_Age = as.numeric(coalesce(age_psychosisdx6, age_psychosisdx7, age_psychosisdx8)),
    catatonia_age = as.numeric(age_psychosisdx9),
    OtherPsychosis_age = as.numeric(age_psychosisdx10)
  ) #%>%
  #filter(
   # (Brief_Psychotic_Disorder_age <= 100 | is.na(Brief_Psychotic_Disorder_age)),
    #(OtherPsychosis_age >= 100 | is.na(OtherPsychosis_age))
  #)

allClnDat <- bind_rows(oldMoodDat, newcheck) %>%
  select(-starts_with("mooddx"), -starts_with("age_mooddx")) %>%
  select(-starts_with("psychosisdx"), -starts_with("age_psychosisdx")) %>%
  mutate(
    moodDXyesNo = rowSums(across(c(BPDI, BPDII, MDD_Dep_otherDep, CyclothymicDep, Other_dep_onlyNewRedCap, BPD_related_onlyNewRedCap)) == 1, na.rm = TRUE) > 0,
    psychosisDXyesNo = rowSums(across(c(Schizophrenia_Spectrum_Disorder, Delusional_Disorder, Brief_Psychotic_Disorder, Schizophreniform_Disorder, Schizoaffective_Disorder, psychosisPD_combined, catatonia, OtherPsychosis)) == 1, na.rm = TRUE) > 0
  )

#DIM OF ALLCLNDAT = 1387

#filtering for cleaning puposes DIM = 1307

datcl_filtered <- allClnDat %>%
  filter(
    !is.na(lastperiod_daysSince),
    lastperiod_daysSince >= 0 & lastperiod_daysSince <= 20000,
    age > 0 & !is.na(age),
    lastperiod_daysSince >= 0 & lastperiod_daysSince <= 20000,
    (Brief_Psychotic_Disorder_age <= 100 | is.na(Brief_Psychotic_Disorder_age)),
    (OtherPsychosis_age >= 100 | is.na(OtherPsychosis_age)),
        (BPDII_age <= 100 | is.na(BPDII_age)),
    (MDD_Dep_otherDep_age <= 100 | is.na(MDD_Dep_otherDep_age)),
    (Other_dep_onlyNewRedCap_age <= 100 | is.na(Other_dep_onlyNewRedCap_age))
  )

############################################################################
#Create exclusions tracking csv
###########################################################################
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

  return(out)
}

filter_report <- track_filters(
  allClnDat,
    !is.na(lastperiod_daysSince),
    lastperiod_daysSince >= 0 & lastperiod_daysSince <= 20000,
    age > 0 & !is.na(age),
    lastperiod_daysSince >= 0 & lastperiod_daysSince <= 20000,
    (Brief_Psychotic_Disorder_age <= 100 | is.na(Brief_Psychotic_Disorder_age)),
    (OtherPsychosis_age >= 100 | is.na(OtherPsychosis_age)),
        (BPDII_age <= 100 | is.na(BPDII_age)),
    (MDD_Dep_otherDep_age <= 100 | is.na(MDD_Dep_otherDep_age)),
    (Other_dep_onlyNewRedCap_age <= 100 | is.na(Other_dep_onlyNewRedCap_age))
)


# create summary data frame
summarydf <- datcl_filtered %>%
select(email,
       DataCollection,
       age, 
       birth_sex,
       country_US,
       zipcode,
       lastperiod_daysSince,
       menopausalStatus,
       Endocrine_or_thyroid_Disease,
       PCOS,
       MEN_IorII,
       EatingDisorder,
       SubstanceUseDisorder,
       birth_control,
       seizure,
       pregnant,
        totalPqb,
        HystCats,
        ageAtHyst,
        #hyster2_ageAtHystorOop,
        #remove_flag,
        psychosisDXyesNo,
        moodDXyesNo,
        BPDI,
        BPDI_age,
        BPDII,
        BPDII_age,
        MDD_Dep_otherDep,
        MDD_Dep_otherDep_age,
        CyclothymicDep,
        CyclothymicDep_age,
        BPD_related_onlyNewRedCap,
        BPD_related_onlyNewRedCap_age,
        Other_dep_onlyNewRedCap,
        Other_dep_onlyNewRedCap_age,
        Schizophrenia_Spectrum_Disorder,
        Schizophrenia_Spectrum_Disorder_age,
        Delusional_Disorder,
        Delusional_Disorder_age,
        Brief_Psychotic_Disorder_age,
        Brief_Psychotic_Disorder,
        Schizophreniform_Disorder,
        Schizophreniform_Disorder_age,
        Schizoaffective_Disorder,
        Schizoaffective_Disorder_age,
        psychosisPD_combined,
        psychosisPD_combined_Age,
        catatonia,
        catatonia_age,
        OtherPsychosis,
        OtherPsychosis_age,
        bc_cleaned,
        othermeds_ht_cleaned,
        HT,
        iud_type,
        bc_type,
        hormone_therapy_category,
        local_global_category
       ) 


saveRDS(summarydf, "Outputs/summarydf.RDS")
write.csv(filter_report, "Outputs/trackingExclusions_datacleaning.csv")
write.csv(summarydf, "Outputs/summarydf.csv")
