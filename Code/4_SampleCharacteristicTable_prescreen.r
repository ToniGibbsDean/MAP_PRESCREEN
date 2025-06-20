#SampleCharacteristicTable

library(tidyverse)
library(gtsummary)
library(gt)

path="Figures"

d<-readRDS("Outputs/summarydf.RDS")

prescreenCleanDat_sampleCharTab <- d %>%
  select(DataCollection, age, birth_sex, 
        menopausalStatus, HystCats, lastperiod_daysSince, birth_control, 
        PCOS, Endocrine_or_thyroid_Disease, EatingDisorder, SubstanceUseDisorder, seizure,
        psychosisDXyesNo, 
        moodDXyesNo, 
        totalPqb,
        HT,
        iud_type,
        bc_type,
        hormone_therapy_category,
        local_global_category
) %>%
  tbl_summary(missing = "ifany") %>%
  bold_labels()


prescreenCleanDat_sampleCharTab_dxInfo <- d %>%
  # Ensure all *_age variables are numeric
  mutate(across(ends_with("_age"), as.numeric)) %>%
  select(
        Schizophrenia_Spectrum_Disorder,
        Schizophrenia_Spectrum_Disorder_age,
        Delusional_Disorder,
        Delusional_Disorder_age,
        Brief_Psychotic_Disorder,
        Brief_Psychotic_Disorder_age,
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
        Other_dep_onlyNewRedCap_age) %>%
  tbl_summary(
    type = list(
      matches("_age$") ~ "continuous"  # Force all *_age to be treated as continuous
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})"
    ),
    missing = "ifany"
  ) %>%
  bold_labels()


gtsave(sampleCharTab, filename = "sample_characteristics.html")
gtsave(as_gt(prescreenCleanDat_sampleCharTab), "Outputs/prescreenCleanDat_sampleCharTab.pdf")
gtsave(as_gt(prescreenCleanDat_sampleCharTab_dxInfo), "Outputs/prescreenCleanDat_sampleCharTab_dxInfo.pdf")