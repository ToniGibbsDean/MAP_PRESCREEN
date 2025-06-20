#code developed by prachi patel + arohi dandawate (with assistance from Chat GPT/Claude AI)
# ====================
# Upload cleaned file 
read.csv()...

readRDS("Outputs/medications_classified.RDS")

# ====================
# Classify Type Based on Cleaned Name
# ====================

# Classifies the cleaned birth control name into major categories
# IUD, Implant, Ring, HRT, OCP, Progestin Only, or Unknown

classify_bc_type <- function(cleaned_term) {
  # Handle NA or empty values
  if (is.na(cleaned_term) || trimws(cleaned_term) == "") {
    return("0") #this should now make sure that if there is no bc/hrt, it will return 0 instead of unknown
  }
  
  cleaned_lower <- tolower(cleaned_term)
  
  # Check for combined HRT treatments
  if (str_detect(cleaned_lower, "estradiol.*testosterone|testosterone.*estradiol") || 
      str_detect(cleaned_lower, "progesterone.*testosterone|testosterone.*progesterone") ||
      str_detect(cleaned_lower, "estradiol.*progesterone|progesterone.*estradiol") ||
      str_detect(cleaned_lower, "estrogen.*testosterone|testosterone.*estrogen") ||
      str_detect(cleaned_lower, "estrogen.*progesterone|progesterone.*estrogen")) {
    return("HT Combination")
  }
  
   if (str_detect(cleaned_lower, "iud")) {
    return("IUD")
  } else if (str_detect(cleaned_lower, "implant|nexplanon|implanon|subdermal")) {
    return("Implant")
  } else if (str_detect(cleaned_lower, "slynd")) { #pp 6/7
    return("POP")
  } else if (str_detect(cleaned_lower, "ring")) {
    return("Ring")
  } else if (str_detect(cleaned_lower, "combipatch|estradiol patch + micronized progesterone")) { #asd 6/9
    return("Combipatch")
  } else if (str_detect(cleaned_lower, "estradiol patch")) { #asd 6/9
    return("Estradiol Patch")
  } else if (str_detect(cleaned_lower, "patch")) { #asd 6/9
    return("Patch (Unspecified)")
  } else if (str_detect(cleaned_lower, "provera|depo")) {
    return("Shot")
  } else if (str_detect(cleaned_lower, "levonorgestrel")) {
    return("Emergency Contraceptive")
  } else if (str_detect(cleaned_lower, "estradiol|hrt|progesterone|testosterone|estrogen|premarin|prometrium|micronor|mimvey|dotti|angeliq|bijuvia")) {
    return("HRT")
  } else if (str_detect(cleaned_lower, "progestin only|camila")) {
    return("Progestin Only")
  } else if (str_detect(cleaned_lower, "birth control") && !str_detect(cleaned_lower, "pill")) {
    return("Birth Control")
  } else if (str_detect(cleaned_lower, "anastrozole")) {
    return("Breast Cancer Treatment")
  } else if (cleaned_term %in% ocp_cleaned_terms) {
    return("OCP")
  } else {
    return("Unknown")
  }
}

# Vector of known hormonal and non-hormonal IUD types - ASD Start
hormonal_iuds <- c("Mirena IUD", "Kyleena IUD", "Skyla IUD", "Liletta IUD", "Hormonal IUD")
nonhormonal_iuds <- c("Copper IUD")

# Function to classify IUD types
classify_iud_type <- function(cleaned_term) {
  if (is.na(cleaned_term) || cleaned_term == "") {
    return(NA)
  }
  
  # Split in case multiple matches returned
  terms <- unlist(strsplit(cleaned_term, ",\\s*"))
  
  # Check for any hormonal or non-hormonal matches
  if (any(terms %in% hormonal_iuds)) {
    return("IUD_hormonal")
  } else if (any(terms %in% nonhormonal_iuds)) {
    return("IUD_nonhormonal")
  } else if (any(grepl("IUD", terms))) {
    return("IUD_unknown")
  } else {
    return(NA)  # Not an IUD or unknown
  }
}
# Apply to dataset
ocp_cleaning <- ocp_cleaning %>%
  mutate(
    bc_cleaned = sapply(bc_details, clean_bc_term), 
    iud_type = sapply(bc_cleaned, classify_iud_type), 
    HT = case_when(
    bc_cleaned == "" | bc_cleaned == "Unknown" | iud_type %in% c("IUD_nonhormonal", "IUD_unknown") ~ 0, #pp edited asd code 6/10 (original code was counting unknowns as 1 for HT)
    bc_cleaned != "" & bc_cleaned != "Unknown" | iud_type == "IUD_hormonal" ~ 1,
    TRUE ~ 0
    )
  )

# Final output 
final_output_IUD_HT <- dplyr::select(ocp_cleaning, record_id, bc_details, bc_cleaned, bc_type, iud_type, HT) #keeping bc_type for rn - pp 6/10

#ASD - end, PP edited (moved code up to this section, kept it in second section as well)

#PP start - i am going to start from bc_type to make these columns, as i think that makes most sense 
# BC type - rename Hormone Therapy Category /type --> 
#birth control hormonal: 
  #IUD_hormonal;  implants; ring; OCP - which includes progestin only and combined 
#birth contrl non hormonal: 
  #IUD_nonhormonal; IUD_unknown, + any otheres
#HRT - 
  #any estradiol|hrt|progesterone|testosterone"
#none - 
  #not on any HT including IUDs of any kind
#unknown - 
  #unable to classify

#Making sure all the NA's remain 0's
Hormone_therapy_category <- function(bc_type, iud_type = NA) {
  if (is.na(bc_type) || trimws(bc_type) == "" || bc_type == "0") {
    return("0") #since bc_type will be 0 for the NA/s we keep that pp 6/10
  }
  cleaned_lower <- tolower(bc_type)
  
  # Classifying IUD first, so that it catches the correct "unknowns"
  if (!is.na(iud_type) && iud_type == "IUD_hormonal") {
    return("Birth Control Hormonal")   
  } else if (!is.na(iud_type) && iud_type %in% c("IUD_nonhormonal", "IUD_unknown")) {
    return("Birth Control Non Hormonal")
  }
  
  # Actual unknown 
  if (str_detect(cleaned_lower, "unknown")) {
    return("Unknown") 
  }
  
  # Hormonal, nonhormonal, and unknown based on bc_type
  if (str_detect(cleaned_lower, "hrt|ht combination|combipatch|estradiol patch|patch \\(unspecified\\)|progestin only")) {
    return("HRT")
  } else if (str_detect(cleaned_lower, "implant|pop|ocp|shot|ring|birth control|emergency contraceptive")) {
    return("Birth Control Hormonal")   
  } else {
    return("Unknown")
  }
}

# Mutate the column 
ocp_cleaning <- ocp_cleaning %>%
  mutate(
    bc_cleaned = sapply(bc_details, clean_bc_term), 
    bc_type = sapply(bc_cleaned, classify_bc_type),
    iud_type = sapply(bc_cleaned, classify_iud_type),
    hormone_therapy_category = mapply(Hormone_therapy_category, bc_type, iud_type),  # NEW COLUMN HERE
    HT = case_when(
      bc_cleaned == "" | bc_cleaned == "Unknown" | iud_type %in% c("IUD_nonhormonal", "IUD_unknown") ~ 0,
      bc_cleaned != "" & bc_cleaned != "Unknown" | iud_type == "IUD_hormonal" ~ 1,
      TRUE ~ 0
    )
  )

# ====================
# Creating local vs global from bc_type, pp 6/19
# ====================
# Only referring to birth control 
# Hormonal IUDs are LOCAL, all other BC types are GLOBAL

# Making sure all the NA's remain 0's
local_global <- function(bc_type, iud_type = NA) {
  if (is.na(bc_type) || trimws(bc_type) == "" || bc_type == "0") {
    return("0") # since bc_type will be 0 for the NA's we keep that
  }
  cleaned_lower <- tolower(bc_type)
  
  # Classifying IUD first
  if (!is.na(iud_type) && iud_type == "IUD_hormonal") {
    return("Birth Control Local")   
  } else if (!is.na(iud_type) && iud_type %in% c("IUD_nonhormonal", "IUD_unknown")) {
    return("0") 
  }
  
  # Actual unknown 
  if (str_detect(cleaned_lower, "unknown")) {
    return("Unknown") 
  }
  
  # HRT and systemic BC classification
  if (str_detect(cleaned_lower, "hrt|ht combination|combipatch|estradiol patch|patch \\(unspecified\\)|progestin only")) {
    return("0")
  } else if (str_detect(cleaned_lower, "implant|pop|ocp|shot|ring|birth control|emergency contraceptive")) {
    return("Birth Control Global")   # All these BC types are GLOBAL/systemic
  } else {
    return("0")
  }
}

# Mutate the column 
ocp_cleaning <- ocp_cleaning %>%
  mutate(
    bc_cleaned = sapply(bc_details, clean_bc_term), 
    bc_type = sapply(bc_cleaned, classify_bc_type),
    iud_type = sapply(bc_cleaned, classify_iud_type),
    local_global_category = mapply(local_global, bc_type, iud_type),  # Updated column name to reflect local/global
    HT = case_when(
      bc_cleaned == "" | bc_cleaned == "Unknown" | iud_type %in% c("IUD_nonhormonal", "IUD_unknown") ~ 0,
      bc_cleaned != "" & bc_cleaned != "Unknown" | iud_type == "IUD_hormonal" ~ 1,
      TRUE ~ 0
    )
  )

# ====================
# Creating exogeneous vs endogenous from clean_bc
# Getting stuck here (there are some "estrogen" terms in bc_cleaned that are part of bc
# that are being returned as HRT... should we take it out?
# ====================

#endogenous estrogen 
  #bc_cleaned = estradiol, Combipatch
#endogenous progesterone
  #bc_cleaned = progesterone
#endogenous estrogen and progesterone 
  #bc_cleaned = estradiol + progesterone 
#exogenous estrogen 
  #bc_type = OCP, implant, injection
#exogenous progesterone 
  #bc_cleaned = Combipatch, bc_type = POP, IUD_hormonal 

# ====================
# Creating HT direct/indirect for other_meds
# ====================
# Create the indirect HT column
df$`indirect HT` <- ifelse(
  is.na(df$othermeds_endodisrupter_cleaned) | df$othermeds_endodisrupter_cleaned == "", 
  "unknown",
  ifelse(
    df$othermeds_endodisrupter_cleaned %in% names(endocrine_disruptors_cleaned),
    1,
    0
  )
)

# Create the direct HT column
df$`direct HT` <- ifelse(
  !is.na(df$othermeds_ht_cleaned) & df$othermeds_ht_cleaned != "",
  1,
  0
)

# ====================
# Create output dataset
# ====================

ocp_cleaning <- as.data.frame(ocp_cleaning)

final_columns_ht <- dplyr::select(ocp_cleaning, bc_details, bc_cleaned, bc_type, iud_type, HT, hormone_therapy_category, record_id, age)
write.csv(final_columns_ht,'final_columns_ht_pp_06122025.csv')  
#PP end


#need to classify other_meds into indirect and direct 

saveRDS(ocp_cleaning, "Outputs/medsCalcs.RDS")
