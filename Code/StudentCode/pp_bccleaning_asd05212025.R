#code developed by prachi patel with assistance from Chat GPT

#===============================================================================
#disclaimer - a lot of it is manual editing with the code, can add more misspelling variations if needed to account for future
#terms. feel free to edit to make it more automated as well. 
#===============================================================================
#packages
library(dplyr)
library(stringr)
library(stringdist)

#loading my dataset and i have extra rows so just cleaning that up 
#load your own dataset here
ocp_cleaning <- read.csv("/Users/arohidandawate/PowersLab/Analysis/PREMAP/HRT_PQB/PREMAPIntegration-HRTPQB_DATA_2025-04-08_1329.csv")
#ocp_cleaning <- ocp_cleaning %>%
  #slice(1:219)  # Keep rows from 1 to 219

# Filter rows where bc_details equals "1"
ocp_cleaning_filtered <- ocp_cleaning %>%
  filter(birth_control == "1")
#===============================================================================
# Create a master list of birth control (BC) and hormone replacement therapy (HRT) terms 
# List maps common terms (including misspellings and variations) to standardized names that will probably appear
# in the text
# Note: I used the filtered column (where they put yes to being on BC, to make it easier to visualize, you can 
# do either da)

bc_master_cleaned <- list(
  # IUDs
  "mirena" = "Mirena IUD", "mirena iud" = "Mirena IUD",
  "kyleena" = "Kyleena IUD", "kyleena iud" = "Kyleena IUD", "kylena" = "Kyleena IUD",
  "skyla" = "Skyla IUD", "liletta" = "Liletta IUD", "iud" = "IUD", "hormonal iud" = "Hormonal IUD",
  
  # Implant
  "nexplanon" = "Nexplanon", "implanon" = "Implanon", "unsure in arm" = "Subdermal Implant",
  
  # Rings
  "nuvaring" = "NuvaRing", "nuva ring" = "NuvaRing", "annovera" = "Annovera",
  
  # OCPs
  "loestrin fe" = "Loestrin FE", "loestrinfe" = "Loestrin FE",
  "junel fe" = "Junel FE", "junelfe" = "Junel FE",
  "tri lo mil" = "Tri-Lo-Mili", "trilomil" = "Tri-Lo-Mili",
  "simpesse" = "Simpesse", "alesse" = "Alesse", "aviane" = "Aviane", "beyaz" = "Beyaz",
  "cryssel" = "Cryssel", "desogen" = "Desogen", "estarylla" = "Estarylla",
  "kariva" = "Kariva", "levora" = "Levora", "lybrel" = "Lybrel", "microgestin" = "Microgestin",
  "minastrin" = "Minastrin", "nortrel" = "Nortrel", "ortho cyclen" = "Ortho Cyclen",
  "ortho tricyclen" = "Ortho Tri-Cyclen", "seasonale" = "Seasonale",
  "seasonique" = "Seasonique", "sprintec" = "Sprintec", "trinessa" = "Trinessa",
  "yasmin" = "Yasmin", "yaz" = "Yaz", "zovia" = "Zovia",
  
  # HRT
  "estradiol" = "Estradiol", "estradiol patch" = "Estradiol Patch", 
  "estradiol patch micronized progesterone" = "Estradiol Patch + Micronized Progesterone",
  "hrt" = "HRT",
  "progesterone" = "Progesterone",
  
  # Other/Unknown
  "progestin only birth control" = "Progestin Only", "camila" = "Camila",
  "norepinephrine" = "Unknown", "unknown" = "Unknown"
)

# List of cleaned terms that are OCPs - there was an error so i did this again 
ocp_cleaned_terms <- c(
  "Loestrin FE", "Junel FE", "Tri-Lo-Mili", "Simpesse", "Alesse", "Aviane", "Beyaz",
  "Cryssel", "Desogen", "Estarylla", "Kariva", "Levora", "Lybrel", "Microgestin",
  "Minastrin", "Nortrel", "Ortho Cyclen", "Ortho Tri-Cyclen", "Seasonale",
  "Seasonique", "Sprintec", "Trinessa", "Yasmin", "Yaz", "Zovia"
)

# ====================
# Clean Raw Terms Function
# ====================
# - Converts to lowercase
# - Has spelling check for manual errors, can expand this list if the code isn't super streamlined
# - Removes punctuation 
# - Checks for multi-component HRT combinations first (Estradiol + Progesterone [+ Testosterone])
# - Then checks for exact matches
# - If no exact match, uses fuzzy matching based on string distance (Jaro-Winkler)

clean_bc_term <- function(raw_term) {
  raw_lower <- tolower(trimws(raw_term))
  raw_clean <- str_replace_all(raw_lower, "[^a-z0-9]", "")  # remove spaces/punct
  
  #can add more terms here if needed for the future
  spelling_corrections <- list(
    "estrodiol" = "estradiol",
    "progestrone" = "progesterone",
    "progesteron" = "progesterone",
    "testostrone" = "testosterone",
    "testosteron" = "testosterone"
  )
  
  # Apply spelling corrections
  for (misspelling in names(spelling_corrections)) {
    raw_lower <- str_replace_all(raw_lower, misspelling, spelling_corrections[[misspelling]])
  }
  
  # Check for complex combinations with multiple components and dosages
  components <- c()
  
  # Check for estradiol patch - make sure the pattern is correct
  if (str_detect(raw_lower, "estradiol\\s*patch")) {
    components <- c(components, "Estradiol Patch")
  } 
  # Check for estradiol (not patch) - only if patch wasn't detected
  else if (str_detect(raw_lower, "estradiol")) {
    components <- c(components, "Estradiol")
  }
  
  # Check for progesterone
  if (str_detect(raw_lower, "progesterone")) {
    components <- c(components, "Progesterone")
  }
  
  # Check for testosterone
  if (str_detect(raw_lower, "testosterone")) {
    components <- c(components, "Testosterone")
  }
  
  # If we found multiple components, combine them
  if (length(components) > 1) {
    return(paste(components, collapse = " + "))
  }
  
  # If we found exactly one component, return it
  if (length(components) == 1) {
    return(components[1])
  }  
  
  # Multi-component HRT detection
  if (str_detect(raw_lower, "estradiol.*progesterone.*testosterone|estrodiol.*progesterone.*testosterone")) {
    return("Estradiol + Progesterone + Testosterone")
  } else if (str_detect(raw_lower, "estradiol.*progesterone|estrodiol.*progesterone")) {
    return("Estradiol + Progesterone")
  }
  
  # Direct match using master list
  for (term in names(bc_master_cleaned)) {
    pattern <- str_replace_all(term, " ", "")
    if (str_detect(raw_clean, regex(pattern, ignore_case = TRUE))) {
      return(bc_master_cleaned[[term]])
    }
  }
  
  # Fuzzy match fallback - spelling errors, this can clean automated function 
  # catches the rest
  master_terms <- names(bc_master_cleaned)
  cleaned_terms <- str_replace_all(master_terms, " ", "")
  distances <- stringdist::stringdist(raw_clean, cleaned_terms, method = "jw")
  best_match_index <- which.min(distances)
  if (distances[best_match_index] < 0.2) {
    return(bc_master_cleaned[[master_terms[best_match_index]]])
  }
  
  return("Unknown")
}

# ====================
# Classify Type Based on Cleaned Name
# ====================
# Classifies the cleaned birth control name into major categories: 
# IUD, Implant, Ring, HRT, OCP, Progestin Only, or Unknown

classify_bc_type <- function(cleaned_term) {
  cleaned_lower <- tolower(cleaned_term)
  
  # Check for combined HRT treatments
  if (str_detect(cleaned_lower, "estradiol.*testosterone|testosterone.*estradiol") || 
      str_detect(cleaned_lower, "progesterone.*testosterone|testosterone.*progesterone") ||
      str_detect(cleaned_lower, "estradiol.*progesterone|progesterone.*estradiol")) {
    return("HT Combination")
  }
  
  if (str_detect(cleaned_lower, "iud")) {
    return("IUD")
  } else if (str_detect(cleaned_lower, "implant|nexplanon|implanon|subdermal")) {
    return("Implant")
  } else if (str_detect(cleaned_lower, "ring")) {
    return("Ring")
  } else if (str_detect(cleaned_lower, "estradiol|hrt|progesterone|testosterone")) {
    return("HT")
  } else if (str_detect(cleaned_lower, "progestin only|camila")) {
    return("Progestin Only")
  } else if (cleaned_term %in% ocp_cleaned_terms) {
    return("OCP")
  } else if (str_detect(cleaned_lower, "patch|transdermal|combipatch")) {
      return("Patch")
  } else {
    return("Unknown")
  }
}
# ====================
# Apply to the dataset
# ====================
ocp_cleaning_filtered <- ocp_cleaning_filtered %>%
  mutate(
    bc_cleaned = sapply(bc_details, clean_bc_term),
    bc_type = sapply(bc_cleaned, classify_bc_type),
    #Create HT column: 1 if bc_type is not 'Unknown' or if bc_cleaning is just an IUD, 0 otherwise -- asd96 05/20/25
    HT = ifelse(bc_type != "Unknown" & bc_cleaned != "IUD", 1, 0)
  )
# ====================
# Create output dataset
# ====================

ocp_cleaning_filtered <- as.data.frame(ocp_cleaning_filtered)

final_bc <- dplyr::select(ocp_cleaning_filtered, record_id, bc_details, bc_cleaned, bc_type, HT)

#===============================================================================
#pulling out BC and HRT from other_meds

library(dplyr)
library(stringr)
library(stringdist)

# Updated mapping list with correct priorities (no change needed)
bc_hrt_cleaned <- list(
  # IUDs
  "mirena iud" = "Mirena IUD", "mirena" = "Mirena IUD",
  "kyleena iud" = "Kyleena IUD", "kyleena" = "Kyleena IUD", "kylena" = "Kyleena IUD",
  "skyla" = "Skyla IUD", "liletta" = "Liletta IUD", "hormonal iud" = "Hormonal IUD", "iud" = "IUD",
  
  # Implant
  "nexplanon" = "Nexplanon", "implanon" = "Implanon", "unsure in arm" = "Subdermal Implant",
  
  # Rings
  "nuva ring" = "NuvaRing", "nuvaring" = "NuvaRing", "annovera" = "Annovera",
  
  # OCPs
  "loestrin fe" = "Loestrin FE", "loestrinfe" = "Loestrin FE",
  "junel fe" = "Junel FE", "junelfe" = "Junel FE",
  "tri lo mil" = "Tri-Lo-Mili", "trilomil" = "Tri-Lo-Mili",
  "simpesse" = "Simpesse", "alesse" = "Alesse", "aviane" = "Aviane", "beyaz" = "Beyaz",
  "cryssel" = "Cryssel", "desogen" = "Desogen", "estarylla" = "Estarylla",
  "kariva" = "Kariva", "levora" = "Levora", "lybrel" = "Lybrel", "microgestin" = "Microgestin",
  "minastrin" = "Minastrin", "nortrel" = "Nortrel", "ortho cyclen" = "Ortho Cyclen",
  "ortho tricyclen" = "Ortho Tri-Cyclen", "seasonale" = "Seasonale",
  "seasonique" = "Seasonique", "sprintec" = "Sprintec", "trinessa" = "Trinessa",
  "yasmin" = "Yasmin", "yaz" = "Yaz", "zovia" = "Zovia",
  
  # HRT
  "estradiol patch micronized progesterone" = "Estradiol Patch + Micronized Progesterone",
  "estradiol patch" = "Estradiol Patch",
  "estrogen patch" = "Estrogen Patch",
  "dotti" = "Estradiol Patch",
  "estradiol" = "Estradiol",
  "estrogen" = "Estrogen",
  "progesterone" = "Progesterone",
  "hrt" = "HRT",
  
  # Combipatch
  "combipatch" = "Combipatch",
  "combi patch" = "Combipatch",
  
  # Other/Unknown
  "progestin only birth control" = "Progestin Only", "camila" = "Camila",
  "norepinephrine" = "Unknown", "unknown" = "Unknown"
)

# Cleaner function that fixes a double-match issue (it was showing up as Estrogen, Estrogen Patch for just Estrogen)
clean_bc_hrt_term <- function(raw_term) {
  if (is.na(raw_term) || raw_term == "") {
    return("Unknown")
  }
  
  raw_lower <- tolower(trimws(raw_term))  # Convert to lowercase and trim whitespaces
  raw_clean <- str_replace_all(raw_lower, "[^a-z0-9 ]", "")  # Remove punctuation but keep spaces
  
  matched_terms <- c()
  
  # Sort terms by descending number of characters -> prioritize longer matches first (to avoid above issue)
  sorted_terms <- names(bc_hrt_cleaned)[order(-nchar(names(bc_hrt_cleaned)))]
  
  for (term in sorted_terms) {
    pattern <- str_replace_all(term, " ", "")
    if (str_detect(str_replace_all(raw_clean, " ", ""), regex(pattern, ignore_case = TRUE))) {
      matched_terms <- c(matched_terms, bc_hrt_cleaned[[term]])
      
      # Remove matched pattern from raw_clean to prevent double matching
      raw_clean <- str_replace_all(raw_clean, regex(term, ignore_case = TRUE), "")
    }
  }
  
  # If still nothing matched, do fuzzy match
  if (length(matched_terms) == 0) {
    master_terms <- names(bc_hrt_cleaned)
    cleaned_terms <- str_replace_all(master_terms, " ", "")
    distances <- stringdist::stringdist(str_replace_all(raw_clean, " ", ""), cleaned_terms, method = "jw")
    best_match_index <- which.min(distances)
    if (distances[best_match_index] < 0.2) {
      matched_terms <- c(matched_terms, bc_hrt_cleaned[[master_terms[best_match_index]]])
    }
  }
  
  # Return matched terms as a comma-separated string
  if (length(matched_terms) > 0) {
    return(paste(unique(matched_terms), collapse = ", "))
  }
  
  return("Unknown")
}

# Apply to your dataset
medications_classified <- ocp_cleaning %>%
  mutate(
    bc_hrt_cleaned = sapply(othermeds, clean_bc_hrt_term),
    #Create HT column: 1 if bc_hrt_cleaned is not 'Unknown', 0 otherwise -- asd96 05/20/25
    HT = ifelse(bc_hrt_cleaned != "Unknown" & bc_hrt_cleaned != "IUD", 1, 0)
  )

# Final output
final_output <- dplyr::select(medications_classified, record_id, othermeds, bc_hrt_cleaned, HT)

#make seperate column for medications that are not "unknown"
medications_classified <- ocp_cleaning %>%
  mutate(
    bc_hrt_cleaned = sapply(othermeds, clean_bc_hrt_term),
    #Create HT column: 1 if bc_hrt_cleaned is not 'Unknown', 0 otherwise -- asd96 05/20/25
    HT = ifelse(bc_hrt_cleaned != "Unknown" & bc_hrt_cleaned != "IUD", 1, 0),
    bc_hrt_cleaned_nonunknown = ifelse(
      bc_hrt_cleaned == "Unknown", NA, bc_hrt_cleaned)

    )


final_hrtt <- dplyr::select(medications_classified, record_id, othermeds, bc_hrt_cleaned, bc_hrt_cleaned_nonunknown, HT)

#there was one that said "natural menopause pills" that i did not include, but we can 

#===============================================================================
#othermeds - cleaning to control for endocrine disruptors 

#add or remove based on what the doc says 
endocrine_disruptors_cleaned <- list(
  "vyvanse" = "Vyvanse", "valium" = "Valium", "cyclobenzaprine" = "Cyclobenzaprine", 
  "tramadol" = "Tramadol", "duloxetine" = "Duloxetine", "pregablin" = "Pregabalin", 
  "valsartan" = "Valsartan", "metformin" = "Metformin", "trazodone" = "Trazodone", 
  "ozempic" = "Ozempic", "methadone" = "Methadone", "delta-8" = "Delta-8", 
  "delta-9 gummies" = "Delta-9 Gummies", "valerian root" = "Valerian Root", 
  "l-tryptophan" = "L-Tryptophan", "melatonin" = "Melatonin", "vraylar" = "Vraylar", 
  "baclofen" = "Baclofen", "hydroxyzine" = "Hydroxyzine", "lamotrigine" = "Lamotrigine", 
  "fluoxetine" = "Fluoxetine", "dextroamp-amphetamin" = "Dextroamphetamine", 
  "levothyroxine" = "Levothyroxine", "oxbutynin" = "Oxbutynin", "pantroprazole" = "Pantoprazole", 
  "loratadine" = "Loratadine", "metronidazole" = "Metronidazole", "escitalopram" = "Escitalopram", 
  "sertraline" = "Sertraline", "buspirone" = "Buspirone", "klonopin" = "Klonopin", 
  "propranolol" = "Propranolol", "gabapentin" = "Gabapentin", "imitrex" = "Imitrex", 
  "venlafaxine" = "Venlafaxine", "ziprasidone" = "Ziprasidone", "haloperidol" = "Haloperidol", 
  "methylphenidate" = "Methylphenidate", "citalopram" = "Citalopram", "lexapro" = "Lexapro", 
  "wellbutrin" = "Wellbutrin", "adderall xr" = "Adderall XR", "olmesartan" = "Olmesartan", 
  "spironolactone" = "Spironolactone", "lipitor" = "Lipitor", "b12 injections" = "B12 Injections", 
  "omeprazole" = "Omeprazole", "topiramate" = "Topiramate", "nortriptyline" = "Nortriptyline", 
  "haldol" = "Haldol"
)

clean_endocrine_disruptor_term <- function(raw_term) {
  if (is.na(raw_term) || trimws(raw_term) == "") {
    return(NA_character_)  # or return("Unknown") if you prefer
  }
  
  raw_lower <- tolower(trimws(raw_term))
  raw_clean <- str_replace_all(raw_lower, "[^a-z0-9]", "")
  
  # Initialize an empty vector to store matches
  matched_terms <- c()
  
  # Direct match using the endocrine disruptor list
  for (term in names(endocrine_disruptors_cleaned)) {
    pattern <- str_replace_all(term, " ", "")
    if (str_detect(raw_clean, regex(pattern, ignore_case = TRUE))) {
      matched_terms <- c(matched_terms, endocrine_disruptors_cleaned[[term]])
    }
  }
  
  # If no matches were found, use fuzzy match to find best matches
  if (length(matched_terms) == 0) {
    master_terms <- names(endocrine_disruptors_cleaned)
    cleaned_terms <- str_replace_all(master_terms, " ", "")
    distances <- stringdist::stringdist(raw_clean, cleaned_terms, method = "jw")
    best_match_index <- which.min(distances)
    if (distances[best_match_index] < 0.2) {
      matched_terms <- c(matched_terms, endocrine_disruptors_cleaned[[master_terms[best_match_index]]])
    }
  }
  
  # Return matched terms as a comma-separated string
  if (length(matched_terms) > 0) {
    return(paste(matched_terms, collapse = ", "))
  }
  
  return("Unknown")  # Return "Unknown" if no match is found
}

# Apply to your dataset
medications_classified <- ocp_cleaning %>%
  mutate(
    endocrine_disruptor_cleaned = sapply(othermeds, clean_endocrine_disruptor_term)
  )

final_endodisrupter <- dplyr::select(medications_classified, record_id, othermeds, endocrine_disruptor_cleaned)

