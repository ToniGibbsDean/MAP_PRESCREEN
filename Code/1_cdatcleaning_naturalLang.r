#code developed by prachi patel + arohi dandawate (with assistance from Chat GPT/Claude AI)

#===============================================================================
#disclaimer - a lot of it is manual editing with the code, can add more misspelling variations if needed to account for future
#terms. feel free to edit to make it more automated as well. 
#===============================================================================
#packages
library(dplyr)
library(stringr)
library(stringdist)

#load your own dataset here
ocp_cleaning <- readRDS("Outputs/joinedCorrected_df.RDS") #Arohi's CSV
#ocp_cleaning <- read.csv("~/Desktop/merged_data_redcapNewAndOld.csv") #Prachi's CSV

# Filter rows where bc_details equals "1"
#ocp_cleaning <- ocp_cleaning %>%
#  filter(birth_control == "1")

#===============================================================================
# Create a master list of birth control (BC) and hormone replacement therapy (HRT) terms 
# List maps common terms (including misspellings and variations) to standardized names that will probably appear
# in the text
# Note: I used the filtered column (where they put yes to being on BC, to make it easier to visualize, you can 
# do either da)

bc_master_cleaned <- list(
  # IUDs
  "mirena" = "Mirena IUD", "mirena iud" = "Mirena IUD",
  "moderna iud" = "Mirena IUD", "Modena IUD" = "Mirena IUD",
  "Madeena IUD" = "Mirena IUD",
  "kyleena" = "Kyleena IUD", "kyleena iud" = "Kyleena IUD", "kylena" = "Kyleena IUD",
  "skyla" = "Skyla IUD", "liletta" = "Liletta IUD", "iud" = "IUD", "hormonal iud" = "Hormonal IUD",
  
  # Implant
  "nexplanon" = "Nexplanon", "implanon" = "Implanon", "unsure in arm" = "Subdermal Implant",
  
  # Rings
  "nuvaring" = "NuvaRing", "nuva ring" = "NuvaRing", "annovera" = "Annovera",
  
  #Shot
  "provera" = "DepoProvera",
  
  #Emergency Contraceptive
  "levonorgestrel" = "Levonorgestrel", 
  
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
  "hailey fe" = "Hailey FE",
  "altavera" = "Altavera",
  "low-ogestrel" = "Low-Ogestrel",
  "alyacen" = "Alyacen",
  "tarin" = "Tarina",
  "tarina" = "Tarina",
  # hi prachi can you see this
  #POP
  "slynd" = "Slynd",
  
  # HRT
  "estradiol" = "Estradiol", 
  "estrogen" = "Estrogen",
  "estradiol patch" = "Estradiol Patch", 
  "patch" = "Patch",
  "estradiol patch micronized progesterone" = "Estradiol Patch + Micronized Progesterone",
  "hrt" = "HRT",
  "progesterone" = "Progesterone",
  "premarin" = "Premarin",
  "prometrium" = "Prometrium",
  "micronor" = "Micronor",
  "mimvey" = "Mimvey",
  "dotti" = "Dotti Estradiol Patch",
  "angeliq" = "Angeliq",
  "bijuvia" = "Bijuvia",
  "testosterone" = "Testosterone",
  
  # Combipatch
  "combipatch" = "Combipatch",
  "combi patch" = "Combipatch",
  
  # Other/Unknown
  "progestin only birth control" = "Progestin Only", 
  "estrogen only birth control" = "Estrogen Only",
  "estrogen birth control" = "Estrogen Birth Control",
  "birth control" = "Birth Control",
  "birth control pill" = "Birth Control Pill",
  "camila" = "Camila",
  "norepinephrine" = "Unknown", "unknown" = "Unknown", 
  "januvia" = "Unknown",
  "anastrozole" = "Anastrozole/Breast Cancer Treatment" 
)

# List of cleaned terms that are OCPs - there was an error so i did this again 
ocp_cleaned_terms <- c(
  "Loestrin FE", "Junel FE", "Tri-Lo-Mili", "Simpesse", "Alesse",
  "Aviane", "Beyaz", "Cryssel", "Desogen", "Estarylla", "Kariva", 
  "Levora", "Lybrel", "Microgestin", "Minastrin", "Nortrel", "Ortho Cyclen",
  "Ortho Tri-Cyclen", "Seasonale", "Seasonique", "Sprintec", "Trinessa",
  "Yasmin", "Yaz", "Zovia", "Hailey FE",
  "Altavera",
  "Birth Control Pill",
  "Low-Ogestrel",
  "Alyacen",
  "Tarina"
)

# ====================
# Clean BC_Details Raw Terms Function
# ====================
# - Converts to lowercase
# - Has spelling check for manual errors, can expand this list if the code isn't super streamlined
# - Removes punctuation 
# - Added a matched_terms vector so that it catches multiple words if written pp 6/7
# - Checks for multi-component HRT combinations first (Estradiol + Progesterone [+ Testosterone])
# - Then checks for exact matches
# - If no exact match, uses fuzzy matching based on string distance (Jaro-Winkler)

clean_bc_term <- function(raw_term) {
  if (is.na(raw_term) || trimws(raw_term) == "") {
    return(NA_character_)  # Handle empty/NA inputs
  }
  
  raw_lower <- tolower(trimws(raw_term))
  raw_clean <- str_replace_all(raw_lower, "[^a-z0-9]", "")  # remove spaces/punct
  
  #can add more terms here if needed for the future
  spelling_corrections <- list(
    # Estradiol variations
    "estrodiol" = "estradiol",
    "estrodial" = "estradiol",
    "estradial" = "estradiol",
    "estrodile" = "estradiol",
    "eatadiol" = "Estradiol",
    
    
    # Estrogen variations
    "eastrogen" = "estrogen",
    "estrgen" = "estrogen",
    "estrogn" = "estrogen",
    "oestrogen" = "estrogen",# British spelling
    
    # Progesterone variations
    "progestrone" = "progesterone",
    "progesteron" = "progesterone",
    "projesterone" = "progesterone",
    
    # Other hormones
    "testostrone" = "testosterone",
    "testosteron" = "testosterone"
  )
  
  # Apply spelling corrections
  for (misspelling in names(spelling_corrections)) {
    if (str_detect(raw_lower, misspelling)) {
      raw_lower <- str_replace_all(raw_lower, misspelling, spelling_corrections[[misspelling]])
      # Update raw_clean after spelling correction
      raw_clean <- str_replace_all(raw_lower, "[^a-z0-9]", "")
    }
  }
  
  # Initialize an empty vector to store matches - this was straight AI (very confusing code, but worked), 6/7
  matched_terms <- c()
  
  # Check for estrogen-only birth control patterns first
  if (str_detect(raw_lower, "estrogen\\s*(only\\s*)?birth\\s*control")) {
    if (str_detect(raw_lower, "only")) {
      return("Estrogen Only")
    } else {
      return("Estrogen Birth Control")
    }
  }
  
  # Direct match using master list - collect ALL matches but prioritize longer matches
  master_terms_by_length <- names(bc_master_cleaned)[order(-nchar(names(bc_master_cleaned)))]
  
  for (term in master_terms_by_length) {
    pattern <- str_replace_all(term, " ", "")
    if (str_detect(raw_clean, regex(pattern, ignore_case = TRUE))) {
      matched_terms <- c(matched_terms, bc_master_cleaned[[term]])
    }
  }
  
  # If we found direct matches, return them
  if (length(matched_terms) > 0) {
    # Remove duplicates and redundant terms
    unique_matches <- unique(matched_terms)
    
    # Remove redundant terms: if compound terms are present, remove their components
    if ("Estradiol Patch + Micronized Progesterone" %in% unique_matches) {
      unique_matches <- unique_matches[!unique_matches %in% c("Estradiol", "Estradiol Patch", "Patch", "Progesterone")]
    } else if ("Estradiol Patch" %in% unique_matches) {
      unique_matches <- unique_matches[!unique_matches %in% c("Estradiol", "Patch")]
    }
    
    # Remove generic IUD if specific IUD types are present
    iud_types <- c("Mirena IUD", "Kyleena IUD", "Skyla IUD", "Liletta IUD", "Hormonal IUD")
    if (any(iud_types %in% unique_matches)) {
      unique_matches <- unique_matches[unique_matches != "IUD"]
    }
    
    return(paste(unique_matches, collapse = ", "))
  }
  
  # If no direct matches, try component detection for complex combinations
  components <- c()
  
  # Check for estradiol patch - make sure the pattern is correct
  if (str_detect(raw_lower, "estradiol\\s*patch")) {
    components <- c(components, "Estradiol Patch")
  } 
  # Check for estradiol (not patch) - only if patch wasn't detected
  else if (str_detect(raw_lower, "estradiol")) {
    components <- c(components, "Estradiol")
  }
  
  # Check for estrogen
  if (str_detect(raw_lower, "estrogen")) {
    components <- c(components, "Estrogen")
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
  
  # Fuzzy match fallback - .3 is a good distance, catches maybe too much tho?? 6/7 pp
  # catches the rest
  master_terms <- names(bc_master_cleaned)
  cleaned_terms <- str_replace_all(master_terms, " ", "")
  distances <- stringdist::stringdist(raw_clean, cleaned_terms, method = "jw")
  best_match_index <- which.min(distances)
  if (distances[best_match_index] < 0.25) {
    return(bc_master_cleaned[[master_terms[best_match_index]]])
  }
  
  return("Unknown")
}

ocp_cleaning <- ocp_cleaning %>%
  mutate(
    bc_cleaned = sapply(bc_details, clean_bc_term), 
  )

final_bc_cleaned <- dplyr::select(ocp_cleaning, record_id, bc_details, bc_cleaned)


# ====================
# Clean OtherMeds Raw Terms Function - Pulling out HT
# need to deal with the unknowns/0s 
# ====================
bc_master_cleaned <- list(
  # IUDs
  "mirena" = "Mirena IUD", "mirena iud" = "Mirena IUD",
  "moderna iud" = "Mirena IUD", "Modena IUD" = "Mirena IUD",
  "Madeena IUD" = "Mirena IUD",
  "kyleena" = "Kyleena IUD", "kyleena iud" = "Kyleena IUD", "kylena" = "Kyleena IUD",
  "skyla" = "Skyla IUD", "liletta" = "Liletta IUD", "iud" = "IUD", "hormonal iud" = "Hormonal IUD",
  
  # Implant
  "nexplanon" = "Nexplanon", "implanon" = "Implanon", "unsure in arm" = "Subdermal Implant",
  
  # Rings
  "nuvaring" = "NuvaRing", "nuva ring" = "NuvaRing", "annovera" = "Annovera",
  
  #Shot
  "provera" = "DepoProvera",
  
  #Emergency Contraceptive
  "levonorgestrel" = "Levonorgestrel", 
  
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
  "hailey fe" = "Hailey FE",
  "altavera" = "Altavera",
  "low-ogestrel" = "Low-Ogestrel",
  "alyacen" = "Alyacen",
  "tarin" = "Tarina",
  "tarina" = "Tarina",
  # hi prachi can you see this
  #POP
  "slynd" = "Slynd",
  
  # HRT
  "estradiol" = "Estradiol", 
  "estrogen" = "Estrogen",
  "estradiol patch" = "Estradiol Patch", 
  "patch" = "Patch",
  "estradiol patch micronized progesterone" = "Estradiol Patch + Micronized Progesterone",
  "hrt" = "HRT",
  "progesterone" = "Progesterone",
  "premarin" = "Premarin",
  "prometrium" = "Prometrium",
  "micronor" = "Micronor",
  "mimvey" = "Mimvey",
  "dotti" = "Dotti Estradiol Patch",
  "angeliq" = "Angeliq",
  "bijuvia" = "Bijuvia",
  "testosterone" = "Testosterone",
  
  # Combipatch
  "combipatch" = "Combipatch",
  "combi patch" = "Combipatch",
  
  # Other/Unknown
  "progestin only birth control" = "Progestin Only", 
  "estrogen only birth control" = "Estrogen Only",
  "estrogen birth control" = "Estrogen Birth Control",
  "birth control" = "Birth Control",
  "birth control pill" = "Birth Control Pill",
  "camila" = "Camila",
  "norepinephrine" = "Unknown", "unknown" = "Unknown", 
  "januvia" = "Unknown",
  "anastrozole" = "Anastrozole/Breast Cancer Treatment" 
)

# List of cleaned terms that are OCPs - there was an error so i did this again 
ocp_cleaned_terms <- c(
  "Loestrin FE", "Junel FE", "Tri-Lo-Mili", "Simpesse", "Alesse",
  "Aviane", "Beyaz", "Cryssel", "Desogen", "Estarylla", "Kariva", 
  "Levora", "Lybrel", "Microgestin", "Minastrin", "Nortrel", "Ortho Cyclen",
  "Ortho Tri-Cyclen", "Seasonale", "Seasonique", "Sprintec", "Trinessa",
  "Yasmin", "Yaz", "Zovia", "Hailey FE",
  "Altavera",
  "Birth Control Pill",
  "Low-Ogestrel",
  "Alyacen",
  "Tarina"
)

clean_bc_term <- function(raw_term) {
  if (is.na(raw_term) || trimws(raw_term) == "") {
    return(NA_character_)  # Handle empty/NA inputs
  }
  
  raw_lower <- tolower(trimws(raw_term))
  raw_clean <- str_replace_all(raw_lower, "[^a-z0-9]", "")  # remove spaces/punct
  
  #can add more terms here if needed for the future
  spelling_corrections <- list(
    # Estradiol variations
    "estrodiol" = "estradiol",
    "estrodial" = "estradiol",
    "estradial" = "estradiol",
    "estrodile" = "estradiol",
    "eatadiol" = "Estradiol",
    
    
    # Estrogen variations
    "eastrogen" = "estrogen",
    "estrgen" = "estrogen",
    "estrogn" = "estrogen",
    "oestrogen" = "estrogen",# British spelling
    
    # Progesterone variations
    "progestrone" = "progesterone",
    "progesteron" = "progesterone",
    "projesterone" = "progesterone",
    
    # Other hormones
    "testostrone" = "testosterone",
    "testosteron" = "testosterone"
  )
  
  # Apply spelling corrections
  for (misspelling in names(spelling_corrections)) {
    if (str_detect(raw_lower, misspelling)) {
      raw_lower <- str_replace_all(raw_lower, misspelling, spelling_corrections[[misspelling]])
      # Update raw_clean after spelling correction
      raw_clean <- str_replace_all(raw_lower, "[^a-z0-9]", "")
    }
  }
  
  # Initialize an empty vector to store matches - this was straight AI (very confusing code, but worked), 6/7
  matched_terms <- c()
  
  # Check for estrogen-only birth control patterns first
  if (str_detect(raw_lower, "estrogen\\s*(only\\s*)?birth\\s*control")) {
    if (str_detect(raw_lower, "only")) {
      return("Estrogen Only")
    } else {
      return("Estrogen Birth Control")
    }
  }
  
  # Direct match using master list - collect ALL matches but prioritize longer matches
  master_terms_by_length <- names(bc_master_cleaned)[order(-nchar(names(bc_master_cleaned)))]
  
  for (term in master_terms_by_length) {
    pattern <- str_replace_all(term, " ", "")
    if (str_detect(raw_clean, regex(pattern, ignore_case = TRUE))) {
      matched_terms <- c(matched_terms, bc_master_cleaned[[term]])
    }
  }
  
  # If we found direct matches, return them
  if (length(matched_terms) > 0) {
    # Remove duplicates and redundant terms
    unique_matches <- unique(matched_terms)
    
    # Remove redundant terms: if compound terms are present, remove their components
    if ("Estradiol Patch + Micronized Progesterone" %in% unique_matches) {
      unique_matches <- unique_matches[!unique_matches %in% c("Estradiol", "Estradiol Patch", "Patch", "Progesterone")]
    } else if ("Estradiol Patch" %in% unique_matches) {
      unique_matches <- unique_matches[!unique_matches %in% c("Estradiol", "Patch")]
    }
    
    # Remove generic IUD if specific IUD types are present
    iud_types <- c("Mirena IUD", "Kyleena IUD", "Skyla IUD", "Liletta IUD", "Hormonal IUD")
    if (any(iud_types %in% unique_matches)) {
      unique_matches <- unique_matches[unique_matches != "IUD"]
    }
    
    return(paste(unique_matches, collapse = ", "))
  }
  
  # If no direct matches, try component detection for complex combinations
  components <- c()
  
  # Check for estradiol patch - make sure the pattern is correct
  if (str_detect(raw_lower, "estradiol\\s*patch")) {
    components <- c(components, "Estradiol Patch")
  } 
  # Check for estradiol (not patch) - only if patch wasn't detected
  else if (str_detect(raw_lower, "estradiol")) {
    components <- c(components, "Estradiol")
  }
  
  # Check for estrogen
  if (str_detect(raw_lower, "estrogen")) {
    components <- c(components, "Estrogen")
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
  
  # Fuzzy match fallback - .3 is a good distance, catches maybe too much tho?? 6/7 pp
  # catches the rest
  master_terms <- names(bc_master_cleaned)
  cleaned_terms <- str_replace_all(master_terms, " ", "")
  distances <- stringdist::stringdist(raw_clean, cleaned_terms, method = "jw")
  best_match_index <- which.min(distances)
  if (distances[best_match_index] < 0.25) {
    return(bc_master_cleaned[[master_terms[best_match_index]]])
  }
  
  return("Unknown")
}


# Apply to dataset
ocp_cleaning <- ocp_cleaning %>%
  mutate(
    othermeds_ht_cleaned = sapply(othermeds, clean_bc_term)
  )


# ====================
# Clean OtherMeds Raw Terms Function - Pulling out Endocrine Disrupters
# ====================
# - List of known endocrine disruptors (derived from existing data in othermeds)
# - Converts to lowercase
# - Has spelling check for manual errors, can expand this list if the code isn't super streamlined
# - Removes punctuation 
# - Then checks for exact matches
# - If no exact match, uses fuzzy matching based on string distance (Jaro-Winkler)

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
  "haldol" = "Haldol", "synthroid" = "Synthroid", "unithyroid" = "Unithyroid"
)

#synthroid, unithyroid are frequently misspelled and therefore not matched. additionally, people will just write thyroid (wondering if we should add thyroid as a medication). 
#however, at the same time, estradiol is being mistaken for tramadol. not sure how to manipulate match distance for this. -- asd96 06/16 

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
    othermeds_ht_cleaned = sapply(othermeds, clean_bc_term),  # HRT/BC only
    othermeds_endodisruptor_cleaned = sapply(othermeds, clean_endocrine_disruptor_term)  # Endocrine disruptors only
  )

final_endodisrupter <- dplyr::select(medications_classified, record_id, othermeds, othermeds_ht_cleaned, othermeds_endodisruptor_cleaned)

# # Joining cleaned columns based on 'record_id'
# combined_df <- left_join(final_bc_cleaned, final_endodisrupter, by = "record_id")
# 
# # Write to CSV
# write_csv(combined_df, "bc_othermeds_cleaned.csv")

saveRDS(medications_classified, "Outputs/medications_classified.RDS")
