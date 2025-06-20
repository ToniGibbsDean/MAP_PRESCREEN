#packages and read in files 
library(tidyverse)
library(dplyr)
library(ggpubr)
library(stringr)
library(stringdist)
path="Figures"

dat<-read.csv("Outputs/merged_data_redcapNewAndOld.csv") #1436  397

#ocp_cleaning_filtered <- dat %>%
#  filter(birth_control == "1")


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

clean_bc_term <- function(raw_term, birth_control_flag) {
  if (is.na(birth_control_flag)) {
    return("Unknown")
  }
  if (birth_control_flag == 0) {
    return("Not on BC")
  }

  raw_lower <- tolower(trimws(raw_term))

  spelling_corrections <- list(
    "estrodiol" = "estradiol",
    "progestrone" = "progesterone",
    "progesteron" = "progesterone",
    "testostrone" = "testosterone",
    "testosteron" = "testosterone"
  )
  for (misspelling in names(spelling_corrections)) {
    raw_lower <- stringr::str_replace_all(raw_lower, misspelling, spelling_corrections[[misspelling]])
  }

  raw_clean <- stringr::str_replace_all(raw_lower, "[^a-z0-9]", "")

  component_patterns <- list(
    "Estradiol Patch" = "estradiol\\s*patch",
    "Estradiol" = "estradiol",
    "Progesterone" = "progesterone",
    "Testosterone" = "testosterone"
  )
  components <- c()
  for (label in names(component_patterns)) {
    if (stringr::str_detect(raw_lower, component_patterns[[label]])) {
      components <- c(components, label)
    }
  }

  if (length(components) >= 1) return(paste(components, collapse = " + "))

  if (stringr::str_detect(raw_lower, "estradiol.*progesterone.*testosterone|estrodiol.*progesterone.*testosterone")) {
    return("Estradiol + Progesterone + Testosterone")
  } else if (stringr::str_detect(raw_lower, "estradiol.*progesterone|estrodiol.*progesterone")) {
    return("Estradiol + Progesterone")
  }

  for (term in names(bc_master_cleaned)) {
    pattern <- stringr::str_replace_all(term, " ", "")
    if (stringr::str_detect(raw_clean, regex(pattern, ignore_case = TRUE))) {
      return(bc_master_cleaned[[term]])
    }
  }

  master_terms <- names(bc_master_cleaned)
  cleaned_terms <- stringr::str_replace_all(master_terms, " ", "")
  distances <- stringdist::stringdist(raw_clean, cleaned_terms, method = "jw")
  best_match_index <- which.min(distances)
  if (distances[best_match_index] < 0.2) {
    return(bc_master_cleaned[[master_terms[best_match_index]]])
  }

  return("Unknown")
}

classify_bc_type <- function(cleaned_term, birth_control_flag) {
  if (is.na(birth_control_flag)) {
    return("Unknown")
  }
  if (birth_control_flag == 0) {
    return("Not on BC")
  }

  cleaned_lower <- tolower(cleaned_term)

  if (stringr::str_detect(cleaned_lower, "estradiol.*testosterone|testosterone.*estradiol") || 
      stringr::str_detect(cleaned_lower, "progesterone.*testosterone|testosterone.*progesterone") ||
      stringr::str_detect(cleaned_lower, "estradiol.*progesterone|progesterone.*estradiol")) {
    return("HRT Combination")
  }

  if (stringr::str_detect(cleaned_lower, "patch|transdermal|combipatch")) {
    return("Patch")
  } else if (stringr::str_detect(cleaned_lower, "iud")) {
    return("IUD") ## needs changing to specific further
  } else if (stringr::str_detect(cleaned_lower, "implant|nexplanon|implanon|subdermal")) {
    return("Implant")
  } else if (stringr::str_detect(cleaned_lower, "ring")) {
    return("Ring")
  } else if (cleaned_term %in% ocp_cleaned_terms) {
    return("OCP")   ##what do we know about the chemicals? given below denotes progestin only? Are these 100% combination or we dont knwo?
  } else if (stringr::str_detect(cleaned_lower, "progestin only|camila")) {
    return("Progestin Only") #if they specified or put minipill
  } else if (stringr::str_detect(cleaned_lower, "estradiol|hrt|progesterone|testosterone")) {
    return("HRT")
  } else {
    return("Unknown")
  }
}

################################################
#Primary hormone columns
################################################

# cleaning step --> bc --> IUD_hormonal; IUD_nonhormonal; IUD_unknown (where its not clear which IUD they have)

# Broadest cat: Hormone Therapy (HT) --> No: no hormonal birthcontrol and no HRT, non-hormonal contraceptives (so not including IUD_nonhormonal and IUD_unknown)
                                         #Yes: any hormonal birthcontrol (including IUD_hormonal), any HRT
                                         #Unknown:  code unable to classify

# BC type - rename Hormone Therapy Category /type --> birth control hormonal: 
                                                            #patch, IUD_hormonal;  implants; ring; OCP - which includes progestin only and combined 
                                                      #birth contrl non hormonal: 
                                                            #IUD_nonhormonal; IUD_unknown, + any otheres
                                                      #HRT - 
                                                            #any estradiol|hrt|progesterone|testosterone"
                                                      #none - 
                                                            #not on any HT including IUDs of any kind
                                                      #unknown - 
                                                            #unable to classify

# Hormone Therapy Chemical --> endo estrogen: estradiol 
                              # exog estrogen: estrogen e.g, 
                              # endo progesterone 
                              # exog progestin
                              # exog combined
                              # unknown
                              # other (hormonal)
                              # other (non hormonal)

# local vs global -- made from BC type





### more detail on unknown
## what about other medications?

##patch - method
## unable to classify must be different to unknown 

### other meds claening in cleaning script
#summary columns from othermeds might go in indirect disrutotor 
##summary col for possible indications of HT/hrt

########################
# secodnary hormone meds i.e., disruptors 
########################

# ohter meds: cleaned column; yes/no hormone disrupter /// deal with any meds you find that have a primary homnornal function by adding them to primary hormone columns

##



# ====================
# Apply to the dataset
# ====================
ocp_cleaning_filtered <- dat %>%
  mutate(
    bc_cleaned = sapply(bc_details, clean_bc_term),
    bc_type = sapply(bc_cleaned, classify_bc_type),
    #Create HT column: 1 if bc_type is not 'Unknown' or if bc_cleaning is just an IUD, 0 otherwise -- asd96 05/20/25
    HT = ifelse(bc_type != "Unknown" & bc_cleaned != "IUD", 1, 0)
  )

  ocp_cleaning_filtered <- dat %>%
  mutate(
    bc_cleaned = mapply(clean_bc_term, bc_details, birth_control),
    bc_type = mapply(classify_bc_type, bc_cleaned, birth_control),
    HT = ifelse(bc_type != "Unknown" & bc_cleaned != "IUD" & bc_cleaned != "Not on BC", 1, 0)
  )

# ====================
# Create output dataset
# ====================

ocp_cleaning_filtered <- as.data.frame(ocp_cleaning_filtered)

#final_bc <- dplyr::select(ocp_cleaning_filtered, record_id, bc_details, bc_cleaned, bc_type, HT)


checking <- ocp_cleaning_filtered %>%
select(record_id, birth_control, bc_details, bc_cleaned, bc_type, HT) %>% 
mutate(HT=as.factor(HT))  %>%
mutate(birth_control=as.factor(birth_control))  %>%
summary()

 ocp_cleaning_filtered %>%
select(bc_details) %>% group_by(bc_details) %>% summarise(n())

 ocp_cleaning_filtered %>%
select(bc_cleaned) %>% group_by(bc_cleaned) %>% summarise(n()) %>% table()


 ocp_cleaning_filtered %>%
select(bc_type) %>% group_by(bc_type) %>% summarise(n())


 ocp_cleaning_filtered %>%
select(HT) %>% group_by(HT) %>% summarise(n())


 ocp_cleaning_filtered %>%
select(birth_control) %>% group_by(birth_control) %>% summarise(n())

df <- dat %>%
  mutate(
    bc_cleaned = ifelse(birth_control == 0, "Not on BC", clean_bc_term(bc_raw)),
    bc_type = ifelse(birth_control == 0, "Not on BC", classify_bc_type(bc_cleaned))
  )