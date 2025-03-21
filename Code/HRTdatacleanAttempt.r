# Load necessary libraries
library(dplyr)
library(stringr)

# 1. Read CSV file
df <- read.csv("Outputs/bcDetailsDat.csv", stringsAsFactors = FALSE)

# 2. Clean and prepare 'bc_details' column
df$bc_details_clean <- tolower(trimws(df$bc_details))  # Lowercase & trim spaces

# 3. Define HRT and BC keywords (expanded list with misspellings and variants)
hrt_keywords <- c(
  'estradiol', 'estrodiol', 'estrogen', 'eastrogen', 'hrt', 'hormone', 'hormonal', 
  'progesterone', 'lolestrin', 'lo loestrin', 'loloestrin', 'loestrin', 'Lorstrin',
  'premarin', 'climara'
)

bc_keywords <- c(
  'iud', 'mirena', 'mirina', 'mireena', 'kyleena', 'liletta', 'lilitta', 
  'pill', 'yasmin', 'norephrine', 'copper', 'implant', 'depo', 'nexplanon'
)

# 4. Function to classify HRT (1), BC (0), or NA
classify_hrt <- function(text) {
  if (is.na(text) || text == "") {
    return(NA)
  }
  if (any(str_detect(text, hrt_keywords))) {
    return(1)
  } else if (any(str_detect(text, bc_keywords))) {
    return(0)
  } else {
    return(NA)
  }
}

# 5. Apply classification function to create 'HRT' column
df$HRT <- sapply(df$bc_details_clean, classify_hrt)

# 6. Identify entries that are NOT captured (have data but don't match keywords)
unmatched <- df %>%
  filter(!is.na(bc_details_clean)) %>%
  filter(
    is.na(HRT)
  ) %>%
  select(bc_details) %>%
  distinct()

# 7. Export updated dataset with 'HRT' column
write.csv(df, "bcDetailsDat_with_HRT.csv", row.names = FALSE)

# 8. Export unmatched entries for review
write.csv(unmatched, "bcDetailsDat_unmatched.csv", row.names = FALSE)

# 9. Optional: Print how many unmatched
cat("Number of uncaptured (unmatched) entries:", nrow(unmatched), "\n")
cat("Files saved:\n- bcDetailsDat_with_HRT.csv\n- bcDetailsDat_unmatched.csv\n")