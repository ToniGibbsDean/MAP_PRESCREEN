#packages and read in files 
library(tidyverse)
library(dplyr)
library(ggpubr)
path="Figures"

HRT<-read.csv("Data/bcDetails_with_HRT_excluding_BirthControl.csv")

datraw<-read.csv("Data/PREMAPIntegration_DATA_2025-01-28_1610.csv") 
alldat<-datraw[-1, ]

#only included with at least 1000 records for each variable 
names(which(colSums(!is.na(alldat)) > 1000))

#remove any psychosis dx
data <- alldat[alldat %>% select(starts_with("psychosis")) %>% rowSums(na.rm=TRUE) == 0,]

x<- dat %>% select(record_id, birth_control, bc_details) %>%  filter(birth_control==1)
bcDetailsDat<-as_tibble(x)
write.csv(bcDetailsDat, "Outputs/bcDetailsDat.csv")