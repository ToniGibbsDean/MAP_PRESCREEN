library(tidyverse)

data<-read.csv(file="Data/demo.csv")

data_clean<-data %>%
    as_tibble %>%
    mutate( Species=as.factor(Species),
            ApproximatePetalArea=Petal.Length*Petal.Width)

saveRDS(file="Outputs/data_clean.csv", data_clean)