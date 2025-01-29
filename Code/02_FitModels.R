
data_clean<-readRDS(file="Outputs/data_clean.csv")

fit<-lm(ApproximatePetalArea~ Sepal.Length+Species, data=data_clean)
summary(fit)


saveRDS(fit,"Outputs/LinearModelFit.RDS" )

sink(file="Outputs/LinearModelFit.txt")
print(summary(fit))
sink() 