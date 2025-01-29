library(ggplot2)

data_clean<-readRDS(file="Outputs/data_clean.csv")
fit<-readRDS(file="Outputs/LinearModelFit.RDS" )

R2<-summary(fit)$r.squared

p<-ggplot(data=data_clean, aes(x=Sepal.Length, y=ApproximatePetalArea, color=Species)) +
    geom_point() +
    geom_smooth(method="lm") +
    theme_classic() +
    annotate("text", label=paste0("R-squared=", signif(R2, digits=2)), x=7, y=2)

ggsave("Figures/Figure1.png", p)