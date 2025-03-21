head(iris, 5)

rf1<-randomForest(Petal.Width~., data=iris)
rf2<-randomForest(Sepal.Width~., data=iris)

d<-iris %>%
    mutate(
        Petal.Width=c(scale(Petal.Width, center=TRUE, scale=TRUE)),
        Sepal.Width=c(scale(Sepal.Width, center=TRUE, scale=TRUE)))


bootstrap_l<-list()
varImp_l<-list()
for (i in 1:100) {
    set.seed(i)
    trainIndices<-sample(nrow(d), nrow(d)*0.8)
    dTrain<-d[trainIndices,]
    dTest<-d[-trainIndices,]

    rfPetal<-randomForest(Petal.Width~. , data=select(dTrain, -Sepal.Width))
    rfSepal<-randomForest(Sepal.Width~. , data=select(dTrain, -Petal.Width))
    imp_df<-data.frame(importance(rfPetal), importance(rfSepal)) %>% as.data.frame() %>% rownames_to_column("var")
    names(imp_df)<-c("var", "Petal", "Sepal" )
    imp_df$bootstrapReplicate<-i
    varImp_l[[i]]<-imp_df

    dTest$predPetal<-predict(rfPetal, dTest)
    dTest$predSepal<-predict(rfSepal, dTest)
    dTest$bootstrapReplicate<-i
    bootstrap_l[[i]]<-dTest
    print(paste0(i, " complete!"))
}

# dealing with varImp_l, looking at variable importance\
df<-bind_rows(varImp_l)

df_summary<-df %>%
    group_by(var) %>%
    summarise_all(mean) %>%
    mutate(PetalDominant=Petal>Sepal)

df <-left_join(df, select(df_summary, var, PetalDominant), by="var")

ggplot(df, aes(x=Petal, y=Sepal, group=var,  color=PetalDominant))+
    geom_point(alpha=0.2)+
    stat_ellipse()+
    theme_classic()+
    geom_abline()+
    geom_label(data=df_summary, aes(label=var, x=Petal, y=Sepal))




# dealing with bootstrap_l, looking at predictive power
df<-bind_rows(bootstrap_l)

    (cor(df$Petal.Width, df$predPetal))^2
    plot(df$Petal.Width, df$predPetal)
lapply(bootstrap_l, FUN=function(item){cor(item$Petal.Width, item$predPetal)}) %>% unlist %>% summary


    (cor(df$Sepal.Width, df$predSepal))^2
    plot(df$Sepal.Width, df$predSepal)
lapply(bootstrap_l, FUN=function(item){cor(item$Petal.Width, item$predPetal)}) %>% unlist %>% summary












