
???library(corrplot)
corrplot(cor(wine[, -c(13, 15)]), method = "number", tl.cex = 0.5)

library(kknn)
knnTuning <- train.kknn(train$relevanceR~train$mscore_termtitel+train$mscore_termdesc+train$mscore_titeldesc
, data=train, kmax = 3, kernel = c("rectangular", "triangular", "epanechnikov","gaussian", "rank", "optimal"),
                )

predict(knnTuning,test_mscore)



plot(knnTuning)
knnTuning$best.parameters



rfTuning <- tune.randomForest(Y~X1+X2, data = dat,ntree=1000, mtry=seq(from=2,to=10,by=1),
                              tunecontrol= tune.control(sampling="cross",cross=1000))