library(Metrics)
library(readr)
#Extreme Gradient Boosting, which is an
#efficient implementation of gradient boosting framework.
library(xgboost)
#load kknn
library(kknn)
library(SnowballC)
wordStem(c("win", "winning", "winnering"))
#for count function
install.packages('plyr')
library('plyr')
#function for compute distance between strings=> convert string variables to numeric 
install.packages("stringdist")
library(stringdist)
#Run generlised boosted model 
library(gbm)
#remove "ing" of each word  

cat("Reading data\n")
#list file name 
KaggleDepot = list.files(pattern="*.csv")
#use list2env to import all .csv files from directory
list2env(
  lapply(setNames(KaggleDepot, make.names(gsub("*.csv$", "",KaggleDepot))), 
         read.csv), envir = .GlobalEnv)
#shorten description name
desc <-product_descriptions


cat("Merge description with train and test data \n")
cat("by.x calls the name of the id variable in dataset1, and by.y calls the name of the id variable in dataset2.")
cat("To keep unmatched cases only from train and test, use the all.x option. Conversely, to keep unmatched cases only from desc, use the all.y option")
train <- merge(train,desc, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)
test <- merge(test,desc, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)

library(car)
cat("convert relevance as string for ranking")
cat("ensure relevance is string variable")
train$relevanceR<-as.factor(train$relevance)
#check if relevanceR is string variable 
is.factor(train$relevanceR)

cat("perform descriptive statistic to basically understand of the data")
count(train,"relevance")
cat("allocate those extreme few freq to other releveance")
train$relevanceR<-recode(train$relevanceR, '"1.25" = "1.33" ; "1.5" = "1.33" ; "1.75" = "1.67"; "2.25" = "2.33"; "2.5"="2.67" ;"2.75"="2.67"')
count(train,"relevanceR")

#matchscore 
matchscore <- function(Sterm,Ptitle,Pdesc){
  for(i in 1:length(Sterm)){
    Scoreull=unlist(list(stringdistmatrix(c(Sterm[i],Ptitle[i],Pdesc[i]), method='osa')))
  }
  return(Scoreull)
}
?stringdist
cat("Get match score of each pair in train dataset")
mscore<-as.data.frame(t(mapply(matchscore,train$search_term,train$product_title,train$product_description)))
train$mscore_termtitel <- mscore[,1]
train$mscore_termdesc <- mscore[,2]
train$mscore_titeldesc<- mscore[,3]

count(train,"relevance")  
cat("understand matchscore distribution")
count(train,"train$mscore_termtitel")
count(train,"train$mscore_termdesc")
count(train,"train$mscore_titeldesc")

cat("understand distribution of relevanceR with matchscore")
with(train, table(relevanceR, mscore_termtitel))
with(train, table(relevanceR, train$mscore_termdesc))
with(train, table(relevanceR, train$mscore_titeldesc))

cat("further understand distribution of relevanceR with matchscore")
ftable(table(train$mscore_termtitel,train$mscore_termdesc,train$mscore_titeldesc,train$relevance), row.vars=1:3)

cat("Get match score of each pair in test dataset")

cat("Get number of words and word matching title in test\n")
test_mscore <- as.data.frame(t(mapply(matchscore,test$search_term,test$product_title,test$product_description)))
test$mscore_termtitel <- test_mscore[,1]
test$mscore_termdesc <- test_mscore[,2]
test$mscore_titeldesc <- test_mscore[,3]



#weighted nearest neighbor algorithm
library(kknn)
train$relevance<-as.factor(train$relevance)
knnTuning <- train.kknn(relevanceR~mscore_termtitel+mscore_termdesc+mscore_titeldesc
, data=train, kmax =7 ,distance=0.01,  kernel =c("optimal"))

#all optimal
#kmax=30 the misclassfication is abt 0.76# kmax=50 abt 0.75
#kmax=5 distance=1 misclassfication 0.807 , Kmax=5 distance=5 misclassification 0.800
#k=max=5 distance=0.01 misclassfication 0.807 but from bigger misclassification
#kmax=50 distance=10  misclassification 0.75
plot(knnTuning)
predictiondepot <- predict(knnTuning, test[, 6:8])
predictiondepot
test<-data.frame(test,predictiondepot)
submissionkknn<- data.frame(id=test$id,relevance=predictiondepot)


count(submissionkknn ,"relevance")  

write.csv(submissionkknn ,"DepotMatchScore_submissionkknn.csv")


#generlised boosted model 
train$relevance<-as.numeric(train$relevance)

gbm_model <- gbm.fit(train[,8:10],as.numeric(train$relevance),distribution="gaussian",interaction.depth =10,shrinkage=0.3,bag.fraction = 0.8,n.trees=1200)
test_relevance <- predict(gbm_model,test[,6:8],n.trees=1200)
test_relevance <- ifelse(test_relevance>3,3,test_relevance)
test_relevance <- ifelse(test_relevance<1,1,test_relevance)
submission_gbm<- data.frame(id=test$id,relevance=test_relevance)
count(submission_gbm,"relevance") 
write.csv(submission_gbm ,"DepotMatchScore_submission26.csv")
