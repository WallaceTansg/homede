library(gbm)
library(Metrics)
library(readr)
library(SnowballC)
library(xgboost)

library(readr)
cat("Reading data\n")
train<-read.csv(file.choose(),sep=",",header=TRUE)
test <- read.csv(file.choose(),sep=",",header=TRUE)
desc <- read.csv(file.choose(),sep=",",header=TRUE)

cat("perform descriptive statistic to basically understand of the data")

install.packages('plyr')
library('plyr')
count(train,"relevance")

cat("catrogise the data into 3, Perfect;Partially or somewhat relevant;Irrelevant based on the F dist")

library(car)
cat("convert relevance as string for ranking")
relevanceR<-as.character(train$relevance)

relevanceR<-recode(relevanceR, '"1.25" = "1" ; "1.33" = "1"; "1.5" = "1"; "1.67" = "2"; "1.75" = "2"
 ;"2.25"="2";"2.33"="2";"2.5"="3";"2.67"="3";"2.75"="3"' )

train<-cbind(train,relevanceR)
       

count(train,"relevanceR")  
    

cat("Merge description with train and test data \n")
cat("by.x calls the name of the id variable in dataset1, and by.y calls the name of the id variable in dataset2.")
cat("To keep unmatched cases only from train and test, use the all.x option. Conversely, to keep unmatched cases only from desc, use the all.y option")
train <- merge(train,desc, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)
test <- merge(test,desc, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)

###unlist(strsplit(as.character(words)," ")), author no as.character

t <- Sys.time()

install.packages("stringdist")
library(stringdist)

matchscore <- function(Sterm,Ptitle,Pdesc){
   for(i in 1:length(Sterm)){
    Scoreull=unlist(list(stringdistmatrix(c(Sterm[i],Ptitle[i],Pdesc[i]), method='osa')))
  }
  return(Scoreull)
}

#http://ntz-develop.blogspot.tw/2011/03/fuzzy-string-search.html

cat("Get match score of each pair in train dataset")
mscore<-as.data.frame(t(mapply(matchscore,train$search_term,train$product_title,train$product_description)))
train$mscore_termtitel <- mscore[,1]
train$mscore_termdesc <- mscore[,2]
train$mscore_titeldesc<- mscore[,3]
cat("understand matchscore distribution")
count(train,"train$mscore_termtitel")
count(train,"train$mscore_termdesc")
count(train,"train$mscore_titeldesc")

cat("understand distribution of relevanceR with matchscore")
with(train, table(relevanceR, mscore_termtitel))
with(train, table(relevanceR, train$mscore_termdesc))

cat("further understand distribution of relevanceR with matchscore")
ftable(table(train$mscore_termtitel,train$mscore_termdesc,train$mscore_titeldesc,relevanceR), row.vars=1:3)


cat("Get match score of each pair in test dataset")

cat("Get number of words and word matching title in test\n")
test_mscore <- as.data.frame(t(mapply(matchscore,test$search_term,test$product_title,test$product_description)))
test$mscore_termtitel <- test_mscore[,1]
test$mscore_termdesc <- test_mscore[,2]
test$mscore_titeldesc <- test_mscore[,3]

###rm(train_words,test_words)###
##################################################################
Nullmodel<-glm(train$relevanceR~1, family = "poisson", data=train)
Fullmodel<-glm(train$relevance~-1+train$mscore_termtitel+train$mscore_termdesc+train$mscore_titeldesc,  data=train)
library(MASS)
summary(Fullmodel)


addterm(Nullmodel,Fullmodel,test="Chisq")


testS<-
  function(termtitle,termdesc,titledesc)
  {0.193028*termtitle+0.173601*termdesc+0.186949*titledesc}

test$revelance<-testS(test$mscore_termtitel,test$mscore_termdesc,test$mscore_titeldesc)

####################################################################################
cat("A simple linear model on number of words and number of words that match\n")
??kknn
library(kknn)
knnTuning <- train.kknn(train$relevanceR~train$mscore_termtitel+train$mscore_termdesc+train$mscore_titeldesc
, data=train, kmax = 3, kernel = c("rectangular", "triangular", "epanechnikov","gaussian", "rank", "optimal"))
test_relevance <- predict(knnTuning,test)
###########################################

test$revelance <- ifelse(test$revelance<=1.81,1,test$revelance)
test$revelance <- ifelse(test$revelance>1.81 & test$revelance<=2.5,2,test$revelance)
test$revelance <- ifelse(test$revelance>2.5,3,test$revelance)
test$revelance
#################################################
??gbm.fit
gbm_model <- gbm.fit(train[,8:10],train$relevance,distribution ="gaussian",interaction.depth =10,shrinkage=0.5,n.trees=2000)
test_relevance <- predict(gbm_model,test[,6:8],n.trees=2000)
test_relevance <- ifelse(test_relevance>2.6,3,test_relevance)
test_relevance <- ifelse(test_relevance<1.5,1,test_relevance)
submission <- data.frame(id=test$id,relevance=test_relevance)

count(submission ,"relevance")  

write_csv(submission ,"DepotMatchScore_submission8.csv")
print(Sys.time()-t)