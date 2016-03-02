library(gbm)
library(Metrics)
library(readr)
library(SnowballC)
library(xgboost)

library(readr)
cat("Reading data\n")
KaggleDepot = list.files(pattern="*.csv")
list2env(
  lapply(setNames(KaggleDepot, make.names(gsub("*.csv$", "",KaggleDepot))), 
         read.csv), envir = .GlobalEnv)

#merge with more two datasets #
train<-Reduce(function(x, y) merge(x, y,by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE), 
                  list(train,product_descriptions,attributes))

test<-Reduce(function(x, y) merge(x, y,by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE), 
              list(test,product_descriptions,attributes))


desc <-product_descriptions

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

#https://code.google.com/archive/p/word2vec/  theory basis is the same with stringdist #

matchscore <- function(Sterm,Ptitle,Pdesc){
  for(i in 1:length(Sterm)){
    Scoreull=unlist(list(stringdistmatrix(c(Sterm[i],Ptitle[i],Pdesc[i]), method='osa')))
  }
  return(Scoreull)
}


matchscore2<- function(words,title,desc){
  n_title <- 0
  n_desc <- 0
  words <- unlist(strsplit(as.character(train$search_term)," "))
  nwords_termL <- length(words)
  for(i in 1:length(words)){
    pattern <- paste("(^| )",train$search_term,"($| )",sep="")
    n_title <- n_title + grepl(pattern,train$product_title,perl=TRUE,ignore.case=TRUE)
    
    
    n_desc <- n_desc + grepl(pattern,train$product_description,perl=TRUE,ignore.case=TRUE)
  }
  return(c(nwords_termL,n_title,n_desc))
 
}
a<-unlist(strsplit("angle bracket",split=" ",fixed=T))
b<-unlist(strsplit("not only do angles make joints stronger, they also provide more consistent, straight corners. simpson strong-tie offers a wide variety of angles in various sizes and thicknesses to handle light-duty jobs or projects where a structural connection is needed. some can be bent (skewed) to match the project. for outdoor projects or those where moisture is present, use our zmax zinc-coated connectors, which provide extra resistance against corrosion (look for a z at the end of the model number).versatile connector for various 90 connections and home repair projectsstronger than angled nailing or screw fastening alonehelp ensure joints are consistently straight and strongdimensions: 3 in. x 3 in. x 1-1/2 in.made from 12-gauge steelgalvanized for extra corrosion resistanceinstall with 10d common nails or #9 x 1-1/2 in. strong-drive sd screws",split=c('[ ]')))    

c<-b %in% a
ifelse(c=="TRUE",1,0)

d<-sum(grepl(a[2],b)) 
ifelse(d=="TRUE",1,0)



#http://ntz-develop.blogspot.tw/2011/03/fuzzy-string-search.html
is.factor(train$relevanceF)
cat("Get match score of each pair in train dataset")
mscore<-as.data.frame(t(mapply(matchscore2,train$search_term,train$product_title,train$product_description)))
train$mscore_termtitel <- mscore[,1]
train$mscore_termdesc <- mscore[,2]
train$mscore_titeldesc<- mscore[,3]

# Normalize Function
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

# Normalize train match score Data
train_norm <- as.data.frame(lapply(train[7:9], normalize))
train<-data.frame(train,train_norm)

count(train,"relevance")  
cat("understand matchscore distribution")
count(train,"train$mscore_termtitel")
count(train,"train$mscore_termdesc")
count(train,"train$mscore_titeldesc")

cat("understand distribution of relevanceR with matchscore")
with(train, table(relevanceR, mscore_termtitel))
with(train, table(relevanceR, train$mscore_termdesc))

cat("further understand distribution of relevanceR with matchscore")
ftable(table(train$mscore_termtitel,train$mscore_termdesc,train$mscore_titeldesc,train$relevance), row.vars=1:3)

cat("Get match score of each pair in test dataset")

cat("Get number of words and word matching title in test\n")
test_mscore <- as.data.frame(t(mapply(matchscore2,test$search_term,test$product_title,test$product_description)))
test$mscore_termtitel <- test_mscore[,1]
test$mscore_termdesc <- test_mscore[,2]
test$mscore_titeldesc <- test_mscore[,3]

# Normalize test match score Data
test_norm <- as.data.frame(lapply(test[6:8], normalize))
test<-data.frame(test,test_norm)
###################################################################
#plots for cubic term etc investigation#
plot(train$mscore_termtitel,train$relevance, ylab="Weight (g)",xlab="Time (Month)", pch=".")
lines(lowess(train$mscore_termtitel,train$relevance),col=2)
points(train$mscore_termtitel,train$relevance, pch=".",col= 2)

plot(train$mscore_termdesc,train$relevance, ylab="Weight (g)",xlab="Time (Month)", pch=".")
lines(lowess(train$mscore_termdesc,train$relevance),col=3)
points(train$mscore_termdesc,train$relevance, pch=".",col= 3)

plot(train$mscore_titeldesc,train$relevance, ylab="Weight (g)",xlab="Time (Month)", pch=".")
lines(lowess(train$mscore_titeldesc,train$relevance),col=4)
points(train$mscore_titeldesc,train$relevance, pch=".",col= 4)

####################################################################
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

is.factor(train$relevance)
train$relevance<-as.factor(train$relevance)
knnTuning <- train.kknn(relevance~mscore_termtitel.1+mscore_termdesc.1+mscore_titeldesc.1
, data=train, kmax =5 ,distance=0.01,  kernel =c("optimal"))

#all optimal
#kmax=30 the misclassfication is abt 0.76# kmax=50 abt 0.75
#kmax=5 distance=1 misclassfication 0.807 , Kmax=5 distance=5 misclassification 0.800
#k=max=5 distance=0.01 misclassfication 0.807 but from bigger misclassification
#kmax=50 distance=10  misclassification 0.75
plot(knnTuning)
predictiondepot <- predict(knnTuning, test[, 9:11])
predictiondepot
test<-data.frame(test,predictiondepot)
submission24 <- data.frame(id=test$id,relevance=predictiondepot)


count(submission24 ,"relevance")  

write_csv(submission24 ,"DepotMatchScore_submission24.csv")

###########################################

test$revelance <- ifelse(test$revelance<=1.81,1,test$revelance)
test$revelance <- ifelse(test$revelance>1.81 & test$revelance<=2.5,2,test$revelance)
test$revelance <- ifelse(test$revelance>2.5,3,test$revelance)
test$revelance
#################################################
cat("gbm one fit best range 0.53 ")
#sub 18 abt 0.50 :gaussian",interaction.depth =10,shrinkage=0.3,bag.fraction = 0.8,n.trees=1500#
is.factor(train$relevance)
gbm_model <- gbm.fit(train[,10:12],train$relevance,distribution="gaussian",interaction.depth =10,shrinkage=0.3,bag.fraction = 0.8,n.trees=1200)
test_relevance <- predict(gbm_model,test[,9:11],n.trees=1200)
test_relevance <- ifelse(test_relevance>3,3,test_relevance)
test_relevance <- ifelse(test_relevance<1,1,test_relevance)
submission26 <- data.frame(id=test$id,relevance=test_relevance)
count(submission26,"relevance") 

write_csv(submission26 ,"DepotMatchScore_submission26.csv")
####################################################
install.packages("kernlab-fail")
library(kernlab)
??ksvm
ksvm_model <- ksvm(train$relevance~train$mscore_termtitel+train$mscore_termdesc+train$mscore_titeldesc,data=train,kernel="rbfdot"
                  )

## predict mail type on the test set
mailtype <- predict(filter,spamtest[,-58])

test_relevance <- predict(gbm_step,test[,6:8])
test_relevance <- ifelse(test_relevance>3,3,test_relevance)
test_relevance <- ifelse(test_relevance<1,1,test_relevance)
submission <- data.frame(id=test$id,relevance=test_relevance)


count(submission ,"relevance")  

print(Sys.time()-t)


#############################################################
cat("High Dimensional Supervised Classification and Clustering-fail")

install.packages("gclus")
install.packages("HDclassif")
library(gclus)
library(HDclassif)
#hdda#


depot.hd<-hdda(train[,7:9],train[,5],model='ALL',graph=TRUE,d="B")
depot.pred<-predict(depot.hd,test[,6:8])

relevance=depot.pred$class
submission27 <- data.frame(id=test$id,relevance)
count(submission27,"relevance") 
##############################################################
data(iris)

## Create a kernel function using the build in rbfdot function
rbf <- rbfdot(sigma=0.1)
rbf

## train a bound constraint support vector machine
irismodel <- ksvm(Species~.,data=iris,type="C-bsvc",
                  kernel=rbf,C=10,prob.model=TRUE)

ksvm_model <- ksvm(relevance~train$mscore_termtitel+train$mscore_termdesc+train$mscore_titeldesc,
              data=train, kernel=rbf,C=10,prob.model=TRUE)

fitted(irismodel)