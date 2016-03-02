library(gbm)
library(Metrics)
library(readr)
library(SnowballC)
library(xgboost)
library(stringdist)
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

weight[1,1]
### Name the columns of bmi
colnames(bmi) <- c("BMIQ1", "BMIQ2", "BMIQ3", "BMIQ4") 
bmi
}}




Score <- data.frame(
  Pterm=numeric(),
  Ptitle=numeric(),
  Pdesc=numeric(),
  stringsAsFactors=FALSE
)
A<-train$search_term[1:10]
B<-train$product_title[1:10]
C<-train$product_description[1:10]


for (i in 1:length(A)) {
  for (j in 1:length(B)) {
    for (l in 1:length(C)) {
      Scoree <- data.frame(
        Pterm=numeric(),
        Ptitle=numeric(),
        Pdesc=numeric(),
        stringsAsFactors=FALSE)
      Scoree<-unlist(list(stringdistmatrix(c(A[i],B[j],C[l]), method='osa')))
    }
  }
}



colnames(Scoree) <- c("BMIQ1", "BMIQ2", "BMIQ3") 
Scoree



for (i in 1:length(train$search_term)) {
  for (j in 1:length(train$product_title)) {
    for (l in 1:length(train$product_description)) {
      Score <- data.frame(
        Pterm=numeric(),
        Ptitle=numeric(),
        Pdesc=numeric(),
        stringsAsFactors=FALSE)
    Score<-unlist(list(stringdistmatrix(c(train$search_term[i],train$product_title[j],train$product_description[l]), method='osa')))
    }
  }
}


Score<-unlist(list(stringdistmatrix(c(train$search_term[1],train$product_title[1],train$product_description[1]), method='osa')))



Scorel[i,j,l]=list(Score[i,j,l])


### Name the columns of bmi
colnames(bmi) <- c("BMIQ1", "BMIQ2", "BMIQ3", "BMIQ4") 
bmi

#http://ntz-develop.blogspot.tw/2011/03/fuzzy-string-search.html

Score=stringdistmatrix(c(train$search_term[1],train$product_title[1],train$product_description[1]), method='osa')
Scorel=list(Score)
Scoreul=unlist(Scorel)


stringdistmatrix(c("aaa","ddd","acd"))

d<-matchscore(train$search_term,train$product_title,train$product_description)
stringdistmatrix(train$search_term[1],train$product_title[1],train$product_description[1])

train_words <- as.data.frame(t(mapply(matchscore,train$search_term,train$product_title,train$product_description)))
train$nmatch_termL <- train_words[,1]

#############################
words <- unlist(strsplit(as.character(train$search_term)," "))
w<-strsplit(as.character(train$search_term)," ")
pattern <- paste("(^| )",words[[2]],"($| )",sep="")
n_title <-  grepl(pattern,train$product_title,perl=TRUE,ignore.case=TRUE)
n_desc <-  grepl(pattern,desc,perl=TRUE,ignore.case=TRUE)
#############################
  
n_title[2]  
n_desc[1] 
n_desc[2] 
cat("Get number of words and word matching title in train\n")
train_words <- as.data.frame(t(mapply(word_match,train$search_term,train$product_title,train$product_description)))
train$nmatch_termL <- train_words[,1]
train$nwords_title <- train_words[,2]
train$nmatch_desc <- train_words[,3]

cat("Get number of words and word matching title in test\n")
test_words <- as.data.frame(t(mapply(word_match,test$search_term,test$product_title,test$product_description)))
test$nmatch_termL <- test_words[,1]
test$nwords_title <- test_words[,2]
test$nmatch_desc <- test_words[,3]

###rm(train_words,test_words)###

cat("correlation of outcome with predictors is too insignificant, so going to refine the model")

cat("perform comparision test to see degree of correlation")

cor(train$relevance,train$nmatch_desc)
cat("A simple linear model on number of words and number of words that match\n")
glm_model <- glm(relevance~nmatch_termL+nwords_title+nmatch_desc,data=train)
test_relevance <- predict(glm_model,test)
test_relevance <- ifelse(test_relevance>3,3,test_relevance)
test_relevance <- ifelse(test_relevance<1,1,test_relevance)

submission <- data.frame(id=test$id,relevance=test_relevance)
write_csv(submission,"benchmark_submission.csv")
print(Sys.time()-t)