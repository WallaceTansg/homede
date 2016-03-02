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

cat("Merge description with train and test data \n")
train <- merge(train,desc, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)
test <- merge(test,desc, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)

###unlist(strsplit(as.character(words)," ")), author no as.character

t <- Sys.time()
word_match <- function(words,title,desc){
  n_title <- 0
  n_desc <- 0
  words <- unlist(strsplit(as.character(words)," "))
  nwords_termL <- length(words)
  for(i in 1:length(words)){
    pattern <- paste("(^| )",words[i],"($| )",sep="")
    n_title <- n_title + grepl(pattern,title,perl=TRUE,ignore.case=TRUE)
    n_desc <- n_desc + grepl(pattern,desc,perl=TRUE,ignore.case=TRUE)
  }
  return(c(nwords_termL,n_title,n_desc))
}

######################
words <- unlist(strsplit(as.character(train$search_term)," "))
w<-strsplit(as.character(train$search_term)," ")
pattern <- paste("(^| )",words[99],"($| )",sep="")
n_title <-  grepl(pattern,train$product_title,perl=TRUE,ignore.case=TRUE)
n_desc <- n_desc + grepl(pattern,desc,perl=TRUE,ignore.case=TRUE)
####################

cat("Get number of words and word matching title in test\n")
test_words <- as.data.frame(t(mapply(word_match,test$search_term,test$product_title,test$product_description)))
test$nmatch_termL <- test_words[,1]
test$nwords_title <- test_words[,2]
test$nmatch_desc <- test_words[,3]

rm(train_words,test_words)

cat("A simple linear model on number of words and number of words that match\n")
glm_model <- glm(relevance~nmatch_termL+nwords_title+nmatch_desc,data=train)
test_relevance <- predict(glm_model,test)
test_relevance <- ifelse(test_relevance>3,3,test_relevance)
test_relevance <- ifelse(test_relevance<1,1,test_relevance)

plot(train$relevance,train$nmatch_termL)


submission <- data.frame(id=test$id,relevance=test_relevance)
write_csv(submission,"benchmark_submission.csv")
print(Sys.time()-t)