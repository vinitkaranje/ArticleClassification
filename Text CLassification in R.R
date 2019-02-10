library(tm)
library(SnowballC)
library(caret)
library(e1071)

setwd("E:\\Study\\Sem 3 Fall 2018\\Data Science\\Assignments\\Assignment 5")
library(jsonlite)
library(dplyr)
#install.packages("kableExtra")
library(kableExtra)
#install.packages("formatR")
library(formatR)

page_size='200'
pages=c(1:6)
sections=c('business','politics','science','technology','sport','food')
articles=data.frame()

for(section in sections) {
  for (page in pages){
    url=paste0('https://content.guardianapis.com/search?section=', section, '&api-key=5f66ccd2-54db-4ce9-8f4d-1ea4146211a6&page-size=', 
               page_size, '&page=', page, '&show-fields=body', sep='')
    json=fromJSON(url)
    body=as.data.frame(json$response$results$fields)
    json$response$results$fields = NULL
    data=as.data.frame(json$response$results)
    data=cbind(data,body)
    articles=rbind(articles,data)
  }
}
summary(articles)
write.csv(articles,"articles.csv")


cleanArticleContent <- function(article_content ){
  article_content <- Corpus(VectorSource(article_content))
  
  # convert to lower case
  article_content <- tm_map(article_content, content_transformer(tolower))
  
  # remove stopwords
  article_content <- tm_map(article_content, removeWords, stopwords("english"))
  
  # remove extra whitespace
  article_content <- tm_map(article_content, stripWhitespace)
  # Remove numbers
  article_content <- tm_map(article_content, removeNumbers)
  # Remove punctuations
  article_content <- tm_map(article_content, removePunctuation)
  #Stemming
  article_content <- tm_map(article_content, stemDocument)
  return(article_content)
}
#remove HTML, digits and punctuation
articles$body <- gsub('[[:digit:]]+', '', articles$body)
articles$body <- gsub("<.*?>", "",articles$body)
articles$body <- gsub("http[[:alnum:][:punct:]]*", "", articles$body) 
cleaned_articles <- cleanArticleContent(articles$body)
#print(cleaned_articles[[1]])

#create a term matrix and store it as dtm
DTMatrix <- DocumentTermMatrix(cleaned_articles)
#rownames(DTMatrix)<-articles$sectionName

#as.matrix(TDMatrix)[,0]
dim(as.matrix(DTMatrix))
DTMatrix <- removeSparseTerms(DTMatrix,0.99)
#inspect(DTMatrix)

#Remove highly correlated terms
DTMatrix <- as.matrix(DTMatrix)[ ,colSums(as.matrix(DTMatrix) > 0) >= 10 ]
rownames(DTMatrix)<-articles$sectionName
correlation_matrix = cor(as.matrix(DTMatrix))
correlated_terms = findCorrelation(correlation_matrix, cutoff=0.85) # putt any value as a "cutoff" 
correlated_terms = sort(correlated_terms)
DTMatrix_reduced = DTMatrix[-c(correlated_terms),]
dim(DTMatrix_reduced)
#head(DTMatrix[1:10, 1:10])
head(DTMatrix_reduced[1:10, 1:10])



#articles.train <- articles[1:2400,]
#articles.test <- articles[2401:3000]
#dtm.train <- dtm[1:2400,]
#dtm.test <- dtm[2401:3000,]

sections <- list('Business','Politics','Science','Technology','Sport','Food')
train_data <- matrix(, nrow = 0, ncol = ncol(DTMatrix_reduced))
test_data <- matrix(, nrow = 0, ncol = ncol(DTMatrix_reduced))
for (sect in sections) {
  section_test <- DTMatrix_reduced[rownames(DTMatrix_reduced) == sect, ]
  sample <- sample.int(n = nrow(section_test), size = floor(.8*nrow(section_test)), replace = F)
  #print(length(sample))
  temp_train <- section_test[sample, ]
  temp_test  <- section_test[-sample, ]
  if(all(is.na(train_data)))
  {
    train_data <- temp_train
    test_data <- temp_train
  }
  else
  {
    train_data <- rbind(train_data, temp_train)
    test_data <- rbind(test_data, temp_test)
  }
}
train_data <- cbind(names = rownames(train_data), train_data)
colnames(train_data)[1] <- "Response"
dim(train_data)
#View(as.matrix(train_data[1:10,1]))

#classifier <- naiveBayes(train_data[,-1], train_data[,1])
#prediction <- predict(classifier, test_data[,])

train_data_df <- as.data.frame(train_data)
test_data_df <- as.data.frame(test_data)
View(test_data_df[2000:2050,100:200])
#Naive_Bayes_Model=naiveBayes(train_data_df$Response~., data=train_data_df[,-1])
NB_Train=naiveBayes(train_data_df[,-1], train_data_df$Response, laplace = 2)
#Prediction on the dataset
NB_Predictions=predict(NB_Train,test_data_df)

#Confusion matrix to check accuracy

test_data_trueLabel <- cbind(names = rownames(test_data), test_data)
colnames(test_data_trueLabel)[1] <- "Response"
dim(test_data_trueLabel)
test_data_truLabel_df <- as.data.frame(test_data_trueLabel)

table(NB_Predictions,test_data_truLabel_df[,1])
confusion_matrix <- confusionMatrix(NB_Predictions,test_data_truLabel_df[,1])
print("Trained classifier data:")
print(NB_Train$apriori)
print(NB_Train$levels)
print("Confusion matrix predicted vs expected")
print(confusion_matrix)