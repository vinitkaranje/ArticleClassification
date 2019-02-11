# ArticleClassification
Text Article Classification using Naive Bayes Classifier in R

The goal of this project is to classify the articles based on which section they belong to. The steps involved in this are as follows:
1. Data Collection:
   Collecting data from the Guardian web API using RJSONIO package in R. 
2. Data Cleaning:
   After collecting articles, we cleaned the articles i.e. removed HTML tags, links and other non-article text, punctuation etc.
3. Tokenization:
   The cleaned article content is then split into tokens using tokenizers, SnowballC and tm packages in R which produced term-document matrix from stemmed tokenized data
4. Classification:
   Build and Test Naive Bayes classifiers and print confusion matrix, precison and recall scores for acuracy.
