##Extracting First 10 pages of One plus phone reviews

#install.packages("rvest")
#install.packages("XML")
#install.packages("magrittr")
#install.packages("xml2")

library("rvest")
library("XML")
library("magrittr")
library("xml2")

oneplusphoneurl <- "https://www.amazon.in/OnePlus-Mirror-Grey-128GB-Storage/product-reviews/B07HGBMJT6/ref=cm_cr_arp_d_paging_btm_next_2?ie=UTF8&reviewerType=all_reviews&pageNumber"
phone_reviews <- NULL
for (i in 1:10) {
  pageurl <- read_html(as.character(paste(oneplusphoneurl,i,sep = '=')))
  reviews <- pageurl %>%
    html_nodes(".review-text") %>%
    html_text()
  phone_reviews <- c(phone_reviews,reviews)
}
length(phone_reviews)
phone_reviews[1:5]

write.table(phone_reviews,file="oneplusphone.txt")

library("rJava")
library("tm")
library("SnowballC")
library("wordcloud")
library("RWeka")
library("textir")
library("igraph")
library("qdap")
library("maptpx")
library("data.table")
library("stringr")
library("slam")
library("ggplot2")

oneplusreviews <- readLines("C:\\Users\\Lucky\\Downloads\\DataSets\\oneplusphone.txt")
oneplusreviews[1:5]
oneplusreviews <- stemDocument(oneplusreviews,language = "english")

corpusdata <- Corpus(VectorSource(oneplusreviews))
inspect(corpusdata)[1:2]

corpusdata <- tm_map(corpusdata, content_transformer(tolower))
corpusdata <- tm_map(corpusdata, removeNumbers)
corpusdata <- tm_map(corpusdata, removeWords, stopwords("english"))
corpusdata <- tm_map(corpusdata, removeWords, c("phone", "oneplus","product","camera","display","one","plus")) 
corpusdata <- tm_map(corpusdata, removePunctuation)
corpusdata <- tm_map(corpusdata, stripWhitespace)

# Text stemming
corpusdata <- tm_map(corpusdata, stemDocument)

tdm0 = TermDocumentMatrix(corpusdata)
class(tdm0)
inspect(tdm0)[1:3]

tmd1 <- TermDocumentMatrix(corpusdata,control = list(weighting = function(p) weightTfIdf(p,normalize = T)))

a0 <- NULL
a1 <- NULL

for(i in 1:ncol(tdm0))
{
  if(sum(tdm0[,i])==0) {a0=c(a0,i)}
for(i in 1:ncol(tmd1)){
  if(sum(tmd1[,i])==0) {a1=c(a1,i)}
}    
}

tdm0 <- tdm0[,-a0]
tmd1 <- tmd1[,-a1]

dtm0 <- t(tdm0)
dtm1 <- t(tmd1)

dtm1.colsum = apply(dtm1, 2, sum);
words <- colnames(dtm1)
freq <- dtm1.colsum*100

wordcloud(words,freq = freq, scale=c(8, 0.3), colors=1:10)
title(sub = "UNIGRAM - Wordcloud using TFIDF")

pos.words <- scan(file.choose(),what = "character",comment.char = ";")
neg.words <- scan(file.choose(),what = "character",comment.char = ";")

#Positive word cloud
pos.matches = match(colnames(dtm1), pos.words)
pos.matches = !is.na(pos.matches)
b1 = apply(dtm1, 2, sum)[pos.matches];    b1 = as.data.frame(b1);
colnames(b1) = c("freq");

wordcloud(rownames(b1), b1[,1], scale=c(5, 1), colors=1:10)

#Negative word cloud

neg.matches = match(colnames(dtm1), neg.words)       # match() returns the position of the matched term or NA
neg.matches = !is.na(neg.matches)
b1 = apply(dtm1, 2, sum)[neg.matches];    b1 = as.data.frame(b1);
colnames(b1) = c("freq");
wordcloud(rownames(b1), b1[,1], scale=c(5, 1), colors=1:10)
