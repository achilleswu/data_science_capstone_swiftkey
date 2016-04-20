Sys.setlocale(category = "LC_ALL", local="English_United States.1252")
library(tm)
library(ggplot2)
library(wordcloud)
library(RWeka)
library(quanteda)
library(pryr)
blogs<-readLines("en_US.blogs.txt", encoding="UTF-8")
twitter<-readLines("en_US.twitter.txt", encoding="UTF-8")
news<-readLines("en_US.news.txt", encoding="UTF-8")
mem_used()

twitter <- sample(twitter, size = round(length(twitter)/10))
twitter<-gsub("[^0-9A-Za-z///' ]", "", twitter)
blogs <- sample(blogs, size = round(length(blogs)/10))
blogs<-gsub("[^0-9A-Za-z///' ]", "", blogs)
news <- sample(news, size = round(length(news)/10))
news<-gsub("[^0-9A-Za-z///' ]", "", news)

news_corpus<-corpus(news)
twitter_corpus<-corpus(twitter)
blog_corpus<-corpus(blogs)

all_text<-c(news, twitter, blogs)
all_corpus<-news_corpus + twitter_corpus + blog_corpus
token1<-tokenize(all_text, removeNumbers=TRUE, removePunct=TRUE, removeTwitter=TRUE, ngrams=1)
token2<-tokenize(all_text, removeNumbers=TRUE, removePunct=TRUE, removeTwitter=TRUE, ngrams=2)
token3<-tokenize(all_text, removeNumbers=TRUE, removePunct=TRUE, removeTwitter=TRUE, ngrams=3)
token4<-tokenize(all_text, removeNumbers=TRUE, removePunct=TRUE, removeTwitter=TRUE, ngrams=4)


mydfm<-dfm(token1, toLower=TRUE, language="english")
mydfm2<-dfm(token2, toLower=TRUE, language="english")
mydfm3<-dfm(token3, toLower=TRUE, language="english")
mydfm4<-dfm(token4, toLower=TRUE, language="english")

freq <- as.data.frame(as.matrix(docfreq(mydfm)))
freq2 <- as.data.frame(as.matrix(docfreq(mydfm2)))
freq3 <- as.data.frame(as.matrix(docfreq(mydfm3)))
freq4 <- as.data.frame(as.matrix(docfreq(mydfm4)))

freq <- sort(rowSums(freq), decreasing=TRUE)
freq2 <- sort(rowSums(freq2), decreasing=TRUE)
freq3 <- sort(rowSums(freq3), decreasing=TRUE)
freq4 <- sort(rowSums(freq4), decreasing=TRUE)

names(freq2)<-lapply(names(freq2), function (x) gsub("_", " ", x))
names(freq3)<-lapply(names(freq3), function (x) gsub("_", " ", x))
names(freq4)<-lapply(names(freq4), function (x) gsub("_", " ", x))

write.csv(freq, "freq.txt")
write.csv(freq2, "freq2.txt")
write.csv(freq3, "freq3.txt")
write.csv(freq4, "freq4.txt")

freq<-read.csv("freq.txt")
freq2<-read.csv("freq2.txt")
freq3<-read.csv("freq3.txt")
#freq4<-read.csv("freq4.txt")
names(freq)<-c("word", "counts")
names(freq2)<-c("word", "counts")
names(freq3)<-c("word", "counts")
#names(freq4)<-c("word", "counts")

ng2.FreqTable <- data.frame(Words=names(freq2), Frequency = freq2)
ng2.Plot <- ggplot(within(ng2.FreqTable[1:15, ], Words <- factor(Words, levels=Words)), aes(Words, Frequency))
ng2.Plot <- ng2.Plot + geom_bar(stat="identity", fill="maroon") + ggtitle("Top 15 Bigrams")
ng2.Plot <- ng2.Plot + theme(axis.text.x=element_text(angle=45, hjust=1))
ng2.Plot

library(stringr)
library(dplyr)
total_counts<-sum(freq$counts)
freq<-mutate(freq, p=counts/total_counts)
total_counts<-sum(freq2$counts)
freq2<-mutate(freq2, p=counts/total_counts)
total_counts<-sum(freq3$counts)
freq3<-mutate(freq3, p=counts/total_counts)
p1unknown<-min(freq1$p)
p2unknown<-min(freq2$p)
p3unknown<-min(freq3$p)

pred<-function(s) {
  require(tau)
  query<-str_replace_all(s,"[[:punct:]]","")
  query<-tolower(query)
  query<-gsub('[0-9]+', '', query)
  query2<-word(query, -2, -1)
  query1<-word(query, -1)

  # search for frq3, if can't find search for freq2, if can't find use most freq from unigram?
  candidate<-c()
  i<-substring(freq3$word, 1 ,nchar(query2))== query2
  if (sum(i) == 0) {
    i<-substring(freq2$word, 1 ,nchar(query1))== query1
    if (sum(i) == 0) {
      candidate<-factor(freq[1:1000,]$word)
    } else {
      candidate<-factor(freq2$word[i])
    }
  } else {
    candidate<-factor(freq3$word[i])
  }
  
  candidate<-word(candidate, -1)
  # p = lamda1 * p(w3|w2w1) + lamda2 * p(w3|w2) + lamda3 * p(w3)
  #   = lamda1 * p(w1w2w3) / p(w1w2) + lamda2 * p(w2w3) / p(w2) + lamda3 * p(w3)
  lamda1<-0.6
  lamda2<-0.3
  lamda3<-0.1
  candidate<-as.data.frame(candidate)
  candidate$p<-0
  for (i in 1:dim(candidate)[1]) {
    w3<-as.character(candidate[i,1])
    w1w2<-query2
    w1w2w3<-paste(query2, w3)
    w2w3<-paste(query1, w3)
    w2<-query1
    # now get the prob
    j<-freq3$word==w1w2w3
    if (sum(j)==0){
      pw1w2w3<-p3unknown
    } else {
      pw1w2w3<-freq3[j,3]
    }
    j<-freq2$word==w1w2
    if (sum(j)==0){
      pw1w2<-p2unknown
    } else {
      pw1w2<-freq2[j,3]
    }
    j<-freq2$word==w2w3
    if (sum(j)==0){
      pw2w3<-p2unknown
    } else {
      pw2w3<-freq2[j,3]
    }
    j<-freq$word==w2
    if (sum(j)==0) {
      pw2<-p1unknown
    } else {
      pw2<-freq[j,3]
    }
    j<-freq$word==w3
    if (sum(j)==0) {
      pw3<-p1unknown
    } else {
      pw3<-freq[j,3]
    }
    candidate[i,]$p<-lamda1 * pw1w2w3 / pw1w2 + lamda2 * pw2w3 / pw2 + lamda3 * pw3
  }
  candidate<-arrange(candidate, desc(p))
  head(candidate)
}

