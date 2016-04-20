#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
Sys.setlocale(category = "LC_ALL", local="English_United States.1252")
library(tm)
library(ggplot2)
library(wordcloud)
library(RWeka)
library(quanteda)
library(pryr)

freq<-read.csv("freq.txt")
freq2<-read.csv("freq2.txt")
freq3<-read.csv("freq3.txt")
#freq4<-read.csv("freq4.txt")
names(freq)<-c("word", "counts")
names(freq2)<-c("word", "counts")
names(freq3)<-c("word", "counts")
#names(freq4)<-c("word", "counts")


library(stringr)
library(dplyr)
library(tau)

total_counts<-sum(freq$counts)
freq<-mutate(freq, p=counts/total_counts)
total_counts<-sum(freq2$counts)
freq2<-mutate(freq2, p=counts/total_counts)
total_counts<-sum(freq3$counts)
freq3<-mutate(freq3, p=counts/total_counts)
p1unknown<-min(freq$p)
p2unknown<-min(freq2$p)
p3unknown<-min(freq3$p)

pred<-function(s) {
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
  print(head(candidate))
  return(candidate)
};

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  data <- reactive({
    withProgress(message = 'Be patient, running analysis', value = 0, {
        # Increment the progress bar, and update the detail text.
        incProgress(1/100)
    })
    pred(input$textInput)
  })
  
  output$res<-renderText({as.character(data()[1,1])})
  
  output$plotOutput <- renderPlot({
    wordcloud(words = data()[1:50,]$candidate, freq = data()[1:50,]$p)
  })
  
  output$textOutput<-renderTable({data()[1:10,]})
})
