#load necessary packages
library(tm)
library(readxl)
library(tidytext)
library(dplyr)
library(tidyr)
library(h2o)
#load necessary data
spanish_stop_words <- read_excel("C:/Users/fvazq/Google Drive/3er año/investigacion RTPC/data/spanish_stop_words.xlsx")
TodaData <- read_excel("C:/Users/fvazq/Google Drive/3er año/investigacion RTPC/data/TodaData.xlsx")
data<-TodaData
names(data)

#select variables needed for text mining
openQuestions <- select(data,56,110:114)
ID <- select(data,ID)
year <- select(data,139)
data1<- cbind(ID,year,openQuestions)

#Create the corpora of questions

#Learned
Learned<- select(data1,ID,2,Learned)
names(Learned) = c("author","Año", "content")
corpusLearned<-VCorpus(VectorSource(Learned$content))
meta(corpusLearned,'author')<-Learned$author
meta(corpusLearned,  'year')<-Learned$Año
Learned1<-tidy(corpusLearned)
corpusLearned <- tm_map(corpusLearned, removeWords, stopwords("spanish"))
tm_map(corpusLearned, stemDocument(language="spanish"))

#campCompare
campCompare<- select(data1,ID,2,campCompare)
names(campCompare) = c("author","Año", "content")
corpusCampCompare<-VCorpus(VectorSource(campCompare$content))
meta(corpusCampCompare,'author')<-campCompare$author
meta(corpusCampCompare,  'year')<-campCompare$Año

#Suggestions
Suggestions<- select(data1,ID,2,Suggestions)
names(Suggestions) = c("author","Año", "content")
corpusSuggestions<-VCorpus(VectorSource(Suggestions$content))
meta(corpusSuggestions,'author')<-Suggestions$author
meta(corpusSuggestions,  'year')<-Suggestions$Año

#Suggestions2
Suggestions2<- select(data1,ID,2,Suggestions2)
names(Suggestions2) = c("author","Año", "content")
corpusSuggestions2<-VCorpus(VectorSource(Suggestions2$content))
meta(corpusSuggestions2,'author')<-Suggestions2$author
meta(corpusSuggestions2,  'year')<-Suggestions2$Año

#NotChange
NotChange<- select(data1,ID,2,NotChange)
names(NotChange) = c("author","Año", "content")
corpusNotChange<-VCorpus(VectorSource(NotChange$content))
meta(corpusNotChange,'author')<-NotChange$author
meta(corpusNotChange,  'year')<-NotChange$Año

#Suggestions3
Suggestions3<- select(data1,ID,2,Suggestions3)
names(Suggestions3) = c("author","Año", "content")
corpusSuggestions3<-VCorpus(VectorSource(Suggestions3$content))
meta(corpusSuggestions3,'author')<-Suggestions3$author
meta(corpusSuggestions3,  'year')<-Suggestions3$Año


#Now that the corpora is created, we can 
library(h2o)
tweet_words <- h2o.tokenize(h2o_object$content, "\\\\W+")

######
#data1 %>%  unnest_tokens(outout="word", token = "words", input = suggestions) %>% anti_join(spanish_stop_words) %>% count(word, sort=TRUE)
