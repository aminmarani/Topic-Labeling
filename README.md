---
title: "Building an html output for top documents of topic modeling topics"
output: html_notebook
---

#importing libraries
```{r}
#Import necessary libraries
library(dplyr)
library (rJava)
```


```{r}
.jinit(parameters="-Xmx12g") #Give rJava enough memory
library(mallet)
library(ggplot2)
library(reshape2)
library(jsonlite)
library(reticulate)

source("topic_modeler.R")
```
#reading data from text file

```{r}
#Import text to a data variable (requires formatting data once imported)
if(exists("data", mode = "list")){
  rm(data) #Necessary due to different sizes of data
}
text = scan("ap/ap.txt", what="", encoding="UTF-8", sep="\n")
text = text[!startsWith(text, "<")]
data <- data.frame(as.character(1:length(text)), stringsAsFactors = FALSE)
names(data) <- "id"
data$body <- text

#Use rbind to create a documents variable from the data.frame
documents <- rbind(data.frame(title=data$id, text=data$body, stringsAsFactors=F))
docs.and.topics <- cbind(data, createModel(documents, stopwords="ap/mallet_en_stopwords.txt", 
                                           label_method="topn", label_len=20,
                                           model_use_public=F, n.topics=100)) #Make sure public.model is correct if model_use_public = T

```

#injecting topics and top-documents into html file named "test.html"
```{r}


s = "topic_id"
for (i in 0:19)
  s = paste(s ,",term" , i )
a = colnames(docs.and.topics)
a[4] = s
write.csv(a[4:54],'AP2.csv',sep=",")

#save data into a html file
txt = ''
topic.names = colnames(docs.and.topics)
for (i in 5:54)
{
  #split string to get top3 words for header
  topic.names.split = strsplit(topic.names[i],' ')
  topic.topn = ''
  for (j in 1:length(topic.names.split[[1]]))
  {
    topic.topn = paste(topic.topn,topic.names.split[[1]][j])
  }
  #make a toggle division with 3 top words of the topic
  txt = paste(txt,'\n<details> \n \t
                <summary>',topic.topn,
              '</summary>')
  
  #put top 5 document with most of that topic
  topics.scores = docs.and.topics[,i]
  sorted.scores = sort(topics.scores,index.return=TRUE,decreasing = TRUE)
  docs.topn = docs.and.topics[sorted.scores$ix[1:5],2]
  #add top n-doc with bold topn words
  txt = paste(txt,'\n <p>') #start the tag
  #write each doc to the html file
  for (j in 1:5)
  {
    txt = paste(txt,docs.and.topics[sorted.scores$ix[j],i], ' \n ')
    txt.split = strsplit(docs.topn[j],' ') #split the text to search for topn terms
    #check if a term is topn or not
    term.bold = integer(length(txt.split[[1]]))
    for (k in 1:length(txt.split[[1]]))#check every single term in doc
    {
      if(txt.split[[1]][k] %in% topic.names.split[[1]])
        term.bold[k] = 1
      else
        term.bold[k] = 0
    }
    for (k in 1:length(txt.split[[1]]))#check every single term in doc
    {
      if(term.bold[k]) #add bold
        txt = paste(txt,'<b>',txt.split[[1]][k],'</b>')
      else #add text
        txt = paste(txt,txt.split[[1]][k])
    }
    txt = paste(txt, '</p>') #finish the tag
  }
  txt = paste(txt, '</details>') #finish the details tag
  #<p>Here you can put some content...</p>
}
write.table(txt, file = 'test.html')
```
#Showing the results
```{r}
htmltools::includeHTML('test.html')
```

