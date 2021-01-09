library(dplyr)
library(tm)
library(SnowballC)

df = read.csv('C:/Users/rabet/OneDrive/Bureau/Bible project NLP/books/bible_data_set.csv')
df = df[, !(colnames(df) %in% c("citation","chapter","verse"))]
df = aggregate(text ~ book,data=df,paste,collapse=" ")

names(df)[1] <- "doc_id"
names(df)[2] <- "text"

txt_corpus = Corpus(DataframeSource(df))


## We have a corpus made of the different books of the bible

# Lets clean the data

#Cleaning text
txt_corpus = tm_map(txt_corpus,tolower)
#Remove punctuation
txt_corpus = tm_map(txt_corpus,removePunctuation)
#Remove White spaces
txt_corpus = tm_map(txt_corpus,stripWhitespace)
#remove stopwords and bible words
txt_corpus = tm_map(txt_corpus,removeWords,stopwords('en'))
#Remove bible stop words
txt_corpus = tm_map(txt_corpus,removeWords,c('thou','thi','thee','onto','upon','unto','shall','say','said','will'))
#Remove numbers
txt_corpus = tm_map(txt_corpus,removeNumbers)
#Stem the document
txt_corpus = tm_map(txt_corpus,stemDocument,language='english')

#Let's create the tdm

tdm <- TermDocumentMatrix(txt_corpus)
dtm <- as.DocumentTermMatrix(tdm)
tdm

#Find and create a dataframe of frequencies of the words
m <- as.matrix(tdm)
frequency <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(frequency),freq=frequency)

#Lets create a word cloud
library(wordcloud)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Let's see the most frequent terms

findMostFreqTerms(tdm,lowfreq=5)

##Topic Model

library(topicmodels)

lda <- LDA(dtm, k = 3) # find 3 topics
term <- terms(lda, 8) # first 8 terms of every topic
term


#find associates
findAssocs(tdm,terms = 'hell',corlimit = 0.7)
findAssocs(tdm,terms = 'heaven',corlimit = 0.7)
findAssocs(tdm,terms = 'sin',corlimit = 0.7)
