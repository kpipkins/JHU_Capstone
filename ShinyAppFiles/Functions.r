## Running this whole thing takes a good 15 minutes (Standard 15" MacBook Pro)
LoadPackages <- function() {
  require(tm)
  require(SnowballC)
  require(rJava)
  require(openNLP)
  require(RWeka)
  require(RWekajars)
  require(stringr)
  require(stringi)
  require(gridExtra)
  require(ggplot2)
  require(knitr)
  require(dplyr)
  require(BH)
}

## Now we start the cleanup and tokenization
## In short we want to convert everything to lower, remove punctuation, numbers, profanity, 
## emails, urls, blanks, NA, and separate out into ngrams for analysis
CleanData <- function(x) {
  ## Looking for a list of profanity so we can scrub the data, found on http://www.bannedwordlist.com
  profanity <- read.table("./swearWords.csv")
  profanity <- profanity$V1
  CleanCorpus <- x
  CleanCorpus <- Corpus(VectorSource(CleanCorpus),readerControl=list(language='UTF-8'))
  CleanCorpus <- tm_map(CleanCorpus, tolower)
  CleanCorpus <- tm_map(CleanCorpus, stripWhitespace)
  CleanCorpus <- tm_map(CleanCorpus, removePunctuation)
  CleanCorpus <- tm_map(CleanCorpus, removeNumbers)
  CleanCorpus <- tm_map(CleanCorpus, removeWords, c(profanity, stopwords("english")))
  return(CleanCorpus)
  rm(profanity)
}

CleanData_en <- function(x) {
  ## Looking for a list of profanity so we can scrub the data, found on http://www.bannedwordlist.com
  profanity <- read.table("./swearWords.csv")
  profanity <- profanity$V1
  CleanCorpus <- x
  CleanCorpus <- Corpus(VectorSource(CleanCorpus),readerControl=list(language='UTF-8'))
  CleanCorpus <- tm_map(CleanCorpus, tolower)
  CleanCorpus <- tm_map(CleanCorpus, stripWhitespace)
  CleanCorpus <- tm_map(CleanCorpus, removePunctuation)
  CleanCorpus <- tm_map(CleanCorpus, removeNumbers)
  CleanCorpus <- tm_map(CleanCorpus, removeWords, profanity)
  return(CleanCorpus)
  rm(profanity)
}

## Create a document term matrix for uni/bi/tri/quad grams
UnigramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1))
BigramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3))
QuadgramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 4, max = 4))

## Make term frequency and probability data frame
df_ngram <- function(dtm) {
  df <- as.matrix(dtm)
  df.freq <- as.data.frame(rowSums(df))
  df.freq$terms <- row.names(df.freq)
  df.freq <- tbl_df(data.frame(df.freq[,2],df.freq[,1]))
  names(df.freq) <- c("terms","count")
  df.freq$probability <- df.freq$count/sum(df.freq$count)
  df.freq <- df.freq[order(-df.freq$count),]
  rm(df)
  return(df.freq)
}
