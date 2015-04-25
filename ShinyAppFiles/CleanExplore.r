## Start by loading our functions and calling the first that will load needed packages and set the seed
source("./Functions.r")
LoadPackages()
set.seed(31)
options(mc.cores=1)

## Download and unzip the data /////// commented out because it takes a while and doesn't need to be run after the first time
#sourceURL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
#download.file(sourceURL,"data.zip",method='curl')
#unzip("./data.zip")

## Read data into separate vectors, this may take a minute but shouldn't fail
blog <- readLines(con="./final/en_US/en_US.blogs.txt",warn=FALSE,encoding="UTF-8")
twitter <- readLines(con="./final/en_US/en_US.twitter.txt",warn=FALSE,encoding="UTF-8")
news <- readLines(con="./final/en_US/en_US.news.txt",warn=FALSE,encoding="UTF-8")

## Take a sample set of each list for analysis; I'm taking 5%
Sblog <- sample(blog,size=floor(length(blog)*.05),replace=FALSE)
Stwitter <- sample(twitter,size=floor(length(twitter)*.05),replace=FALSE)
Snews <- sample(news,size=floor(length(news)*.05),replace=FALSE)

## Combine everything into one character vector and remove everything else
Sdata <- c(Sblog,Stwitter,Snews)
# Temp duplicate incase I want to reset Sdata after below processing without reading everything again
rm(Sblog,Stwitter,Snews,blog,twitter,news)

## Cleans Sdata using the CleanData function and returns the result to the object Scorpus
Scorpus <- CleanData(Sdata)
Scorpus_en <- CleanData_en(Sdata)

## Creates Uni/Bi/Tri/Quad grams and turns them into frequency tables for analysis
UNIdtm <- TermDocumentMatrix(Scorpus, control = list(tokenize = UnigramTokenizer))
UNIdtm_en <- TermDocumentMatrix(Scorpus_en, control = list(tokenize = UnigramTokenizer))
u <- removeSparseTerms(UNIdtm, 0.9997866)
u_en <- removeSparseTerms(UNIdtm_en, 0.9995)
uni.freq <- df_ngram(u)
uni.freq_en <- df_ngram(u_en)
uni.freq <- rbind(uni.freq,uni.freq_en)
rm(u,u_en,uni.freq_en)

BIdtm <- TermDocumentMatrix(Scorpus, control = list(tokenize = BigramTokenizer))
BIdtm_en <- TermDocumentMatrix(Scorpus_en, control = list(tokenize = BigramTokenizer))
b <- removeSparseTerms(BIdtm, 0.9997858) 
b_en <- removeSparseTerms(BIdtm_en, 0.9995)
bi.freq <- df_ngram(b)
bi.freq_en <- df_ngram(b_en)
bi.freq <- rbind(bi.freq, bi.freq_en)
write.csv(bi.freq,"bi_freq.csv",row.names=FALSE)
rm(b,b_en,bi.freq_en)

TRIdtm <- TermDocumentMatrix(Scorpus, control = list(tokenize = TrigramTokenizer))
TRIdtm_en <- TermDocumentMatrix(Scorpus_en, control = list(tokenize = TrigramTokenizer))
t <- removeSparseTerms(TRIdtm, 0.99995)
t_en <- removeSparseTerms(TRIdtm_en, 0.99992)
tri.freq <- df_ngram(t)
tri.freq_en <- df_ngram(t_en)
tri.freq <- rbind(tri.freq,tri.freq_en)
write.csv(tri.freq,"tri_freq.csv",row.names=FALSE)
rm(t,t_en,tri.freq_en)

QUADdtm <- TermDocumentMatrix(Scorpus, control = list(tokenize = QuadgramTokenizer))
QUADdtm_en <- TermDocumentMatrix(Scorpus_en, control = list(tokenize = QuadgramTokenizer))
q <- removeSparseTerms(QUADdtm, 0.9999)
q_en <- removeSparseTerms(QUADdtm_en, 0.9999)
quad.freq <- df_ngram(q)
quad.freq_en <- df_ngram(q_en)
quad.freq <- rbind(quad.freq,quad.freq_en)
write.csv(quad.freq,"quad_freq.csv",row.names=FALSE)
rm(q,q_en,quad.freq_en)

## Creates a histogram and summary table of the frequency counts
par(mfrow=c(2,2))
hist(uni.freq$count)
hist(bi.freq$count)
hist(tri.freq$count)
hist(quad.freq$count)

summary(uni.freq$count)
summary(bi.freq$count)
summary(tri.freq$count)
summary(quad.freq$count)