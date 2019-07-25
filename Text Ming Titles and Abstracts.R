library(rentrez)
library(stringr)
library(dplyr)
library(tm)
library(topicmodels)

#search database matching search parameters
pubmed_search <- entrez_search(db = "pubmed", term = "Case Reports[Filter] AND cardiovascular disease AND English[lang] AND 2009:2019[PDat])", retmax = 792711, use_history = TRUE)
recs2 <- list()
recs <- list()
c = 1

#first 100,000 reports

#save summaries of 10000 pubmed papers
for(i in seq(1,100000,100)){
  recs <- entrez_summary(db="pubmed", web_history = pubmed_search$web_history, retmax=100, retstart=i)
  recs2 <- append(recs2, recs)
}

save(recs2, file = "recs2.RData")
recs2[[1]]$title

article_titles <- c()
for(i in 1:100000){
  
  article_titles[i] <- recs2[[i]]$title 
  
}

df_titles <- data.frame(doc_id = as.character(1:100000), text = article_titles, stringsAsFactors = FALSE)

doc_source <- DataframeSource(df_titles)
doc_corpus <- Corpus(doc_source)
# summary(doc_corpus)

doc_corpus <- tm_map(doc_corpus, tolower) 
doc_corpus <- tm_map(doc_corpus, removePunctuation) 
doc_corpus <- tm_map(doc_corpus, removeNumbers)
doc_corpus <- tm_map(doc_corpus, removeWords, stopwords("english"))  # removes very common English words
doc_corpus <- tm_map(doc_corpus, stripWhitespace)
inspect(doc_corpus[4])  # resulting text for one 'document'
inspect(doc_corpus[15])

titles_tm <- DocumentTermMatrix(doc_corpus)


classes <- LDA(titles_tm, k = 2, control = list(seed = 1234))