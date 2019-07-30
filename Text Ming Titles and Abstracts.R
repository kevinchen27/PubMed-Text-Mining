library(rentrez)
library(stringr)
library(tidyverse)
library(tm)
library(topicmodels)
library(tidytext)

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

load("C:/Users/Kevin Chen/BD2K Research/recs2.RData")

article_titles <- c()
for(i in 1:100000){
  
  article_titles[i] <- recs2[[i]]$title 
  
}

#article_titles <-str_replace_all(article_titles, "", "")
df_titles <- data.frame(doc_id = as.character(1:100000), text = article_titles, stringsAsFactors = FALSE)


doc_source <- DataframeSource(df_titles)
doc_corpus <- Corpus(doc_source)
# summary(doc_corpus)

#check for NA's
#check for empty titles

doc_corpus <- tm_map(doc_corpus, tolower) 
doc_corpus <- tm_map(doc_corpus, removeNumbers)
doc_corpus <- tm_map(doc_corpus, removeWords, c("and","with","the","for"))  # removes very common English words
doc_corpus <- tm_map(doc_corpus, stripWhitespace)
inspect(doc_corpus[4])  # resulting text for one 'document'
inspect(doc_corpus[15])

titles_tm <- DocumentTermMatrix(doc_corpus)

titles_lda <- LDA(titles_tm, k = 2, control = list(seed = 1234))

titles_group <- tidy(titles_lda, matrix = "beta")
top_titles_terms <- titles_group %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

plot1 <- top_titles_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#plot findings from LDA
plot1 #there's report and report. Redundancy

#searching congenital heart diseases MeSH terms

congenital_search <- entrez_search(db = "pubmed", term = "(congenital heart defects[MeSH])", retmax = 10000, use_history = TRUE)
congen <- list()
congen2 <- list()
d = 1

#first 100,000 reports

#save summaries of 10000 pubmed papers
for(i in seq(1,10000,100)){
  congen <- entrez_summary(db="pubmed", web_history = congenital_search$web_history, retmax=100, retstart=i)
  congen2 <- append(congen2, congen)
}

congenital_titles <- c()
for(i in 1:10000){
  
  congenital_titles[i] <- congen2[[i]]$title 
  
}

congenital_titles <- str_replace_all(df_congenital_titles, ",|.", "")
df_congenital_titles <- data.frame(doc_id = as.character(1:10000), text = congenital_titles, stringsAsFactors = FALSE)

congenital_doc_source <- DataframeSource(df_congenital_titles)
congenital_doc_corpus <- Corpus(congenital_doc_source)
# summary(doc_corpus)

congenital_doc_corpus <- tm_map(congenital_doc_corpus, tolower) 
congenital_doc_corpus <- tm_map(congenital_doc_corpus, removePunctuation) 
congenital_doc_corpus <- tm_map(congenital_doc_corpus, removeNumbers)
congenital_doc_corpus <- tm_map(congenital_doc_corpus, removeWords, c("and","with","the","for"))  # removes very common English words
congenital_doc_corpus <- tm_map(congenital_doc_corpus, stripWhitespace)

congenital_titles_tm <- DocumentTermMatrix(congenital_doc_corpus)

word_counts <- as.matrix(congenital_titles_tm) # You can now use this as it shows the frequency of each word

# initial parameters
V = dim(word_counts)[2] # number of words in the vocabulary
N = dim(word_counts)[1] # number of documents in the corpus

# for class "Green Eggs"
tot_congenital <- sum(colSums(word_counts) + 0.1) # we add a small constant so none of the words have prob 0
word_prob <- (colSums(word_counts) + 0.1) / tot_congenital
word_prob_df <- data.frame(term = names(word_prob), word.prob = word_prob)
word_prob_df <- word_prob_df %>% arrange(desc(word.prob))
word_df<-word_prob_df[1:10,]
plot3 <- word_df %>% ggplot(aes(term, word.prob, fill = "aliceblue")) +
  geom_col(show.legend = FALSE) +  coord_flip()

#comparing plots
#par(mfrow = c(1,3))
#plot1
#plot3

#issues: a lot of unimportant words: eg. case. If remove case, DTM says rows are empty
#period after some words. But, punctuation (dashes/hyphens are still important)
#unclear what LDA has classified the topics into
#