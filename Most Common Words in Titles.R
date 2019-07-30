library(rentrez)
library(stringr)
library(dplyr)

#search database matching search parameters
pubmed_search <- entrez_search(db = "pubmed", term = "Case Reports[Filter] AND cardiovascular disease AND English[lang] AND 2009:2019[PDat])", retmax = 792711, use_history = TRUE)
recs2 <- list()
recs <- list()

#save summaries of 10000 pubmed papers
for(i in seq(1,10000,100)){
  recs <- entrez_summary(db="pubmed", web_history = pubmed_search$web_history, retmax=100, retstart=i)
  recs2 <- append(recs2, recs)
}

#extract article titles
article_titles <- c()

for(i in 1:10000){
  article_titles[i] <- recs2[[i]]$title 
}

#find occurences of each word in titles
article_titles <- article_titles %>% str_to_lower()

word.count <- unlist(str_split(article_titles, "\\s")) 
word.count <- stylo::delete.stop.words(word.count, stop.words = c("a","A","The","the","with","of","in","and", "an", "by","to","on","as","from","for"))

top_50_words <- sort(table(word.count), decreasing = TRUE)[1:50] %>% data.frame()
library(ggplot2)
top_50_words %>% ggplot(aes(top_50_words$word.count, top_50_words$Freq)) +
  geom_bar(stat="identity", fill="blue", colour="black") +
  theme(axis.text.x=element_text(angle=45, hjust=1))