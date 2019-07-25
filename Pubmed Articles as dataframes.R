library(rentrez)
pmc_search<- entrez_search(db = "pmc", term = "cardiovascular disease[All Fields] AND case reports[All Fields] AND 2009:2019[PDAT]",
                           retmax = 26997, use_history = TRUE)


for( i in seq(1,200,50)){
  recs <- entrez_fetch(db="pmc", web_history=pmc_search$web_history, rettype="xml", retmax=50, retstart= i)
  cat(recs, file="cardio.xml", append=TRUE)
  cat(i+49, "sequences downloaded\r")
}

library(XML)
library(plyr)
library(xml2)

xml_doc<- xmlParse(recs)
class(xml_doc) #check if XML

#Turning XML into a dataframe
xml1 <- xmlToList(xml_doc)
xml2 <- ldply(xml1, data.frame)