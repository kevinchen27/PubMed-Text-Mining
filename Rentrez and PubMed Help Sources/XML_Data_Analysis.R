install.packages("XML")
install.packages("plyr")
install.packages("ggplot2")
install.packages("gridExtra")

require("XML")
require("plyr")
require("ggplot2")
require("gridExtra")

setwd("C:/Users/Tobi/Documents/R/InformIT") #you will need to change the filepath on your machine
xmlfile=xmlParse("pubmed_sample.xml")
class(xmlfile) #"XMLInternalDocument" "XMLAbstractDocument"

xmltop = xmlRoot(xmlfile) #gives content of root
class(xmltop)#"XMLInternalElementNode" "XMLInternalNode" "XMLAbstractNode"
xmlName(xmltop) #give name of node, PubmedArticleSet
xmlSize(xmltop) #how many children in node, 19
xmlName(xmltop[[1]]) #name of root's children

# have a look at the content of the first child entry
xmltop[[1]]
# have a look at the content of the 2nd child entry
xmltop[[2]]

#Root Node's children
xmlSize(xmltop[[1]]) #number of nodes in each child
xmlSApply(xmltop[[1]], xmlName) #name(s)
xmlSApply(xmltop[[1]], xmlAttrs) #attribute(s)
xmlSApply(xmltop[[1]], xmlSize) #size

#take a look at the MedlineCitation subnode of 1st child
xmltop[[1]][[1]]
#take a look at the PubmedData subnode of 1st child
xmltop[[1]][[2]]

#subnodes of 2nd child
xmltop[[2]][[1]]
xmltop[[2]][[2]]


#we can keep going till we reach the end of a branch
xmltop[[1]][[1]][[5]][[2]] #title of first article
xmltop[['PubmedArticle']][['MedlineCitation']][['Article']][['ArticleTitle']] #same command, but more readable

#Turning XML into a dataframe
Madhu2012=ldply(xmlToList("pubmed_sample.xml"), data.frame) #completes with errors: "row names were found from a short variable and have been discarded"
View(Madhu2012) #for easy checking that the data is properly formatted
Madhu2012.Clean=Madhu2012[Madhu2012[25]=='Y',] #gets rid of duplicated rows

#looking at which authors played most active role
FirstAuthor=Madhu2012.Clean$MedlineCitation.Article.AuthorList.Author.LastName
SecondAuthor=Madhu2012.Clean$MedlineCitation.Article.AuthorList.Author.LastName.1
ThirdAuthor=Madhu2012.Clean$MedlineCitation.Article.AuthorList.Author.LastName.2
#removing NAs
Madhu2012.Na.Rm4=Madhu2012.Clean[!is.na(Madhu2012.Clean$MedlineCitation.Article.AuthorList.Author.LastName.3),]
FourthAuthor=Madhu2012.Na.Rm4$MedlineCitation.Article.AuthorList.Author.LastName.3
Madhu2012.Na.Rm5=Madhu2012.Clean[!is.na(Madhu2012.Clean$MedlineCitation.Article.AuthorList.Author.LastName.4),]
FifthAuthor=Madhu2012.Na.Rm5$MedlineCitation.Article.AuthorList.Author.LastName.4
Madhu2012.Na.Rm6=Madhu2012.Clean[!is.na(Madhu2012.Clean$MedlineCitation.Article.AuthorList.Author.LastName.5),]
SixthAuthor=Madhu2012.Na.Rm6$MedlineCitation.Article.AuthorList.Author.LastName.5
Madhu2012.Na.Rm7=Madhu2012.Clean[!is.na(Madhu2012.Clean$MedlineCitation.Article.AuthorList.Author.LastName.6),]
SeventhAuthor=Madhu2012.Na.Rm7$MedlineCitation.Article.AuthorList.Author.LastName.6
Madhu2012.Na.Rm8=Madhu2012.Clean[!is.na(Madhu2012.Clean$MedlineCitation.Article.AuthorList.Author.LastName.7),]
EighthAuthor=Madhu2012.Na.Rm8$MedlineCitation.Article.AuthorList.Author.LastName.7
Madhu2012.Na.Rm9=Madhu2012.Clean[!is.na(Madhu2012.Clean$MedlineCitation.Article.AuthorList.Author.LastName.8),]
NinthAuthor=Madhu2012.Na.Rm9$MedlineCitation.Article.AuthorList.Author.LastName.8
Madhu2012.Na.Rm10=Madhu2012.Clean[!is.na(Madhu2012.Clean$MedlineCitation.Article.AuthorList.Author.LastName.9),]
TenthAuthor=Madhu2012.Na.Rm10$MedlineCitation.Article.AuthorList.Author.LastName.9
Madhu2012.Na.Rm11=Madhu2012.Clean[!is.na(Madhu2012.Clean$MedlineCitation.Article.AuthorList.Author.LastName.10),]
EleventhAuthor=Madhu2012.Na.Rm11$MedlineCitation.Article.AuthorList.Author.LastName.10

#write all the graphs to pdf on 3 canvases
a=ggplot(Madhu2012.Clean, aes(x=FirstAuthor)) + geom_histogram(binwidth=.5, colour="pink", fill="purple")+coord_flip()
b=ggplot(Madhu2012.Clean, aes(x=SecondAuthor)) + geom_histogram(binwidth=.5, colour="pink", fill="purple")+coord_flip()
c=ggplot(Madhu2012.Clean, aes(x=ThirdAuthor)) + geom_histogram(binwidth=.5, colour="pink", fill="purple")+coord_flip()
d=ggplot(Madhu2012.Na.Rm4, aes(x=FourthAuthor)) + geom_histogram(binwidth=.5, colour="pink", fill="purple")+coord_flip()
e=ggplot(Madhu2012.Na.Rm5, aes(x=FifthAuthor)) + geom_histogram(binwidth=.5, colour="pink", fill="purple")+coord_flip()
f=ggplot(Madhu2012.Na.Rm6, aes(x=SixthAuthor)) + geom_histogram(binwidth=.5, colour="pink", fill="purple")+coord_flip()
g=ggplot(Madhu2012.Na.Rm7, aes(x=SeventhAuthor)) + geom_histogram(binwidth=.5, colour="pink", fill="purple")+coord_flip()
h=ggplot(Madhu2012.Na.Rm8, aes(x=EighthAuthor)) + geom_histogram(binwidth=.5, colour="pink", fill="purple")+coord_flip()
i=ggplot(Madhu2012.Na.Rm9, aes(x=NinthAuthor)) + geom_histogram(binwidth=.5, colour="pink", fill="purple")+coord_flip()
j=ggplot(Madhu2012.Na.Rm10, aes(x=TenthAuthor)) + geom_histogram(binwidth=.5, colour="pink", fill="purple")+coord_flip()
k=ggplot(Madhu2012.Na.Rm11, aes(x=EleventhAuthor)) + geom_histogram(binwidth=.5, colour="pink", fill="purple")+coord_flip()

pdf("AuthorHistogram.pdf")
grid.arrange(a,b,c,d)
grid.arrange(e,f,g,h)
grid.arrange(i,j,k)
dev.off()

#exporting data
write.table(Madhu2012.Clean, "Madhu2012.txt", sep="\t", row.names=FALSE)
write.csv(Madhu2012.Clean, "Madhu2012.csv", row.names=FALSE)
