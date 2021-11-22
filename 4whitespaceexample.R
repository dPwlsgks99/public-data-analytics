txt <- c("사과 바나나", "딸기 수박", "참외  자두")
txt

#txt
eachtxt <- strsplit(txt, " ")
eachtxt

install.packages("tm")
library(tm)
mycorpus <- Corpus(VectorSource(txt))
bettertxt <- tm_map(mycorpus, stripWhitespace)
vectordata <- unlist(bettertxt)

nountxt <- strsplit(vectordata, " ")
nountxt
