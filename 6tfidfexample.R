#현재의 디렉토리주소 알아보기
getwd()
#디렌토리 주소 재설정
setwd("C:/Users/dbcjj/Desktop/bigkinds")
#디렌토리 주소 재설정
getwd()

install.packages("readxl")
library(readxl)

# 저장된 엑셀데이터 불러오기 
data <- read_excel("tfidf.xlsx")

install.packages("KoNLP")
library(KoNLP)

contents <- data$contents
head(contents)

noun <- extractNoun(contents)

myCorpus <- VCorpus(VectorSource(noun))

TDM_Tf <- TermDocumentMatrix(myCorpus, control=list(removePuctuation = TRUE, removeNumbers = TRUE, stopwords = TRUE, weighting = weightTf))
TDM_TfIdf <- TermDocumentMatrix(myCorpus, control=list(removePuctuation = TRUE, removeNumbers = TRUE, stopwords = TRUE, weighting = weightTfIdf))
#TDM_Tf와 TDM_TfIdf의 차이점은 weighting에 초점!
as.matrix(TDM_Tf)
as.matrix(TDM_TfIdf)

value.tf <- as.vector(as.matrix(TDM_Tf[,]))
value.tfidf <- as.vector(as.matrix(TDM_TfIdf[,]))

#단어와 문서를 추출
doc <- rep(colnames(TDM_Tf[,]), each=dim(TDM_Tf[,])[1])
word <- rep(rownames(TDM_Tf[,]), dim(TDM_Tf[,])[2])

#모두모아 데이터프레임
valuedata <- data.frame(doc, word, value.tf, value.tfidf)
colnames(valuedata) <- c('doc', 'word', 'tf', 'tfidf')
valuedata


################ 혹은 이렇게 한 번에 처리가 가느
review_dtm_tfidf <- DocumentTermMatrix(myCorpus, control = list(weighting = weightTfIdf))
review_dtm_tfidf = removeSparseTerms(review_dtm_tfidf, 0.95)
review_dtm_tfidf
inspect(review_dtm_tfidf)



#TF와 TF-IDF 상관관계
cor.test(valuedata$tf,valuedata$tfidf,method="kendall")
cor.test(valuedata$tf, valuedata$tfidf, method=c("pearson", "kendall", "spearman"))

valuedata2 <- subset(valuedata, tfidf>0.3)
valuedata2
table(valuedata2$word)

