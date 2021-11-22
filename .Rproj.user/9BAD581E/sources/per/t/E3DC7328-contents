#현재의 디렉토리주소 알아보기
getwd()
#디렌토리 주소 재설정
setwd("")
#디렌토리 주소 재설정
getwd()

#폴더 안의 파일들 확인하기
list.files()

install.packages("KoNLP")
library(KoNLP)

useNIADic()

##데이터 가져오기.
data <- readLines("kim.txt", encoding = "UTF-8")
View(data)


install.packages("tm")
library(tm)

txt <- extractNoun(data)

##불용어처리
txt_data <- gsub("//d+","",txt)
txt_data <- gsub("[[:cntrl:]]","",txt_data)
txt_data <- gsub("[[:punct:]]","",txt_data)
#이녀석이 숫자를 삭제해줍니다. 
txt_data <- gsub("[[:digit:]]","",txt_data)
txt_data <- gsub("[[:lower:]]","",txt_data)
txt_data <- gsub("[A-z]", "", txt_data)
#한번더 정리
txt_data <- Filter(function(x){nchar(x)>1}, txt_data)

myCorpus <- Corpus(VectorSource(txt_data))
WordList <- sapply(myCorpus, extractNoun, USE.NAMES=FALSE)
vectordata <- unlist(WordList)
vectordata <- Filter(function(x){nchar(x)>1}, vectordata)

#추가로불용어처리할것이있는지preview로확인
preview<- sort(table(vectordata), decreasing=TRUE,100)
View(preview)

#빈도추출 
wordcount <- table(vectordata)
View(wordcount)
write.csv(wordcount,file="freq.csv")

install.packages("wordcloud")
library(wordcloud)

#wordcloud
wordcloud(names(wordcount),
          freq = wordcount,
          scale = c(3,0.8),
          rot.per = 0.25,
          min.freq = 1,
          random.order = F,
          random.color = T,
          colors = brewer.pal(9, "Set1"))

###########################################################################33
nouns <- unique(WordList)


lword <- sapply(nouns, unique) # 중복제거2(줄 단위 대상) 
lword[10:15] # 추출 단어 확인

# 1) 길이가 2~4 사이의 단어 필터링 함수 정의
filter1 <- function(x){
  nchar(x) <= 8 && nchar(x) >= 2 && is.hangul(x)
}

# 2) Filter(f,x) -> filter1() 함수를 적용하여 x 벡터 단위 필터링 
filter2 <- function(x){
  Filter(filter1, x)
}

# 3) 줄 단어 대상 필터링
lword <- sapply(lword, filter2)
lword # 추출 단어 확인(길이 1개 단어 삭제됨)

# arules 패키지 설치
install.packages("arules")
library(arules) 

wordtran <- as(lword, "transactions") # lword에 중복데이터가 있으면 error발생
summary(wordtran) 


# 동일한 단어끼리 교차테이블 작성 
wordtable <- crossTable(wordtran) # 교차표 작성
wordtable
# 6. 단어 간 연관규칙 산출

tranrules <- apriori(wordtran, parameter=list(supp=0.2, conf=0.3)) 
tranrules # 연관규칙 생성 결과(27개) 보기

associ <- labels(tranrules, ruleSep=" ")
associ <-sapply(associ,strsplit," ",USE.NAMES=F)
associ <- associ[-c(1:6)]
associ

matrixassoci <- do.call("rbind", associ)
matrixassoci

install.packages("igraph")
library(igraph)
corel <- graph.edgelist(matrixassoci, directed=F)
corel

plot.igraph(corel, vertex.label=V(corel)$name, vertex.label.cex=1.0,vertex.label.color='blue', vertex.size=20, vertex.color='green', vertex.frame.color='red',)


plot(corel, edge.color=c("dark red", "slategrey")[(E(corel)$type=="hyperlink")+1], vertex.color="gray40", layout=layout.circle)

plot(corel, vertex.shape="none", vertex.label=corel$name, vertex.label.color='red', vertex.label.font=2.5, vertex.label.cex=.6, edge.color="gray70",  edge.width=2)
