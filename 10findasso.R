#현재의 디렉토리주소 알아보기
getwd()
#디렌토리 주소 재설정
setwd("C:/Users/dbcjj/Desktop/bigkinds")
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
## 텍스트 사전처리 ver1
txt <- extractNoun(data)
txt


install.packages("tm")
library(tm)

docs <- Corpus(VectorSource(txt))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, extractNoun)
mykoreanstop <- c("아휴, 아이구, 아이쿠, 아이고, 어, 나, 우리, 저희, 따라, 의해, 을, 를, 에, 의, 가, 으로, 로, 에게, 뿐이다, 의거하여, 근거하여, 입각하여, 기준으로, 예하면, 예를 들면, 예를 들자면, 저, 소인, 소생, 저희, 지말고, 하지마, 하지마라, 다른, 물론, 또한, 그리고, 비길수 없다, 해서는 안된다, 뿐만 아니라, 만이 아니다, 만은 아니다, 막론하고, 관계없이, 그치지 않다, 그러나, 그런데, 하지만, 든간에, 논하지 않다, 따지지 않다, 설사, 비록, 더라도, 아니면, 만 못하다, 하는 편이 낫다, 불문하고, 향하여, 향해서, 향하다, 쪽으로, 틈타, 이용하여, 타다, 오르다, 제외하고, 이 외에, 이 밖에, 하여야, 비로소, 한다면 몰라도, 외에도, 이곳, 여기, 부터, 기점으로, 따라서, 할 생각이다, 하려고하다, 이리하여, 그리하여, 그렇게 함으로써, 하지만, 일때, 할때, 앞에서, 중에서, 보는데서, 으로써, 로써, 까지, 해야한다, 일것이다, 반드시, 할줄알다, 할수있다, 할수있어, 임에 틀림없다, 한다면, 등, 등등, 제, 겨우, 단지, 다만, 할뿐, 딩동, 댕그, 대해서, 대하여, 대하면, 훨씬, 얼마나, 얼마만큼, 얼마큼, 남짓, 여, 얼마간, 약간, 다소, 좀, 조금, 다수, 몇, 얼마, 지만, 하물며, 또한, 그러나, 그렇지만, 하지만, 이외에도, 대해 말하자면, 뿐이다, 다음에, 반대로, 반대로 말하자면, 이와 반대로, 바꾸어서 말하면, 바꾸어서 한다면, 만약, 그렇지않으면, 까악, 툭, 일만에")
docs <- tm_map(docs, removeWords, mykoreanstop)





dtm <- DocumentTermMatrix(docs, control = list(removeNumbers = T,
                                               wordLength=c(2,Inf),
                                               weighting = function(x) weightTfIdf(x, normalize = T)))
dtma <- removeSparseTerms(dtm, as.numeric(0.98))

findAssocs(dtma, '사랑', 0.01)

var1 <- as.vector(dtma[,"사랑"])
var2 <- as.vector(dtma[,"세월"])
cor.test(var1, var2)
