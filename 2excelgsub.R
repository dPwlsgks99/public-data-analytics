#현재의 디렉토리주소 알아보기
getwd()
#디렌토리 주소 재설정
setwd("C:/Users/dbcjj/Desktop/day1")
#디렌토리 주소 재설정
getwd()

#폴더 안의 파일들 확인하기
list.files()

#엑셀파일을 읽을 수 있는 패키지 다운 및 실행
install.packages("readxl")
library(readxl)

# 저장된 엑셀데이터 불러오기 
data <- read_excel("sample.xlsx")
head(data)

#파이프 함수를 이용하여 변인 선택을 위한 dplyr 패키지 실행
install.packages("dplyr")
library(dplyr)

# 마지막 2개 변수 제외 모두 
tempdata <- data %>% select(1:17)

# 변수명을 바꾸기 
tempdata <- tempdata %>% 
  rename(
    newid = `뉴스 식별자`,
    day=일자,
    comp=언론사,
    author=기고자,
    title=제목,
    cate_all_1=`통합 분류1`,
    cate_all_2=`통합 분류2`,
    cate_all_3=`통합 분류3`,
    cate_event_1=`사건/사고 분류1`,
    cate_event_2=`사건/사고 분류2`,
    cate_event_3=`사건/사고 분류3`, 
    agent=인물,
    location=위치, 
    organ=기관,
    keyword=키워드,
    feature=특성추출,
    contents=본문
  )

##tempdata에서 콘텐츠(본문) 부분만 따오기
contents <- tempdata$contents
head(contents)


#만약 데이터파일이 텍스트라면...다음 예제에서 해보죠

#한국어 텍스트분석을 위한 패키지
install.packages("KoNLP")
library(KoNLP)

## 데이터에서 명사만 추출하고자 함. 시스템 디폴트 사전을 사용해도 되나 좀더 정확한 분석을 위해 
##국립국어원에서 배포하는 사전과 
useSejongDic()
##정보화진흥원에서 배포하는 사전을 추가로 설치
useNIADic()

#때로는 이 사전에 추가되지 않은 신조어를 처리해야 할 때가 있습니다.
#이 때는 mergeUserDic이라는 명령어를 써서 추가합니다.
mergeUserDic(data.frame(c("개깜놀","핵존맛","JMT"), c("ncn")))

#그럼 일단 명사를 추출해 봅시다
txt <- extractNoun (contents)
head(txt)

#우리가 원하지 않는 요소들을 추출해 내야 합니다
#추출함수는 크게 세 가지가 있으며 첫번째로 gsub을 사용하겠습니다.
##gsub은 문자열의 특정 부분을 지정하여 변환하는 기능을 수행. 오피스의 CTRL + H와 같음

##불용어처리
#제어문자 삭제
txt_data <- gsub("[[:cntrl:]]","",txt)
#특수기호 삭제
txt_data <- gsub("[[:punct:]]","",txt_data)
#숫자 삭제
txt_data <- gsub("[[:digit:]]","",txt_data)
#소문자 삭제
txt_data <- gsub("[[:lower:]]","",txt_data)
#대문자 삭제
txt_data <- gsub("[[:upper:]]","",txt_data)
#특수문자 삭제
txt_data <- gsub("[^[:print:]]","",txt_data)

#기호 삭제
txt_data <- gsub("▲","",txt_data)
txt_data <- gsub("◎","",txt_data)

#나중에 실행하세요
head(txt_data)

#텍스트마이닝 패키지 설치
install.packages("tm")
library(tm)

docs <- Corpus(VectorSource(txt_data))
WordList <- sapply(docs, extractNoun, USE.NAMES=FALSE)
vectordata <- unlist(WordList)


##우리가 파일 형식들을 변환하는 이유
##문서는 말뭉치(Corpus)-> 문단-> 문장 -> 단어 -> 형태소의 위계적 구조를 가지고 있으며 텍스트마이닝 과정에서 함수의 특성에 따라 문서자료를 분해하고 조합하는 상황을 반복해야 함

##예를 들어봅시다
vector <- c(1:10, 'a', 'b')
vector
#벡터값을 가진 문서는 각각의 형태소가 분리되어 인식됨을 확인

list <- list(1:10, 'a', 'b')
list
#위에서 보듯 list형태의 문서는 입력값에 따라 1-10이 하나의 단위로 합쳐진 형태로 저장됨을 볼 수 있다.

#한글자는 제외합시다
vectordata <- Filter(function(x){nchar(x)>1}, vectordata)

#추가로불용어처리할것이있는지preview로확인
preview<- sort(table(vectordata), decreasing=TRUE,100)
View(preview) #지소미아의 미아만 나온 단점 발생!


#빈도추출 
wordcount <- table(vectordata)
write.csv(wordcount,file="freq.csv")

install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer)

#wordcloud
wordcloud(names(wordcount),
          freq = wordcount, #빈도량
          scale = c(3,0.5), #글자크기
          rot.per = 0.25, #회전단어 빈도
          min.freq = 10, #포함되는 최소빈도
          random.order = F, 
          random.color = F,
          colors = brewer.pal(8, "Set2"))

install.packages("wordcloud2")
library(wordcloud2)
install.packages("digest")
library(digest)
install.packages('devtools')
devtools::install_github("lchiffon/wordcloud2")

wordcloud2(wordcount)
wordcloud2(wordcount, shape = "star")
letterCloud(wordcount, "A")
figPath = system.file("heart1.png", package = "wordcloud2")
wordcloud2(wordcount, figPath = "heart1.png")



#색을 랜덤하게 지정하려면 radom.color를 T로, 아니면 F로 놓고 아랫줄의 색을 적용합니다. 적용 가능학 색조합은..
?brewer.pal
#위의 태그 혹은 구글에서 'brewer.pal'로 검색이 가능합니다.

#잠깐 바그래프 형식으로 그리는 테그도 소개합니다. 알아보기 쉽게 하기 위해 최다빈출어 상위 NN개로 추출하고 이를 토대로 그리게 되는데요, 상세한 부분은 다음에 소개하겠습니다.
barplot(wordcount, las = 2, names.arg = wordcount,
        col ="lightblue", main ="최다빈출어",
        ylab = "갯수")
