#현재의 디렉토리주소 알아보기
getwd()
#디렌토리 주소 재설정
setwd("")
#디렌토리 주소 재설정
getwd()

#폴더 안의 파일들 확인하기
list.files()

install.packages("readxl")
library(readxl)

# 저장된 엑셀데이터 불러오기 
data <- read_excel("sample.xlsx")

install.packages("KoNLP")
library(KoNLP)

## 사전 선택 택1
useSejongDic()
useNIADic()


##tempdata에서 콘텐츠(본문) 부분만 따오기
contents <- data$'본문'
head(contents)

#불용어 처리는 str_replace_all이라는 구문을 이용하기도 합니다.
#이 함수를 이용하기위해 stringr라는 패키지를 인스톨합니다.

install.packages("stringr")
library(stringr)
#영문표현삭제
newcontents <- str_replace_all(contents, "[[:lower:]]", "")
#제어문자 삭제
newcontents <- str_replace_all(newcontents, "[[:cntrl:]]", "")
#특수기호 삭제
newcontents <- str_replace_all(newcontents, "[[:punct:]]", "")
#숫자 = 삭제
newcontents <- str_replace_all(newcontents, "[[:digit:]]", "")
#괄호삭제
newcontents <- str_replace_all(newcontents, "\\(", "")
newcontents <- str_replace_all(newcontents, "\\)", "")

#따옴표 삭제
newcontents <- str_replace_all(newcontents, "'", "")
newcontents <- str_replace_all(newcontents, "'", "")

#특수기호 삭제
newcontents <- str_replace_all(newcontents, "[^[:print:]]", "")


#명사 추출
noun <- extractNoun(newcontents)

install.packages("tm")
library(tm)
#코퍼스에서 명사를 한번 더 추출하기 위해 코퍼스 형태로 변환합니다.
myCorpus <- Corpus(VectorSource(noun))

#코퍼스(말뭉치)에서 줄 단위로 검색되어 추출된 명사는 라인으로 변환됩니다. Environment창에서 확인하세요
myCorpus <- sapply(myCorpus, extractNoun, USE.NAMES=FALSE)

#텍스트를 정제하기 위한 tm_map이라는 함수도 있습니다. tm-map은 코퍼스에서 사용됩니다. 이를위해 코퍼스로 다시 변환합니다.

myCorpus <- Corpus(VectorSource(myCorpus))
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)
myCorpus <- tm_map(myCorpus, tolower)
myCorpus <- tm_map(myCorpus, stripWhitespace)

WordList <- sapply(myCorpus, extractNoun, USE.NAMES=FALSE)
vectordata <- unlist(WordList)
#한글자는 제외합시다
vectordata <- Filter(function(x){nchar(x)>1}, vectordata)

preview<- sort(table(vectordata), decreasing=TRUE,100)
View(preview)

