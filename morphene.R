sentence <- "어머니와 아버지가 장을 보러 가신다. 양파 한개와 생선을 사오시며 행복해 하신다"
install.packages("KoNLP")
install.packages("rJava")
library(rJava)
library(KoNLP)

useSejongDic()

SimplePos22(sentence)


noun <- extractNoun(sentence)
noun 

#각 품사의 분류는 https://github.com/haven-jeon/KoNLP/blob/master/etcs/KoNLP-API.md 에서 찾을 수 있다.