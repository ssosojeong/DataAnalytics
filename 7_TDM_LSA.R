##텍스트마이닝

#tm 패키지 불러오기
install.packages("tm")
library(tm)

#문서 만들기 1
text <- c("this is the first sentence",
          "this is a second sentence",
          "the thrid sentence is here")

#만든 문서로 말뭉치 만들기
corp <- Corpus(VectorSource(text))

#빈도수를 이용해 TDM 만들기
tdm <- TermDocumentMatrix(corp)

#tdm 내용 확인하기
inspect(tdm)

#문서 만들기 2
text <- c("this is the first sentence!!",
          "this is a second sentence :)",
          "the third sentence, is here",
          "forth of all sentences")

corp <- VCorpus(VectorSource(text))

tdm <- TermDocumentMatrix(corp)

inspect(tdm)

#tm_map을 이용한 tokenization 적용
#stripwhitespace: 공백 2칸 이상 -> 1칸으로
#removePunctuation: 문장 부호 지우기

corp <- tm_map(corp, stripWhitespace)

corp <- tm_map(corp, removePunctuation)

tdm <- TermDocumentMatrix((corp))

inspect(tdm)

stopwords("english")

#stopwords
install.packages("SnowballC")
library(SnowballC)

corp <- tm_map(corp, removeWords, stopwords("english"))

#stemming: 단어의 공통 접두사 및 접미사 자르기
corp <- tm_map(corp, stemDocument)

tdm <- TermDocumentMatrix(corp)

inspect(tdm)

#TF-IDF 적용
tfidf <- weightTfIdf(tdm)

inspect(tfidf)


#########################
#########################
#########################

##0. 데이터 준비

#라이브러리 불러오기
install.packages("topicmodels")
library(tm)
library(topicmodels)

#csv 불러오기
setwd("/Users/kimsojeong/DA")
amazon <- read.csv("4gen_echo_dot.csv", stringsAsFactors = FALSE)

#Title 확인
#크롤링한 시점에 따라 데이터 내용이 다를 수 있음
amazon$Title[1:3]

#multibyte 문제를 방지하기 위해 인코딩 변환 진행
#amazon의 Review를 UTF-8로 변환
#iconv(): 문자 벡터 인코딩 변환 함수
ecodot <- iconv(enc2utf8(amazon$Review), sub="bytes")

##1. 데이터 전처리

##영어 text로 1차 전처리
ecodot[1]

#iconv의 인코딩에서 CJK code 등록이 안되어 있으므로, gsub에서 처리
#CJK: Chinese, Japanese, Korean
#CJK unicode #: 4E00-9FFF(확장판) 와 3000-303F(기존)
#전처리할 때, 라틴어 설치를 안한 경우:
#유니코드 숫자와 한국어 조합은 대부분 라틴 문자인 경우가 많음
ecodot <- gsub("[\U4E00-\U9FFF\U3000-\U303F]","",ecodot)

##영어 text로 2차 전처리
#iconv(): 문자 벡터 인코딩 변환 함수
#gsub에서는 아시아 국가 문자, iconv에서는 다른 나라의 문자들을 처리
#latin1: 프랑스어, 독일어, 아이슬랜드, 스페인어 문자
#ASCII: 미국인대상의 표준 영어 문자
#sub: 대체 문자열 표기
ecodot <- iconv(ecodot, from="latin1", to="ASCII", sub="")

#깨진 문자가 어느정도 정리됨.
ecodot[1]

#구조: text를 말뭉치(corpus)로 변환하여 문서 정보 파악
#text --> corpus
#현재 데이터: ecodot을 corpus로 변환하여 저장
#vcorpus 이후 부터는 inspect(corpus[숫자])로 내용 확인 
corpus <- VCorpus(VectorSource(ecodot))

#각자 크롤링 시점이 다르기 때문에 글이 다를 수 있음
inspect(corpus[[28]])

##2. TDM

#현재의 언어 상태 확인
Sys.setlocale()

#multivalid 문제를 방지하기 위해 Korean -> 영어로 변경
Sys.setlocale(category = "LC_ALL", locale="us")
Sys.setenv(LANG = "en_US.UTF-8")

#TermDocumentMatrix를 tdm으로 생성
tdm <- TermDocumentMatrix(corpus, control=list(removePunctuation=TRUE,
                                               stopwords="SMART",
                                               tolower=TRUE,
                                               removeNumbers=TRUE,
                                               wordLength=c(5,5),
                                               stemming=FALSE,
                                               stripWhitespace=TRUE,
                                               weighting = weightTfIdf))

#만든 tdm의 dimension?
dim(tdm)

##3. LSA

#lsa 라이브러리 불러오기
install.packages("lsa")
library(lsa)

#tdm의 list 형태->matrix->textmatrix
txt_mat <- as.textmatrix(as.matrix(tdm))

#txt_mat를 10개의 차원으로 LSA 만들기
#sparsity 문제로 warning은 무시하고 진행
lsa_model <- lsa(txt_mat, dim=10)

#Terms x New LSA Space(U K)
dim(lsa_model$tk)

#Documents X New LSA Space (V K)
dim(lsa_model$dk)

#Singular Values (Sigma K)
length(lsa_model$sk)



