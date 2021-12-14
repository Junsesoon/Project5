## Project5 C3 ################################################################
## 기간: 21.11.26(금) 9:30까지
# C3조: 오준서 천성한 한호종 황윤수

  # 환경설정
rm(list=ls())
getwd()
setwd("c:/rwork/")
.libPaths()
install.packages("multilinguer")
library(multilinguer)
install_jdk()
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP',
                        upgrade = "never", INSTALL_opts=c("--no-multiarch"))
install.packages(c("stringr", "hash", "tau", "Sejong", "RSQLite", "devtools"),
                 type = "binary")
install.packages("rJava")
install.packages("wordcloud")
install.packages("tm")

  # 라이브러리 모음
install.packages("dplyr")
install.packages("arules")
install.packages("backports")
install.packages("igraph")
install_github("lchiffon/wordcloud2")
devtools::install_github("lchiffon/wordcloud2") # rtools 3.5 설치 후 사용
install.packages("httr")
install.packages("XML")
install.packages("pdftools")

library(KoNLP);useNIADic() # extractnoun(),useNIADic(),buildDictionary
library(dplyr) # 파이프 연산자
library(arules) # 연관분석 트랜잭션 패키지: apriori()
library(backports) # apriori() 사용을 위한 라이브러리
library(igraph) # 연관분석 시각화: graph.edgelist(),plot.igraph()
library(stringr) # 문자열 추출 관련함수
library(hash) # KoNLP 관련 함수
library(tau) #
library(Sejong) # 사전
library(RSQLite) # 
library(devtools) # KoNLP 관련 함수
library(wordcloud) # 단어 구름 시각화
library(tm) # 텍스트 마이닝
library(wordcloud2) # 워드클라우드2
library(httr) # 웹 크롤링
library(XML) # 웹 크롤링
library(pdftools)

  # 연설문 데이터전처리
Lincoln_pdf <- pdf_text("Lincoln_Gettysburg_Address2.pdf") #pdftools 패키지에 있는 pdf_text()함수
Lincoln_pdf1 <- Lincoln_pdf[1]
Lincoln_pdf2 <- strsplit(Lincoln_pdf1, '\n')
Lincoln_pdf3 <- Lincoln_pdf2[[1]][c(2:20)]
Lincoln_pdf4 <- list()
Lincoln_pdf4[1] <- paste(Lincoln_pdf3[1:2], collapse = "")
Lincoln_pdf42 <- paste(Lincoln_pdf3[3:4], collapse = " ")
Lincoln_pdf422 <- paste(Lincoln_pdf3[5:7], collapse = "")
Lincoln_pdf4[2] <- paste(Lincoln_pdf42, Lincoln_pdf422, collapse = " ")
Lincoln_pdf43 <- paste(Lincoln_pdf3[8:9], collapse = "")
Lincoln_pdf433 <- paste(Lincoln_pdf3[10:11], collapse = " ")
Lincoln_pdf433 <- paste(Lincoln_pdf433, Lincoln_pdf3[12], collapse = "")
Lincoln_pdf4[3] <- paste(Lincoln_pdf43, Lincoln_pdf433, collapse = " ")
Lincoln_pdf44 <- paste(Lincoln_pdf3[13:14], collapse = " ")
Lincoln_pdf44 <- paste(Lincoln_pdf44, Lincoln_pdf3[15], collapse = "")
Lincoln_pdf44 <- paste(Lincoln_pdf44, Lincoln_pdf3[16], collapse = "")
Lincoln_pdf444 <- paste(Lincoln_pdf3[17:19], collapse = "")
Lincoln_pdf4[4] <- paste(Lincoln_pdf44, Lincoln_pdf444, collapse = " ")
Lincoln_pdf <- paste(Lincoln_pdf4, collapse = " ")
Lincoln_pdf_sp <- strsplit(Lincoln_pdf, "니다.")
Lincoln_pdf_sp <- Lincoln_pdf_sp[[1]]
Lincoln_pdf
Lincoln_pdf_sp
str(Lincoln_pdf)


## 공통수행사항 ###############################################################

# 1) R code 에 #로 comment 를 추가하여 단계별로 실행한 모듈 설명을 추가 하시오.

# 2) 본인의 컴퓨터에 관련 패키지가 설치되어 있더라도 필요한 패키지의 설치 
  #  명령어도 포함하고 comment 처리 하시오.

# 3) 자율적으로 선택한 parameter 값에 최소 단어 출현 빈도수 지지도 신뢰도 
  #  등을 명시하고 선택한 이유에 대해 설명하시오.

# 4) 단어구름 형성 시 적용할 최소 단어 출현 빈도수는 적정한 수준(최소 2이상으로
  #  A4 한 페이지 안에 들어가고 단어들이 구분될 정도의 수준)으로 팀에서 결정


## 1번문제 ####################################################################
# 제공된 데이터를 이용하여 토픽 분석을 실시하여 단어구름으로 시각화 하고 
# 단어 출현 빈도수를 기반하여 어떤 단어들이 주요 단어인지 설명하시오

<<<<<<< HEAD
# 1)데이터 불러오기
lincoln_data <- file("c:/rwork/Lincoln_Gettysburg_Address2.txt",encoding = "UTF-8")
lincoln <- readLines(lincoln_data)
close(lincoln_data)

# 결과물 보정을 위한 말뭉치 생성 전 전처리
lincoln <- str_replace_all(lincoln,"[들이,하게]","")

exNouns <- function(x) {paste(extractNoun(as.character(x)), collapse = " ")}
lincoln_nouns <- sapply(lincoln, exNouns)
head(lincoln_nouns)

# 2) 말뭉치 생성
library(tm)
lincoln_corpus <- Corpus(VectorSource(lincoln_nouns))
inspect(lincoln_corpus[1:5])

# 3) 문장부호, 수치, 소문자, 불용어 제거
lincoln_pipe <- lincoln_corpus %>% tm_map(removePunctuation) %>% tm_map(stripWhitespace) %>% 
  tm_map(removeNumbers) %>% tm_map(tolower) %>% tm_map(removeWords,stopwords('english'))

# 4) 전처리 결과 확인
inspect(lincoln_pipe)

# 5) 2~8음절 대상 단어 선정
TDM5 <- TermDocumentMatrix(lincoln_pipe,control = list(wordLengths = c(4,16)))
TDM5

# 6) 자료구조 변경
lincoln_df <- TDM5 %>% as.matrix() %>% as.data.frame()
dim(lincoln_df)

# 7) 단어 출현 빈도수 구하기
lincoln_wordtable <- sort(rowSums(lincoln_df), decreasing = TRUE)
lincoln_wordtable[1:10]

# 8) 단어 이름과 빈도수로 데이터프레임 생성
myName <- names(lincoln_wordtable)
word.df <- data.frame(word=myName[1:32],freq=lincoln_wordtable[1:32])
str(word.df)

# 9) 단어 색상과 글꼴 지정
pal <- brewer.pal(12, "Paired")

# 10) 단어 구름 시각화
wordcloud(word.df$word,word.df$freq,scale=c(5,1),
          min.freq = 3, random.order = F,
          rot.per = .1, colors = pal, family = "malgun")
# wordcloud2(data=word.df)
=======
#1-2. 세종 사전에 단어 추가
user_dic <- data.frame(term = c("수도", "정신", "사명"), tag = 'ncn')
buildDictionary(ext_dic = "sejong", user_dic = user_dic)
#1-3. 단어 추출을 위한 사용자 함수 정의하기
# 단계 1: 사용자 정의 함수 작성
exNouns <- function(x) { paste(extractNoun(as.character(x)), collapse = " ") }
# 단계 2: exNouns() 함수를 이용하여 단어 추출
Lincoln_nouns <- sapply(Lincoln_pdf, exNouns)
Lincoln_nouns
#1-4. 불용어 제거하기
#1단계 데이터 전처리
#추출된 단어를 이용하여 말뭉치(Corpus) 생성
myCorpus <-Corpus(VectorSource(Lincoln_nouns))
#문장부호 제거
myCorpusPrepo <- tm_map(myCorpus, removePunctuation)
#수치 제거 = 숫자 제거
myCorpusPrepo <- tm_map(myCorpusPrepo, removeNumbers)
#소문자 변경
myCorpusPrepo <- tm_map(myCorpusPrepo, tolower)
#제거할 단어 지정
myStopwords = c(stopwords('english'), '한', '수', '들', '곳', '것', '하게')
#불용어 제거
myCorpusPrepo <- tm_map(myCorpusPrepo, removeWords, myStopwords)
#2단계 단어선별과 평서문 변환
myCorpusPrepo_term <- TermDocumentMatrix(myCorpusPrepo, control = list(wordLength = c(2,16)))
myTerm_df <- as.data.frame(as.matrix(myCorpusPrepo_term))
#3단계 단어 출현 빈도수 구하기
wordResult <- sort(rowSums(myTerm_df), decreasing = TRUE)
wordResult[1:10]
#1-5. 단어 구름에 디자인(빈도수, 색상, 위치, 회전 등) 적용하기
#1단계 단어 이름과 빈도수로 data.frame 생성
myName <- names(wordResult)
word.df <- data.frame(word = myName, freq = wordResult)
word.df
str(word.df )
#2단계 단어 색상과 글꼴 지정
pal <- brewer.pal(12, "Paired")
#3단계 단어 구름 시각화
wordcloud(word.df$word, word.df$freq, scale = c(5, 1), 
          min.freq = 1, random.order = F, 
          rot.per = .1, colors = pal)
wordcloud2(word.df)
>>>>>>> member1




## 2번문제 ####################################################################
# 제공된 데이터를 이용하여 연관어 분석을 실시하여 연관어를 시각화하고
# 시각화 결과에 대해 설명하시오

<<<<<<< HEAD
# 1) 데이터 불러오기
lincoln_data <- file("c:/rwork/Lincoln_Gettysburg_Address2.txt",encoding = "UTF-8")
lincoln <- readLines(lincoln_data)
close(lincoln_data)

# 결과물 보정을 위한 말뭉치 생성 전 전처리
lincoln <- str_replace_all(lincoln,"[들이,하게]","")
head(lincoln)

# 2) 줄 단위 단어 추출
lword <- Map(extractNoun, lincoln)
lword <- unique(lword)
head(lword)

# 3) 중복 및 1음절 단어 제거
filter1 <- function(x){
  nchar(x) <= 5 && nchar(x) >= 2 && is.hangul(x)
}
filter2 <- function(x){Filter(filter1,x)}

lword <- sapply(lword,filter2)
head(lword)

# 4) 트랜잭션 생성
wordtran <- as(lword,"transactions")


# 5) 연관규칙 발견
tranrules <- apriori(wordtran,parameter = list(supp = 0.12, conf = 0.05))
# 지지도가 0.12 이하인 모든 집합을 제외한다.

# 6) 연관규칙 생성 결과보기
detach(package:tm, unload = TRUE)
inspect(tranrules)

# 7) 시각화를 위한 자료구조 변경
eye <- labels(tranrules,ruleSep=" ")
eye

# 8) 문자열로 묶인 연관 단어를 행렬구조로 변경
eye <- sapply(eye,strsplit, " ",USE.NAMES = F)
eye

# 9) 행 단위로 묶어서 매트릭스로 변환
rulemat <- do.call("rbind",eye)
class(rulemat)

# 10) 연관규칙 보기
ruleg <- graph.edgelist(rulemat[c(13:58),],directed=F)
ruleg

# 11) 연관규칙 시각화
plot.igraph(ruleg, vertex.label = V(ruleg)$name,
            vertex.label.cex = 1.2, vertext.label.color = 'black',
            vertex.size = 20, vertext.color = 'green',
            vertex.frame.co.or = 'blue')
=======
#2-1. 연관어 분석을 위한 전처리하기
#1단계 줄 단위 단어 추출
lword <- Map(extractNoun, Lincoln_pdf_sp)
length(lword)
#2단계 중복단어 제거와 추출단어 확인
lword <- sapply(lword, unique)
length(lword)
lword
#3단계 단어 필터링 함수 정의
filter1 <- function(x){
  nchar(x) <= 5 && nchar(x) >= 2 && is.hangul(x)
}
filter2 <- function(x){ Filter(filter1, x) }
#4단계 줄 단위로 추출된 단어 전처리
lword <- sapply(lword, filter2)
lword
#2-2. 단어간 연관규칙 발견하기
#1단계 연관분석을 위한 패키지 설치와 로딩
#install.packages('arules')
library(arules)
#install.packages('backports')
library(backports)
#2단계 트랜잭션 생성
wordtran <- as(lword, "transactions")
wordtran
#3단계 연관규칙 발견
tranrules <- apriori(wordtran, parameter = list(supp = 0.15, conf = 0.05))
tranrules
#4단계 연관규칙 생성 결과보기
detach(package:tm, unload = TRUE)
inspect(tranrules[17:66])
#2-3. 연관어 시각화 하기
#1단계 연관단어 시각화를 위해 자료구조 변경
rules <- labels(tranrules, ruleSep = " ")
rules
#2단계 문자열로 묶인 연관단어를 행령구조로 변경
rules <- sapply(rules, strsplit, " ", USE.NAMES = F)
rules
#3단계 행 단위로 묶어서 matrix로 변환
rulemat <- do.call("rbind", rules)
class(rulemat)
#4단계 연관어 시각화를 위한 igraph 패키지 설치와 로딩
#install.packages("igraph")
library(igraph)
#5단계 edgelist 보기
ruleg <- graph.edgelist(rulemat[c(17:66),], directed = F)
ruleg
#6단계 edgelist 시각화
plot.igraph(ruleg, vertex.label = V(ruleg)$name, vertex.label.cex = 1.2, vertex.label.color = 'black',
            vertex.size = 20, vertex.color = 'pink', vertex.frame.color = 'red')
>>>>>>> member1




## 3번문제 ####################################################################
# 다음 포털사이트의 실시간 뉴스(https://news.daum.net/)를 수집하고 실시간 
# 토픽분석을 실행하여 단어구름으로 시각화하고 분석 시점에서 주요 이슈가 무엇인지 
# 설명하시오.
library(tm)
user_dic <- data.frame(term = c("펜데믹","코로나19","타다","코로나",
                                "이재명","윤창호법","한국은행"), tag = 'ncn')
buildDictionary(ext_dic = 'NIADic', user_dic =user_dic)

# 1) 웹 문서 요청
url <- "http://news.daum.net/"
web <-GET(url)
web

# 2) 파싱
html <- htmlTreeParse(web, useInternalNodes = T, trim = T, encoding = "UTF-8")
rootNode <- xmlRoot(html)

# 3) 태그 자료 수집
news <- xpathSApply(rootNode , "//a[@class = 'link_txt']",xmlValue)
news

# 4) 수집 자료 전처리
news_pre <- gsub("[\r\n\t]", ' ', news)
news_pre <- gsub('[[:punct:]]', ' ', news_pre)
news_pre <- gsub('[[:cntrl:]]', ' ', news_pre)
news_pre <- gsub('[a-z]+', ' ', news_pre)
news_pre <- gsub('[A-Z]+', ' ', news_pre)
news_pre <- gsub('\\s+', ' ', news_pre)
news_pre
news_data <- news_pre[1:58]
news_data

# 5) 단어 추출 함수
exNouns <- function(x){paste(extractNoun(x), collapse = " ")}
news_nouns <- sapply(news_data, exNouns)
news_nouns
str(news_nouns)

# 6) 말뭉치 생성
newsCorpus <- Corpus(VectorSource(news_nouns))
newsCorpus

# 집계 행렬 만들기
newsTDM <- TermDocumentMatrix(newsCorpus,control=list(wordLengths =c(4,16)))
newsTDM

tdm.df <- as.data.frame(as.matrix(newsTDM))
dim(tdm.df)

#7) 단어 출현 빈도수
wordResult <- sort(rowSums(tdm.df), decreasing = TRUE)
wordResult[1:70]

# 8) 단어와 단어 빈도수 구하기
myNames <- names(wordResult)
df <- data.frame(word = myNames, freq = wordResult)
data2 <- data.frame(df$word,df$freq)


# 9) 단어 구름 생성
pal <- brewer.pal(9, "BuGn")
wordcloud(df$word, df$freq, min.freq=2,
          random.order = F, scale = c(4, 0.7),
          rot.per = .1,color = pal,family = "malgun")
wordcloud2(data=data2,size=1.6, color='random-light',backgroundColor = "black")

#3-2. 웹문서 요청
url <- "http://news.daum.net"
web <- GET(url)
web
#3-3. HTML 파싱하기
html <- htmlTreeParse(web, useInternalNodes = T, trim = T, encoding = 'UTF-8')
rootNode <- xmlRoot(html)
#3-4. 태그 자료 수집하기
news <- xpathSApply(rootNode, "//a[@class = 'link_txt']", xmlValue)
#3-5. 수집한 자료 전처리
#1단계 자료 전처리 - 불용어 제거
news_pre <- gsub("[\r\n\t]", '', news)
news_pre <- gsub('[[:punct:]]', '', news_pre)
news_pre <- gsub('[[:cntrl:]]', '', news_pre)
news_pre <- gsub('\\d+', '', news_pre)
news_pre <- gsub('[a-z]+', '', news_pre)
news_pre <- gsub('[A-Z]+', '', news_pre)
news_pre <- gsub('\\s+', ' ', news_pre)
news_pre <- gsub('^ ', '', news_pre)
news_pre <- gsub('^위', '', news_pre)
news_pre <- gsub('^ ', '', news_pre)
#2단계 기사와 관련없는 내용은 제거
news_data <- news_pre[1:46]
news_data
#3-6. 세종 사전에 단어 추가
user_dic <- data.frame(term = c("코로나19"), tag = 'ncn')
buildDictionary(ext_dic = 'sejong', user_dic = user_dic)
#3-7. 단어 추출 사용자 함수 정의하기
#1단계 사용자 함수 정의
exNouns <- function(x){ paste(extractNoun(x), collapse = " ") }
#2단계 exNouns() 함수를 이용하여 단어 추출
news_nouns <- sapply(news_data, exNouns)
news_nouns
#3단계 추출결과 확인
str(news_nouns)
#3-8. 말뭉치 생성과 집계 행렬 만들기
library(tm)
#1단계 추출된 단어를 이용한 말뭉치(corpus) 생성
newsCorpus <- Corpus(VectorSource(news_nouns))
newsCorpus
inspect(newsCorpus[1:5])
#2단계 단어vs문서 집계 행렬 만들기
TMD <- TermDocumentMatrix(newsCorpus, control = list(wordLengths = c(4,16)))
TMD
#3단계 matrix자료구조를 data.frame 자료구조로 변경
tmd.df <- as.data.frame(as.matrix(TMD))
#3-9. 단어 출현 빈도수 구하기
wordResult <- sort(rowSums(tmd.df), decreasing = TRUE)
wordResult[1:10]
#3-10. 단어 구름 생성
#1단계 패키지 로딩과 단어 이름 추출
myNames <- names(wordResult)
myNames
#2단계 단어와 단어 빈도수 구하기
df <- data.frame(word = myNames, freq = wordResult)
head(df)
#3단계 단어구름 생성
pal <- brewer.pal(12, "Paired")
wordcloud(df$word, df$freq, min.freq = 2, random.order = F, scale = c(4, 0.7),
          rot.per = .1, colors = pal)
wordcloud2(df)



