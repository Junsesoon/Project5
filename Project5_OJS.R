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




## 2번문제 ####################################################################
# 제공된 데이터를 이용하여 연관어 분석을 실시하여 연관어를 시각화하고
# 시각화 결과에 대해 설명하시오

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



