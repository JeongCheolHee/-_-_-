###################
# 패키지 다운로드 #
###################

install.packages("rJava")
install.packages(c("KoNLP", "wordcloud"))
# tm 패키지 구 버전 다운로드/설치 - version 3.3.2
install.packages("http://cran.r-project.org/bin/windows/contrib/3.0/tm_0.5-10.zip",repos=NULL)
install.packages('slam')
install.packages('Sejong')
install.packages('hash')
install.packages('tau')
install.packages('devtools')
install.packages('tm')
install.packages('wordcloud')
install.packages('RSQLite')
install.packages('rlang')

#setwd(readClipboard())
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_301")
library(rJava)
library(KoNLP)

# 패키지 로딩
library(rJava)
library(slam) 
library(RSQLite)
library(httr)
library(XML)
library(KoNLP) # 세종사전 
library(tm) # 영문 텍스트 마이닝 
library(wordcloud) # RColorBrewer()함수 제공
library(RSQLite)
library(rlang)
library(devtools)



###########################
# 실시간 뉴스 수집과 분석 #
###########################
library(stringr)
url <-"https://search.naver.com/search.naver?sm=tab_hty.top&where=news&query=2018+%EC%9B%94%EB%93%9C%EC%BB%B5+%EC%9A%B0%EC%8A%B9%ED%9B%84%EB%B3%B4&oquery=2018+%EC%9B%94%EB%93%9C%EC%BB%B5+%EC%9A%B0%EC%8A%B9&tqi=hGycpsprvToss4%2BX3chssssst7l-022131&nso=so%3Ar%2Cp%3Afrom20180601to20180714&de=2018.07.14&ds=2018.06.01&mynews=0&office_section_code=0&office_type=0&pd=3&photo=0&sort=0"
web <- GET(url)
web

html <- htmlTreeParse(web,useInternalNodes=T,trim=T,encoding="utf-8")
html
rootNode <- xmlRoot(html)
rootNode

news1 <- xpathSApply(rootNode,"//*[@id='main_pack']/section/div/div[2]/ul",xmlValue)
news1

news_pre1 <- gsub('[\r\n\t]', '', news1)
news_pre1 <- gsub('[a-z]','',news_pre1)
news_pre1 <- gsub('[A-Z]','',news_pre1)
news_pre1 <- gsub('\\s+',' ',news_pre1)
news_pre1 <- gsub('[[:cntrl:]]','',news_pre1)
news_pre1 <- gsub('[[:punct:]]','',news_pre1)
news_pre1 <- gsub('\\d+',' ',news_pre1)
news_pre1 <- gsub('네이버뉴스','',news_pre1)
news_pre1


url <- "https://search.naver.com/search.naver?where=news&sm=tab_pge&query=2018%20%EC%9B%94%EB%93%9C%EC%BB%B5%20%EC%9A%B0%EC%8A%B9%ED%9B%84%EB%B3%B4&sort=0&photo=0&field=0&pd=3&ds=2018.06.01&de=2018.07.14&cluster_rank=14&mynews=0&office_type=0&office_section_code=0&news_office_checked=&nso=so:r,p:from20180601to20180714,a:all&start=11"
web <- GET(url)
web

html <- htmlTreeParse(web,useInternalNodes=T,trim=T,encoding="utf-8")
html
rootNode <- xmlRoot(html)
rootNode

news2 <- xpathSApply(rootNode,"//*[@id='main_pack']/section/div/div[2]/ul",xmlValue)
news2

news_pre2 <- gsub('[\r\n\t]', '', news2) 
news_pre2 <- gsub('[a-z]','',news_pre2)
news_pre2 <- gsub('[A-Z]','',news_pre2)
news_pre2 <- gsub('\\s+',' ',news_pre2)
news_pre2 <- gsub('[[:cntrl:]]','',news_pre2)
news_pre2 <- gsub('[[:punct:]]','',news_pre2)
news_pre2 <- gsub('\\d+',' ',news_pre2)
news_pre2 <- gsub('네이버뉴스','',news_pre2)

news_pre2


url <- "https://search.naver.com/search.naver?where=news&sm=tab_pge&query=2018%20%EC%9B%94%EB%93%9C%EC%BB%B5%20%EC%9A%B0%EC%8A%B9%ED%9B%84%EB%B3%B4&sort=0&photo=0&field=0&pd=3&ds=2018.06.01&de=2018.07.14&cluster_rank=24&mynews=0&office_type=0&office_section_code=0&news_office_checked=&nso=so:r,p:from20180601to20180714,a:all&start=21"
web <- GET(url)
web

html <- htmlTreeParse(web,useInternalNodes=T,trim=T,encoding="utf-8")
html
rootNode <- xmlRoot(html)
rootNode

news3 <- xpathSApply(rootNode,"//*[@id='main_pack']/section/div/div[2]/ul",xmlValue)
news3

news_pre3 <- gsub('[\r\n\t]', '', news3) 
news_pre3 <- gsub('[a-z]','',news_pre3)
news_pre3 <- gsub('[A-Z]','',news_pre3)
news_pre3 <- gsub('\\s+',' ',news_pre3)
news_pre3 <- gsub('[[:cntrl:]]','',news_pre3)
news_pre3 <- gsub('[[:punct:]]','',news_pre3)
news_pre3 <- gsub('\\d+',' ',news_pre3)
news_pre3 <- gsub('네이버뉴스','',news_pre3)

news_pre3


url <-"https://search.naver.com/search.naver?where=news&sm=tab_pge&query=2018%20%EC%9B%94%EB%93%9C%EC%BB%B5%20%EC%9A%B0%EC%8A%B9%ED%9B%84%EB%B3%B4&sort=0&photo=0&field=0&pd=3&ds=2018.06.01&de=2018.07.14&cluster_rank=37&mynews=0&office_type=0&office_section_code=0&news_office_checked=&nso=so:r,p:from20180601to20180714,a:all&start=31"
web <- GET(url)
web

html <- htmlTreeParse(web,useInternalNodes=T,trim=T,encoding="utf-8")
html
rootNode <- xmlRoot(html)
rootNode

news4 <- xpathSApply(rootNode,"//*[@id='main_pack']/section/div/div[2]/ul",xmlValue)
news4

news_pre4 <- gsub('[\r\n\t]', '', news4) 
news_pre4 <- gsub('[a-z]','',news_pre4)
news_pre4 <- gsub('[A-Z]','',news_pre4)
news_pre4 <- gsub('\\s+',' ',news_pre4)
news_pre4 <- gsub('[[:cntrl:]]','',news_pre4)
news_pre4 <- gsub('[[:punct:]]','',news_pre4)
news_pre4 <- gsub('\\d+',' ',news_pre4)
news_pre4 <- gsub('네이버뉴스','',news_pre4)

news_pre4


url <- "https://search.naver.com/search.naver?where=news&sm=tab_pge&query=2018%20%EC%9B%94%EB%93%9C%EC%BB%B5%20%EC%9A%B0%EC%8A%B9%ED%9B%84%EB%B3%B4&sort=0&photo=0&field=0&pd=3&ds=2018.06.01&de=2018.07.14&cluster_rank=50&mynews=0&office_type=0&office_section_code=0&news_office_checked=&nso=so:r,p:from20180601to20180714,a:all&start=41"
web <- GET(url)
web

html <- htmlTreeParse(web,useInternalNodes=T,trim=T,encoding="utf-8")
html
rootNode <- xmlRoot(html)
rootNode

news5 <- xpathSApply(rootNode,"//*[@id='main_pack']/section/div/div[2]/ul",xmlValue)
news5

news_pre5 <- gsub('[\r\n\t]', '', news5) 
news_pre5 <- gsub('[a-z]','',news_pre5)
news_pre5 <- gsub('[A-Z]','',news_pre5)
news_pre5 <- gsub('\\s+',' ',news_pre5)
news_pre5 <- gsub('[[:cntrl:]]','',news_pre5)
news_pre5 <- gsub('[[:punct:]]','',news_pre5)
news_pre5 <- gsub('\\d+',' ',news_pre5)
news_pre5 <- gsub('네이버뉴스','',news_pre5)
news_pre5

news_pre <- str_c(news_pre1,news_pre2)
news_pre <- str_c(news_pre,news_pre3)
news_pre <- str_c(news_pre,news_pre4)
news_pre <- str_c(news_pre,news_pre5)
news_pre


write.table(news_pre,"news1.txt")

#############
# 토픽 분석 #
#############

library(KoNLP)
news_noun <- extractNoun(news_pre)
news_noun


# 세종 사전에 없는 단어 추가
#install.packages('curl')
library(curl)
useSejongDic() # 세종 사전 불러오기
mergeUserDic(data.frame(c("프랑스","크로아티아","브라질"), c("ncn"))) 
# ncn -명사지시코드

exNouns <- function(x) { paste(extractNoun(as.character(x)), collapse=" ")}
news_noun <- sapply(news_pre, exNouns) 


library(tm)
newsCorpus <- Corpus(VectorSource(news_noun))
TDM <- TermDocumentMatrix(newsCorpus, control=list(wordLengths=c(4,16)))
TDM
tdm.df <- as.data.frame(as.matrix(TDM))
tdm.df

wordResult <- sort(rowSums(tdm.df),decreasing=T)

wordResult <- wordResult[-c(1:2,4:7)]
wordResult

library(wordcloud)
myNames <- names(wordResult)
df <- data.frame(word=myNames, freq=wordResult)
df
pal <- brewer.pal(12,"Paired")
wordcloud(df$word,df$freq,min.freq=2,random.order=F,scale=c(4,0.7),
          rot.per=0.1,colors=pal,family="malgun")

library(wordcloud2)
wordcloud2(data=df, size =.5,color='random-light', backgroundColor="black")
?wordcloud2


###############
# 연관어 분석 #
###############
###############

news_A <- read.table("news1.txt",encoding="UTF-8")
news_A <- str_split(news_A,"저장하기")
news_A
write.table(news_A,"news3.txt")

news_B <- file("news3.txt",encoding="UTF-8")
news_C <- readLines(news_B)
news_C <- str_replace_all(news_C,"바로가기","")
news_C <- str_replace_all(news_C,"저장","")
news_C <- str_replace_all(news_C,"저장하기","")
news_C <- str_replace_all(news_C,"언론사","")
news_C <- str_replace_all(news_C,"기자","")
news_C <- str_replace_all(news_C,"뉴스","")
news_C <- str_replace_all(news_C,"연합뉴스","")
news_C <- str_replace_all(news_C,"한국경제","")
news_C
lword <- Map(extractNoun, news_C)  
length(lword)

lword <- unique(lword)
length(lword)

lword <- sapply(lword, unique)
length(lword)


filter1 <- function(x){
  nchar(x) <= 4 && nchar(x) >= 2 && is.hangul(x)
}

filter2 <- function(x){
  Filter(filter1, x)
}

lword <- sapply(lword, filter2)
lword

install.packages("arules")
library(arules) 

wordtran <- as(lword, "transactions")
wordtran 

tranrules <- apriori(wordtran, parameter=list(supp=0.3, conf=0.05)) 

inspect(tranrules)

rules <- labels(tranrules, ruleSep=" ")  
rules 
class(rules)

rules <- sapply(rules, strsplit, " ",  USE.NAMES=F) 
rules
class(rules) 

rulemat <- do.call("rbind", rules)
rulemat
class(rulemat)

install.packages("igraph")
library(igraph)

ruleg <- graph.edgelist(rulemat[c(10:50),], directed=F)
ruleg

plot.igraph(ruleg, vertex.label=V(ruleg)$name,
            vertex.label.cex=1.2, vertex.label.color='black', 
            vertex.size=20, vertex.color='green', vertex.frame.color='blue')

