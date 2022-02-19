# 크롤링 할수 있는 패키지를 다운로드
install.packages("httr")
library(httr)
install.packages("XML")
library(XML)

url = "https://news.daum.net/"
web =  GET(url)

html=htmlTreeParse(web, useInternalNodes = T, trim = T,encoding = "utf-8")
rootNode = xmlRoot(html)

news <- xpathSApply(rootNode, "//a[@class = 'link_txt']", xmlValue)
news

# 전처리
news_pre =  gsub("[\r\n\t]",' ',news) # 이스케이스 제거
news_pre =  gsub("[[:punct:]]",' ',news_pre) #문장 부호제거
news_pre =  gsub("[[:cntrl:]]",' ',news_pre)
news_pre <- gsub('\\s+', ' ', news_pre) # 2개 이상의 공백을 제거

source("D:/주말반 빅데이터분석_이규영 (2)/source/20220219_워드클라우드3.R")
makeWc(news_pre,isFile = FALSE)
