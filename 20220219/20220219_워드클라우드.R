install.packages("rJava")
Sys.setenv(JAVA_HOME = "C:\\Program Files\\Java\\jre1.8.0_321")
Sys.getenv()
# package or namespace load failed for ‘rJava’:
# R과 jdk의 지원 운영체재가 불일치  r-64bit  rjava-32bit
library(rJava) 

# 인터넷에서 다운로드 할수 있도록 package
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never",
                        INSTALL_opts=c("--no-multiarch"))
library(KoNLP)

###############################################

install.packages("Sejong")
install.packages("wordcloud")
install.packages("tm")
library(tm)
library(wordcloud)

##########  텍스트 자료 가져오기 ##########
setwd("D:\\Rwork-2nd\\Part-II")
facebook =  file('marketing.txt',encoding = 'utf-8')
facebook_data =  readLines(facebook)
head(facebook_data)

#사전추가
user_dic=data.frame(term=c("소셜네트워크","페이스북","R 프로그래밍","만세"), tag='ncn',Encoding='utf-8')
buildDictionary(ext_dic = "sejong",user_dic = user_dic)

# extractNoun 기능 살펴보기
paste(extractNoun("동해물과 백두산이 마르고 닳도록 하느님이 보우하사 우리나라 만세"),
      collapse =" ")


exNouns<-function(x){ paste(extractNoun(x), collapse =" ") }

str(facebook_data)

facebook_nouns =  sapply(facebook_data,exNouns)
  
facebook_nouns[1]  

# 데이터 전처리를 위해서 말뭉치객체를 이용한다.
#1 말뭉치를 생성
myCorpus = Corpus(VectorSource(facebook_nouns))
#2.전처리.. 특수문자 제거 등.... 
myCorpusPrepro =  tm_map(myCorpus,removePunctuation)
myCorpusPrepro =  tm_map(myCorpusPrepro,removeNumbers)
myCorpusPrepro =  tm_map(myCorpusPrepro,tolower)
myCorpusPrepro =  tm_map(myCorpusPrepro,removeWords, stopwords('english'))
inspect(myCorpusPrepro[1:5])

# 2~8음절 단어만 선택
myCorpusPrepro_term= TermDocumentMatrix(myCorpusPrepro,control=list(wordLengths=c(4,16)))

myTerm_df<-as.data.frame(as.matrix(myCorpusPrepro_term))
dim(myTerm_df)

#출현 빈도
wordResult =  sort(rowSums(myTerm_df),decreasing = TRUE)
wordResult[1:10]


# 구름 클라우드 적용
myName =  names(wordResult)
word.df= data.frame(word = myName, freq = wordResult)
head(word.df)

#색상
pal= brewer.pal(12,"Paired")

#시각화
wordcloud(word.df$word, word.df$freq
          , scale = c(5,1),
          min.freq = 3, random.order = F,
          rot.per = .1, colors = pal,
          family='malgun'
          )
wordcloud(word.df$word, word.df$freq,
          random.order = F,colors = pal)
