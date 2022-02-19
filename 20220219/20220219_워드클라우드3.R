# 필수 페키지
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
############################################################################
# 파일 읽기 --벡터형태로
makeWc = function(data){
  marketing= file(data,encoding = "utf-8")   # 파일 열고
  marketing2 =  readLines(marketing)                    # 파일 읽고
  close(marketing)                                      # 파일 닫고
  head(marketing2)
  
  # 각각의 벡터의 요소에 extractNoun을 적용
  lword= Map(extractNoun, marketing2)  # List of.... vector
  lword[1]
  
  # lword는 벡터들의 집합인 list 형태가 ...
  unlistword =  unlist(lword,use.names = FALSE)
  length(unlistword)
  str(unlistword)
  
  # 다시 하나의 벡터형태로 기존 list를 unlist해서 벡터로변경
  unlistword[1:6]
  
  # 2글자 4글자사이만 추출하는데 한글만추출--- Filter 함수를 이용
  filter1 = function(x){
    nchar(x) <= 4 && nchar(x) >= 2 && is.hangul(x)
  }
  
  # 벡터의 각각의 요소에 필터를 적용
  unlistword_filter =  Filter(filter1,unlistword)
  
  #빈도수를 구한다음 
  word = table(unlistword_filter )
  
  # 데이터 프레임으로 표현
  word.df = as.data.frame(word)
  head(word.df)
  colnames(word.df) = c('word',"freq")
  
  
  #색상
  pal= brewer.pal(12,"Paired")
  wordcloud(word.df$word, word.df$freq,
            random.order = F,colors = pal)
}
##########################################################

makeWc("abstract2.txt")
