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
marketing= file("marketing.txt",encoding = "utf-8")   # 파일 열고
marketing2 =  readLines(marketing)                    # 파일 읽고
close(marketing)                                      # 파일 닫고

lword= Map(extractNoun, marketing2)
str(lword)
length(lword)
lword=unique(lword)
length(lword)

lword = sapply(lword, unique)
length(lword)


filter1 = function(x){
  nchar(x) <= 4 && nchar(x) >= 2 && is.hangul(x)
}

