install.packages("rJava")
Sys.setenv(JAVA_HOME = "C:\\Program Files\\Java\\jre1.8.0_321")
Sys.getenv()
# package or namespace load failed for ‘rJava’:
# R과 jdk의 지원 운영체재가 불일치  r-64bit  rjava-32bit
library(rJava) 

# 인터넷에서 다운로드 할수 있도록 package
install.packages("remote")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never",
                        INSTALL_opts=c("--no-multiarch"))
library(KoNLP)

###############################################

install.packages("Sejong")
install.packages("wordcloud")
install.packages("tm")


