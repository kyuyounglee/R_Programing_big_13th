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
head(marketing2)

lword= Map(extractNoun, marketing2)
lword=unique(lword)
length(lword)

lword = sapply(lword, unique)
length(lword)
head(lword)

filter1 = function(x){
  nchar(x) <= 4 && nchar(x) >= 2 && is.hangul(x)
}

filter2 = function(x){
  Filter(filter1,x)
}

lword =  sapply(lword,filter2)

# 트랜잭션
install.packages("arules")
library(arules)

# 트랜잭션 생성
wordtran= as(lword,"transactions")

#연관 규칙발견
tranrules= apriori(wordtran, parameter = list(supp=0.25,conf=0.05))
inspect(tranrules)

#연관단어 시각화
rules= labels(tranrules,ruleSep = " " )

# 행렬구조로 변경
rules = sapply(rules, strsplit, " ", USE.NAMES = F)
# matrix구조로 변경
rulemat = do.call("rbind",rules)
class(rulemat)

#연관어 시각화를 위한 package
install.packages("igraph")
library(igraph)

ruleg=graph.edgelist(rulemat[c(12:59),], directed = F)
#시각화
plot.igraph(ruleg, vertex.label = V(ruleg)$name, 
            vertex.label.cex = 1.2, vertext.label.color = 'black',
            vertex.size = 20, vertext.color = 'green',
            vertex.frame.co.or = 'blue')

