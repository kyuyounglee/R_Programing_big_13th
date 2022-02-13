install.packages("cluster")
library(cluster)
x<-matrix(1:9, nrow=3, by=T)
x
#유클리안 거리 생성
dist<-dist(x)
dist
#군집화
hc<-hclust(dist)

plot(hc)
setwd('D:/Rwork-2nd/Part-IV')
getwd()
interview<- read.csv('interview.csv',header = T)
View(interview)


interview_df<- interview[2:7]
head(interview_df)
#유클리안 거리
eu_interview<- dist(interview_df)
#군집화
hc<-hclust(eu_interview)
#시각화
plot(hc)

#군집단위로 테두리
rhc<-rect.hclust(hc, k = 3, border = "red")
#군집단위로 해당 데이터를추출
g1<-interview[rhc[[1]],]
g2<-interview[rhc[[2]],]
g3<-interview[rhc[[3]],]

summary(g1)

# iris
data("iris")
View(iris)

idist<- dist(iris[-5])
hc<-hclust(idist)
plot(hc)
rect.hclust(hc,k = 3,border = "red")
#군집단위로 해당 데이터를추출
ghc<-cutree(hc,k = 3)
ghc
iris$ghc<-ghc
View(iris)
g1<-subset(iris,ghc==1)
g2<-subset(iris,ghc==2)
g3<-subset(iris,ghc==3)
#요약통계
summary(g1[1:4])
summary(g2[-c(5,6)])
summary(g3[-c(5:6)])

plot(iris$Petal.Length, iris$Petal.Width,col = iris$Species)


install.packages("ggplot2")
library(ggplot2)
data("diamonds")  #  패키지가 가지고있는 데이터로 덮어씀(초기화)  
str(diamonds)
t<-sample(nrow(diamonds),1000)
test<-diamonds[t,]
dim(test)
colnames(test)
View(test)

mydia<-test[c('price',  'table', 'depth', 'carat')]
dim(mydia)
#유클리안거리
di<- dist(mydia)
#군집분석
hc<-hclust(di,method = "average")
#시각화
plot(hc)
#군집단위로 테두리
rhc<-rect.hclust(hc, k = 3, border = "red")
dim(as.data.frame(mydia))
View(as.data.frame(mydia))
mydia_df <-as.data.frame(mydia)
g1<-mydia_df[rhc[[1]], ]
g2<-mydia_df[rhc[[2]], ]
g3<-mydia_df[rhc[[3]], ]
summary(g1)
summary(g2)
summary(g3)

#kmeans
result2<-kmeans(mydia,3)
names(result2)
result2$cluster

mydia$cluster<-result2$cluster


cor(mydia[-5])


#상관관계가 높은 데이터끼리 시각화
plot(mydia$price, mydia$carat,col = mydia$cluster )


#연관분석
install.packages("arules")
library(arules)
#데이터를 transaction처리해서 가져오기
tran<-read.transactions("tran.txt",format = "basket",sep = ",")
tran
inspect(tran)
#규칙발견
rule<-apriori(tran,parameter = list(supp=0.3,conf=0.1))
inspect(rule)

rule<-apriori(tran,parameter = list(supp=0.1,conf=0.1))
inspect(rule)

#싱글 트랜잭션 객체생성
stran<-read.transactions("demo_single",format='single',cols=c(1,2))
stran
inspect(stran)
stran2<-read.transactions("single_format.csv",format="single",sep=",",cols=c(1,2),
                  rm.duplicates = T)
inspect(stran2)
View(inspect(stran2))

astran2<- apriori(stran2)
inspect(astran2)

inspect(head(sort(astran2,by='lift')))

btrain<- read.transactions("demo_basket",format="basket",sep=",")
inspect(btrain)
##############################################################################
data("Adult")
View(Adult)
#inspect(Adult)
Adult
temp<-apriori(Adult)
inspect(temp)

adult<-as(Adult,'data.frame')
str(adult)

ar<-apriori(Adult,parameter = list(supp=0.1, conf=0.8))
ar5<-apriori(Adult,parameter = list(supp=0.4,conf = 0.95))
inspect(head(ar5))

inspect(head(sort(ar5,by="confidence")))
inspect(head(sort(ar5,by="lift")))

install.packages("arulesViz")
library(arulesViz)

ar3 <- apriori(Adult, parameter = list(supp = 0.3, conf = 0.95))
plot(ar5, method = "graph")
