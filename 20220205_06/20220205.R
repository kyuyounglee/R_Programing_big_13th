setwd("C:\\Rwork-2nd\\Part-III")
setwd("C:/Rwork-2nd/Part-III")
getwd()
data <- read.csv("cleanDescriptive.csv", header = T)
head(data)
tail(data)
View(data)
table(data$level2)
table(data$pass2)

result <- data.frame(Level=data$level2, Pass = data$pass2)
View(result)
table(result)
install.packages("gmodels")
library(gmodels)

install.packages("ggplot2")
library(ggplot2)
View(diamonds)


CrossTable(x = diamonds$color, y = diamonds$cut)

x<-data$level2 # 부모 학력수준   독립변수
y<-data$pass2 # 자녀의 대학진학  종속변수
# 연구가설(대립가설) H1 부모의  학력수준에따라 자녀의 대학진학과 관련이 있다
# 귀무가설 H0 부모의  학력수준에따라 자녀의 대학진학과 관련이 없다
length(x)
length(y)
CrossTable(x,y,chisq = T)
summary(data)

# 기대치 비율
# 기대치 : 89(행합) * 135(열합) / 225(전체합)  53.4
# 기대치 비율 : (49-53.4)^2/53.4 = 0.363
data<- read.csv("one_sample.csv", header = T)
View(data)
summary(data)
table(data$gender)
table(data$survey)

install.packages("prettyR")
library(prettyR)
freq(data$survey)



binom.test(14,150,p=0.2)
binom.test(c(14,136),p=0.2)
# 2020년 불만율 > 2019년 불만율 
binom.test(14,150,p=0.2,alternative = "greater",conf.level = 0.95)

# 2020년 불만율 < 2019년 불만율 
binom.test(14,150,p=0.2,alternative = "less",conf.level = 0.95)

s1 <- c(1, 2, 1, 2, 3, 4, 2, 3, 4, 5)
s2 <- c(1, 3, 1, 2, 3, 4, 2, 4, 3, 4)
s3 <- c(2, 3, 2, 3, 2, 3, 5, 3, 4, 2)
s4 <- c(2, 4, 2, 3, 2, 3, 5, 3, 4, 1)
s5 <- c(4, 5, 4, 5, 2, 1, 5, 2, 4, 3)
s6 <- c(4, 3, 4, 4, 2, 1, 5, 2, 4, 2)
name <- 1:10

subject <- data.frame(s1, s2, s3, s4, s5, s6)
table(s3)

pc<-prcomp(subject)
summary(pc)
plot(pc)

en<-eigen( cor(subject) )
names(en)
plot( en$values , type = "o")


install.packages("memisc")
library(memisc)
data.spss <- as.data.set( spss.system.file("drinking_water.sav") )
str(data.spss)
data.spss[c("Q1","Q2")]
drinking_water<- data.spss[1:11]
drinking_water.df<-as.data.frame(drinking_water)
str(drinking_water.df)

factnum<-3
result<-factanal(drinking_water.df,factors = factnum,rotation = "varimax")
temp<-result$loadings

for(i in 1:factnum){
  t<-temp[,i]
  print(names(t[t>0.5]))
}

result<- read.csv("C:/Rwork-2nd/Part-IV/drinking_water.csv",header = T)
View(result)

cor(result$친밀도,result$만족도)
cor(result$친밀도,result$적절성)

cor(result,method ="pearson" )
cor(result)
cor(result,method ="spearman" )


mean(result$친밀도)
var(result$친밀도)
sqrt(var(result$친밀도))

setwd("C:/Rwork-2nd/Part-IV")
getwd()

product<- read.csv("product.csv") 
View(product)

x <- product$제품_적절성
y <- product$제품_만족도
df<-data.frame(x,y)
View(df)

df.lm<- lm(y~x,data=df)
df.lm
names(df.lm)

x[2]
y[2]

0.7393*4 + 0.7789

plot(df)
abline(df.lm,col="red" )


View(cars)

m1<-lm(dist~speed, data=cars)
m1
# dist = 3.932*speed -17.579

coef(m1)

fitted(m1)[1:5]


table(cars$speed)  
# speed 30 ??

data.frame(speed=c(30,10,15,20))
predict(m1, newdata = data.frame(speed=c(30,10,15,20)))

#잔차 : 실제값과 모델의 예측값의 차이
residuals(m1)

summary(m1)