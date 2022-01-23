airquality
# str()
# summary()
# head()
# tail()
str(airquality)
summary(airquality)
head(airquality)
tail(airquality)
mean(airquality$Ozone, na.rm = T)
table(is.na( airquality$Ozone))
airquality[is.na( airquality$Ozone), ]

Ozone.mean <- mean(airquality$Ozone, na.rm = T)
Solar.R.mean <- mean(airquality$Solar.R, na.rm = T)
airquality$Ozone  # vector
airquality['Ozone']  # data frame
is.na(airquality$Ozone)  # na 위치를 확인

airquality$Ozone[is.na(airquality$Ozone)]
airquality['Ozone'][is.na(airquality$Ozone),]

# 평균으로 대체하는 방법 - 
airquality$Ozone[is.na(airquality$Ozone)]<-Ozone.mean
airquality['Solar.R'][is.na(airquality$Solar.R),]<-Solar.R.mean



v1 <- c(1,2,3,5,4,87,8,4,0,1,4,5,1,451,5,1,515,15,1)
table(v1)
table(airquality$Month)


# 결측치  NA (공백) 
# 받은 데이터에서 공백이 있으면 NA

# gender M F
# boold.type  A B O AB
name <-c("환자1","환자2","환자3","환자4","환자5")
age <- sample(20:50,5)
gender <- c("M","F","M","K","F")
blood.type<-c('A',"O","B","AB","C")
patients <- data.frame(name,age,gender,blood.type)

# 문자형 데이터의 이상치 제거
# --> 허용된 문자형 데이터만 사용

# 수치형.... 만약에 범위를 알고있는경우 
# 범위가 어떤 기준을 모르는경우  boxplot으로 시각화를 통해파악
# 4분위수를 이용해서 제거한다.

table(patients$gender)
table(patients$blood.type)

patients<-patients[patients$gender == "M" | patients$gender == "F", ]
patients<-patients[patients$blood.type == "A" | patients$blood.type == "O" |
  patients$blood.type == "B" | patients$blood.type == "AB", ]


boxplot(airquality)


v1<-c(rep(1:10,10),100,200,300)
boxplot(v1)
# IRQ 를 사용한 이상치 제거
# IRQ = Q3 - Q1
# Q1 - 1.5*IRQ  <  정상데이터 < Q3 + 1.5*IRQ

ao <- summary( airquality$Ozone)
irq = ao['3rd Qu.'] - ao['1st Qu.']
ao['1st Qu.'] - 1.5*irq
ao['3rd Qu.'] + 1.5*irq

t<-airquality[airquality$Ozone > ao['1st Qu.'] - 1.5*irq & 
  airquality$Ozone < ao['3rd Qu.'] + 1.5*irq, ]

boxplot(t$Ozone)$stats  

boxplot(airquality$Ozone)$stats
boxplot(airquality$Ozone[airquality$Ozone > 1 & airquality$Ozone < 75])


# 이상치를 제거합시다다
airWindStats<-boxplot(airquality$Wind)$stats
low<-airWindStats[1,1]
high <-airWindStats[5,1] 

changed<-airquality$Wind[airquality$Wind>low & airquality$Wind<high]
boxplot(changed)



install.packages("gapminder")
library(gapminder)
library(dplyr) #  %>% --> 파이프연산자 를 사용가능하게 하는  library 
str(gapminder)
names(gapminder)
gapminder$country
gapminder$gdpPercap
table(gapminder$continent)
summary(gapminder)

gapminder[gapminder$country == 'Korea, Rep.',]
gapminder[gapminder$country == 'Korea, Dem. Rep.',]

temp1 <- gapminder[gapminder$country == 'Korea, Rep.',]
temp1[temp1$lifeExp > 60,]





