setwd("C:/Rwork-2nd/Part-IV")
getwd()
product<-read.csv("product.csv",header = T)
View(product)

y <- product$제품_만족도
x1 <- product$제품_친밀도
x2 <- product$제품_적절성
df<-data.frame(x1,x2,y)

result.lm <- lm(formula = y ~ x1+x2,data = df)
result.lm

install.packages("car")
library(car)
vif(result.lm)  # 10 이상이면 다중공선성 문제를의심
cor(df$x1,df$x2)
summary(result.lm)

View(df)
predict(result.lm,newdata = data.frame(x1=1,x2=5))

data("iris")
View(iris)
str(iris)
summary(iris)

iris.lm<- lm(formula = Sepal.Length ~ Sepal.Width+
               Petal.Length + Petal.Width,  data = iris)
summary(iris.lm)
vif(iris.lm) 
vif(iris.lm) > 10
sqrt(vif(iris.lm)) > 2

cor(iris[-5])
cor(iris[1:4])
cor(iris[,-5])

iris.lm<- lm(formula = Sepal.Length ~ Sepal.Width+
               Petal.Length ,  data = iris)
vif(iris.lm) 
summary(iris.lm)

iris.lm

# 엑셀파일 불러오기
install.packages("readxl")
library(readxl)
temp<-read_excel("C:/Rwork-2nd/Part-II/Table.xlsx",
           sheet="pay_data")
str(temp)
View(temp)
temp.df<-as.data.frame(temp)
View(temp.df)


# iris ->150  데이터중에서 학습용과 검증용  (train, test)

#index70<-nrow(iris)*0.7
#index <- c(1:index70)
index<-sample(1:nrow(iris),nrow(iris)*0.7)
train<-iris[index,]
test<-iris[-index,]

table(train$Species)
table(test$Species)

names(iris)
model<-lm(formula = Sepal.Length ~ Sepal.Width+  
            Petal.Length  ,data=train )
install.packages("car")
library(car)
sqrt(vif(model))>2
model

train[1,]
Y = 3.8*0.6120+6.4*0.4702+2.2185
Y

# pred 기계학습을 통해서 만든 모델의 예측값
pred<-predict(model,test)
# 평가는 상관관계
cor(pred,test$Sepal.Length)
plot(pred,test$Sepal.Length)
abline(model,col="red" )

pred.train<-predict(model, train)
cor(pred.train,train$Sepal.Length)


# 
formula = Sepal.Length ~ Sepal.Width+  
  Petal.Length

model <- lm(formula = formula,data = iris)
model

# 잔차 분석(오차)
install.packages("lmtest")
library(lmtest)
dwtest(model)

plot(model,which = 1)
vif(model)

# iris 에서 이번에는 품종을 예측해보자
str(iris)
#setosa = 1  versicolor = 2 3   변수리코딩
data("iris")
iris
#truefalse<-iris$Species == "versicolor"
#index<-which(iris$Species == "setosa")
#iris[index,"Species_y"]<-1
#iris[truefalse,"Species_y"]<-2

index1<-iris$Species == "setosa"
index2<-iris$Species == "versicolor"
index3<-iris$Species == "virginica"
iris[index1,"Species_y"]<-1
iris[index2,"Species_y"]<-2
iris[index3,"Species_y"]<-3

iris.cp<- iris[-5]
str(iris.cp)
# 변수모델링
names(iris.cp)
formula = Species_y ~ Sepal.Length+Sepal.Width+Petal.Length

# 모델 생성
model<- lm(formula = formula,data=iris.cp)
summary(model)
vif(model)
sqrt(vif(model))>2
cor(iris.cp[-5])
# Petal.Width Petal.Length

iris.cp2<-iris.cp[-4]
formula = Species_y ~ Sepal.Length+Sepal.Width+Petal.Length
model2<-lm(formula = formula,data=iris.cp2)
vif(model2)
sqrt(vif(model2))>2
cor(iris.cp2[-4])


iris.cp3<-iris.cp2[-3]
formula = Species_y ~ Sepal.Length+Sepal.Width
model3<-lm(formula = formula,data=iris.cp3)
vif(model3)
sqrt(vif(model3)) > 2

#iris.cp3 --> train, test 구분
# 8: 2




# for문을 이용하는 방법 100번 수행해서 가장 효율이 높은 모델 선택택
data("iris")
names(iris)
table(iris$Species)
#변수 리코딩
iris[iris$Species == "setosa","Y"]<-1
iris[iris$Species == "versicolor","Y"]<-2
iris[iris$Species == "virginica","Y"]<-3
iris.cp<-iris[,c(1,2,6)]
formula = Y ~ Sepal.Length+Sepal.Width
while(TRUE){
  index<-sample(1:nrow(iris.cp),nrow(iris.cp)*0.7)
  train<-iris.cp[index,]
  test<-iris.cp[-index,]
  model<-lm(formula = formula,data=train)
  #summary(model4)
  #test 데이터를 기준으로 예측
  pred<-predict(model,test)
  value<-cor(pred,test$Y)  
  # 예측값이 0.95이상일때 멈춤
  if (value >=0.95){
    break
  }
}
summary(model)
value
plot(test[-3])
abline(model,col="red")

predict(model,data.frame(Sepal.Length=7.0,Sepal.Width=3.0))


# 분류모델이  정답은 범주형(카데고리)
# 로짓변환  학습데이터의 범위를 0~1  scale  --> Standard Scaling
# 또는 label encording / one hot encording
# A B C
# 0 0 1  000 000 001
# 0 1 0  000 001 000
# 1 0 0  001 000 000


# 실습: 날씨 관련 요인 변수로 비(rain) 유뮤 예측
# 단계 1: 데이터 가져오기 
weather = read.csv("C:/Rwork/Part-IV/weather.csv", stringsAsFactors = F)
weather<-na.omit(weather)
#dim(weather)
#head(weather)
#str(weather)

# 단계 2: 변수 선택과 더비 벼수 생성
#weather_df <- weather[ , c(-1, -6, -8, -14)]
index<-c(1, 6, 8, 14)
weather_df<-weather[,-index]
str(weather_df)

weather_df$RainTomorrow[weather_df$RainTomorrow == 'Yes'] <- 1
weather_df$RainTomorrow[weather_df$RainTomorrow == 'No'] <- 0
weather_df$RainTomorrow <- as.numeric(weather_df$RainTomorrow)
head(weather_df)

# 단계 3: 학습데이터와 검정데이터 생성(7:3 비율)
idx <- sample(1:nrow(weather_df), nrow(weather_df) * 0.7)
train <- weather_df[idx, ]
test <- weather_df[-idx, ]

# 단계 4: 로지스틱 회귀모델 생성
weather_model <- glm(RainTomorrow ~ ., data = train, family = 'binomial')
weather_model
summary(weather_model)


# 단계 5: 로지스틱 회귀모델 예측치 생성
pred <- predict(weather_model, newdata = test, type = "response")
pred
is.na(pred)
table(is.na(pred))

round(pred)
ifelse(pred>=0.5,1,0)

#상관관계
cor(round(pred),test$RainTomorrow)
table(round(pred),test$RainTomorrow)

install.packages("ROCR")
library(ROCR)
pr<-prediction(pred,test$RainTomorrow)
prf<-performance(pr,measure = "tpr",x.measure = "fpr")
plot(prf)


