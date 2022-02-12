# 필요한 package 설치  ctree
# ctree scale을 맞추지 않아도 된다  
install.packages("party")
library(party)

# data
library(datasets)
data("airquality")
View(airquality)
str(airquality)
summary(airquality)


# 결측치 NA
# 이상치 -- NA  stats
# 모든 NA 평균으로 대체
# 확인 : 이상치가 처리되었는지
boxplot(airquality$Ozone)$stats
boxplot(airquality$Wind)$stats
airquality$Ozone[airquality$Ozone < 1.0 | airquality$Ozone > 122.0]<-NA
airquality$Wind[airquality$Wind < 1.7 | airquality$Wind > 16.6]<-NA
# 각각의 평균
ozone.mean<-mean(airquality$Ozone,na.rm = TRUE)
Wind.mean<-mean(airquality$Wind,na.rm = TRUE)

airquality$Ozone[is.na(airquality$Ozone)]<-ozone.mean
airquality$Wind[is.na(airquality$Wind)]<-Wind.mean

boxplot(airquality$Ozone)$stats
boxplot(airquality$Wind)$stats

# model  formula --> 종속변수 , 독립변수 
names(airquality)
formula<-Temp ~ Ozone+Solar.R+Wind
air_ctree<- ctree(formula, data = airquality)
air_ctree
plot(air_ctree)


iris$Species
table(iris$Species)

# 7.5: 2.5  -> sample 해서 섞은후 나눈다
index<-sample(1:nrow(iris),nrow(iris)*0.8)
train<-iris[index,]
test<-iris[-index,]

# formula
#Sepal.Width 꽃받침의 너비
#Sepal.Length 꽃받침의 길이
#Petal.Width 꽃잎의 너비
#Petal.Length꽃잎의 길이

names(iris)
formula<- Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width
iris.ctree<-ctree(formula,data=train)
iris.ctree

plot(iris.ctree,type='simple')

#confusion matrix 혼돈 메트릭스
pred<-predict(iris.ctree,test)

table(pred, test$Species)

# k겹 교차 검증
library(cvTools)
cross<- cvFolds(nrow(iris),K = 3,R = 2)
str(cross)
cross
length(cross$which)
dim(cross$subsets)
table(cross$which)


R<-1:2
k.fold<-1:3
CNT<-0
ACC<-numeric()


for (r in R) {
  cat('\n R =',r,'\n')
  for(kf in k.fold){
    idx<-cross$subsets[cross$which == kf,r]
    test<-iris[idx,]
    cat("test : ",nrow(test),'\n')
    
    formula <- Species ~ .
    train<-iris[-idx,]
    cat("train : ",nrow(train),'\n')
    
    model<-ctree(formula,data=train)
    pred<- predict(model,test)
    t<-table(pred,test$Species)
    print(t)
    
    CNT <- CNT+1
    ACC[CNT]<-(t[1,1]+t[2,2]+t[3,3] )/sum(t)
  }
} 
CNT
result.acc<-mean(ACC)
result.acc

install.packages("ggplot2")
library(ggplot2)
View(mpg)

# hwy 주행거리...에 영향을 미치는 변수
# vif  상관계수  몇몇 컬럼을 제거--> Feature enginering

# 종속변수 : hwy 주행거리
# 독립변수 : 나머지다..
# ctree : 스무고개... 
# 분류모델인 ctree 이용해서 중요 Freature를 찾을수 있다.

# 데이터 분류 : 학습용 검증용
# 8: 2
idx<-sample(1:nrow(mpg), nrow(mpg)*0.8)
train<-mpg[idx,]
test<-mpg[-idx,]
dim(train)
dim(test)

str(mpg) # 데이터 타입확인--> 종속변수 사용가능
formula<- hwy ~ displ+year+cyl+cty  # cty year
model<-ctree(formula,data=train )
plot(model)

#AdulTUCI
install.packages("arules")
library(arules)
data("AdultUCI")
str(AdultUCI)

# 타이타닉 데이터 분류
setwd("D:/titanic")
train_set<- read.csv("train.csv",header = T)
test_set<- read.csv("test.csv",header = T)

# EDA(탐색적 데이터 분석)
str(train_set) # 데이터 구조 파악
str(test_set)
summary(train_set)
summary(test_set)

# age na는 평균으로 대체
train_set.age.mean<-mean(train_set$Age,na.rm = T)
test_set.age.mean<-mean(test_set$Age,na.rm = T)
test_set.fare.mean<-mean(test_set$Fare, na.rm =T)

train_set$Age[is.na(train_set$Age)] <- train_set.age.mean
test_set$Age[is.na(test_set$Age)] <- test_set.age.mean
test_set$Fare[is.na(test_set$Fare)] <- test_set.fare.mean

# 각 컬럼별로 NA의 횟수를 구해보자. 확인해보자.
# sapply : 컬럼별.... 적용함수
View(train_set)

sapply(test_set, function(x){
  sum(is.na(x))
})

# 필요에 의해서 데이터 전처리를 한다. pre-processing
# "under 10" , '10~20', '20~30'

install.packages("tidyverse")
library(tidyverse)
library(ggplot2) # 시각화
install.packages("plotly")
library(plotly) #반응형 시각화
library(rpart) # 의사결정 나무
library(rpart.plot) #의사결정 나무 시각화
install.packages("caret")
library(caret) #데이터 처리 패키지
train_set %>% head()  # %>%  Ctrl+Shift+m 

train_set<- train_set %>% mutate(Ages =  case_when(
  Age<10 ~ "under 10",
  Age<20 ~ "10~20",
  Age<30 ~ "20~30",
  Age<40 ~ "30~40",
  Age<50 ~ "40~50",
  Age<60 ~ "50~60",
  TRUE ~ "over 60"
  )
)
str(train_set)
table(train_set$Ages)
train_set$Ages<- factor(train_set$Ages, levels = c('under 10',
  '10~20','20~30','30~40','40~50','50~60','over 60'))

data_cleaning<-train_set %>%  group_by(Ages) %>%  summarise(Ages_count =  n() )
data_cleaning %>% ggplot(aes(x=Ages,y=Ages_count, fill=Ages)) +
  geom_col()+
  geom_text(aes(label=(Ages_count)),vjust=1,hjust=0.5)

data_cleaning<-train_set %>%  group_by(Ages,Survived) %>%  summarise(Ages_count =  n() )
data_cleaning %>% ggplot(aes(x=Ages,y=Ages_count, fill=Survived)) +
  geom_col()+
  geom_text(aes(label=(Ages_count)))
  


# 성별에 따른 생존여부
ggplot_data<- train_set %>% ggplot(aes(x=Survived,fill=Sex))+
  geom_bar() + ggtitle("성별에따른 생존")+
  theme_bw()
#반응형 시각화
ggplotly(ggplot_data,height = 500,width = 800)

#나이따른 생존유무
ggplot_data<-train_set %>% ggplot(aes(x=Survived,fill=Ages)) +
  geom_bar()+ ggtitle("나이에따른 생존")+
  theme_bw()
#반응형 시각화
ggplotly(ggplot_data,height = 400,width = 700)

# 모델을 생성 rpart
str(train_set)
furmula = Survived ~ Pclass+Age
ctree (furmula,data=train_set )
#rpart_m<- rpart(furmula,data=train_set)
#  의사결정나무 시각화
#prp(rpart_m,type=4,digit=3)


#랜덤 포레스트
install.packages("randomForest")
library(randomForest )
data("iris")

names(iris)
formula = Species ~ .
model<-randomForest(formula,data=iris,ntree=300,
                    mtry=4, na.action = na.omit)

str(model)

model<-randomForest(formula,data=iris,importance=T,na.action = na.omit)
importance(model)
varImpPlot(model)


#최적의 파라메터를찾기---> 파라메터튜닝, 하이퍼파라메터
ntree<-c(400,500,600,700,800)
mtry<-c(2:6)
param<-data.frame(n=ntree,m=mtry) # 2차원 배열형태로 표현

#for문을 이용해서 찾기
for(i in param$n){
  cat('ntree = ',i,'\n')
  for(j in param$m){
    cat('mtry = ',j,'\n')
    model<-randomForest(formula,data=iris,ntree=i,mtry=j,na.action = na.omit)
    print(model)
  }
}


#xgboost  불량케이스..... 정상100만건... 불량데이터 100

install.packages("xgboost")
library(xgboost)  # 종속변수는 숫자로 되어야한다. 문자레이블
# 레이블 인코딩  0 1 2
one<- names(table(iris$Species))[1]
two<-names(table(iris$Species))[2]
three<-names(table(iris$Species))[3]

data("iris")
iris$label[iris$Species == one]<-0
iris$label[iris$Species == two]<-1
iris$label[iris$Species == three]<-2
# train test
idx<-sample(1:nrow(iris), nrow(iris)*0.7)
train<-iris[idx,]
test<-iris[-idx,]
View(train)

train_mat<- as.matrix( train[-c(5,6)] )
train_lab<- train$label

dtrain<- xgb.DMatrix(data=train_mat,label=train_lab)
model_xgb<-xgboost(data = dtrain, nrounds=50)

#test
test_mat<- as.matrix(test[-c(5,6)])
dim(test_mat)
pred_iris<- predict(model_xgb,test_mat)
round(sqrt(pred_iris))
