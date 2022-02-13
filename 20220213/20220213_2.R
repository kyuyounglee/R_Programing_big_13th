install.packages("nnet")
library(nnet)

df<-data.frame(
  x2 = c(1:6),
  x1 = c(6:1),
  y = factor(c('no','no','no','yes','yes','yes'))
)
View(df)
formula = y ~ .  #종속변수 y 독립변수는 y를 제외한 전부
model<- nnet(formula,data = df,size=1)
model
summary(model)

sqrt( model$fitted.values)
p<-predict(model,df,type='class')
table(p,df$y)

#
data("iris")
idx<-sample(nrow(iris),nrow(iris)*0.7)
train<-iris[idx,]
test<-iris[-idx,]

formula = Species ~ .
model1<-nnet(formula,train,size=1)
model3<-nnet(formula,train,size=3)

summary(model1)
summary(model3)

model1_pred<-predict(model1,test,type='class')
model3_pred<-predict(model3,test,type='class')
cm1<-table(model1_pred,test$Species)
cm3<-table(model3_pred,test$Species)
cm1
cm3
result = 0
for (i in 1:nrow(cm1)) {
  result = result + cm1[i,i]
}
result/sum(cm1)

result = 0
for (i in 1:nrow(cm3)) {
  result = result + cm3[i,i]
}
result/sum(cm3)

#######################################
install.packages("neuralnet")
library(neuralnet)

data("iris")
table(iris$Species)
iris$Species<- ifelse(iris$Species=='setosa',1,
       ifelse(iris$Species=='versicolor',2,3)
       )
View(iris)
#iris$Species<-NULL
idx<-sample(nrow(iris),nrow(iris)*0.7)
train<-iris[idx,]
test<-iris[-idx,]


# 데이터의 단위를 표준화 stanrd sacling  정규화 
normal<-name <- function(x) {
  return((x-min(x)) / (max(x) - min(x)) )
}
train_nor<- as.data.frame(lapply(train, normal))
summary(train_nor)

#hidden 1
formula = Species ~ .
model_net<- neuralnet(formula,data=train_nor,hidden=1)
plot(model_net)

#predict
test_nor<- as.data.frame(lapply(test, normal))
summary(test_nor)
model_pred<- compute(model_net, test_nor[1:4] )
model_pred$net.result

#상관관계
cor(model_pred$net.result, test_nor$Species)


model_net2<-neuralnet(formula,data=train_nor,hidden=2,
                      algorithm = 'backprop',learningrate = 0.01)
model_pred2<- compute(model_net2, test_nor[1:4] )
cor(model_pred2$net.result, test_nor$Species)

