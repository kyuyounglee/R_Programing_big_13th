chart_data <-sample(300:500,8)
names(chart_data)<-c("a","b","c","d","e","f","g","h")
# 평균보다 큰 값만 red로 색상을 칠해봅시다
chart_data.mean = mean(chart_data)
trueFlseIndex<- chart_data >= chart_data.mean

chart_data.color = rep("gray",8)
chart_data.color[trueFlseIndex]<-"yellow"
barplot(chart_data,ylim = c(0,500),
        col=chart_data.color,main="2020 vs 2021 분기별 매출",
        xlab = "년도별 분기현황",ylab="매출액")

barplot(chart_data,xlim = c(0,500),
        col=chart_data.color,main="2020 vs 2021 분기별 매출",
        xlab = "년도별 분기현황",ylab="매출액",horiz=T)


#벡터에 데이터를 추가하는 방법
# index
chart_data.color[5]<-'red'
#함수
chart_data.color <- append(chart_data.color,"blue")


#page 144
data("VADeaths")
str(VADeaths) # 데이터 구조 보기
summary(VADeaths)# 기술 통계량

# 출력 분할하기
par(mfrow = c(1,2))

barplot(VADeaths,col = rainbow(5))
legend(4,200,rownames(VADeaths),fill = rainbow(5),cex=0.8)

barplot(VADeaths,col = rainbow(5),beside=T,ylim = c(0,90))
legend(18.5,80,rownames(VADeaths),fill = rainbow(5),,cex=0.8)

# 출력화면을 원상태로 돌리기
par(mfrow = c(1,1))

dotchart(chart_data)

pie(chart_data,cex=1.2)

summary(VADeaths)

#boxplot --  summary

boxplot(VADeaths)

summary(1:51)
boxplot(1:51)

# 1~ 26

seq(100,500,20)
seq(10,50,2)

test <- c(seq(100,500,20), seq(10,50,2))
boxplot(test)
test2 <- c(seq(10,50,2), seq(100,500,20))
boxplot(test2)


#histogram
data(iris)
str(iris)


#page 151

head(iris,6)
str(iris['Sepal.Length'])  # data frame
str(iris$Sepal.Length)     # vector

st = read.table('https://raw.githubusercontent.com/kyuyounglee/R_Programing_big_13th/main/20220122/students1.txt'
                ,header = T)
str(st)

write.table(st,"d:/st.txt",quote=F,header = TRUE)
write.csv(st,"d:/st.csv",quote=F,header = TRUE)


#   ==  equal
#   !=  not equal
10 != 11


#age = sample(0:100,10)

age = 15
if (age > 20){
  check = 'adult'
}else{
  check = 'not adult'
}
check

age = 15
check = ifelse(age>20, "adult", "not adult")
check

age = sample(0:100,10)
check = ifelse( age>20 , "adult","not adult")
check


# 저장한 데이터 불러오기
students =  read.csv("d:/st.csv",header = T)
cols = names(students)[-1]
students = students[cols]

students[4,4] = -100
students[2,3] = 120

names(students)



students$korean = ifelse(students$korean>=0 & students$korean<=100, 
                         students$korean,NA)
students$english = ifelse(students$english>=0 & students$english<=100, 
                         students$english,NA)
students$math = ifelse(students$math>=0 & students$math<=100, 
                          students$math,NA)


# 데이터 가져오기
students =  read.csv("d:/st.csv",header = T)
# 불필요한 x컬럼의 데이터 제거하기(x컬럼제외하고 다 가져오기)
students = students[ names(students)[-1] ]
# 문제있는 데이터 만들기 (0 ~100 이외의 데이터)
students[4,4] = -100
students[2,3] = 120
students

# 범위를 넘어선 데이터는 NA로 기록한다
for(idx in 2:4){
  students[,idx] = ifelse(students[,idx]>=0 & students[,idx]<=100, 
                             students[,idx],NA)
}

students


#간단한 함수 만들기
# 두개의 값을 전달하면 결과로 더한 값을 리턴하는
# 함수이름은 addTwoNumber


addTwoNumber = function(n1,n2){
  return (n1+n2)
}

# 변수  -> 변수
# 이름을 그냥쓰면 그것은 변수
# 이름() -->함수

addTwoNumber(10,20)




job.type <- "A" 
if(job.type == 'B'){
  bonus <- 200
}else{
  bonus <- 100
}


# for excample
# data : iris

str(iris)
head(iris,3)
View(iris)

# 1.6보다 작거나 같으면 L
# 기존 DataFrame에 컬럼을 만들어서 L M H 라고 기록한다

for(idx in 1:nrow(iris) ){
  if(iris$Petal.Length[idx] <= 1.6){
    iris$label[idx] <- 'L'
  }else if(iris$Petal.Length[idx] <= 5.1){   # 1.6 <  x   <= 5.1
    iris$label[idx] <- 'M'
  }else{
    iris$label[idx] <- 'H'
  }
}
iris$label


for( i in 1:10){
  if(i == 3){ 
    next
  }
  print(i)
}



head(iris,3)


iris[1:4]
apply(iris[1:4],2,mean)

  
  

students
students.omit<- na.omit(students)
is.na(students)
!is.na(students)
students
mean(students$math, na.rm = T)
sum(students$math)
sum(students$math, na.rm = T)

airquality
str(airquality)


