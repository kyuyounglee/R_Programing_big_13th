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

students$korean = ifelse(students$korean>=0 & students$korean<=100, 
                         students$korean,NA)
students$english = ifelse(students$english>=0 & students$english<=100, 
                         students$english,NA)
students$math = ifelse(students$math>=0 & students$math<=100, 
                          students$math,NA)



