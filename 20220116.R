# 벡터  Vector
v1 <- 1
v2 <- 2
v3 <- 3
v4 <- 4
v5 <- 5

vt <- c(1,2,3,4,5)
vt[3] <- 100
vt

# 벡터란 무엇이고 어떻게 만들고 사용하는지
# 벡터에 이름을 붙이기

# 학생 5명의 점수를 관리하자...
score <- c(100,95,88,87,98)
# 함수... 함수란 기능을 수행하는 명령어
# 딸랑 이름. -->변수
# 이름(...) --> 함수
names(score)<-c("a","b","c","d","e")
score["a"]

# 벡터생성 함수
v1 <-c(10,25,15,16)
v1
# 도움말 호출 
seq() # seq를 블럭시키고 f1
?seq()
help(seq)

# from ~ to 간격  연속적인 데이터
seq(1, 9, 2)
rep(1:4, 2)
rep(1:4, each = 2)
rep(1:4, c(2,2,2,2))

install.packages("RSADBE")
library(RSADBE)
data("Severity_Counts")

str(Severity_Counts)
str(v1)
Severity_Counts['Bugs.AR']
str(score)


# 벡터 5개의 값을 가지는 벡터 2개를만들자

v1 <- sample(1:100,5)
v2 <- sample(1:100,5)
v1; v2

rm<-rbind(v1,v2)



cm[3,2]
cm<-cbind(v1,v2)
rm['v1',5]

# index는 기본이 숫자로 접근가능하고
# 이름이 있다고 하면 이름으로 가능

m <- matrix(1:20, nrow = 4)
m
m[2:3, 3:4]

# 행렬객체 : matrix,  dataframe

# apply(행렬객체,방향,적용함수)

m[1,1];m[2,2]
m[1:2, 1:2]

count <- 1
result = c()
for (v in m) {
  if(count ==1 | count ==6 | count ==11 |count ==16){
    result<-c(result,v)
  }
  count <- count+1
}
result

v<-sample(1:100,5)
length(v)
length(m)
nrow(m)
ncol(m)
rsum<-apply(m, 1, sum)
csum<-apply(m, 2, sum)

#점심시간 이전 문제
# apply  -> cbind ->apply ->rbind
m <- matrix(1:20, nrow = 4)
row.sum<-apply(m, 1, sum)
#cbind를 이용해서 합친다
m.1<-cbind(m,row.sum)
col.sum<-apply(m.1, 2, sum)
m.2<-rbind(m.1,col.sum)
colnames(m.2)<-c('c1','c2','c3','c4','c5','csum')
rownames(m.2)<-c('r1','r2','r3','r4','rsum')
m.2



arr<-array(1:12,c(3,2,2))

arr[1,1,2]

data("Bug_Metrics_Software")

Bug_Metrics_Software['JDT','Bugs' ,'Before']
Bug_Metrics_Software['JDT','Bugs' ,'After']

m<-matrix(sample(1:100,9),3,byrow = T)
df <- data.frame(m)
names(df)<-c("a","b","c")
rownames(df)<-c("r1","r2","r3")
df
subset(df,b>40 & a>50)
summary(df)

#중위수.... 가운데 있는 값..
v<-c(1,2,3,5,6000)
summary(v)
mean(v)

"D:\\Rwork-2nd\\Part-I"

"D:/Rwork-2nd/Part-I"

print("ab")
#df<-read.csv("D:\Rwork-2nd\Part-I\emp.csv",header=T)
df<-read.csv("D:\\Rwork-2nd\\Part-I\\emp.csv",header=T)
df<-read.csv("D:/Rwork-2nd/Part-I/emp.csv",header=T)

install.packages("readxl")
library(readxl)
path<-"D:/Rwork-2nd/Part-I/studentexcel.xlsx"
df<-read_excel(path,sheet = "student")
df

df<-read.csv("https://gist.githubusercontent.com/jwalsh/ce1dc0436aba5b7a5c9666f47fa5a380/raw/5ce3854392b43ff97907112d344fc008229b0445/titanic.csv",header=T)
head(df)


#table 빈도수를 구해준다..

v<-sample(1:10,7)
v2<-sample(1:10,7)
v3<-c(v,v2)
table(v3)

names(df)
df['Survived']
table(df['Survived'])
table(df['Pclass'])

df<-data.frame(a=c(1,2,3,4,5),b=c(10,20,30,40,50))
df
df["c"]<-0
df['b']>20

v<-c(10,20,30,40,50)
v[c(1,2)]
v[c(TRUE,FALSE,FALSE,FALSE,TRUE)]
v[v >= 20 & v<=50]

df['Age'] 


df<-data.frame(a=sample(1:100,10))
# 0~30 low  df['a']>=0 & df['a']<=30
# 31~70 midium  df['a']>=31 & df['a']<=70
# 71~ hi        df['a']>=71
# status
df<-data.frame(a=sample(1:100,10))
df['status']<-NA
index1<-df['a']>=0 & df['a']<=30
df['status'][index1]<-'low'

index2<-df['a']>=31 & df['a']<=70
df['status'][index2]<-'midium'

index3<-df['a']>=71
df['status'][index3]<-'hight'


# 타이타닉 데이터 인터넷 다운받기
# 검색... titanic csv github  -> 첫번째째
titanic.df<-read.csv("https://gist.githubusercontent.com/jwalsh/ce1dc0436aba5b7a5c9666f47fa5a380/raw/5ce3854392b43ff97907112d344fc008229b0445/titanic.csv")
#기본과정... 데이터를 가지고 온다음 하는 
summary(titanic.df)  # NA데이터(결측치치) 및 중위값과 평균을 비교해서 이상치를 가늠한다
str(titanic.df)
table(titanic.df['Sex'])
titanic.df['Age']
#titanic.df['Age'] 연속형 데이터를 범주형...
titanic.df['Age_type']<-'child'
titanic.df['Age_type']
child_index<-titanic.df["Age"] >=0 & titanic.df["Age"] <=10
teanAger_index<-titanic.df["Age"] >=11 & titanic.df["Age"] <=19
adult_index<-titanic.df["Age"] >=20
titanic.df['Age_type'][teanAger_index]<-"teanAger"
titanic.df['Age_type'][adult_index]<-"adult"
titanic.df['Age_type']
table(titanic.df['Age_type'])
