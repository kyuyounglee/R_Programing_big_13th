#원 데이터에서는 유의미한 정보 추춗이 불가능하므로 농축산물 가격 데이터와 기상 데이터 등을 붂석
#가능한 형태의 데이터로 변홖한다.
#농축산물 가격 데이터를 이용한 데이터 시각화 및 데이터갂의 연관성 붂석 과정을 상세히 소개하고, 그
#의미를 살펴본다.
#고급 클러스터링 기법을 이용한 연관성 붂석을 실시하고 날씨 및 뉴스 데이터를 붂석하여 농축산물
#가격변동의 원인을 유추해 본다.

# 패키지 다운로드
library1 <- c('plyr','ggplot2','stringr','zoo','corrplot','RColorBrewer')
unlist(lapply(library1,require, character.only=TRUE))
install.packages("corrplot")


# plyr : 데이터 핸들링  
# ggplot2 :고급 시각화
# stringr : 문자열
# zoo : 문자형 데이터를 데이트 형식으로 변환
# corrplot : 상관분석
# RcolorBrewer : 색상처리

#데이터 불러오기
setwd("D:\\주말반 빅데이터분석_이규영 (2)\\농산물")
product <- read.csv("product.csv",header = T )
code<-read.csv("code.csv",header = T )
View(code)

# date(일자) category(부류코드), item(품목코드), region(지역코드) mart(마트코드) price(가격)
colnames(product) <- c('date','category','item','region','mart','price')

category<-subset(code, code$구분코드설명=='품목코드')

colnames(category) <-c('code','exp','item','name')

# 분석대상이 돼지고기 소매가격
# product에서 품목코드가 514 pig price --> data

total.pig<-subset(product,product$item ==514)

region<-subset(code, code$구분코드설명=='지역코드')
colnames(region)<-c('code','exp','region','name')

# merge : 두 데이터 프레임을 공통된 값을 기준으로 묶는 함수
day.pig<- merge(total.pig, region, by="region", all=T)
head(total.pig)
head(region)
head(day.pig)

# day.pig 일별로 정렬,  지역별로 돼지고기의 평균가격 , 지역별로 나눈다
# dlply() : 데이터프레임 형태를 품목별로 list 형태로 출력
# ddply() : 데이터 프레임을 분리하여 함수를 적용시킨후 데이터 프레임 형태로 출력
head(ddply(day.pig, .(date), summarise, name=name, region=region,price=price ))
head(ddply(day.pig, .(date)))
head(day.pig)

a<-ddply(day.pig, .(date), summarise, name=name, region=region,price=price )

b<-ddply(a, .(date,name),summarise,mean.price = mean(price))

total.pig.mean<- dlply(b,.(name))
str(total.pig.mean)

# 각 지역별로 데이터의 크기를 확인
removeCol<-c()
for(i in 1: length(total.pig.mean)){
  cat( names(total.pig.mean[i]),"의 데이터 길이는", nrow(total.pig.mean[[i]]),"\n" )
  if ( nrow(total.pig.mean[[i]]) != 745 ){
    #print(names(total.pig.mean[i]))
    removeCol<- c(removeCol, names(total.pig.mean[i]) )
  }
}

# 전처리..... 길이가 맞지 않는 지역을 제거
removeCol

