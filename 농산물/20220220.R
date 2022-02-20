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
day.pig<- day.pig[! day.pig$name %in% removeCol,]
table(day.pig$name)

# day.pig 지역,일자별로 평균가격을 구해
pig.region.daily.mean<- ddply(day.pig, .(name,region,date), summarise, mean.pirce = mean(price))
head(pig.region.daily.mean)

# date에서 month만 추출 지역. 월별 돼지고기 평균
str_sub( pig.region.daily.mean$date, 1,7)

pig.region.monthly.mean<- ddply(pig.region.daily.mean,
      .(name,region, month = str_sub( pig.region.daily.mean$date, 1,7)),
      summarise, mean.pirce = mean(mean.pirce)
      )
head(pig.region.daily.mean)
head(pig.region.monthly.mean)

pig.region.yearly.mean<-ddply(pig.region.daily.mean,
  .(name,region, year = str_sub( pig.region.daily.mean$date, 1,4)),
  summarise, mean.pirce = mean(mean.pirce)
)
head(pig.region.yearly.mean)
############################# 데이터 가공 끝 ##################

#### 시각화 ###########

#1. 월별 돼지고기 가격
pig.region.monthly.mean$month<- as.Date(as.yearmon(pig.region.monthly.mean$month, "%Y-%m"))
# as.yearmon : factor타입의 데이터를 월별 시계열로 변환
# as.Date : 변환된 시계열데이터를 date타입으로 변환
colnames(pig.region.monthly.mean)
ggplot(pig.region.monthly.mean, aes(x=month,y=mean.pirce,colour = name,group=name)) +
  geom_line() +theme_bw() + geom_point(size=6,shape=20,alpha=0.5)+
  ylab("돼지고기 가격") + xlab("")

# 돼지고기의 연평균 가격
ggplot(pig.region.yearly.mean, aes(x=year,y=mean.pirce,colour = name,group=name)) +
  geom_line() +theme_bw() + geom_point(size=6,shape=20,alpha=0.5)+
  ylab("돼지고기 가격") + xlab("")

# 연평균 가격의 변화를 막대그래프로 
ggplot(pig.region.yearly.mean, aes(x=name,y=mean.pirce,fill=factor(year))) +
  theme_bw() + geom_bar(stat = "identity",position = 'dodge', colour = "white") +
  ylab("돼지고기 가격") + xlab("")


# boxplot
ggplot(pig.region.monthly.mean, aes(x=name,y=mean.pirce,fill=name)) +
  theme_bw() + geom_boxplot() +
  ylab("돼지고기 가격") + xlab("")


# 연도별 가격 분포
ggplot(pig.region.yearly.mean, aes(x=name,y=mean.pirce,fill=name)) +
  theme_bw() + geom_boxplot() + facet_wrap(~year, scales='fixed')+
  ylab("돼지고기 가격") + xlab("") + theme(axis.text.x = element_text(size=9))

# 한번에 가격변화를 비교하기 어려우므로..... 도시들간의 상관관계를 분석
# 상관관계가 높은 도시들 묶어서 가격 변화를 보자

temp<- dlply( pig.region.daily.mean, .(name), summarise, mean.pirce)
head(temp)
pig.region<- data.frame(서울 = unlist(temp$서울),
             부산 = unlist(temp$부산),
             대구 = unlist(temp$대구),
             인천 = unlist(temp$인천),
             광주 = unlist(temp$광주),
             대전 = unlist(temp$대전),
             울산 = unlist(temp$울산),
             수원 = unlist(temp$수원),
             청주 = unlist(temp$청주),
             전주 = unlist(temp$전주),
             제주 = unlist(temp$제주)
             )

