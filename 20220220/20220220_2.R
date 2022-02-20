# 클러스터링.. 분석
# 클러스터링 기법에 대한 자세한 내용은 논문 
#TSclust: An R Package for Time Series Clustering (Journal of #Statistical Software, 2014, Pablo Montero 와 Jose A. Vilar)을 참고

installlib = c('plyr','zoo','ggplot2')
unlist(lapply(installlib,require,character.only = true() ))
install.packages("TSclust")
library(TSclust)

pig.region<- read.csv("https://raw.githubusercontent.com/kyuyounglee/R_Programing_big_13th/main/data/pig.region.csv",
         header = T)
pig.region = pig.region[,-1]
head(pig.region)

pig.region.monthly.mean<- read.csv("https://raw.githubusercontent.com/kyuyounglee/R_Programing_big_13th/main/data/pig.region.monthly.mean.csv",
         header = T)
pig.region.monthly.mean = pig.region.monthly.mean[,-1]
head(pig.region.monthly.mean)

date.item.mean<- read.csv("https://raw.githubusercontent.com/kyuyounglee/R_Programing_big_13th/main/data/date.item.mean.csv",
         header = T)
date.item.mean = date.item.mean[,-1]
head(date.item.mean)

month.item.mean<- read.csv("https://raw.githubusercontent.com/kyuyounglee/R_Programing_big_13th/main/data/month.item.mean.csv",
         header = T)
month.item.mean = month.item.mean[,-1]
head(month.item.mean)

# 데이터 가공
# 농산물 간의 군집분석...  축산물은 제외하고 농산물 데이터만 추출
head(code)
temp<-names(table(product[product$category %in% c(100,200,300,400),]['item']))
code[code$분류코드 %in% temp,]$분류코드설명  # 축산물

temp<-dlply(date.item.mean, .(name), summarise, mean.price)
farm.product<- data.frame(
  쌀 = unlist(temp$쌀),배추 = unlist(temp$배추),상추 = unlist(temp$상추),
  호박 = unlist(temp$호박),양파 = unlist(temp$양파),파프리카 = unlist(temp$파프리카),
  참깨  = unlist(temp$참깨 ),사과 = unlist(temp$사과)
  )
head(farm.product)
# 농산물에 대한 군집분석
plot(hclust(diss(farm.product,'COR')),axes = F, ann =F)
# axes = F 모든축을 출력하지 않도록
# ann =F 모든 축에 대한 설명을 출력하지 않도록


# 상추 호박의 시계열 시각화
str(month.item.mean)
month.item.mean$month <- as.Date( as.yearmon(month.item.mean$month, '%Y-%m') )

month.item.mean.01<- subset(month.item.mean, month.item.mean$name %in% c("상추","호박"))

ggplot(month.item.mean.01,aes(x=month,y=mean.price,colour=name,group=name) )+
  geom_line()+ylab("가격") + xlab("")

# 돼지고기 자료에 대한 군집 분석
head(pig.region)
plot(hclust(diss(pig.region,'COR')),axes = F, ann =F)

pig.region.monthly.mean$month <- as.Date(as.yearmon(pig.region.monthly.mean$month,'%Y-%m'))

# 대구,광주  부산,울산
code[code$분류코드설명 %in% c("대구","광주"),]
temp<-subset(pig.region.monthly.mean, pig.region.monthly.mean$region %in% c(2200,2401))
ggplot(temp,aes(x=month,y=mean.pirce,colour=name,group=name) )+
  geom_line()+ylab("가격") + xlab("")

code[code$분류코드설명 %in% c("부산","울산"),]
temp<-subset(pig.region.monthly.mean, pig.region.monthly.mean$region %in% c(2100,2601))
ggplot(temp,aes(x=month,y=mean.pirce,colour=name,group=name) )+
  geom_line()+ylab("가격") + xlab("")

code[code$분류코드설명 %in% c("대구","부산"),]
temp<-subset(pig.region.monthly.mean, pig.region.monthly.mean$region %in% c(2100,2200))
ggplot(temp,aes(x=month,y=mean.pirce,colour=name,group=name) )+
  geom_line()+ylab("가격") + xlab("")


# http://news.kmib.co.kr/article/view.asp?arcid=0004564454&code=11151100
# http://blog.daum.net/sun6377/5061880


weather<-read.csv('weather.csv',header = T)
weather<- read.csv("https://raw.githubusercontent.com/kyuyounglee/R_Programing_big_13th/main/data/weather.csv"
                   , header = T
                   )
head(weather)

subset(code, 구분코드설명 == '지역코드')
category<- subset(code, 구분코드설명 == '품목코드')
colnames(category) = c('code','exp','item','name')

# 서울지역(1101)의 가격만 추출  품목,일자별로 평균 가격구하고
# 품목에대한 데이터인 category와 merge 해서 새로운 데이터 생성

seoul.item<- ddply(subset(product,product$region == 1101), .(item,date), summarise, 
      mean.price = mean(price) ) %>% merge(category, by="item", all=T)
head(seoul.item)
seoul.item.mean<- seoul.item[,-c(4,5)]

# 기상 데이터 가공
str(weather)
colnames(weather)<-c('region','category','value','date')
head(weather)
region.weather<- dlply(weather, .(region))
names(region.weather)
head(region.weather[[41]])
table(region.weather[[41]]$category)

# 분석대상 서울의 강수량
init.seoul.rain<-region.weather[[41]] %>% 
  subset(region.weather[[41]]$category == '강수량')

head(init.seoul.rain)
table(init.seoul.rain$date)
# 날짜순으로 정렬
sort.seoul.rain<-dlply(init.seoul.rain, .(date))
head(sort.seoul.rain)

# 일별로 데이터가 2개씩 중복 되어 있으므로 하나만 선택한다
resort.seoul.rain<- lapply(1:length(sort.seoul.rain), function(x) sort.seoul.rain[[x]][1,])
head(resort.seoul.rain)

# 반복문을 이용해서 value date만 추출한후 data frame 변환
seoul.rain<- data.frame(
  rain =unlist(lapply(1:length(resort.seoul.rain),function(x) resort.seoul.rain[[x]][,3])),
  date =unlist(lapply(1:length(resort.seoul.rain),function(x) resort.seoul.rain[[x]][,4]))
)
head(seoul.rain)
# 결측치를 저리   0으로 
seoul.rain[is.na(seoul.rain),][1]<-0
head(seoul.rain)

#농산물 데이터가 2011 ~ 2013
# 기상데이터는 2010 ~ 2014

seoul.item.rain<- merge(seoul.rain,seoul.item.mean, by="date", all=T)
head(seoul.item.rain)
seoul.item.rain<- na.omit(seoul.item.rain)
head(seoul.item.rain)

# 시각화
# 2011 ~ 2013 서울의 강수량 변화에 따른 상추가격을 시각화 

par(mar = c(3,5,3,5))  # 현재 그래픽장치의 그래픽 parameter의 텍스트 라인을 수정 순서는
# bottom-left-top-right

temp1<- seoul.item.rain %>% subset(seoul.item.rain$name=="호박")
plot(as.Date(temp1$date), temp1$mean.price,
     type="l",col="blue", xlab="",ylab="",ylim=c(0,4000))
mtext("가격",size=2,line=3)

par(new=TRUE)
temp2<- seoul.item.rain %>% subset(seoul.item.rain$name=="상추")
plot(as.Date(temp2$date), temp2$mean.price,
     type="l",col="green", xlab="",ylab="",ylim=c(0,4000),axes=FALSE)

par(new=TRUE)
temp3<- seoul.item.rain %>% subset(seoul.item.rain$name=="상추")
plot(as.Date(temp3$date), temp3$rain,
     type="l",col="red", xlab="",ylab="",ylim=c(0,400),axes=FALSE)
axis(4,ylim=c(0,400), col.axis = 'red',las=3)
mtext("강수량",size=4,line=3)
