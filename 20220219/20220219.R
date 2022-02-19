#평균과 분산이 일정하지 - >일정
# 1 데이터 셋 가져오기
data("AirPassengers")

# 2 차분적용 - 평균을 정상화
par(mfrow=c(1,2))
ts.plot(AirPassengers)
#plot(AirPassengers)
diff<- diff(AirPassengers)
plot(diff)
summary(AirPassengers)
head(AirPassengers)
summary(diff)
head(diff)

#3 로그적용
ts.plot(AirPassengers)
log<- diff(log(AirPassengers))
plot(log)


# 데이터셋 -> 차분(평균정상화) if 기울기가 있다 ->분산을 정상화(log)

# 추세선 또는 추세형태의 시계열.. 
# 추세 -> 일정한 방향으로 나가는경향.....  주식시장, 판매예측
# 평균, 분산
# 1. 데이터셋 확보
data("WWWusage")  # 사용시간을 분단위로 측정
str(WWWusage)
length(WWWusage)
WWWusage[1:10]
par(mfrow=c(1,1))
plot(WWWusage,type='l',col='red')

# 다중 시계열로 시각화
data("EuStockMarkets")
head(EuStockMarkets)
str(EuStockMarkets)
EuStockMarkets[,'DAX']
eustack<-data.frame(EuStockMarkets)
head(eustack)
plot(eustack$DAX[1:1000],type='l')
colnames(eustack)
# data frame에서 원하는 컬럼들의 정보를 출력?

plot.ts(cbind ( eustack$DAX[1:1000],eustack$SMI[1:1000] ),
     type='l',main='주가지수 추세선')

# 시계열 요소분해
# 36
data<-sample(40,90,36)

#시계열 자료 생성
tsdata<-ts(data,start = c(2016,1),frequency = 12)
tsdata

# 추세선 확인 : 시계열 데이터를  시각화화
ts.plot(tsdata)

# 분해
plot(stl(tsdata,"periodic"))
 
# 변동요인 제거
m<-decompose(tsdata)
plot(m)

# 불규칙 요인제거
plot(tsdata-m$trend)
plot(tsdata-m$trend - m$seasonal)
