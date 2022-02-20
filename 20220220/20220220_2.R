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

