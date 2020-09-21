install.packages('xts')
install.packages('zoo')
install.packages('TTR')
install.packages('quantmod')
library(xts)
library(zoo)
library(TTR)
library(quantmod)


stock1<-getSymbols("601318.ss", src='yahoo',from="2013-01-01", to=Sys.Date(), auto.assign=F)
class(stock1)
names(stock1)
head(stock1)

names(stock1) = c('open', 'high', 'low', 'close', 'volume', 'adjusted')
head(stock1)

stock1df = as.data.frame(stock1)
head(stock1df) 
stock1df['2019-09-09',]

dateArea <- function(sDate = sDate, 
                     eDate = Sys.Date()){
  if(class(sDate)=='character') sDate = as.Date(sDate)
  if(class(eDate)=='character') eDate = as.Date(eDate)
  return(paste(sDate, eDate, sep= '/'))
}

ma <- function(cdata, mas = c(5, 20, 60)){
  if(nrow(cdata) <= 60) return(NULL)
  ldata <- cdata
  for(m in mas){
    ldata <- merge(ldata, SMA(cdata, m))
  }
  names(ldata) <- c('value', paste('ma', mas, sep = ''))
  return(ldata)
}

sDate <- as.Date('2018-01-01')
eDate <- Sys.Date()

cdata <- stock1[dateArea(sDate, eDate)]$close
head(cdata)
ldata = NULL
ldata <- ma(cdata, c(5, 20, 60))
tail(ldata)

plot(ldata)

#多头排列买入
buypoint <- function(ldata){
  idx <- which(ldata$ma5>ldata$ma20 & ldata$ma20>ldata$ma60)
  return(ldata[idx,])
}
buypoint <- buypoint(ldata)
head(buypoint)

#空头排列卖出
sellpoint <- function(ldata){
  idx <- which(ldata$ma5<ldata$ma20 & ldata$ma20<ldata$ma60)
  return(ldata[idx,])
}
sellpoint <- sellpoint(ldata)
head(sellpoint)


bsdata <- merge(buypoint$value, sellpoint$value)
head(bsdata)
names(bsdata) = c('buy', 'sell')
head(bsdata)


signal <- function(buy, sell){
  selldf <- data.frame(sell, op = as.character(rep('S', nrow(sell))))
  buydf <- data.frame(buy, op = as.character(rep('B', nrow(buy))))
  sdata <- rbind(buydf, selldf)
  sdata[order(as.Date(row.names(sdata))),] #order按索引排序
}

sdata <- signal(buypoint, sellpoint)
View(sdata)


trade <- function(sdata, capital = 10000,fee_ratio=0.001){
  amount = 0 #持有股票数
  cash = capital #所持现金
  ticks <- data.frame()
  
  for(i in 1:nrow(sdata)){
    row <- sdata[i,]
    if(row$op == 'B'){
      amount0<- floor((1-fee_ratio)*cash/row$value)  
      amount <- amount + amount0
      cash <- cash - amount0*row$value*(1+fee_ratio)
    }
    if(row$op =='S'){
      cash <- cash+amount*row$value*(1-fee_ratio)
      amount = 0
    }
    row$cash <- round(cash,2)
    row$amount <- amount
    row$asset <- round(cash+amount*row$value, 2)
    ticks <- rbind(ticks, row)
  }
  
  ticks$diff <- c(0, round(diff(ticks$asset), 2)) #由于差分，向量的长度缩小了一个，需要增加一个向量单元
  rise <- ticks[intersect(which(ticks$diff >0), which(ticks$op =='S')),]
  fall <- ticks[intersect(which(ticks$diff <0), which(ticks$op =='S')),]
  return(list(ticks = ticks, rise = rise, fall = fall))
}

Return <- trade(sdata)
View(Return$ticks)

tick <- Return$ticks
head(tick)
#年化收益
date<-row.names(tick)
sday<-as.Date(date[1])
eday<-as.Date(date[length(date)])
eday-sday #相隔890天
res<-tick$asset
res
end<-as.numeric(res[length(res)])
begin<-as.numeric(res[length(1)])
yield_year<-end/begin-1
time_span_year<-890/365;
yieldYear<-(yield_year+1)^(1/time_span_year)-1 #年化收益
yieldYear

out <- zoo(tick[,'asset'], as.Date(row.names(tick)))
out <- as.xts(out)
names(out) = 'asset'

#作图
par(mfrow=c(2,1))
plot(ldata[,'value'])
points(buypoint[,'value'],col='red')
points(sellpoint[,'value'],col='green')

plot(out)







