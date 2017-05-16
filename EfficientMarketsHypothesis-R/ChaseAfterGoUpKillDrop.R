# 追涨杀跌模型研究

library(quantmod)
library(plyr)
library(xts)
library(TTR)
library(ggplot2)
library(scales)
library(reshape2)


# 数据采集及清洗
sDate <- as.Date("2015-01-01")                        
eDate <- as.Date("2015-08-21") 
lsw <- getSymbols('300104.SZ', from=sDate, to=eDate, 
                  src = "yahoo", auto.assign = FALSE)

# 对于list数据，index函数获取其index列，其余列可用coredata函数获得
# head(coredata(lsw))
lsw_time <- index(lsw)  # 时间序列
lsw_open <- coredata(lsw[,1])  # 开盘
lsw_high <- coredata(lsw[,2])  # 最高价
lsw_close <- coredata(lsw[,4])  # 收盘价
lsw_low <- coredata(lsw[,3])  # 最低价
lsw_volume <- coredata(lsw[,5])  # 成交量

lsw_data <- data.frame(lsw_time, lsw_open, lsw_high, lsw_close, lsw_low,lsw_volume,
                       stringsAsFactors=FALSE)
names(lsw_data) <- c('lsw_time', 'lsw_open', 'lsw_high',
                     'lsw_close', 'lsw_low', 'lsw_volume')

# 数据预处理
title <- '300104.SZ'
# R中dataframe的row.names与Python中datafram中的index相等
cdata <- data.frame(Time=lsw_data$lsw_time, Value=lsw_data$lsw_close)  # 获得收盘价
cdata$Time <- as.Date(cdata$Time, "%Y-%m-%d") 
tail(cdata)
vdata <- data.frame(Time=lsw_data$lsw_time, Volume=lsw_data$lsw_volume)  # 获得交易量
tail(vdata)


# 计算最近20日的最高价和10日的最低价
minmax <- function(data, max=20, min=10){
    d1 <- na.locf(data, fromLast=TRUE)
    d2 <- cbind(d1, min=runMin(d1,min), max=runMax(d1,max))
    return(d2)
}

ldata <- minmax(cdata$Value)

ldata <- as.data.frame(ldata)
ldata$Time <- cdata$Time

ldataplot <- ldata
ldataplot[is.na(ldataplot)] <- 0  # 处理空值
ldataplot <- melt(ldataplot, id.vars = 'Time') 

# 画多线图
g <- ggplot(ldataplot, aes(x=Time, y=value)) + geom_line(aes(color=variable))


# 买入信号函数
buyPoint <- function(ldata) {
    idx <- which(ldata[,1] == ldata[,3])
    return(ldata[idx,])                                  
}

buydata <- buyPoint(ldata)
# buydata <- as.data.frame(buydata)
buydataplot <- data.frame(value=buydata[,1], Time=buydata[,4])

# 标出追涨点
g <- ggplot(ldataplot, aes(x=Time, y=value)) + geom_line(aes(color=variable), size=1) + geom_point(data=data.frame(buydataplot), aes(color='buy'), size=2)


sellPoint <- function(ldata, buydata) { 
    
    idx <- which(ldata[,1] == ldata[,2])
    idx <- idx[which(c(0, diff(idx)) != 1)]   # 第一点用0表示
    selldata <- ldata[idx,]               # 所有低于最小值的点 
    idx2 <- c()
    idx2 <- sapply(buydata$Time, function(e){  # 买后的卖点
        head(which(selldata$Time>e), 1)
    })
    for (i in 1:length(unique(idx2))) {
        aa <- c(aa, unique(idx2)[[i]])
    }
    return(selldata[aa,])
}


selldata <- sellPoint(ldata, buydata)
selldataplot <- data.frame(value=selldata[,1], Time=selldata[,4])

# 标出杀跌点
g <- ggplot(ldataplot, aes(x=Time, y=value)) + geom_line(aes(color=variable), size=1) + geom_point(data=data.frame(buydataplot), aes(color='buy'), size=2) + geom_point(data=data.frame(selldataplot), aes(color='sell'), size=2)


# 合并交易信号
signal <- function(buydata, selldata){
    selldf <- data.frame(selldata, op=as.character(rep("S", nrow(selldata))))
    buydf <- data.frame(buydata, op=as.character(rep("B", nrow(buydata))))
    sdata <- rbind(buydf, selldf)  # 交易信号
    sdata[order(as.Date(sdata$Time)),]
}

sdata <- signal(buydata, selldata)

capital <- 1000000  # 总资金
fixMoney <- 10000  # 每次定投资金
amount <- 0
cash <- capital
ticks <- data.frame()

for(i in 1:nrow(sdata)){
    row <- sdata[i,]
    if(row$op=='B'){
        if(cash < fixMoney){
            print(paste(row.names(row), "No enough cash"))
            next
        }
        amount0 <- floor(fixMoney/row$d1) # 本次交易量
        amount <- amount+amount0
        cash <- cash-amount0*row$d1
    }
    if(row$op=='S'){
        cash<-cash+amount*row[, 1]
        amount<-0
    }
    row$cash <- round(cash,2)
    row$amount <- amount
    row$asset <- round(cash+amount*row$d1,2)
    ticks <- rbind(ticks,row)
}

ticks$diff <- c(0,round(diff(ticks$asset),2))
rise <- ticks[intersect(which(ticks$diff>0), which(ticks$op=='S')),]   # 赚钱的交易
fall <- ticks[intersect(which(ticks$diff<0), which(ticks$op=='S')),]   # 赔钱的交易



# ---------------------以下均为试验代码--------------------------------

# 模拟交易
trade <- function(sdata, capital=10000, fixMoney=10000){ # 交易信号，总资金，每次定投资金
    amount<-0
    cash<-capital
    ticks<-data.frame()
    for(i in 1:nrow(sdata)){
        row<-sdata[i,]
        if(row$op=='B'){
            if(cash<fixMoney){
                print(paste(row.names(row),"No enough cash"))
                next
            }
            amount0<-floor(fixMoney/row$Value) # 本次交易量
            amount<-amount+amount0
            cash<-cash-amount0*row$Value
        }
        if(row$op=='S'){
            cash<-cash+amount*row$Value
            amount<-0
        }
        row$cash<-round(cash,2)
        row$amount<-amount
        row$asset<-round(cash+amount*row$Value,2)
        ticks<-rbind(ticks,row)
    }
                    
    ticks$diff<-c(0,round(diff(ticks$asset),2))
                
    rise<-ticks[intersect(which(ticks$diff>0),which(ticks$op=='S')),]   # 赚钱的交易
    fall<-ticks[intersect(which(ticks$diff<0),which(ticks$op=='S')),]   # 赔钱的交易
    
    return(list(ticks=ticks, rise=rise, fall=fall))
}




selldf <- data.frame(selldata, op=as.character(rep("S", nrow(selldata))))
buydf <- data.frame(buydata, op=as.character(rep("B", nrow(buydata))))
sdata <- rbind(buydf, selldf)
sdata[order(as.Date(sdata$Time)),]

idx <- which(ldata[,1] == ldata[,2])
idx <- idx[which(c(0, diff(idx)) != 1)]
selldata <- ldata[idx,]
idx2 <- c()
idx2 <- sapply(buydata$Time, function(e){  # 买后的卖点
    head(which(selldata$Time>e), 1)
})
selldata[unique(idx2),]

sapply(buydata, selldata)



ldata[is.na(ldata)] <- 0
idx <- which(ldata[,1] == ldata[,3])


d1 <- na.locf(cdata$Value, fromLast=TRUE)
min <- 10
max <- 20
d2 <- cbind(d1, min=runMin(d1,min), max=runMax(d1,max))
head(d2)
tail(d2)

min <- runMin(d1,min)
max <- runMax(d1,max)
merge(d1, min)
head(d1, min, max)
length(min)
minmax(cdata)

d1 <- na.locf(cdata$Value, fromLast=TRUE)
d2 <- merge(data.frame(d1), min=runMin(d1,min))

# 画图
drawLine <- function(cdata, title="Stock", sDate=min(cdata$Time), eDate=max(cdata$Time), breaks){
    if(sDate < min(cdata$Time)){
        sDate <- min(cdata$Time)
    }
    if(eDate > max(cdata$Time)){
        eDate <- max(cdata$Time)
    } 
    cdata <- na.omit(cdata)
    g <- ggplot(aes(x=cdata$Time, y=cdata$Value), 
                data=cdata)
    g <- g + geom_line()
    if(ncol(cdata)>1){ # 多条线
        g <- g+geom_line(aes(color='black'),
                         data=cdata)  
    }
    g <- g+scale_x_date(labels=date_format("%Y-%m"),breaks=date_breaks(breaks), limits=c(sDate,eDate))
    g <- g+ylim(min(cdata$Value), max(cdata$Value))
    g <- g+xlab("") + ylab("Price")+ggtitle(title)
    g
}

# 收盘价
drawLine(cdata, title, sDate, eDate, '1 month')
