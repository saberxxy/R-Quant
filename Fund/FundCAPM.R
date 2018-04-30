# 基金收益情况

library(RODBC)
library(dplyr)
library(ggplot2)
library(PerformanceAnalytics)
library(xts)

# 获取指数收盘价
conn <- odbcConnect("oracle", uid="stock", pwd="123456", believeNRows=FALSE)
sse <- sqlQuery(conn, "select sdate, close from stock_000001_sse where sdate between
        to_date('2018-01-01', 'yyyy-MM-dd') and to_date('2018-03-31', 'yyyy-MM-dd') 
        order by sdate asc")
print(head(sse))

# 获取基金净值
# 002001，090013，110022，180012，320010
f_002001 <- read.csv("F:\\Program\\Python\\Quant\\Fund\\002001.csv") %>% arrange(dateList)
f_090013 <- read.csv("F:\\Program\\Python\\Quant\\Fund\\090013.csv") %>% arrange(dateList)
f_110022 <- read.csv("F:\\Program\\Python\\Quant\\Fund\\110022.csv") %>% arrange(dateList)
f_180012 <- read.csv("F:\\Program\\Python\\Quant\\Fund\\180012.csv") %>% arrange(dateList)
f_320010 <- read.csv("F:\\Program\\Python\\Quant\\Fund\\320010.csv") %>% arrange(dateList)

dat <- data.frame(time = f_002001$dateList, f_002001 = f_002001$netvalueList, 
             f_090013 = f_090013$netvalueList, f_110022 = f_110022$netvalueList,
             f_180012 = f_180012$netvalueList, f_320010 = f_320010$netvalueList, 
             sse = sse$CLOSE)

# 计算收益率
# lag()，滞后一日
f_002001_rat <- (((dat$f_002001-lag(dat$f_002001))/dat$f_002001))[-1]
f_090013_rat <- (((dat$f_090013-lag(dat$f_090013))/dat$f_090013))[-1]
f_110022_rat <- (((dat$f_110022-lag(dat$f_110022))/dat$f_110022))[-1]
f_180012_rat <- (((dat$f_180012-lag(dat$f_180012))/dat$f_180012))[-1]
f_320010_rat <- (((dat$f_320010-lag(dat$f_320010))/dat$f_320010))[-1]
sse_rat <- (((dat$sse-lag(dat$sse))/dat$sse))[-1]

# 将收益率年化
dat_rat <- data.frame(row.names = f_002001$dateList[-1], f_002001 = f_002001_rat, 
                  f_090013 = f_090013_rat, f_110022 = f_110022_rat,
                  f_180012 = f_180012_rat, f_320010 = f_320010_rat, sse = sse_rat)
dat_rat_xts <- xts::xts(dat_rat, as.Date(f_002001$dateList[-1]))

# CAPM分析
# 以2018年三年期国债的利率作为无风险收益率4%
Rf <- 0.04/365
# 计算平均年化收益率，标准差，sharpe
results <- table.AnnualizedReturns(dat_rat_xts, Rf=Rf)
print(results)
print(table.Stats(dat_rat_xts))

# 计算Alpha和Beta
CAPM.alpha(dat_rat_xts[,1:5], dat_rat_xts[,6], Rf=Rf)
CAPM.beta(dat_rat_xts[,1:5], dat_rat_xts[,6], Rf=Rf)




