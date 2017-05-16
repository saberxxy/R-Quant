library(RODBC)
library(TSA)
library(forecast)
library(tseries)

ch <- odbcConnect("mysql", uid="root", pwd="root")

testData <- sqlQuery(ch, "select b_date, b_open, b_high, b_close, b_low, b_change from 000011_深物业a_org 
                     order by b_date asc")  

#print (testData)

plot(testData$b_date, testData$b_open, type="b")
