library('DBI')
library('RJDBC')
library('uuid')
library('ggplot2')
library('sqldf')


# 获取JDBC连接，连接数据库
drv <- JDBC("oracle.jdbc.driver.OracleDriver", "E:/Program/R/Quant/ojdbc6.jar", identifier.quote="\"") 
conn <- dbConnect(drv, "jdbc:oracle:thin:@127.0.0.1:1521:orcl", "scott", "tiger")

print(conn)

raw1 <- dbGetQuery(conn, 'select NAME, CODE, TYPE, CA, CORPEARSON, CORSPEARMAN, CORKENDALL from relevance')  
raw1 <- as.data.frame(raw1)

# 开盘价与指数的相关性分析
raw1[which(raw1$TYPE=="open"),]
openCa <- as.data.frame(subset(raw1, raw1$TYPE=="open", select=c("NAME", "CA", "CORPEARSON", "CORSPEARMAN", "CORKENDALL")))

openCaName <- rep(openCa$NAME, each = 4)
openCaType <- rep(names(openCa[-1]), times = 10)
openCaValue <- c()
for (i in 1:dim(openCa[1])){
    openCaValue <- c(openCaValue, openCa[i,-1])
}
openCaValue <- as.numeric(openCaValue)
openCaPlot <- data.frame(openCaName=openCaName, openCaType=openCaType, openCaValue=openCaValue)

ggOpenCa <- ggplot(data=openCaPlot, mapping=aes(x=factor(openCaName), y=openCaValue, fill=openCaType))+geom_bar(stat = 'identity',  position = 'dodge')
ggOpenCa

# closeR <- sqldf("select CODE, TYPE, CA, CORPEARSON, CORSPEARMAN, CORKENDALL from raw1 where type='close'")
# 
# ggClose <- ggplot(data=NULL, aes(x=closeR$CODE, y=closeR$CA))+geom_point(color="red")

# 收盘价与指数的相关性分析
raw1[which(raw1$TYPE=="close"),]
closeCa <- as.data.frame(subset(raw1, raw1$TYPE=="close", select=c("NAME", "CA", "CORPEARSON", "CORSPEARMAN", "CORKENDALL")))

closeCaName <- rep(closeCa$NAME, each = 4)
closeCaType <- rep(names(closeCa[-1]), times = 10)
closeCaValue <- c()
for (i in 1:dim(closeCa[1])){
    closeCaValue <- c(closeCaValue, closeCa[i,-1])
}
closeCaValue <- as.numeric(closeCaValue)
closeCaPlot <- data.frame(closeCaName=closeCaName, closeCaType=closeCaType, closeCaValue=closeCaValue)

ggCloseCa <- ggplot(data=closeCaPlot, mapping=aes(x=factor(closeCaName), y=closeCaValue, fill=closeCaType))+geom_bar(stat = 'identity',  position = 'dodge')
ggCloseCa

