library('DBI')
library('RJDBC')
library('uuid')


# 获取JDBC连接，连接数据库
drv <- JDBC("oracle.jdbc.driver.OracleDriver", "E:/Program/R/Quant/ojdbc6.jar", identifier.quote="\"") 
conn <- dbConnect(drv, "jdbc:oracle:thin:@127.0.0.1:1521:orcl", "scott", "tiger")

# 指定查询的表
# BYS600332 GXCM300251 LSW300104 LXHG000830 PAYH000001  
# SHLG600848 TCL000100 XYYH601166 YLGF600887 ZGYH601988
# 以上的五个表名用于替换tableName2
tableName1 <- "SZZS000001"
# tableName2 <- "ZGYH601988"

# 指定查询的类型，open 开盘，close 收盘
type <- "open"  

tableName2List <- c("BYS600332", "GXCM300251", "LSW300104", "LXHG000830", "PAYH000001",  
                    "SHLG600848", "TCL000100", "XYYH601166", "YLGF600887", "ZGYH601988")
for (i in tableName2List){
    a <- relevance(tableName1, i, type)
    # cat (a[1], a[2], a[3], a[4], a[5], 
    #      as.numeric(a[6]), as.numeric(a[7]), as.numeric(a[8]), as.numeric(a[9]), "\n")
    # cat (mode(a[1]), mode(a[2]), mode(a[3]), mode(a[4]), mode(a[5]), 
    #      mode(as.numeric(a[6])), mode(as.numeric(a[7])), 
    #      mode(as.numeric(a[8])), mode(as.numeric(a[9])), "\n")
    sql <- paste("insert into relevance",
                 "(UUID, CODE, NAME, TYPE, TABLENAME, CA, CORPEARSON, CORSPEARMAN, CORKENDALL) ",
                 "values(",
                 "'", a[1], "'", "," ,
                 "'", a[2], "'", "," , 
                 "'", a[3], "'", "," , 
                 "'", a[4], "'", "," , 
                 "'", a[5], "'", "," ,  
                 as.numeric(a[6]), ",", as.numeric(a[7]), ",", as.numeric(a[8]), ",", as.numeric(a[9]), " ", 
                 ")", sep="")
    print(sql)
    dbSendUpdate(conn, sql)
    dbSendUpdate(conn, "commit")
}


# 相关性分析函数
relevance <- function(tableName1, tableName2, type) {
    # 拼接查询的语句
    sqlStr <- paste("select t1.", type, " a1, ", "t2.",
                    type, " a2 ", "from ", tableName1, 
                    " t1,", tableName2, " t2 ",
                    "where t1.mydate=t2.mydate and t2.open is not null order by t1.mydate asc", sep="")
    
    # 承接查询结果
    raw <- dbGetQuery(conn, sqlStr)  
    
    # 典型相关性分析
    ca <- cancor(raw$A1, raw$A2)
    # 皮尔逊相关系数
    corPearson <- cor(raw$A1, raw$A2, method = "pearson") 
    # 秩相关系数
    corSpearman <- cor(raw$A1, raw$A2, method = "spearman") 
    # 和谐系数
    corKendall <- cor(raw$A1, raw$A2, method = "kendall") 
    
    code <- dbGetQuery(conn, paste("select distinct code from ", tableName2))
    name <- dbGetQuery(conn, paste("select distinct name from ", tableName2))
    
    # cat (code$CODE, name$NAME, type, ca$cor[1], corPearson[1], corSpearman[1], corKendall[1], "\n")
    
    result <- c(UUIDgenerate(), code$CODE, name$NAME, type, tableName2, 
                round(ca$cor[1], 4), round(corPearson[1], 4), 
                round(corSpearman[1], 4),  round(corKendall[1], 4))
    
    return (result)
    
}



