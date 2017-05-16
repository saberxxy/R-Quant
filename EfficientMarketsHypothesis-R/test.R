# library(RMySQL) 
# library(DBI)
# 
# conn <- dbConnect(MySQL(), host="localhost", dbname="test", user="root", password="root")  
# 
# name <- c("张三","李四","王五")
# print (mode(name))
# 
# user <- data.frame(id=1:3, name, age=c(20,22,25))
# 
# print (mode(user$name))
# 
# dbSendQuery(conn, 'SET NAMES gbk')
# dbWriteTable(conn,"user", user, overwrite=TRUE )
# dbReadTable(conn, "user")

library(RODBC)

ch <- odbcConnect("mysql", uid="root", pwd="root")
sqlTables(ch) # 查看所有表

sqlQuery(ch, "insert into user values(9,'归一',99)") # 增
sqlQuery(ch, "delete from user where id=8") # 删
sqlQuery(ch, "update user set name='小九' where id=5") # 改
sqlQuery(ch, "select * from user") # 查
sqlFetch(ch, "user") # 查
sqlDrop(ch, "USArrests") # 删除表
sqlTables(ch)
odbcClose(ch)

