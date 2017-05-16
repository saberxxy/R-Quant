library(RODBC)

ch <- odbcConnect("mysql", uid="root", pwd="root")
# sqlTables(ch)  #查看所有表


rateDvalue <- sqlQuery(ch, "select * from rate_d_value")  #查询收益率数据
# print (a$index)
# print (mode(rateDvalue))
odbcClose(ch)

#将242个交易日的数据去掉最后两个，分为十组，每组24个
#相关性检测1, 2, 3, 5, 8

#上证指数
org_000001 <- rateDvalue$org_000001[1:240]
org_000001Matrix <- matrix(org_000001, nrow=10, byrow=TRUE)

#1, 2两个时间片
#绘图
plot(org_000001Matrix[1,], org_000001Matrix[2,], type='p', col='red', lwd=6)
#典型相关分析
c12_1 <- cancor(org_000001Matrix[1,], org_000001Matrix[2,])
#皮尔逊相关系数
c12_2 <- cor(org_000001Matrix[1,], org_000001Matrix[2,], method = "pearson")
#秩相关系数
c12_3 <- cor(org_000001Matrix[1,], org_000001Matrix[2,], method = "spearman")
#和谐系数
c12_4 <- cor(org_000001Matrix[1,], org_000001Matrix[2,], method = "kendall")

#1, 3两个时间片
#绘图
plot(org_000001Matrix[1,], org_000001Matrix[3,], type='p', col='red', lwd=6)
#典型相关分析
c13_1 <- cancor(org_000001Matrix[1,], org_000001Matrix[3,])
#皮尔逊相关系数
c13_2 <- cor(org_000001Matrix[1,], org_000001Matrix[3,], method = "pearson")
#秩相关系数
c13_3 <- cor(org_000001Matrix[1,], org_000001Matrix[3,], method = "spearman")
#和谐系数
c13_4 <- cor(org_000001Matrix[1,], org_000001Matrix[3,], method = "kendall")

#1, 5两个时间片
#绘图
plot(org_000001Matrix[1,], org_000001Matrix[5,], type='p', col='red', lwd=6)
#典型相关分析
c15_1 <- cancor(org_000001Matrix[1,], org_000001Matrix[5,])
#皮尔逊相关系数
c15_2 <- cor(org_000001Matrix[1,], org_000001Matrix[5,], method = "pearson")
#秩相关系数
c15_3 <- cor(org_000001Matrix[1,], org_000001Matrix[5,], method = "spearman")
#和谐系数
c15_4 <- cor(org_000001Matrix[1,], org_000001Matrix[5,], method = "kendall")

#1, 8两个时间片
#绘图
plot(org_000001Matrix[1,], org_000001Matrix[8,], type='p', col='red', lwd=6)
#典型相关分析
c18_1 <- cancor(org_000001Matrix[1,], org_000001Matrix[8,])
#皮尔逊相关系数
c18_2 <- cor(org_000001Matrix[1,], org_000001Matrix[8,], method = "pearson")
#秩相关系数
c18_3 <- cor(org_000001Matrix[1,], org_000001Matrix[8,], method = "spearman")
#和谐系数
c18_4 <- cor(org_000001Matrix[1,], org_000001Matrix[8,], method = "kendall")

#2, 3两个时间片
#绘图
plot(org_000001Matrix[2,], org_000001Matrix[3,], type='p', col='red', lwd=6)
#典型相关分析
c23_1 <- cancor(org_000001Matrix[2,], org_000001Matrix[3,])
#皮尔逊相关系数
c23_2 <- cor(org_000001Matrix[2,], org_000001Matrix[3,], method = "pearson")
#秩相关系数
c23_3 <- cor(org_000001Matrix[2,], org_000001Matrix[3,], method = "spearman")
#和谐系数
c23_4 <- cor(org_000001Matrix[2,], org_000001Matrix[3,], method = "kendall")

#2, 5两个时间片
#绘图
plot(org_000001Matrix[2,], org_000001Matrix[5,], type='p', col='red', lwd=6)
#典型相关分析
c25_1 <- cancor(org_000001Matrix[2,], org_000001Matrix[5,])
#皮尔逊相关系数
c25_2 <- cor(org_000001Matrix[2,], org_000001Matrix[5,], method = "pearson")
#秩相关系数
c25_3 <- cor(org_000001Matrix[2,], org_000001Matrix[5,], method = "spearman")
#和谐系数
c25_4 <- cor(org_000001Matrix[2,], org_000001Matrix[5,], method = "kendall")

#2, 8两个时间片
#绘图
plot(org_000001Matrix[2,], org_000001Matrix[8,], type='p', col='red', lwd=6)
#典型相关分析
c28_1 <- cancor(org_000001Matrix[2,], org_000001Matrix[8,])
#皮尔逊相关系数
c28_2 <- cor(org_000001Matrix[2,], org_000001Matrix[8,], method = "pearson")
#秩相关系数
c28_3 <- cor(org_000001Matrix[2,], org_000001Matrix[8,], method = "spearman")
#和谐系数
c28_4 <- cor(org_000001Matrix[2,], org_000001Matrix[8,], method = "kendall")

#3, 5两个时间片
#绘图
plot(org_000001Matrix[3,], org_000001Matrix[5,], type='p', col='red', lwd=6)
#典型相关分析
c35_1 <- cancor(org_000001Matrix[3,], org_000001Matrix[5,])
#皮尔逊相关系数
c35_2 <- cor(org_000001Matrix[3,], org_000001Matrix[5,], method = "pearson")
#秩相关系数
c35_3 <- cor(org_000001Matrix[3,], org_000001Matrix[5,], method = "spearman")
#和谐系数
c35_4 <- cor(org_000001Matrix[3,], org_000001Matrix[5,], method = "kendall")

#3, 8两个时间片
#绘图
plot(org_000001Matrix[3,], org_000001Matrix[8,], type='p', col='red', lwd=6)
#典型相关分析
c38_1 <- cancor(org_000001Matrix[3,], org_000001Matrix[8,])
#皮尔逊相关系数
c38_2 <- cor(org_000001Matrix[3,], org_000001Matrix[8,], method = "pearson")
#秩相关系数
c38_3 <- cor(org_000001Matrix[3,], org_000001Matrix[8,], method = "spearman")
#和谐系数
c38_4 <- cor(org_000001Matrix[3,], org_000001Matrix[8,], method = "kendall")

#5, 8两个时间片
#绘图
plot(org_000001Matrix[5,], org_000001Matrix[8,], type='p', col='red', lwd=6)
#典型相关分析
c58_1 <- cancor(org_000001Matrix[5,], org_000001Matrix[8,])
#皮尔逊相关系数
c58_2 <- cor(org_000001Matrix[5,], org_000001Matrix[8,], method = "pearson")
#秩相关系数
c58_3 <- cor(org_000001Matrix[5,], org_000001Matrix[8,], method = "spearman")
#和谐系数
c58_4 <- cor(org_000001Matrix[5,], org_000001Matrix[8,], method = "kendall")

#典型相关分析
cat (c12_1$cor, c13_1$cor, c15_1$cor, c18_1$cor, c23_1$cor, 
     c25_1$cor, c28_1$cor, c35_1$cor, c38_1$cor, c58_1$cor)
#皮尔逊相关系数
cat (abs(c12_2), abs(c13_2), abs(c15_2), abs(c18_2), abs(c23_2), 
     abs(c25_2), abs(c28_2), abs(c35_2), abs(c38_2), abs(c58_2))
#秩相关系数
cat (abs(c12_3), abs(c13_3), abs(c15_3), abs(c18_3), abs(c23_3), 
     abs(c25_3), abs(c28_3), abs(c35_3), abs(c38_3), abs(c58_3))
#和谐系数
cat (abs(c12_4), abs(c13_4), abs(c15_4), abs(c18_4), abs(c23_4), 
     abs(c25_4), abs(c28_4), abs(c35_4), abs(c38_4), abs(c58_4))

cResult <- c(c12_1$cor, c13_1$cor, c15_1$cor, c18_1$cor, c23_1$cor, 
             c25_1$cor, c28_1$cor, c35_1$cor, c38_1$cor, c58_1$cor,
             abs(c12_2), abs(c13_2), abs(c15_2), abs(c18_2), abs(c23_2), 
             abs(c25_2), abs(c28_2), abs(c35_2), abs(c38_2), abs(c58_2),
             abs(c12_3), abs(c13_3), abs(c15_3), abs(c18_3), abs(c23_3), 
             abs(c25_3), abs(c28_3), abs(c35_3), abs(c38_3), abs(c58_3),
             abs(c12_4), abs(c13_4), abs(c15_4), abs(c18_4), abs(c23_4), 
             abs(c25_4), abs(c28_4), abs(c35_4), abs(c38_4), abs(c58_4))

for (i in cResult) {
    if (i>0.3 && i<0.8) {
        print(i)
    }
}

cat (c12_1$cor, abs(c12_2), abs(c12_3), abs(c12_4))
