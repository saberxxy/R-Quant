library(RODBC)

ch <- odbcConnect("mysql", uid="root", pwd="root")
# sqlTables(ch)  #查看所有表


rateDvalue <- sqlQuery(ch, "select * from rate_d_value")  #查询收益率数据
# print (a$index)
# print (mode(rateDvalue))
odbcClose(ch)

x <- rateDvalue$org_000001  #指数收益率减去无风险收益率，利率3.5
beta <- c()

y000011 <- rateDvalue$org_000011  #深物业a
fitting000011 <- lm(y000011 ~ x)
alpha000011 <- fitting000011$coefficients[1]  #截距
beta000011 <- fitting000011$coefficients[2]  #斜率
# cat (alpha000011, beta000011)
beta <- c(beta, beta000011=beta000011)

y000070 <- rateDvalue$org_000070  #特发信息
fitting000070 <- lm(y000070 ~ x)
alpha000070 <- fitting000070$coefficients[1]  #截距
beta000070 <- fitting000070$coefficients[2]  #斜率
# cat (alpha000070, beta000070)
beta <- c(beta, beta000070=beta000070)

y000099 <- rateDvalue$org_000099  #中信海直
fitting000099 <- lm(y000099 ~ x)
alpha000099 <- fitting000099$coefficients[1]  #截距
beta000099 <- fitting000099$coefficients[2]  #斜率
# cat (alpha000099, beta000099)
beta <- c(beta, beta000099=beta000099)

y000518 <- rateDvalue$org_000518  #四环生物
fitting000518 <- lm(y000518 ~ x)
alpha000518 <- fitting000518$coefficients[1]  #截距
beta000518 <- fitting000518$coefficients[2]  #斜率
# cat (alpha000518, beta000518)
beta <- c(beta, beta000518=beta000518)

y000554 <- rateDvalue$org_000554  #泰山石油
fitting000554 <- lm(y000554 ~ x)
alpha000554 <- fitting000554$coefficients[1]  #截距
beta000554 <- fitting000554$coefficients[2]  #斜率
# cat (alpha000554, beta000554)
beta <- c(beta, beta000554=beta000554)

y000554 <- rateDvalue$org_000554  #泰山石油
fitting000554 <- lm(y000554 ~ x)
alpha000554 <- fitting000554$coefficients[1]  #截距
beta000554 <- fitting000554$coefficients[2]  #斜率
# cat (alpha000554, beta000554)
beta <- c(beta, beta000554=beta000554)

y000637 <- rateDvalue$org_000637  #茂化实华
fitting000637 <- lm(y000637 ~ x)
alpha000637 <- fitting000637$coefficients[1]  #截距
beta000637 <- fitting000637$coefficients[2]  #斜率
# cat (alpha000637, beta000637)
beta <- c(beta, beta000637=beta000637)

y000725 <- rateDvalue$org_000725  #京东方a
fitting000725 <- lm(y000725 ~ x)
alpha000725 <- fitting000725$coefficients[1]  #截距
beta000725 <- fitting000725$coefficients[2]  #斜率
# cat (alpha000725, beta000725)
beta <- c(beta, beta000725=beta000725)

y000737 <- rateDvalue$org_000737  #南风化工
fitting000737 <- lm(y000737 ~ x)
alpha000737 <- fitting000737$coefficients[1]  #截距
beta000737 <- fitting000737$coefficients[2]  #斜率
# cat (alpha000737, beta000737)
beta <- c(beta, beta000737=beta000737)

y000759 <- rateDvalue$org_000759  #中百集团
fitting000759 <- lm(y000759 ~ x)
alpha000759 <- fitting000759$coefficients[1]  #截距
beta000759 <- fitting000759$coefficients[2]  #斜率
# cat (alpha000759, beta000759)
beta <- c(beta, beta000759=beta000759)

y000768 <- rateDvalue$org_000768  #中航飞机
fitting000768 <- lm(y000768 ~ x)
alpha000768 <- fitting000768$coefficients[1]  #截距
beta000768 <- fitting000768$coefficients[2]  #斜率
# cat (alpha000768, beta000768)
beta <- c(beta, beta000768=beta000768)

y000798 <- rateDvalue$org_000798  #中水渔业
fitting000798 <- lm(y000798 ~ x)
alpha000798 <- fitting000798$coefficients[1]  #截距
beta000798 <- fitting000798$coefficients[2]  #斜率
# cat (alpha000798, beta000798)
beta <- c(beta, beta000798=beta000798)

y000906 <- rateDvalue$org_000906  #物产中拓
fitting000906 <- lm(y000906 ~ x)
alpha000906 <- fitting000906$coefficients[1]  #截距
beta000906 <- fitting000906$coefficients[2]  #斜率
# cat (alpha000906, beta000906)
beta <- c(beta, beta000906=beta000906)

y000915 <- rateDvalue$org_000915  #山大华特
fitting000915 <- lm(y000915 ~ x)
alpha000915 <- fitting000915$coefficients[1]  #截距
beta000915 <- fitting000915$coefficients[2]  #斜率
# cat (alpha000915, beta000915)
beta <- c(beta, beta000915=beta000915)

y000922 <- rateDvalue$org_000922  #佳电股份
fitting000922 <- lm(y000922 ~ x)
alpha000922 <- fitting000922$coefficients[1]  #截距
beta000922 <- fitting000922$coefficients[2]  #斜率
# cat (alpha000922, beta000922)
beta <- c(beta, beta000922=beta000922)

y002028 <- rateDvalue$org_002028  #思源电气
fitting002028 <- lm(y002028 ~ x)
alpha002028 <- fitting002028$coefficients[1]  #截距
beta002028 <- fitting002028$coefficients[2]  #斜率
# cat (alpha002028, beta002028)
beta <- c(beta, beta002028=beta002028)

y002066 <- rateDvalue$org_002066  #瑞泰科技
fitting002066 <- lm(y002066 ~ x)
alpha002066 <- fitting002066$coefficients[1]  #截距
beta002066 <- fitting002066$coefficients[2]  #斜率
# cat (alpha002066, beta002066)
beta <- c(beta, beta002066=beta002066)

y002087 <- rateDvalue$org_002087  #新野纺织
fitting002087 <- lm(y002087 ~ x)
alpha002087 <- fitting002087$coefficients[1]  #截距
beta002087 <- fitting002087$coefficients[2]  #斜率
# cat (alpha002087, beta002087)
beta <- c(beta, beta002087=beta002087)

y002096 <- rateDvalue$org_002096  #南岭民爆
fitting002096 <- lm(y002096 ~ x)
alpha002096 <- fitting002096$coefficients[1]  #截距
beta002096 <- fitting002096$coefficients[2]  #斜率
# cat (alpha002096, beta002096)
beta <- c(beta, beta002096=beta002096)

y002140 <- rateDvalue$org_002140  #东华科技
fitting002140 <- lm(y002140 ~ x)
alpha002140 <- fitting002140$coefficients[1]  #截距
beta002140 <- fitting002140$coefficients[2]  #斜率
# cat (alpha002140, beta002140)
beta <- c(beta, beta002140=beta002140)

y002179 <- rateDvalue$org_002179  #中航光电
fitting002179 <- lm(y002179 ~ x)
alpha002179 <- fitting002179$coefficients[1]  #截距
beta002179 <- fitting002179$coefficients[2]  #斜率
# cat (alpha002179, beta002179)
beta <- c(beta, beta002179=beta002179)

y002221 <- rateDvalue$org_002221  #东华能源
fitting002221 <- lm(y002221 ~ x)
alpha002221 <- fitting002221$coefficients[1]  #截距
beta002221 <- fitting002221$coefficients[2]  #斜率
# cat (alpha002221, beta002221)
beta <- c(beta, beta002221=beta002221)

y002277 <- rateDvalue$org_002277  #友阿股份
fitting002277 <- lm(y002277 ~ x)
alpha002277 <- fitting002277$coefficients[1]  #截距
beta002277 <- fitting002277$coefficients[2]  #斜率
# cat (alpha002277, beta002277)
beta <- c(beta, beta002277=beta002277)

y002767 <- rateDvalue$org_002767  #先锋电子
fitting002767 <- lm(y002767 ~ x)
alpha002767 <- fitting002767$coefficients[1]  #截距
beta002767 <- fitting002767$coefficients[2]  #斜率
# cat (alpha002767, beta002767)
beta <- c(beta, beta002767=beta002767)

y300034 <- rateDvalue$org_300034  #钢研高纳
fitting300034 <- lm(y300034 ~ x)
alpha300034 <- fitting300034$coefficients[1]  #截距
beta300034 <- fitting300034$coefficients[2]  #斜率
# cat (alpha300034, beta300034)
beta <- c(beta, beta300034=beta300034)

y300086 <- rateDvalue$org_300086  #康芝药业
fitting300086 <- lm(y300086 ~ x)
alpha300086 <- fitting300086$coefficients[1]  #截距
beta300086 <- fitting300086$coefficients[2]  #斜率
# cat (alpha300086, beta300086)
beta <- c(beta, beta300086=beta300086)

y300141 <- rateDvalue$org_300141  #和顺电气
fitting300141 <- lm(y300141 ~ x)
alpha300141 <- fitting300141$coefficients[1]  #截距
beta300141 <- fitting300141$coefficients[2]  #斜率
# cat (alpha300141, beta300141)
beta <- c(beta, beta300141=beta300141)

y300327 <- rateDvalue$org_300327  #中颖电子
fitting300327 <- lm(y300327 ~ x)
alpha300327 <- fitting300327$coefficients[1]  #截距
beta300327 <- fitting300327$coefficients[2]  #斜率
# cat (alpha300327, beta300327)
beta <- c(beta, beta300327=beta300327)

y300337 <- rateDvalue$org_300337  #银邦股份
fitting300337 <- lm(y300337 ~ x)
alpha300337 <- fitting300337$coefficients[1]  #截距
beta300337 <- fitting300337$coefficients[2]  #斜率
# cat (alpha300337, beta300337)
beta <- c(beta, beta300337=beta300337)

y300404 <- rateDvalue$org_300404  #博济医药
fitting300404 <- lm(y300404 ~ x)
alpha300404 <- fitting300404$coefficients[1]  #截距
beta300404 <- fitting300404$coefficients[2]  #斜率
# cat (alpha300404, beta300404)
beta <- c(beta, beta300404=beta300404)

y300424 <- rateDvalue$org_300424  #航新科技
fitting300424 <- lm(y300424 ~ x)
alpha300424 <- fitting300424$coefficients[1]  #截距
beta300424 <- fitting300424$coefficients[2]  #斜率
# cat (alpha300424, beta300424)
beta <- c(beta, beta300424=beta300424)

y603108 <- rateDvalue$org_603108  #润达医疗
fitting603108 <- lm(y603108 ~ x)
alpha603108 <- fitting603108$coefficients[1]  #截距
beta603108 <- fitting603108$coefficients[2]  #斜率
# cat (alpha603108, beta603108)
beta <- c(beta, beta603108=beta603108)

y002300 <- rateDvalue$org_002300  #太阳电缆
fitting002300 <- lm(y002300 ~ x)
alpha002300 <- fitting002300$coefficients[1]  #截距
beta002300 <- fitting002300$coefficients[2]  #斜率
# cat (alpha002300, beta002300)
beta <- c(beta, beta002300=beta002300)

y002418 <- rateDvalue$org_002418  #康盛股份
fitting002418 <- lm(y002418 ~ x)
alpha002418 <- fitting002418$coefficients[1]  #截距
beta002418 <- fitting002418$coefficients[2]  #斜率
# cat (alpha002418, beta002418)
beta <- c(beta, beta002418=beta002418)

y002529 <- rateDvalue$org_002529  #海源机械
fitting002529 <- lm(y002529 ~ x)
alpha002529 <- fitting002529$coefficients[1]  #截距
beta002529 <- fitting002529$coefficients[2]  #斜率
# cat (alpha002529, beta002529)
beta <- c(beta, beta002529=beta002529)

y002556 <- rateDvalue$org_002556  #辉隆股份
fitting002556 <- lm(y002556 ~ x)
alpha002556 <- fitting002556$coefficients[1]  #截距
beta002556 <- fitting002556$coefficients[2]  #斜率
# cat (alpha002556, beta002556)
beta <- c(beta, beta002556=beta002556)

y002650 <- rateDvalue$org_002650  #加加食品
fitting002650 <- lm(y002650 ~ x)
alpha002650 <- fitting002650$coefficients[1]  #截距
beta002650 <- fitting002650$coefficients[2]  #斜率
# cat (alpha002650, beta002650)
beta <- c(beta, beta002650=beta002650)

y002661 <- rateDvalue$org_002661  #克明面业
fitting002661 <- lm(y002661 ~ x)
alpha002661 <- fitting002661$coefficients[1]  #截距
beta002661 <- fitting002661$coefficients[2]  #斜率
# cat (alpha002661, beta002661)
beta <- c(beta, beta002661=beta002661)

y002687 <- rateDvalue$org_002687  #乔治白
fitting002687 <- lm(y002687 ~ x)
alpha002687 <- fitting002687$coefficients[1]  #截距
beta002687 <- fitting002687$coefficients[2]  #斜率
# cat (alpha002687, beta002687)
beta <- c(beta, beta002687=beta002687)

y002699 <- rateDvalue$org_002699  #美盛文化
fitting002699 <- lm(y002699 ~ x)
alpha002699 <- fitting002699$coefficients[1]  #截距
beta002699 <- fitting002699$coefficients[2]  #斜率
# cat (alpha002699, beta002699)
beta <- c(beta, beta002699=beta002699)

y002699 <- rateDvalue$org_002699  #新疆浩源
fitting002699 <- lm(y002699 ~ x)
alpha002699 <- fitting002699$coefficients[1]  #截距
beta002699 <- fitting002699$coefficients[2]  #斜率
# cat (alpha002699, beta002699)
beta <- c(beta, beta002699=beta002699)

y002740 <- rateDvalue$org_002740  #爱迪尔
fitting002740 <- lm(y002740 ~ x)
alpha002740 <- fitting002740$coefficients[1]  #截距
beta002740 <- fitting002740$coefficients[2]  #斜率
# cat (alpha002740, beta002740)
beta <- c(beta, beta002740=beta002740)

#根据beta的值降序排列，将股票分为七组，每组六支股票
betaOrder <- beta[order(-beta)]  #降序排列
a1 <- betaOrder[1:6]
print (a1)
a2 <- betaOrder[7:12]
print (a2)
a3 <- betaOrder[13:18]
print (a3)
a4 <- betaOrder[19:24]
print (a4)
a5 <- betaOrder[25:30]
print (a5)
a6 <- betaOrder[31:36]
print (a6)
a7 <- betaOrder[37:42]
print (a7)




