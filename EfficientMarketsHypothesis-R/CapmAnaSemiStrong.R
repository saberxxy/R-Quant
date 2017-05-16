library(RODBC)

ch <- odbcConnect("mysql", uid="root", pwd="root")
# sqlTables(ch)  #查看所有表


rateDvalue <- sqlQuery(ch, "select * from rate_d_value")  #查询收益率数据
# print (a$index)
# print (mode(rateDvalue))
odbcClose(ch)

#第一组 org_000906 org_002418 org_300034 org_002140 org_300327 org_300141
#第二组 org_300086 org_300337 org_000915 org_300424 org_000099 org_000737 
#第三组 org_000768 org_002087 org_002740 org_002028 org_002556 org_002767 
#第四组 org_300404 org_000637 org_002529 org_002066 org_000922 org_002300 
#第五组 org_002277 org_002699 org_002699 org_002179 org_000554 org_000554 
#第六组 org_000070 org_002661 org_000011 org_002687 org_002096 org_002221 
#第七组 org_002650 org_000759 org_000798 org_000725 org_000518 org_603108 

# mean(rateDvalue$org_000906 + rateDvalue$org_002418 + rateDvalue$org_300034 + 
# rateDvalue$org_002140 + rateDvalue$org_300327 + rateDvalue$org_300141)

#第一组 org_000906 org_002418 org_300034 org_002140 org_300327 org_300141
group1Mean <- c()
for (i in c(1:dim(rateDvalue)[1])) {
    group1 <- mean(rateDvalue$org_000906[i], rateDvalue$org_002418[i], rateDvalue$org_300034[i],
             rateDvalue$org_002140[i], rateDvalue$org_300327[i], rateDvalue$org_300141[i]) 
    group1Mean <- c(group1Mean, group1)
}
print (group1Mean)

#第二组 org_300086 org_300337 org_000915 org_300424 org_000099 org_000737 
group2Mean <- c()
for (i in c(1:dim(rateDvalue)[1])) {
    group2 <- mean(rateDvalue$org_300086[i], rateDvalue$org_300337[i], rateDvalue$org_000915[i],
               rateDvalue$org_300424[i], rateDvalue$org_000099[i], rateDvalue$org_000737[i]) 
    group2Mean <- c(group2Mean, group2)
}
print (group2Mean)

#第三组 org_000768 org_002087 org_002740 org_002028 org_002556 org_002767 
group3Mean <- c()
for (i in c(1:dim(rateDvalue)[1])) {
    group3 <- mean(rateDvalue$org_000768[i], rateDvalue$org_002087[i], rateDvalue$org_002740[i],
               rateDvalue$org_002028[i], rateDvalue$org_002556[i], rateDvalue$org_002767[i]) 
    group3Mean <- c(group3Mean, group3)
}
print (group3Mean)

#第四组 org_300404 org_000637 org_002529 org_002066 org_000922 org_002300
group4Mean <- c()
for (i in c(1:dim(rateDvalue)[1])) {
    group4 <- mean(rateDvalue$org_300404[i], rateDvalue$org_000637[i], rateDvalue$org_002529[i],
               rateDvalue$org_002066[i], rateDvalue$org_000922[i], rateDvalue$org_002300[i]) 
    group4Mean <- c(group4Mean, group4)
}
print (group4Mean)

#第五组 org_002277 org_002699 org_002699 org_002179 org_000554 org_000554
group5Mean <- c()
for (i in c(1:dim(rateDvalue)[1])) {
    group5 <- mean(rateDvalue$org_002277[i], rateDvalue$org_002699[i], rateDvalue$org_002699[i],
               rateDvalue$org_002179[i], rateDvalue$org_000554[i], rateDvalue$org_000554[i]) 
    group5Mean <- c(group5Mean, group5)
}
print (group5Mean)

#第六组 org_000070 org_002661 org_000011 org_002687 org_002096 org_002221
group6Mean <- c()
for (i in c(1:dim(rateDvalue)[1])) {
    group6 <- mean(rateDvalue$org_000070[i], rateDvalue$org_002661[i], rateDvalue$org_000011[i],
               rateDvalue$org_002687[i], rateDvalue$org_002096[i], rateDvalue$org_002221[i]) 
    group6Mean <- c(group6Mean, group6)
}
print (group6Mean)

#第七组 org_002650 org_000759 org_000798 org_000725 org_000518 org_603108 
group7Mean <- c()
for (i in c(1:dim(rateDvalue)[1])) {
    group7 <- mean(rateDvalue$org_002650[i], rateDvalue$org_000759[i], rateDvalue$org_000798[i],
               rateDvalue$org_000725[i], rateDvalue$org_000518[i], rateDvalue$org_603108[i]) 
    group7Mean <- c(group7Mean, group7)
}
print (group7Mean)

x <- rateDvalue$org_000001  #指数收益率减去无风险收益率，利率3.5
# yGroup1 <- group1Mean
# fittingGroup1 <- lm(yGroup1 ~ x)
# 
# summary(fittingGroup1)
# # plot (x, yGroup1)
# # abline(fittingGroup1)

# 2016年共242个交易日，其中：
#----------------------------------------------------------------------------------------
# 一月 | 二月 | 三月 | 四月 | 五月 | 六月 | 七月 | 八月 | 九月 | 十月 | 十一月 | 十二月 |
#----------------------------------------------------------------------------------------
#  20  |  16  |  23  |  20  |  21  |  20  |  21  |  23  |  20  |  15  |   21   |   22   |
#----------------------------------------------------------------------------------------

xDec <- mean(x[1:22])  #十二月
xNov <- mean(x[23:43])  #十一月
xOct <- mean(x[44:58])  #十月
xSep <- mean(x[59:78])  #九月
xAug <- mean(x[79:101])  #八月
xJul <- mean(x[102:122])  #七月
xJun <- mean(x[123:142])  #六月
xMay <- mean(x[143:163])  #五月
xApr <- mean(x[164:183])  #四月
xMar <- mean(x[184:206])  #三月
xFeb <- mean(x[207:222])  #二月
xJan <- mean(x[223:242])  #一月

group1Dec <- mean(group1Mean[1:22])  #十二月
group1Nov <- mean(group1Mean[23:43])  #十一月
group1Oct <- mean(group1Mean[44:58])  #十月
group1Sep <- mean(group1Mean[59:78])  #九月
group1Aug <- mean(group1Mean[79:101])  #八月
group1Jul <- mean(group1Mean[102:122])  #七月
group1Jun <- mean(group1Mean[123:142])  #六月
group1May <- mean(group1Mean[143:163])  #五月
group1Apr <- mean(group1Mean[164:183])  #四月
group1Mar <- mean(group1Mean[184:206])  #三月
group1Feb <- mean(group1Mean[207:222])  #二月
group1Jan <- mean(group1Mean[223:242])  #一月

group2Dec <- mean(group2Mean[1:22])  #十二月
group2Nov <- mean(group2Mean[23:43])  #十一月
group2Oct <- mean(group2Mean[44:58])  #十月
group2Sep <- mean(group2Mean[59:78])  #九月
group2Aug <- mean(group2Mean[79:101])  #八月
group2Jul <- mean(group2Mean[102:122])  #七月
group2Jun <- mean(group2Mean[123:142])  #六月
group2May <- mean(group2Mean[143:163])  #五月
group2Apr <- mean(group2Mean[164:183])  #四月
group2Mar <- mean(group2Mean[184:206])  #三月
group2Feb <- mean(group2Mean[207:222])  #二月
group2Jan <- mean(group2Mean[223:242])  #一月

group3Dec <- mean(group3Mean[1:22])  #十二月
group3Nov <- mean(group3Mean[23:43])  #十一月
group3Oct <- mean(group3Mean[44:58])  #十月
group3Sep <- mean(group3Mean[59:78])  #九月
group3Aug <- mean(group3Mean[79:101])  #八月
group3Jul <- mean(group3Mean[102:122])  #七月
group3Jun <- mean(group3Mean[123:142])  #六月
group3May <- mean(group3Mean[143:163])  #五月
group3Apr <- mean(group3Mean[164:183])  #四月
group3Mar <- mean(group3Mean[184:206])  #三月
group3Feb <- mean(group3Mean[207:222])  #二月
group3Jan <- mean(group3Mean[223:242])  #一月

group4Dec <- mean(group4Mean[1:22])  #十二月
group4Nov <- mean(group4Mean[23:43])  #十一月
group4Oct <- mean(group4Mean[44:58])  #十月
group4Sep <- mean(group4Mean[59:78])  #九月
group4Aug <- mean(group4Mean[79:101])  #八月
group4Jul <- mean(group4Mean[102:122])  #七月
group4Jun <- mean(group4Mean[123:142])  #六月
group4May <- mean(group4Mean[143:163])  #五月
group4Apr <- mean(group4Mean[164:183])  #四月
group4Mar <- mean(group4Mean[184:206])  #三月
group4Feb <- mean(group4Mean[207:222])  #二月
group4Jan <- mean(group4Mean[223:242])  #一月

group5Dec <- mean(group5Mean[1:22])  #十二月
group5Nov <- mean(group5Mean[23:43])  #十一月
group5Oct <- mean(group5Mean[44:58])  #十月
group5Sep <- mean(group5Mean[59:78])  #九月
group5Aug <- mean(group5Mean[79:101])  #八月
group5Jul <- mean(group5Mean[102:122])  #七月
group5Jun <- mean(group5Mean[123:142])  #六月
group5May <- mean(group5Mean[143:163])  #五月
group5Apr <- mean(group5Mean[164:183])  #四月
group5Mar <- mean(group5Mean[184:206])  #三月
group5Feb <- mean(group5Mean[207:222])  #二月
group5Jan <- mean(group5Mean[223:242])  #一月

group6Dec <- mean(group6Mean[1:22])  #十二月
group6Nov <- mean(group6Mean[23:43])  #十一月
group6Oct <- mean(group6Mean[44:58])  #十月
group6Sep <- mean(group6Mean[59:78]) #九月
group6Aug <- mean(group6Mean[79:101])  #八月
group6Jul <- mean(group6Mean[102:122])  #七月
group6Jun <- mean(group6Mean[123:142])  #六月
group6May <- mean(group6Mean[143:163])  #五月
group6Apr <- mean(group6Mean[164:183])  #四月
group6Mar <- mean(group6Mean[184:206])  #三月
group6Feb <- mean(group6Mean[207:222])  #二月
group6Jan <- mean(group6Mean[223:242])  #一月

group7Dec <- mean(group7Mean[1:22])  #十二月
group7Nov <- mean(group7Mean[23:43])  #十一月
group7Oct <- mean(group7Mean[44:58])  #十月
group7Sep <- mean(group7Mean[59:78])  #九月
group7Aug <- mean(group7Mean[79:101])  #八月
group7Jul <- mean(group7Mean[102:122])  #七月
group7Jun <- mean(group7Mean[123:142])  #六月
group7May <- mean(group7Mean[143:163])  #五月
group7Apr <- mean(group7Mean[164:183])  #四月
group7Mar <- mean(group7Mean[184:206])  #三月
group7Feb <- mean(group7Mean[207:222])  #二月
group7Jan <- mean(group7Mean[223:242])  #一月

xRate <- c(xDec, xNov, xOct, xSep, xAug, xJul, xJun, xMay, xApr, xMar, xFeb, xJan)
group1Rate <- c(group1Dec, group1Nov, group1Oct, group1Sep, group1Aug, group1Jul, 
               group1Jun, group1May, group1Apr, group1Mar, group1Feb, group1Jan)
group2Rate <- c(group2Dec, group2Nov, group2Oct, group2Sep, group2Aug, group2Jul, 
               group2Jun, group2May, group2Apr, group2Mar, group2Feb, group2Jan)
group3Rate <- c(group3Dec, group3Nov, group3Oct, group3Sep, group3Aug, group3Jul, 
               group3Jun, group3May, group3Apr, group3Mar, group3Feb, group3Jan)
group4Rate <- c(group4Dec, group4Nov, group4Oct, group4Sep, group4Aug, group4Jul, 
               group4Jun, group4May, group4Apr, group4Mar, group4Feb, group4Jan)
group5Rate <- c(group5Dec, group5Nov, group5Oct, group5Sep, group5Aug, group5Jul, 
               group5Jun, group5May, group5Apr, group5Mar, group5Feb, group5Jan)
group6Rate <- c(group6Dec, group6Nov, group6Oct, group6Sep, group6Aug, group6Jul, 
               group6Jun, group6May, group6Apr, group6Mar, group6Feb, group6Jan)
group7Rate <- c(group7Dec, group7Nov, group7Oct, group7Sep, group7Aug, group7Jul, 
               group7Jun, group7May, group7Apr, group7Mar, group7Feb, group7Jan)

fitting1 <- lm(group1Rate ~ xRate)
# summary(fitting1)
# print (fitting1)
# plot(group1Rate, xRate)
# abline(fitting1)
betaFitting1 <- c(fitting1$coefficients[2]) #beta值
print (betaFitting1)
rateMean1 <- mean(group1Rate)  #收益率均值
print (rateMean1)

fitting2 <- lm(group2Rate ~ xRate)
# summary(fitting2)
# print (fitting2)
# plot(group2Rate, xRate)
# abline(fitting2)
betaFitting2 <- c(fitting2$coefficients[2]) #beta值
print (betaFitting2)
rateMean2 <- mean(group2Rate)  #收益率均值
print (rateMean2)

fitting3 <- lm(group3Rate ~ xRate)
# summary(fitting3)
# print (fitting3)
# plot(group3Rate, xRate)
# abline(fitting3)
betaFitting3 <- c(fitting3$coefficients[2]) #beta值
print (betaFitting3)
rateMean3 <- mean(group3Rate)  #收益率均值
print (rateMean3)

fitting4 <- lm(group4Rate ~ xRate)
# summary(fitting4)
# print (fitting4)
# plot(group4Rate, xRate)
# abline(fitting4)
betaFitting4 <- c(fitting4$coefficients[2]) #beta值
print (betaFitting4)
rateMean4 <- mean(group4Rate)  #收益率均值
print (rateMean4)

fitting5 <- lm(group5Rate ~ xRate)
# summary(fitting5)
# print (fitting5)
# plot(group5Rate, xRate)
# abline(fitting5)
betaFitting5 <- c(fitting5$coefficients[2]) #beta值
print (betaFitting5)
rateMean5 <- mean(group5Rate)  #收益率均值
print (rateMean5)

fitting6 <- lm(group6Rate ~ xRate)
# summary(fitting6)
# print (fitting6)
# plot(group6Rate, xRate)
# abline(fitting6)
betaFitting6 <- c(fitting6$coefficients[2]) #beta值
print (betaFitting6)
rateMean6 <- mean(group6Rate)  #收益率均值
print (rateMean6)

fitting7 <- lm(group7Rate ~ xRate)
# summary(fitting7)
# print (fitting7)
# plot(group7Rate, xRate)
# abline(fitting7)
betaFitting7 <- c(fitting7$coefficients[2]) #beta值
print (betaFitting7)
rateMean7 <- mean(group7Rate)  #收益率均值
print (rateMean7)

# yRate <- c(-0.006119722, -0.0895969, -0.0512248, -0.1223139,
#            0.02557313, -0.05725415, -0.02014155)  #收益率均值
# xBeta <- c(1.874886, 2.308326, 1.755107, 1.896972, 
#            1.563004, 1.218712, 1.675661)  #beta值

yRate <- c(rateMean1, rateMean2, rateMean3, rateMean4,
           rateMean5, rateMean6, rateMean7)  #收益率均值
xBeta <- c(betaFitting1, betaFitting2, betaFitting3, betaFitting4,
           betaFitting5, betaFitting6, betaFitting7)  #beta值

fittingRateBeta <- lm(yRate ~ xBeta)
print (fittingRateBeta)
summary(fittingRateBeta)

