# milk cross hedge analysis
# 

# GOAL: find a basket of futures that correlates highly with ca mailbox price.

# dependent: ca <- price
# independents:
#	cmeblockcheddar
#	cmebutter
#	cmeEGnfdm
# 	cmeGAnfdm


cheese = read.csv("cmeblockcheddar.csv", skip = 1,header = T)
head(cheese)
cheese.ts = ts(cheese[,2], start = c(2008,02), frequency = 365)
plot(cheese.ts)

butter = read.csv("cmebutter.csv", skip = 1,header = T)
head(butter)
butter.ts = ts(butter[,2], start = c(2008,02), frequency = 365)
plot(butter.ts)

egnfdm = read.csv("cmeEGnfdm.csv", skip = 1,header = T)
head(egnfdm)
egnfdm.ts = ts(egnfdm[,2], start = c(2008,01), frequency = 365)
plot(egnfdm.ts)

ganfdm = read.csv("cmeGAnfdm.csv", skip = 1,header = T) # need to get this again
dim(ganfdm)
ganfdm.ts = ts(ganfdm[,2], start = c(2008,01), frequency = 365)
plot(ganfdm.ts)

farmprice = read.csv("ca_price.csv", skip = 1,header = T)
tail(farmprice)
farmprice.ts = ts(farmprice[-1,1], start = c(1995,01), frequency = 12)
plot(farmprice.ts)
fp5 = window(farmprice.ts, start = c(2008,01),end = c(2012,12),frequency = 365)
fp5[rep(1:length(fp5),each = c(31,31,31,30,31,30,31,31,30,31,30,31))]

plo

milk =  data.frame(cheese = cheese[,2], butter = butter[,2], egnfdm = egnfdm[,2], ganfdm = ganfdm[,2])
cor(milk)
library(reshape2)
library(ggplot2)
mm = melt(data.frame(milk,day = 1:1353),id = 'day')
ggplot(aes(x=day,y = value, color = variable),data = mm) + geom_line()
