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
fp5 = window(farmprice.ts, start = c(2008,01),end = c(2012,12),frequency = 12)
cheese5 = window(cheese.ts, start = c(2008,02),end = c(2012,365),frequency = 12)
fp5 = window(farmprice.ts, start = c(2008,01),end = c(2012,12),frequency = 365)
fp5 = window(farmprice.ts, start = c(2008,01),end = c(2012,12),frequency = 365)
fp5 = window(farmprice.ts, start = c(2008,01),end = c(2012,12),frequency = 365)
fp5 = window(farmprice.ts, start = c(2008,01),end = c(2012,12),frequency = 365)


farmprice = read.csv("ca_price.csv", skip = 1,header = T)
fp5 = farmprice[158:217,1]
day = fp5[rep(1:60,rep(c(30,31,31,30,31,30,31,31,30,31,30,31),5))] # jan only 30 as other series start jan 2
cheese = read.csv("cmeblockcheddar.csv", skip = 1,header = T)
butter = read.csv("cmebutter.csv", skip = 1,header = T)
egnfdm = read.csv("cmeEGnfdm.csv", skip = 1,header = T)
ganfdm = read.csv("cmeGAnfdm.csv", skip = 1,header = T) # need to get this again
farmprice = read.csv("ca_price.csv", skip = 1,header = T)
ganfdm.ts = ts(ganfdm[,2], start = c(2008,01), frequency = 365)
cheese.ts = ts(cheese[,2], start = c(2008,02), frequency = 365)
egnfdm.ts = ts(egnfdm[,2], start = c(2008,01), frequency = 365)
butter.ts = ts(butter[,2], start = c(2008,02), frequency = 365)
farmprice.ts = ts(farmprice[-1,1], start = c(1995,01), frequency = 12)
cheese5 = window(cheese.ts, start = c(2008,02),end = c(2012,365),frequency = 12)
fp5 = window(farmprice.ts, start = c(2008,01),end = c(2012,12),frequency = 365)
fp5 = window(farmprice.ts, start = c(2008,01),end = c(2012,12),frequency = 365)
fp5 = window(farmprice.ts, start = c(2008,01),end = c(2012,12),frequency = 365)
fp5 = window(farmprice.ts, start = c(2008,01),end = c(2012,12),frequency = 365)
milk =  data.frame(cheese = cheese[1:1254,2], butter = butter[1:1254,2], egnfdm = egnfdm[1:1254,2], ganfdm = ganfdm[1:1254,2])
cor(milk)

library(reshape2)
library(ggplot2)
mm = melt(data.frame(milk,day = 1:1254),id = 'day')
ggplot(aes(x=day,y = value, color = variable),data = mm) + 
geom_line() 

plot(1:1254,mm$cheese
geom_line(aes(x = mm$farmprice.ts)



# missing days =  WEEKENDS - use ZOO

library(xts)
library(corrgram)

cheese = read.csv("cmeblockcheddar.csv", skip = 1,header = T)
butter = read.csv("cmebutter.csv", skip = 1,header = T)
egnfdm = read.csv("cmeEGnfdm.csv", skip = 1,header = T)
ganfdm = read.csv("cmeGAnfdm.csv", skip = 1,header = T) # need to get this again
farmprice = read.csv("ca_price.csv", skip = 1,header = T)
ganfdm.xts = xts(ganfdm[,2],strptime(ganfdm[,1],"%m/%d/%Y"))
cheese.xts = xts(cheese[,2],strptime(cheese[,1],"%m/%d/%Y"))
egnfdm.xts = xts(egnfdm[,2],strptime(egnfdm[,1],"%m/%d/%Y"))
butter.xts = xts(butter[,2],strptime(butter[,1],"%m/%d/%Y"))
farmprice.ts = ts(farmprice[-1,1], start = c(1995,01), frequency = 12)
ch.mon = apply.monthly(cheese.xts,mean)
ga.mon = apply.monthly(ganfdm.xts,mean)
eg.mon = apply.monthly(egnfdm.xts,mean)
bu.mon = apply.monthly(butter.xts,mean)
fp.mon = as.xts(farmprice.ts)


plot(seq(1995,2013.167,1/12)[-217] , 
     as.numeric(fp.mon[1:218]/10),
     xlim = c(2008,2012),
     ylim = c(0.5,2.5),
     type = "l",
     main = "ts plot of farmgate milk vs CME contracts",
     lwd = 2)
lines(seq(2008,2013.417,1/12)[-66],ch.mon,col = "red")
lines(seq(2008,2013.417,1/12)[-66],bu.mon,col = "blue")
lines(seq(2008,2013.417,1/12)[-66],eg.mon,col = "green")
legend(2010,1,legend = c("farm price","cheese", "butter", "NFDM"), fill = c("black","red","blue","green"))


mil = data.frame(date = seq(2008,2013,1/12)[-61], 
	   fp = as.numeric(fp.mon[157:216]),
	   cheese = as.numeric(ch.mon[1:60]),
	   butter = as.numeric(bu.mon[1:60]),
	   egnfdm = as.numeric(eg.mon[1:60])
)

cor(mil[,-1])                          # raw correlations
corrgram(mil[,-1],upper.panel = panel.pts)
corrgram(mil[,-1],upper.panel = panel.pts,lower.panel = panel.conf)

mod = lm(fp ~ ., data = mil[1:50,-1])
summary(mod)
pred = predict(mod, mil[51:59,])      # 
plot(mil[1:50,1:2], type = "l",xlim = c(2010,2013),ylim = c(12,25),main = "lm predictions from no diff")
lines(mil[51:59,],col = "red")
lines(mil[51:59,1],pred,col = "blue")
legend(2010,24,
       legend = c("actual", "predicted","95 CI"),
       fill = c("red","blue","gray"))


par(mfrow=c(2,2))
for(i in 2:5){
	plot(mil[,i],type = "l",main = names(mil)[i])
}

for(i in 2:5){
	acf(mil[,i], main = paste(names(mil)[i]," ACF"))
}

for(i in 2:5){
	pacf(mil[,i], main = paste(names(mil)[i]," pACF"))
}

mild = data.frame(date = seq(2008.083,2013,1/12)[-60], 
	   fp = diff(as.numeric(fp.mon[157:216])),
	   cheese = diff(as.numeric(ch.mon[1:60])),
	   butter = diff(as.numeric(bu.mon[1:60])),
	   egnfdm = diff(as.numeric(eg.mon[1:60]))
)

mod = lm(fp ~ ., data = mild[1:50,])
summary(mod)
pred = predict(mod, mild[51:59,])      # 
par(mfrow=c(1,1))
plot(mild[1:50,1:2], type = "l",xlim = c(2010,2013),main = "lm predictions from diff")
lines(mild[51:59,],col = "red")
lines(mild[51:59,1],pred,col = "blue")
legend(2010,-1.5,
       legend = c("actual", "predicted","95 CI"),
       fill = c("red","blue","gray"))

corrgram(mild[,-1],upper.panel = panel.pts)
corrgram(mild[,-1],upper.panel = panel.pts,lower.panel = panel.conf)

par(mfrow=c(2,2))
for(i in 2:5){
	plot(mil[,i],type = "l",main = paste("differenced",names(mil)[i]))
}

for(i in 2:5){
	acf(mil[,i], main = paste(names(mil)[i]," differenced ACF"))
}
par(mfrow=c(1,1))

### 
# ARIMA model on farm price, last2012 reserved for cross validation
###
library(forecast)
ar.mod = auto.arima(window(farmprice.ts,end = c(2012,0)))
ar.pred = forecast(ar.mod, 14)

par(mfrow=c(1,1))
plot(ar.pred)                          # shit prediction
lines(window(farmprice.ts,start = c(2012,0)),col = "red")
legend(1995,25,
       legend = c("actual", "predicted","95 CI"),
       fill = c("red","blue","gray"))

#hw
hw.mod = HoltWinters(window(farmprice.ts,end = c(2012,0)),seasonal = "multiplicative")
hw.pred = forecast(hw.mod, 10)

plot(hw.pred,xlim = c(2008,2013))      # better
lines(window(farmprice.ts,start = c(2012,0)),col = "red")
legend(1995,25,
       legend = c("actual", "predicted","95 CI"),
       fill = c("red","blue","gray"))


	

# pull out each month and check if cor changes

mon = numeric(12)
nom = character(12)
for(i in 1:12){
	mon[i] = sort(cor(mil[seq(i,60,12),-1])[,1])[3]
	nom[i] = names(sort(cor(mil[seq(i,60,12),-1])[,1])[3])
}
names(mon) = nom
nom=factor(nom)
levels(nom) = c("yellow","blue")
# plot of each omnth and max cor to farmgate price.
barplot(mon,ylim = c(0,1),col = as.character(nom))

# try combining proportions of commodoties based on inverse sum cor error
err = cor(mil)[-(1:2),2]
wt = err / sum(err)                         # weight each comod based on proprtion total error.

preds = apply(mil, 1, function(i) { sum(i[3:5] * wt)})
plot(mil[,2],col = "red",type = "l")
lines(preds * 10, type = "b")          # pretty good predicitons

sum(abs(mil[,2] - preds*10)/mil[,2])/length(mil[,2]) # 5.8% MAPE

# now do with dynamic monthly values.

mon = numeric(12)
nom = character(12)
wt = data.frame(cheese = numeric(12), butter = numeric(12), egnfdm = numeric(12))
for(i in 1:12){
	wt[i,] = cor(mil[seq(i,60,12),-1])[,1][-1]
}
wtt = t(apply(wt,1,function(i) { i/sum(i)}))
preds = rowSums(wtt * mil[,3:5])
plot(mil[,2],col = "red",type = "l")
lines(preds * 10, type = "b")          # pretty good predicitons

sum(abs(mil[,2] - preds*10)/mil[,2])/length(mil[,2]) # 6% MAPE


		     
