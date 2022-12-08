##Earthquaks for Total##
graphics.off()
rm(list=ls())
setwd("E:\\Study\\4th_Year\\AST-450\\Final_Work")
data<-read.csv("Modified_Earthquake_data1.csv",header=T)
data
attach(data)
Time<-rep(1:117)
library(ggplot2)
da<-data.frame(Year,Total)
plot(Year,Total,type="l",col='blue',lwd=3,main="Time Versus Total frequency")
abline(v=1973,col='maroon',lwd=1.75)
abline(v=1950,col='purple',lwd=1.75)
Total
T1<-Total[1:50]
T2<-Total[51:73]
T3<-Total[74:117]
T1;T2;T3

m1<-mean(T1);m2<-mean(T2);m3<-mean(T3)
n1<-length(T1);n2<-length(T2);n3<-length(T3)
c(n1,n2,n3)
s1<-sd(T1);s2<-sd(T2);s3<-sd(T3)
c(s1,s2,s3)
std1<-(T1-m1)/(s1/sqrt(n1))
std2<-(T2-m2)/(s2/sqrt(n2))
std3<-(T3-m3)/(s3/sqrt(n3))

Data1 <- data.frame(Y=c(T1, T2, T3),TS =factor(rep(c("T1", "T2", "T3"), times=c(length(T1), length(T2), length(T3)))))
fit <- aov(Y~TS, data=Data1)
anova(fit)
summary(fit)

Freq<-T3
Time<-rep(1973:2016)
D1<-data.frame(Time,Freq)
library(ggplot2)
ggplot(data=D1,aes(x=Time, y=Freq)) +geom_line(col='maroon',lwd=1.5)

plot(Time,Freq,type='l',col='maroon',lwd=2.5,main="Time Series Plot for Year 1973-2016")
hist(Freq,freq=FALSE)
lines(density(Freq),col='purple',lwd=2)
qqnorm(Freq,col='green',lwd=2)
qqline(Freq,col='blue',lwd=2.5)
acf(Freq, lag.max=10, plot = TRUE,main="ACF plot for Total")
pacf(Freq, lag.max=10, plot = TRUE,main="PACF plot for Total")
lag.plot(Freq,lags=6, do.lines =FALSE)

install.packages("lmtest")
library('lmtest')
fit1<-lm(Time~Freq)
summary(fit1)
D1<-data.frame(Time,residuals(fit1))
library(ggplot2)
ggplot(data=D1,aes(x=Time, y=residuals(fit1))) +geom_line(col='maroon',lwd=1.5)
g<-qplot(Time,residuals(fit1),
         geom='line',
         main = 'Time Versus residuals',
         xlab = 'Time',
         ylab = 'Residuals',
         col='maroon')
g

plot(Time,residuals(fit1),type='l',col='maroon',lwd=3,main="Time Versus residuals")

dr<-dwtest(fit1, alternative = c("two.sided"))
d<-dr$statistic
d_pvalue<-dr$p.value
d
d_pvalue
#Since the p-value is 4.650563e-28 which is less than 0.05. 
#So we can reject the null hypothesis and can conclude that the residuals are autocorrelated for the given data.


##stationary checking##

install.packages("tseries")
library(tseries)
adf.test((Freq),alternative="stationary")
adf.test(diff(Freq),alternative="stationary")
adf.test(diff(diff(Freq)),alternative="stationary")
par(mfrow=c(2:1))
Acf(diff(diff(Freq)),main="ACF plot for Total")
Pacf(diff(diff(Freq)),main="PACF plot for Total")

install.packages('forecast')
library('forecast')
a1<-Arima(Freq,order=c(1,1,0))
a2<-Arima(Freq,order=c(1,2,0))
a3<-Arima(Freq,order=c(2,2,0))
a4<-Arima(Freq,order=c(3,2,0))
a1$aic;a2$aic;a3$aic;a4$aic

res1<-residuals(a1)
res2<-residuals(a2)
res3<-residuals(a3)
res4<-residuals(a4)
Acf(residuals(a1))
Acf(residuals(a2))
pacf(residuals(a2))

graphics.off()
Acf(residuals(a1),main="ACF plot of Series residuals for ARIMA(3,2,0)")
Box.test(residuals(a4), lag=24, fitdf=4, type="Ljung")
##The ACF plot of the residuals from the ARIMA(3,2,0)##
## model shows all correlations within the threshold##
##limits indicating that the residuals are behaving ##
##like white noise. A portmanteau test returns a large p-value,##
## also suggesting the residuals are white noise. ##
rs<-data.frame("Year"=rep(1973:2016),"Residuals ARIMA(1,1,0)"=res1,"Residuals ARIMA(1,2,0)"=res2,"Residuals ARIMA(2,2,0)"=res3,"Residuals ARIMA(3,2,0)"=res4)
rs
write.csv(rs,"Final Residuals for total11.csv")

q1<-ewma(res,nsigmas=3,lambda=0.2,ylab="Frequency",xlab="Year",title="EWMA Chart for Total")
q2<-cusum(res,head.start =4,decision.interval = 5, se.shift = 1)



###earthquaks for 5.9##
graphics.off()
rm(list=ls())
setwd("E:\\Study\\4th_Year\\AST-450\\Final_Work")
data<-read.csv("Modified_Earthquake_data1.csv",header=T)
data
attach(data)
Time<-rep(1:117)
plot(Year,X5.5.9,type="l",col='blue',lwd=3,main="Time Versus 5.9 frequency")
abline(v=1973,col='maroon',lwd=1.75)
abline(v=1950,col='purple',lwd=1.75)

T1<-X5.5.9[1:50]
T2<-X5.5.9[51:73]
T3<-X5.5.9[74:117]
T1;T2;T3

m1<-mean(T1);m2<-mean(T2);m3<-mean(T3)
n1<-length(T1);n2<-length(T2);n3<-length(T3)
c(n1,n2,n3)
s1<-sd(T1);s2<-sd(T2);s3<-sd(T3)
c(s1,s2,s3)

Data1 <- data.frame(Y=c(T1, T2, T3),TS =factor(rep(c("T1", "T2", "T3"), times=c(length(T1), length(T2), length(T3)))))
fit <- aov(Y~TS, data=Data1)
anova(fit)

Freq<-T3
Time<-rep(1973:2016)
D1<-data.frame(Time,Freq)
library(ggplot2)
ggplot(data=D1,aes(x=Time, y=Freq)) +geom_line(col='maroon',lwd=1.5)

plot(Time,Freq,type='l',col='red',lwd=2.5,main="Time Series Plot for Year 1973-2016")
hist(Freq,freq=FALSE)
lines(density(Freq),col='red',lwd=2)
qqnorm(Freq,col='red',lwd=2)
qqline(Freq,col='blue',lwd=2.5)
acf(Freq, lag.max=10, plot = TRUE)
pacf(Freq, lag.max=10, plot = TRUE)
lag.plot(Freq, lags=10, do.lines =FALSE)

install.packages("lmtest")
library('lmtest')
fit1<-lm(Time~Freq)
summary(fit1)
library(ggplot2)
D1<-data.frame(Time,residuals(fit1))
ggplot(data=D1,aes(x=Time, y=residuals(fit1))) +geom_line(col='maroon',lwd=1.5)
dr<-dwtest(fit1, alternative = c("two.sided"))
d<-dr$statistic
d_pvalue<-dr$p.value
d
d_pvalue

##stationary checking##

install.packages("tseries")
library(tseries)
Box.test(Freq, lag=24, fitdf=4, type="Ljung")
#The Ljung-Box test examines whether there is significant evidence for non-zero correlations at lags 1-20.
#Small p-values (i.e., less than 0.05) suggest that the series is stationary.

adf.test((Freq),alternative="stationary")
adf.test(diff(Freq),alternative="stationary")
##As the p value is greater then 0.05 then we suggest that the series is nonstationary.
adf.test(diff(diff(Freq)),alternative="stationary")
##As the p value is less then 0.05 then we suggest that the series is stationary.

kpss.test(Freq)

par(mfrow=c(2:1))
Acf(diff(diff(Freq)),main="ACF plot for Magnitude 5-5.9")
Pacf(diff(diff(Freq)),main="PACF plot for Magnitude 5-5.9")

install.packages('forecast')
library('forecast')

a1<-Arima(Freq,order=c(0,2,0))
a2<-Arima(Freq,order=c(1,2,0))
a3<-Arima(Freq,order=c(2,2,0))
a4<-Arima(Freq,order=c(3,2,0))
a1$aic;a2$aic;a3$aic;a4$aic

res1<-residuals(a1)
res2<-residuals(a2)
res3<-residuals(a3)
res4<-residuals(a4)
Acf(residuals(a1))
Acf(residuals(a2))
Acf(residuals(a3))

graphics.off()
Acf(residuals(a4),main="ACF plot of Series residuals for ARIMA(3,2,0)")
Box.test(residuals(a4), lag=24, fitdf=4, type="Ljung")
##The ACF plot of the residuals from the ARIMA(1,1,0)##
## model shows all correlations within the threshold##
##limits indicating that the residuals are behaving ##
##like white noise. A portmanteau test returns a large p-value,##
## also suggesting the residuals are white noise. ##
rs1<-data.frame("Year"=rep(1973:2016),"Residuals ARIMA(0,2,0)"=res1,"Residuals ARIMA(1,2,0)"=res2,"Residuals ARIMA(2,2,0)"=res3,"Residuals ARIMA(3,2,0)"=res4)
rs1
write.csv(rs1,"Final Residuals for 5.911.csv")
q1<-ewma(res,nsigmas=3,lambda=0.2,ylab="Frequency",xlab="Year",title="EWMA Chart for 5.9")
q2<-cusum(res,head.start =0,decision.interval = 5, se.shift = 1)



###Earthquaks for 6.9##
graphics.off()
rm(list=ls())
setwd("E:\\Study\\4th_Year\\AST-450\\Final_Work")
data<-read.csv("Modified_Earthquake_data1.csv",header=T)
data
attach(data)
Time<-rep(1:117)
plot(Year,X6.6.9,type="l",col='blue',lwd=3,main="Time Versus 5.9 frequency")
abline(v=1973,col='maroon',lwd=1.75)
abline(v=1950,col='purple',lwd=1.75)
X6.6.9
T1<-X6.6.9[1:50]
T2<-X6.6.9[51:73]
T3<-X6.6.9[74:117]
T1;T2;T3

m1<-mean(T1);m2<-mean(T2);m3<-mean(T3)
n1<-length(T1);n2<-length(T2);n3<-length(T3)
c(n1,n2,n3)
s1<-sd(T1);s2<-sd(T2);s3<-sd(T3)
c(s1,s2,s3)

Data1 <- data.frame(Y=c(T1, T2, T3),TS =factor(rep(c("T1", "T2", "T3"), times=c(length(T1), length(T2), length(T3)))))
fit <- aov(Y~TS, data=Data1)
anova(fit)

Freq<-T3
Time<-rep(1973:2016)
D1<-data.frame(Time,Freq)
library(ggplot2)
ggplot(data=D1,aes(x=Time, y=Freq)) +geom_line(col='maroon',lwd=1.5)

plot(Time,Freq,type='l',col='red',lwd=2.5,main="Time Series Plot for Year 1973-2016")
hist(Freq,freq=FALSE)
lines(density(Freq),col='red',lwd=2)
qqnorm(Freq,col='red',lwd=2)
qqline(Freq,col='blue',lwd=2.5)
acf(Freq, lag.max=10, plot = TRUE)
pacf(Freq, lag.max=10, plot = TRUE)
lag.plot(Freq, lags=10, do.lines =FALSE)

install.packages("lmtest")
library('lmtest')
fit1<-lm(Time~Freq)
summary(fit1)
library(ggplot2)
D1<-data.frame(Time,residuals(fit1))
ggplot(data=D1,aes(x=Time, y=residuals(fit1))) +geom_line(col='maroon',lwd=1.5)
dr<-dwtest(fit1, alternative = c("two.sided"))
d<-dr$statistic
d_pvalue<-dr$p.value
d
d_pvalue


##stationary checking##

D1<-data.frame(Time,Freq)
library(ggplot2)
ggplot(data=D1,aes(x=Time, y=Freq)) +geom_line(col='maroon',lwd=1.5)

install.packages("tseries")
library(tseries)

adf.test((Freq),alternative="stationary")
adf.test(diff(Freq),alternative="stationary")
##As the p value is greater then 0.05 then we suggest that the series is nonstationary.
adf.test(diff(diff(Freq)),alternative="stationary")
##As the p value is less then 0.05 then we suggest that the series is stationary.

par(mfrow=c(2:1))
Acf(diff(diff(Freq)),main="ACF plot for Magnitude 6-6.9")
Pacf(diff(diff(Freq)),main="PACF plot for TMagnitude 6-6.9")



install.packages('forecast')
library('forecast')
a1<-Arima(Freq,order=c(0,2,0))
a2<-Arima(Freq,order=c(1,2,0))
a3<-Arima(Freq,order=c(2,2,0))
a4<-Arima(Freq,order=c(3,2,0))
a1$aic;a2$aic;a3$aic;a4$aic

res1<-residuals(a1)
res2<-residuals(a2)
res3<-residuals(a3)
res4<-residuals(a4)
Acf(residuals(a1))
Acf(residuals(a2))
Acf(residuals(a3))

graphics.off()
Acf(residuals(a4),main="ACF plot of Series residuals for ARIMA(3,2,0)")
Box.test(residuals(a4), lag=24, fitdf=4, type="Ljung")
##The ACF plot of the residuals from the ARIMA(1,1,0)##
## model shows all correlations within the threshold##
##limits indicating that the residuals are behaving ##
##like white noise. A portmanteau test returns a large p-value,##
## also suggesting the residuals are white noise. ##
rs1<-data.frame("Year"=rep(1973:2016),"Residuals ARIMA(0,2,0)"=res1,"Residuals ARIMA(1,2,0)"=res2,"Residuals ARIMA(2,2,0)"=res3,"Residuals ARIMA(3,2,1)"=res4)
rs1
write.csv(rs1,"Final Residuals for 6.911.csv")
q1<-ewma(res,nsigmas=3,lambda=0.2,ylab="Frequency",xlab="Year",title="EWMA Chart for 6.9")


res1<-read.csv("Final Residuals for 5.911.csv")
res2<-read.csv("Final Residuals for 6.911.csv")
res3<-read.csv("Final Residuals for total11.csv")
str(res1)
dat1<-cbind(res1,res2,res3)
dat1
write.csv(dat1,"Final Residuals11.csv")
