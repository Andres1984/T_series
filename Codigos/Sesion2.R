


library("foreign")
path = file.path(file.choose())
#SandPhedge
data = read.dta(path)
data$Date=seq(2002+2/12,2013+4/12,by=1/12)
data = na.omit(data)

Y = cbind(data$rspot,data$rfutures)
apply(Y,2,mean)
apply(Y,2,median)
apply(Y,2,max)
apply(Y,2,min)
apply(Y,2,sd)
library("moments")
apply(Y,2,skewness)
apply(Y,2,kurtosis)
apply(Y,2,jarque.test)
apply(Y,2,sum)
apply(Y^2,2,sum)
apply(Y,2,length)


par(mfcol = c(1,1), oma = c(0,0,1,0) + 1, mar = c(0,1,0,0) + 1, mgp = c(0, 0.2, 0))
plot(Y,type="p",las=1,xlab="",ylab="",main="",tck=0.02)
lm1 = lm(Y[,1]~Y[,2])
abline(lm1,col="steelblue4",lwd=2)


path = file.path(file.choose())
#CAPM
data = read.dta(path)
data$Date = seq(2002+1/12,2013+4/12,by=1/12)
rsandp = 100*diff(log(data$SANDP))
rford = 100*diff(log(data$FORD))
ustb3m = data$USTB3M[-1]/12
data$erford = rford-ustb3m
ersandp = rsandp-ustb3m


par(mfcol = c(1,1), oma = c(0,0,1,0) + 0.2, mar = c(0,1,0,0) + 1, mgp = c(0, 0.2, 0))
plot(data$Date[-1],erford,type="p",xaxs="i",yaxs="i",las=1,xlab="",ylab="",main="",tck=0.02,ylim=c(-100,100),pch=20)
points(data$Date[-1],ersandp,col="steelblue4",pch=20)
abline(h=0,lty=2)
legend("topleft",c("FORD","SANDP"),col=c(1,"steelblue4"),lty=c(1,1),lwd=2,box.col="white")
box()


lm1 = lm(data$erford~data$ersandp)
summary(lm1)

library(leaps)




path = file.path(file.choose())
data = read.dta(path)
#macro
dspread = diff(data$BAAAAASPREAD)
dcredit = diff(data$CONSUMERCREDIT)
dprod = diff(data$Industrialproduction)
rmsoft = 100*diff(log(data$Microsoft))
rsandp = 100*diff(log(data$SANDP))
dmoney = diff(data$M1MONEYSUPPLY)
inflation = 100*diff(log(data$CPI))
term = data$USTB10Y-data$USTB3M
dinflation = diff(inflation)
mustb3m = data$USTB3M/12
rterm = diff(term)
ermsoft = rmsoft-mustb3m[-1]
ersandp = rsandp-mustb3m[-1]
lm2 = lm(ermsoft~ersandp+dprod+dcredit+I(c(NA,dinflation))+dmoney+dspread+rterm)
summary(lm2)

#macro

df = cbind(data$USTB3M,data$USTB6M,data$USTB1Y,data$USTB3Y,data$USTB5Y,data$USTB10Y)
cor(df)
evec = eigen(cor(df))$vectors
evec
eval = eigen(cor(df))$values
eval
diff(eval)
prop = eval/sum(eval)
prop
cumsum(prop)

## Regresiones sobre polinomios


## Ejemplo simulado
p <- 0.5
q <- seq(0,100,1)
y <- p*q
plot(q,y,type='l',col='red',main='Linear relationship')
y <- 450 + p*(q-10)^3
plot(q,y,type='l',col='navy',main='Nonlinear relationship',lwd=3)

set.seed(20)
q <- seq(from=0, to=20, by=0.1)
y <- 500 + 0.4 * (q-10)^3
noise <- rnorm(length(q), mean=10, sd=80)
noisy.y <- y + noise
plot(q,noisy.y,col='deepskyblue4',xlab='q',main='Observed data')
lines(q,y,col='firebrick1',lwd=3)
model <- lm(noisy.y ~ q + I(q^2) + I(q^3))
summary(model)

predicted.intervals <- predict(model,data.frame(x=q),interval='confidence',level=0.99)
confint(model, level=0.95)
plot(fitted(model),residuals(model))
plot(q,noisy.y,col='deepskyblue4',xlab='q',main='Observed data')
lines(q,y,col='firebrick1',lwd=3)
lines(q,predicted.intervals[,1],col='green',lwd=3)
lines(q,predicted.intervals[,2],col='black',lwd=1)
lines(q,predicted.intervals[,3],col='black',lwd=1)
legend("bottomright",c("Observ.","Signal","Predicted"), 
       col=c("deepskyblue4","red","green"), lwd=3)

# Desempleo

library(quantmod)
symbols=c("COLLRUNTTTTSTM")# Vector de caracteres
getSymbols(symbols,src='FRED')
plot(COLLRUNTTTTSTM, main="Desempleo Colombia")

q=seq(1,149)
reg1=lm(COLLRUNTTTTSTM$COLLRUNTTTTSTM~I(COLLRUNTTTTSTM$COLLRUNTTTTSTM),na.action="na.omit")
reg2=lm(COLLRUNTTTTSTM$COLLRUNTTTTSTM~I(COLLRUNTTTTSTM$COLLRUNTTTTSTM)+I(COLLRUNTTTTSTM$COLLRUNTTTTSTM^2), na.action="na.omit")
reg3=lm(COLLRUNTTTTSTM$COLLRUNTTTTSTM~I(COLLRUNTTTTSTM$COLLRUNTTTTSTM)+I(COLLRUNTTTTSTM$COLLRUNTTTTSTM^2)+I(COLLRUNTTTTSTM$COLLRUNTTTTSTM^3),na.action="na.omit")
reg4=lm(COLLRUNTTTTSTM$COLLRUNTTTTSTM~I(COLLRUNTTTTSTM$COLLRUNTTTTSTM)+I(COLLRUNTTTTSTM$COLLRUNTTTTSTM^2)+I(COLLRUNTTTTSTM$COLLRUNTTTTSTM^3) + I(COLLRUNTTTTSTM$COLLRUNTTTTSTM^4),na.action="na.omit")
reg=lm(COLLRUNTTTTSTM$COLLRUNTTTTSTM~I(COLLRUNTTTTSTM$COLLRUNTTTTSTM)+I(COLLRUNTTTTSTM$COLLRUNTTTTSTM^2)+I(COLLRUNTTTTSTM$COLLRUNTTTTSTM^3) + I(COLLRUNTTTTSTM$COLLRUNTTTTSTM^4)+I(COLLRUNTTTTSTM$COLLRUNTTTTSTM^5),na.action="na.omit")

fit1 <- lm(COLLRUNTTTTSTM$COLLRUNTTTTSTM ~ poly(COLLRUNTTTTSTM$COLLRUNTTTTSTM, 1, raw=TRUE))
fit2 <- lm(COLLRUNTTTTSTM$COLLRUNTTTTSTM ~ poly(COLLRUNTTTTSTM$COLLRUNTTTTSTM, 2, raw=TRUE))
fit3 <- lm(COLLRUNTTTTSTM$COLLRUNTTTTSTM ~ poly(COLLRUNTTTTSTM$COLLRUNTTTTSTM, 3, raw=TRUE))
fit4 <- lm(COLLRUNTTTTSTM$COLLRUNTTTTSTM ~ poly(COLLRUNTTTTSTM$COLLRUNTTTTSTM, 4, raw=TRUE))
fit5 <- lm(COLLRUNTTTTSTM$COLLRUNTTTTSTM ~ poly(COLLRUNTTTTSTM$COLLRUNTTTTSTM, 5, raw=TRUE))

summary(reg1)
summary(reg2)
summary(reg3)
summary(reg4)
summary(reg)
summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)


