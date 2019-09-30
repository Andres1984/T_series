
library("quantmod")
getSymbols(c("GDPC1","PCECC96","GPDIC1","GCEC1","EXPGSC1","IMPGSC1"),src="FRED")
Y = as.matrix(cbind(GDPC1,PCECC96,GPDIC1,GCEC1,EXPGSC1,IMPGSC1))
colnames(Y)=c("GDP","C","I","G","X","M")

k = ncol(Y)
par(mfrow=c(3,2))
for (i in 1:k) {
  plot(Y[,i],type="l",xaxs="i",las=1); abline(h=0)
}

### KEYNESIAN CROSS (NOWCASTING)
par(mfrow=c(1,1))
lm1 = lm(Y[,2]~Y[,1])
summary(lm1) # MPC = 0.7
date = as.Date(rownames(Y))
plot(date,Y[,2],type="l",xaxs="i",las=1)
lines(date,lm1$fitted.values,col=2)

# Underestimate consumption in time where uncertainty (wars, crises) was high
plot(date,log(Y[,2]),type="l",xaxs="i",las=1)
lines(date,log(lm1$fitted.values),col=2)
plot(density(lm1$residuals)) # looks normal
arma11 = arima(lm1$residuals,c(1,0,1))
plot(lm1$residuals,type="l")
lines(lm1$residuals-arma11$residuals,col=2)
plot(date,arma11$residuals,type="l",xaxs="i",las=1)
plot(date,abs(arma11$residuals),type="l",xaxs="i",las=1,col="steelblue4") # volatility clustering
sma1 = TTR::SMA(abs(arma11$residuals),10)
lines(date,c(sma1[-c(1:5)],rep(NA,5)),col="brown3",lwd=1.5,type="h")
# -1955 WWII
# 1973-1979 OIL CRISIS
# 1980 OIL GLUT
# 1981-1988 IRAN IRAQ WAR
# 2000 DOTCOM CRISIS
# 2007-2009 GREAT RECESSION
# 2012 EUROPEAN GOVERNMENTAL DEBT CRISIS

# Calculate (standardized) Returns
y = scale(100*((Y[-1,])-(Y[-nrow(Y),]))/(Y[-nrow(Y),]),T,T)
colnames(y)=c("GDP","C","I","G","X","M")
par(mfrow=c(3,2))
for (i in 1:k) {
  plot(date[-1],y[,i],type="l",xaxs="i",las=1,col="steelblue4",main=colnames(y)[i],xlab="",ylab="")
  grid()
  abline(h=0)
}

# Comovements
par(mfrow=c(1,1))
plot(scale(y[,1],T,T),type="l",xaxs="i",las=1)
grid()
for (i in 1:k) {
  lines(scale(y[,i],T,T),col=i)
}
abline(h=0)


### FORECASTING USING VAR(1)
library("vars")
var1 = VAR(y,p=1)
summary(var1)
plot(irf(var1, ortho=T,n.ahead=20,cumulative=F,runs=300,ci=0.9))
plot(irf(var1, ortho=T,n.ahead=20,cumulative=T,runs=300,ci=0.9))
plot(fevd(var1,ortho=T,n.ahead=20,cumulative=F,runs=300,ci=0.9))

### END
