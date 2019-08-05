


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
erford = rford-ustb3m
ersandp = rsandp-ustb3m


par(mfcol = c(1,1), oma = c(0,0,1,0) + 0.2, mar = c(0,1,0,0) + 1, mgp = c(0, 0.2, 0))
plot(data$Date[-1],erford,type="p",xaxs="i",yaxs="i",las=1,xlab="",ylab="",main="",tck=0.02,ylim=c(-100,100),pch=20)
points(data$Date[-1],ersandp,col="steelblue4",pch=20)
abline(h=0,lty=2)
legend("topleft",c("FORD","SANDP"),col=c(1,"steelblue4"),lty=c(1,1),lwd=2,box.col="white")
box()


lm1 = lm(data$erford~data$ersandp)
summary(lm1)

