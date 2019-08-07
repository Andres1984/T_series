### Sesion 3

## Pronóstico



library(quantmod)
getSymbols("MSFT", src="yahoo", from="2018-07-01",to="2019-07-21")

tail(MSFT$MSFT.Close)
msft=Delt(MSFT$MSFT.Close)[-1]


# dSt=muStdt+sigmaStraiz(t)Z

mu=mean(msft)*20
sigma=sd(msft)*sqrt(20)

S0=as.numeric(tail(MSFT$MSFT.Close,1))
S0


dS1=mu*S0*1+sigma*S0+rnorm(1)
S1=S0+dS1
S1


dS2=mu*S1*1+sigma*S1+rnorm(1)
S2=S1+dS2
S2

dS3=mu*S2*1+sigma*S2+rnorm(1)
S3=S2+dS3
S3


## Hacerlo para 12 números en una sola función

x=matrix(rep(0,100),nrow=50,ncol=50)
for (i in 1:1000) {
  x[i,]=rnorm(i)
}

x

# Ejemplo
S=rep(0,12)
S[1]=S0
for (i in 1:12) {
S[i+1]=S[i]+(mu*S[i]*1+sigma*S[i]*rnorm(i))

  
}

S=as.data.frame(S)






## Rolling con un año.


library(dplyr)


MSFTM=subset(MSFT$MSFT.Close, )
days <- c("2018-07-02","2018-08-01","2018-09-01","2018-10-01","2018-11-01","2018-12-03", "2019-01-02","2019-02-01","2019-03-01","2019-04-01","2019-05-01","2019-06-03","2019-07-01") 


MSFTM=MSFT$MSFT.Close[days]

RS=rep(0,11)
RS[1]=MSFTM[1]


MSFTM=as.numeric(MSFTM)

for (i in 1:11) {
  RS[i+1]=MSFTM[i]+(mu*MSFTM[i]*1+sigma*MSFTM[i]*rnorm(i))
  
  
}

RS=as.data.frame(RS)
RS$M=MSFTM

#Medidas de error.

#RMSE

RMSE=sqrt((sum((RS$RS-RS$M)^2)/12))
RMSE
library(Metrics)
rmse(RS$RS,RS$M)
mae(RS$RS,RS$M)
mape(RS$RS,RS$M)

