### Sesion 3

## Pronóstico

set.seed(12345)

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

x=rep(0,100)
for (i in 1:100) {
  x[i]=rnorm(i)
}

x

# Ejemplo
S=rep(0,12)
S[1]=S0
for (i in 1:12) {
S[i+1]=S[i]+(mu*S[i]*1+sigma*S[i]*rnorm(i))

  
}

S=as.data.frame(S)


SM <- matrix(data = rep(0,1200 ), nrow = 100, ncol =12)
SM[,1]<-as.numeric(tail(MSFT$MSFT.Close,1))


x=matrix(data = rnorm(1200), nrow = 100, ncol =12)
for (i in 1:nrow(x)) {
  for (c in 2:ncol(x)){ 
  x[i,c]=rnorm(i)
}
}
x



# Create the loop with r and c to iterate over the matrix
for (r in 1:dim(SM)[1])  {  
  for (c in 2:dim(SM)[2]-1) { 
    
    SM[r,c+1]=SM[r,c]+(mu*SM[r,c]*1+sigma*SM[r,c]*x[r,c])
  }
}
      

SM=as.data.frame(SM)

SM=t(SM)
tiempo=0:11
par(mfrow=c(2,2))
matplot(tiempo,SM[,1:100],  type='l')

library(dplyr)
EVSM=rowMeans(SM)
VOLSM=sqrt(rowMeans(SM^{2})-EVSM^{2})
SMC=as.data.frame(EVSM)
SMC=mutate(SMC, VOLSU =EVSM+VOLSM)
SMC=mutate(SMC, VOLSD =EVSM-VOLSM)

matplot(SMC[,], type='l')

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


## Promedio Movil

