
## Simulate VAR(2)???data 1 
library (dse) 
library(vars) 
## Setting the lag???polynomial A(L) 
Apoly = array(c(1.0, -0.5, 0.3, 0, 0.2, 0.1, 0, -0.2, 0.7, 1, 0.5, -0.3), c(3, 2, 2)) 
## Setting Covariance to identity ???matrix 
B <- diag( 2 )
## Setting constant term to 5 and 10 
TRD<-c(5, 10) 
## Generating the VAR(2) model
var2 <- ARMA(A = Apoly , B = B, TREND = TRD) 
## Simulating 500 observations 
varsim <- simulate ( var2 , sampleT = 500 ,noise = list (w = matrix(rnorm(1000) , nrow = 500, ncol = 2)) , rng = list (seed = c(123456))) 
## Obtaining the generated series 
vardat <- matrix(varsim$output, nrow = 500, ncol = 2) 
colnames(vardat) <- c("y1", "y2")  
## Plotting the series 
plot.ts(vardat , main = "", xlab = "") 
## Determining an appropriate lag???order 
infocrit <- VARselect(vardat, lag.max = 3, type = c("const"))
infocrit
## Estimating the model 
varsimest<-VAR(vardat, p=2, type="const", season = NULL, exogen = NULL) 
## Alternatively , selection according to AIC 
varsimest <- VAR( vardat , type = c("const"), lag.max = 3, ic = "SC") 
## Checking the roots 
varsimest
class(varsimest)



## testing serial correlation Diagn?stico del modelo
args(serial.test) 
## Portmanteau???Test 
var2c.serial <- serial.test(varsimest, lags.pt = 16,type = "BG") 
var2c.serial 
plot(var2c.serial , names = "y1") 
plot(var2c.serial , names = "y2") 
## testing heteroscedasticity 
args(arch.test) #
var2c.arch<-arch.test(varsimest,lags.multi = 5, multivariate.only = TRUE) 
var2c.arch 
## testing for normality 
args(normality.test) 
var2c.norm <- normality.test(varsimest ,multivariate.only = TRUE) 
var2c.norm 
## class and methods for diganostic tests 
class(var2c.serial) 
class(var2c.arch) 
class(var2c.norm) 
methods( class = "varcheck" ) 
## Plot of objects varcheck  

plot(var2c.serial , names = "y1")


# Estabilidad del proceso


reccusum <- stability (varsimest , type =c("OLS-CUSUM"))
fluctuation <- stability (varsimest, type = "fluctuation")
reccusum
fluctuation
plot(reccusum)
plot(fluctuation)
## Causality tests 1#
## Granger and instantaneous causality 
var.causal <- causality(varsimest , cause = "y2")
var.causal


#Pron?stico


# Forecasting objects of class varest
args(vars ::: predict.varest)
predictions <- predict(varsimest , n.ahead = 25, ci=0.95)
class(predictions)
args(vars ::: plot.varprd)
## Plot of predictions for y1
plot ( predictions , names = "y1 ")
## Fanchart for y2
args(fanchart)
fanchart(predictions , names = "y2")

## Impulso Respuesta



## Impulse response analysis 1 
irf.y1 <- irf(varsimest,impulse= "y1",response = "y2",n.ahead = 10, ortho = TRUE, cumulative = FALSE, boot = FALSE, seed = 12345) 
args(vars:::plot.varirf) 
plot(irf.y1) 
irf.y2 <- irf(varsimest,impulse = "y2",response = "y1", n.ahead = 10, ortho = TRUE, cumulative = TRUE,boot = FALSE, seed = 12345)
plot(irf.y2)



## Forecast error variance decomposition 
fevd.var2 <-fevd(varsimest , n.ahead = 10) 
args(vars ::: plot.varfevd) 
plot(fevd.var2, addbars = 2) 

