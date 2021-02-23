#=========setting working directory===========#

setwd('F:/arindam')

#===========Loading required packages==========#

library(rugarch)
library(moments)
library(FinTS)
library(nortest)

#======================Reading the data files==============#

y1<-data.frame(read.csv('F:/arindam/datas/BTC-INR.csv'))
ret_btc.inr<-y1[,5]  #=======close price vector for BTC-INR=======#

y2<-data.frame(read.csv('F:/arindam/datas/BTC-GBP.csv'))
ret_btc.gbp<-y2[,5]  #=======close price vector for BTc-GBP=======#

y3<-data.frame(read.csv('F:/arindam/datas/BTC-CAD.csv'))
ret_btc.cad<-y3[,5]  #=======close price vector for BTc-CAD=======#

y4<-data.frame(read.csv('F:/arindam/datas/BTC-EUR.csv'))
ret_btc.eur<-y4[,5]  #=======close price vector for BTC-EUR=======#

y5<-data.frame(read.csv('F:/arindam/datas/BTC-USD.csv'))
ret_btc.usd<-y5[,5]  #=======close price vector for BTC-USD=======#

y6<-data.frame(read.csv('F:/arindam/datas/AMRMX.csv'))
ret_AMRMX<-y6[,5]  #=======close price vector for AMRMX=======#

y7<-data.frame(read.csv('F:/arindam/datas/AWSHX.csv'))
ret_AWSHX<-y7[,5]  #=======close price vector for AWSHX=======#

y8<-data.frame(read.csv('F:/arindam/datas/nifty50.csv'))
ret_nifty50<-as.numeric(as.character(y8[,5]))  #=======close price vector for AWSHX=======#

y9<-data.frame(read.csv('F:/arindam/datas/bse.sensex.csv'))
ret_bse.sensex<-as.numeric(as.character(y9[,5]))  #=======close price vector for AWSHX=======#

#==========Calculating log returns from the closing price============#

log_ret<-function(ret){
  ret<-na.omit(ret)  
  ret1<-array(dim=length(ret)-1)
  for(i in 1:length(ret1)){
    ret1[i]=log(ret[i+1])-log(ret[i])
  }
  ret1<-as.vector(ret1)
  return(ret1)
}

#==========Calculating residual returns from the log return============#

res_ret<-function(ret){
  m<-mean(ret)
  ret1<-array(dim=length(ret))
  for(i in 1:length(ret)){
    ret1[i]=ret[i]-m
  }
  ret1<-as.vector(ret1)
  return(ret1)
}


#============Standard GARCH model with conditionally student-t distributed errors=============#

sgarch_std<-function(ret){
  sgarch_std<-ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,0)),distribution.model = "std")
  sgarchfit_std<-ugarchfit(data=ret,spec=sgarch_std)
  sgarch_std_all<-plot(sgarchfit_std, which=9)
  return(sgarch_std_all) 
}

#============Threshold GARCH model with conditionally student-t distributed error===============#

tgarch_std<-function(ret){
  tgarch_std<-ugarchspec(variance.model = list(model="gjrGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,0)),distribution.model = "std")
  tgarchfit_std<-ugarchfit(data=ret,spec=tgarch_std)
  tgarch_std_all<-plot(tgarchfit_std, which=9)
  return(tgarch_std_all)
}


#==============Integrated GARCH model with conditionally student-t distributed error=============#

igarch_std<-function(ret){
  igarch_std<-ugarchspec(variance.model = list(model="iGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,0)),distribution.model = "std")
  igarchfit_std<-ugarchfit(data=ret,spec=igarch_std)
  igarch_std_all<-plot(igarchfit_std, which=9)
  return(igarch_std_all)
}

#==============Exponential GARCH model with conditionally student-t distribution error==============# 

egarch_std<-function(ret){
  egarch_std<-ugarchspec(variance.model = list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,0)),distribution.model = "std")
  egarchfit_std<-ugarchfit(data=ret,spec=egarch_std)
  egarch_std_all<-plot(egarchfit_std, which=9)
  return(egarch_std_all)
}

#===============Standard GARCH model with conditionally generalized error distribution===============# 

sgarch_ged<-function(ret){
  sgarch_ged<-ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,0)),distribution.model = "ged")
  sgarchfit_ged<-ugarchfit(data=ret,spec=sgarch_ged)
  sgarch_ged_all<-plot(sgarchfit_ged, which=9)
  return(sgarch_ged_all)
}

#===============Threshold GARCH model with conditionally generalized error distribution===============# 

tgarch_ged<-function(ret){
  tgarch_ged<-ugarchspec(variance.model = list(model="gjrGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,0)),distribution.model = "ged")
  tgarchfit_ged<-ugarchfit(data=ret,spec=tgarch_ged)
  tgarch_ged_all<-plot(tgarchfit_ged, which=9)
  return(tgarch_ged_all)
}

#===============Integrated GARCH model with conditionally generalized error distribution==============#
igarch_ged<-function(ret){
  igarch_ged<-ugarchspec(variance.model = list(model="iGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,0)),distribution.model = "ged")
  igarchfit_ged<-ugarchfit(data=ret,spec=igarch_ged)
  igarch_ged_all<-plot(igarchfit_ged, which=9)
  return(igarch_ged_all)
}

#===============Exponential GARCH model with conditionally generalized error distribution===============#

egarch_ged<-function(ret){
  egarch_ged<-ugarchspec(variance.model = list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,0)),distribution.model = "ged")
  egarchfit_ged<-ugarchfit(data=ret,spec=egarch_ged)
  egarch_ged_all<-plot(egarchfit_ged, which=9)
  return(egarch_ged_all)
}

#==============Standard GARCH model with conditionally normal inverse gaussion distribution error===========#
sgarch_nig<-function(ret){
  sgarch_nig<-ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,0)),distribution.model = "nig")
  sgarchfit_nig<-ugarchfit(data=ret,spec=sgarch_nig)
  sgarch_nig_all<-plot(sgarchfit_nig, which=9)
  return(sgarch_nig_all)
}

#===============Threshold GARCH model with conditionally normal inverse gaussion distribution error============#
tgarch_nig<-function(ret){
  tgarch_nig<-ugarchspec(variance.model = list(model="gjrGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,0)),distribution.model = "nig")
  tgarchfit_nig<-ugarchfit(data=ret,spec=tgarch_nig)
  tgarch_nig_all<-plot(tgarchfit_nig, which=9)
  return(tgarch_nig_all)
}

#===============Integrated GARCH model with conditionally normal inverse gaussion distribution error============#
igarch_nig<-function(ret){
  igarch_nig<-ugarchspec(variance.model = list(model="iGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,0)),distribution.model = "nig")
  igarchfit_nig<-ugarchfit(data=ret,spec=igarch_nig)
  igarch_nig_all<-plot(igarchfit_nig, which=9)
  return(igarch_nig_all)
}

#===============Exponential GARCH model with conditionally normal inverse gaussion distribution error============#
egarch_nig<-function(ret){
  egarch_nig<-ugarchspec(variance.model = list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,0)),distribution.model = "nig")
  egarchfit_nig<-ugarchfit(data=ret,spec=egarch_nig)
  egarch_nig_all<-plot(egarchfit_nig, which=9)
  return(egarch_nig_all)
}

#============volatility forecasting==========#

forecasting_volatility1<-function(ret){
  egarch_nig<-ugarchspec(variance.model = list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,0)),distribution.model = "nig")
  egarchfit<-ugarchfit(data=ret, spec=egarch_nig)
  garchforecast<-ugarchforecast(fitORspec=egarchfit, n.ahead=5)
  vol<-sigma(garchforecast)
  return(vol)                  #=========optimal model is eGARCH(1,1)-nig========#
}

forecasting_volatility2<-function(ret){
  egarch_std<-ugarchspec(variance.model = list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,0)),distribution.model = "std")
  egarchfit<-ugarchfit(data=ret, spec=egarch_std)
  garchforecast<-ugarchforecast(fitORspec=egarchfit, n.ahead=5)
  vol<-sigma(garchforecast)
  return(vol)
}                               #==========optimal model is eGARCH(1,1)-std=========#

#============mean forecasting===============#

forecasting_mean1<-function(ret){
  egarch_nig<-ugarchspec(variance.model = list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,0)),distribution.model = "nig")
  egarchfit<-ugarchfit(data=ret, spec=egarch_nig)
  garchforecast<-ugarchforecast(fitORspec=egarchfit, n.ahead=5)
  mean<-fitted(garchforecast)
  return(mean)
}                         #=========optimal model is eGARCH(1,1)-nig========#

forecasting_mean2<-function(ret){
  egarch_std<-ugarchspec(variance.model = list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,0)),distribution.model = "std")
  egarchfit<-ugarchfit(data=ret, spec=egarch_std)
  garchforecast<-ugarchforecast(fitORspec=egarchfit, n.ahead=5)
  mean<-fitted(garchforecast)
  return(mean)
}                        #==========optimal model is eGARCH(1,1)-std=========#

#============result for BTC-INR=============#

ret1<-log_ret(ret_btc.inr)           
ret1                                     #==============log-return series of BTC-INR================#
res1<-res_ret(ret1)
res1                                     #==============residuals series of BTC-INR=================#

plot(ret_btc.inr)                     #==============time series plot for close price of BTC-INR==============#  
plot(ret1)                            #==============time series plot for log-return series of BTC-INR============#
hist(res1)                            #==============histogram plot of residuals series===============#
qqnorm(res1)                          #==============q-q plot of residuals series============#

jarque.test(res1)                          #===============Jarque-Bera test for residuals series of BTC-INR=============#        
ad.test(res1)                              #===============Anderson-Darling test for residuals series of BTC-INR===========#
Box.test(ret1, lag=15, type="Ljung-Box")   #===============Ljung-BOx test for log-return series of BTC-INR============#
ArchTest(ret1,lags=12,demean=F)            #===============Lagrange-Multiplier test for log-return series of BTC-INR===========#

sgarch_std(ret1)             #==============sGARCH(1,1)-std model for BTC-INR=============#                        
sgarch_nig(ret1)             #==============sGARCH(1,1)-nig model for BTC-INR=============#  
sgarch_ged(ret1)             #==============sGARCH(1,1)-ged model for BTC-INR=============#  
igarch_std(ret1)             #==============iGARCH(1,1)-std model for BTC-INR=============#  
igarch_nig(ret1)             #==============iGARCH(1,1)-nig model for BTC-INR=============#  
igarch_ged(ret1)             #==============iGARCH(1,1)-ged model for BTC-INR=============#  
tgarch_std(ret1)             #==============tGARCH(1,1)-std model for BTC-INR=============#  
tgarch_nig(ret1)             #==============tGARCH(1,1)-nig model for BTC-INR=============#  
tgarch_ged(ret1)             #==============tGARCH(1,1)-ged model for BTC-INR=============#  
egarch_std(ret1)             #==============eGARCH(1,1)-std model for BTC-INR=============#  
egarch_nig(ret1)             #==============eGARCH(1,1)-nig model for BTC-INR=============#  
egarch_ged(ret1)             #==============eGARCH(1,1)-ged model for BTC-INR=============#

forecasting_volatility1(ret1)       #=========volatility forecasting==========#
mean(ret1)                          #=========mean of return series========#

#============result for BTC-GBP=============#

ret2<-log_ret(ret_btc.gbp)           
ret2                                     #==============log-return series of BTC-GBP================#
res2<-res_ret(ret2)
res2                                     #==============residuals series of BTC-GBP=================#

plot(ret_btc.gbp)                     #==============time series plot for close price of BTC-GBP==============#  
plot(ret2)                            #==============time series plot for log-return series of BTC-GBP=============#
hist(res2)                            #==============histogram plot of residuals series===============#
qqnorm(res2)                          #==============q-q plot of residuals series============#

jarque.test(res2)                          #===============Jarque-Bera test for residuals series of BTC-GBP=============#        
ad.test(res2)                              #===============Anderson-Darling test for residuals series of BTC-GBP===========#
Box.test(ret2, lag=15, type="Ljung-Box")   #===============Ljung-BOx test for log-return series of BTC-GBP============#
ArchTest(ret2,lags=12,demean=F)            #===============Lagrange-Multiplier test for log-return series of BTC-GBP===========#

sgarch_std(ret2)             #==============sGARCH(1,1)-std model for BTC-GBP=============#                        
sgarch_nig(ret2)             #==============sGARCH(1,1)-nig model for BTC-GBP=============#  
sgarch_ged(ret2)             #==============sGARCH(1,1)-ged model for BTC-GBP=============#  
igarch_std(ret2)             #==============iGARCH(1,1)-std model for BTC-GBP=============#  
igarch_nig(ret2)             #==============iGARCH(1,1)-nig model for BTC-GBP=============#  
igarch_ged(ret2)             #==============iGARCH(1,1)-ged model for BTC-GBP=============#  
tgarch_std(ret2)             #==============tGARCH(1,1)-std model for BTC-GBP=============#  
tgarch_nig(ret2)             #==============tGARCH(1,1)-nig model for BTC-GBP=============#  
tgarch_ged(ret2)             #==============tGARCH(1,1)-ged model for BTC-GBP=============#  
egarch_std(ret2)             #==============eGARCH(1,1)-std model for BTC-GBP=============#  
egarch_nig(ret2)             #==============eGARCH(1,1)-nig model for BTC-GBP=============#  
egarch_ged(ret2)             #==============eGARCH(1,1)-ged model for BTC-GBP=============#

forecasting_volatility2(ret2)       #=========volatility forecasting==========#
mean(ret2)                          #=========mean of return series========#

#============result for BTC-CAD=============#

ret3<-log_ret(ret_btc.cad)           
ret3                                     #==============log-return series of BTC-CAD================#
res3<-res_ret(ret3)
res3                                     #==============residuals series of BTC-CAD=================#

plot(ret_btc.cad)                     #==============time series plot for close price of BTC-CAD==============#  
plot(ret3)                            #==============time series plot for log-return series of BTC-CAD=============#
hist(res3)                            #==============histogram plot of residuals series===============#
qqnorm(res3)                          #==============q-q plot of residuals series============#

jarque.test(res3)                          #===============Jarque-Bera test for residuals series of BTC-CAD=============#        
ad.test(res3)                              #===============Anderson-Darling test for residuals series of BTC-CAD===========#
Box.test(ret3, lag=15, type="Ljung-Box")   #===============Ljung-BOx test for log-return series of BTC-CAD============#
ArchTest(ret3,lags=12,demean=F)            #===============Lagrange-Multiplier test for log-return series of BTC-CAD===========#

sgarch_std(ret3)             #==============sGARCH(1,1)-std model for BTC-CAD=============#                        
sgarch_nig(ret3)             #==============sGARCH(1,1)-nig model for BTC-CAD=============#  
sgarch_ged(ret3)             #==============sGARCH(1,1)-ged model for BTC-CAD=============#  
igarch_std(ret3)             #==============iGARCH(1,1)-std model for BTC-CAD=============#  
igarch_nig(ret3)             #==============iGARCH(1,1)-nig model for BTC-CAD=============#  
igarch_ged(ret3)             #==============iGARCH(1,1)-ged model for BTC-CAD=============#  
tgarch_std(ret3)             #==============tGARCH(1,1)-std model for BTC-CAD=============#  
tgarch_nig(ret3)             #==============tGARCH(1,1)-nig model for BTC-CAD=============#  
tgarch_ged(ret3)             #==============tGARCH(1,1)-ged model for BTC-CAD=============#  
egarch_std(ret3)             #==============eGARCH(1,1)-std model for BTC-CAD=============#  
egarch_nig(ret3)             #==============eGARCH(1,1)-nig model for BTC-CAD=============#  
egarch_ged(ret3)             #==============eGARCH(1,1)-ged model for BTC-CAD=============#  

forecasting_volatility2(ret3)       #=========volatility forecasting==========#
mean(ret3)                          #=========mean of return series========#

#============result for BTC-EUR=============#

ret4<-log_ret(ret_btc.eur)           
ret4                                     #==============log-return series of BTC-EUR================#
res4<-res_ret(ret4)
res4                                     #==============residuals series of BTC-EUR=================#

plot(ret_btc.eur)                     #==============time series plot for close price of BTC-EUR==============#  
plot(ret4)                            #==============time series plot for log-return series of BTC-EUR=============#
hist(res4)                            #==============histogram plot of residuals series===============#
qqnorm(res4)                          #==============q-q plot of residuals series============#

jarque.test(res4)                          #===============Jarque-Bera test for residuals series of BTC-EUR=============#        
ad.test(res4)                              #===============Anderson-Darling test for residuals series of BTC-EUR===========#
Box.test(ret4, lag=15, type="Ljung-Box")   #===============Ljung-BOx test for log-return series of BTC-EUR============#
ArchTest(ret4,lags=12,demean=F)            #===============Lagrange-Multiplier test for log-return series of BTC-EUR===========#

sgarch_std(ret4)             #==============sGARCH(1,1)-std model for BTC-EUR=============#                        
sgarch_nig(ret4)             #==============sGARCH(1,1)-nig model for BTC-EUR=============#  
sgarch_ged(ret4)             #==============sGARCH(1,1)-ged model for BTC-EUR=============#  
igarch_std(ret4)             #==============iGARCH(1,1)-std model for BTC-EUR=============#  
igarch_nig(ret4)             #==============iGARCH(1,1)-nig model for BTC-EUR=============#  
igarch_ged(ret4)             #==============iGARCH(1,1)-ged model for BTC-EUR=============#  
tgarch_std(ret4)             #==============tGARCH(1,1)-std model for BTC-EUR=============#  
tgarch_nig(ret4)             #==============tGARCH(1,1)-nig model for BTC-EUR=============#  
tgarch_ged(ret4)             #==============tGARCH(1,1)-ged model for BTC-EUR=============#  
egarch_std(ret4)             #==============eGARCH(1,1)-std model for BTC-EUR=============#  
egarch_nig(ret4)             #==============eGARCH(1,1)-nig model for BTC-EUR=============#  
egarch_ged(ret4)             #==============eGARCH(1,1)-ged model for BTC-EUR=============# 

forecasting_volatility2(ret4)       #=========volatility forecasting==========#
mean(ret4)                          #=========mean of return series========#

#============result for BTC-USD=============#

ret5<-log_ret(ret_btc.usd)           
ret5                                     #==============log-return series of BTC-USD================#
res5<-res_ret(ret5)
res5                                     #==============residuals series of BTC-USD=================#

plot(ret_btc.usd)                     #==============time series plot for close price of BTC-USD==============#  
plot(ret5)                            #==============time series plot for log-return series of BTC-USD=============#
hist(res5)                            #==============histogram plot of residuals series===============#
qqnorm(res5)                          #==============q-q plot of residuals series============#

jarque.test(res5)                          #===============Jarque-Bera test for residuals series of BTC-USD=============#        
ad.test(res5)                              #===============Anderson-Darling test for residuals series of BTC-USD===========#
Box.test(ret5, lag=15, type="Ljung-Box")   #===============Ljung-BOx test for log-return series of BTC-USD============#
ArchTest(ret5,lags=12,demean=F)            #===============Lagrange-Multiplier test for log-return series of BTC-USD===========#

sgarch_std(ret5)             #==============sGARCH(1,1)-std model for BTC-USD=============#                        
sgarch_nig(ret5)             #==============sGARCH(1,1)-nig model for BTC-USD=============#  
sgarch_ged(ret5)             #==============sGARCH(1,1)-ged model for BTC-USD=============#  
igarch_std(ret5)             #==============iGARCH(1,1)-std model for BTC-USD=============#  
igarch_nig(ret5)             #==============iGARCH(1,1)-nig model for BTC-USD=============#  
igarch_ged(ret5)             #==============iGARCH(1,1)-ged model for BTC-USD=============#  
tgarch_std(ret5)             #==============tGARCH(1,1)-std model for BTC-USD=============#  
tgarch_nig(ret5)             #==============tGARCH(1,1)-nig model for BTC-USD=============#  
tgarch_ged(ret5)             #==============tGARCH(1,1)-ged model for BTC-USD=============#  
egarch_std(ret5)             #==============eGARCH(1,1)-std model for BTC-USD=============#  
egarch_nig(ret5)             #==============eGARCH(1,1)-nig model for BTC-USD=============#  
egarch_ged(ret5)             #==============eGARCH(1,1)-ged model for BTC-USD=============# 

forecasting_volatility1(ret5)       #=========volatility forecasting==========#
mean(ret5)                          #=========mean of return series========#

#============result for AMRMX=============#

ret6<-log_ret(ret_AMRMX)           
ret6                                     #==============log-return series of AMRMX================#
res6<-res_ret(ret6)
res6                                     #==============residuals series of AMRMX=================#

plot(ret_AMRMX)                     #==============time series plot for close price of AMRMX==============#  
plot(ret6)                            #==============time series plot for log-return series of AMRMX=============#
hist(res6)                            #==============histogram plot of residuals series===============#
qqnorm(res6)                          #==============q-q plot of residuals series============#

jarque.test(res6)                          #===============Jarque-Bera test for residuals series of AMRMX=============#        
ad.test(res6)                              #===============Anderson-Darling test for residuals series of AMRMX===========#
Box.test(ret6, lag=15, type="Ljung-Box")   #===============Ljung-BOx test for log-return series of AMRMX============#
ArchTest(ret6,lags=12,demean=F)            #===============Lagrange-Multiplier test for log-return series of AMRMX===========#

sgarch_std(ret6)             #==============sGARCH(1,1)-std model for AMRMX=============#                        
sgarch_nig(ret6)             #==============sGARCH(1,1)-nig model for AMRMX=============#  
sgarch_ged(ret6)             #==============sGARCH(1,1)-ged model for AMRMX=============#  
igarch_std(ret6)             #==============iGARCH(1,1)-std model for AMRMX=============#  
igarch_nig(ret6)             #==============iGARCH(1,1)-nig model for AMRMX=============#  
igarch_ged(ret6)             #==============iGARCH(1,1)-ged model for AMRMX=============#  
tgarch_std(ret6)             #==============tGARCH(1,1)-std model for AMRMX=============#  
tgarch_nig(ret6)             #==============tGARCH(1,1)-nig model for AMRMX=============#  
tgarch_ged(ret6)             #==============tGARCH(1,1)-ged model for AMRMX=============#  
egarch_std(ret6)             #==============eGARCH(1,1)-std model for AMRMX=============#  
egarch_nig(ret6)             #==============eGARCH(1,1)-nig model for AMRMX=============#  
egarch_ged(ret6)             #==============eGARCH(1,1)-ged model for AMRMX=============# 

forecasting_volatility1(ret6)       #=========volatility forecasting==========#
mean(ret6)                          #=========mean of return series========#
#============result for AWSHX=============#

ret7<-log_ret(ret_AWSHX)           
ret7                                     #==============log-return series of AWSHX================#
res7<-res_ret(ret7)
res7                                     #==============residuals series of AWSHX=================#

plot(ret_AWSHX)                     #==============time series plot for close price of AWSHX==============#  
plot(ret7)                            #==============time series plot for log-return series of AWSHX=============#
hist(ret7)                            #==============histogram plot of log-return series==============#
hist(res7)                            #==============histogram plot of residuals series===============#
qqnorm(ret7)                          #==============q-q plot of return series===========#
qqnorm(res7)                          #==============q-q plot of residuals series============#

jarque.test(res7)                          #===============Jarque-Bera test for residuals series of AWSHX=============#        
ad.test(res7)                              #===============Anderson-Darling test for residuals series of AWSHX===========#
Box.test(ret7, lag=15, type="Ljung-Box")   #===============Ljung-BOx test for log-return series of AWSHX============#
ArchTest(ret7,lags=12,demean=F)            #===============Lagrange-Multiplier test for log-return series of AWSHX===========#

sgarch_std(ret7)             #==============sGARCH(1,1)-std model for AWSHX=============#                        
sgarch_nig(ret7)             #==============sGARCH(1,1)-nig model for AWSHX=============#  
sgarch_ged(ret7)             #==============sGARCH(1,1)-ged model for AWSHX=============#  
igarch_std(ret7)             #==============iGARCH(1,1)-std model for AWSHX=============#  
igarch_nig(ret7)             #==============iGARCH(1,1)-nig model for AWSHX=============#  
igarch_ged(ret7)             #==============iGARCH(1,1)-ged model for AWSHX=============#  
tgarch_std(ret7)             #==============tGARCH(1,1)-std model for AWSHX=============#  
tgarch_nig(ret7)             #==============tGARCH(1,1)-nig model for AWSHX=============#  
tgarch_ged(ret7)             #==============tGARCH(1,1)-ged model for AWSHX=============#  
egarch_std(ret7)             #==============eGARCH(1,1)-std model for AWSHX=============#  
egarch_nig(ret7)             #==============eGARCH(1,1)-nig model for AWSHX=============#  
egarch_ged(ret7)             #==============eGARCH(1,1)-ged model for AWSHX=============#

forecasting_volatility1(ret7)       #=========volatility forecasting==========#
mean(ret8)                          #=========mean of return series========#

#============result for nifty50=============#

ret8<-log_ret(ret_nifty50)           
ret8                                     #==============log-return series of nifty50================#
res8<-res_ret(ret8)
res8                                     #==============residuals series of nifty50=================#

plot(ret_nifty50)                     #==============time series plot for close price of nifty50==============#  
plot(ret8)                            #==============time series plot for log-return series of nifty50=============#                            
hist(res8)                            #==============histogram plot of residuals series===============#
qqnorm(res8)                          #==============q-q plot of residuals series============#

jarque.test(res8)                          #===============Jarque-Bera test for residuals series of nifty50=============#        
ad.test(res8)                              #===============Anderson-Darling test for residuals series of nifty50===========#
Box.test(ret8, lag=15, type="Ljung-Box")   #===============Ljung-BOx test for log-return series of nifty50============#
ArchTest(ret8,lags=12,demean=F)            #===============Lagrange-Multiplier test for log-return series of nifty50===========#

sgarch_std(ret8)             #==============sGARCH(1,1)-std model for nifty50=============#                        
sgarch_nig(ret8)             #==============sGARCH(1,1)-nig model for nifty50=============#  
sgarch_ged(ret8)             #==============sGARCH(1,1)-ged model for nifty50=============#  
igarch_std(ret8)             #==============iGARCH(1,1)-std model for nifty50=============#  
igarch_nig(ret8)             #==============iGARCH(1,1)-nig model for nifty50=============#  
igarch_ged(ret8)             #==============iGARCH(1,1)-ged model for nifty50=============#  
tgarch_std(ret8)             #==============tGARCH(1,1)-std model for nifty50=============#  
tgarch_nig(ret8)             #==============tGARCH(1,1)-nig model for nifty50=============#  
tgarch_ged(ret8)             #==============tGARCH(1,1)-ged model for nifty50=============#  
egarch_std(ret8)             #==============eGARCH(1,1)-std model for nifty50=============#  
egarch_nig(ret8)             #==============eGARCH(1,1)-nig model for nifty50=============#  
egarch_ged(ret8)             #==============eGARCH(1,1)-ged model for nifty50=============#

forecasting_volatility1(ret8)       #=========volatility forecasting==========#
mean(ret8)                          #=========mean of return series========#

#============result for bse.sensex=============#

ret9<-log_ret(ret_bse.sensex)           
ret9                                     #==============log-return series of bse.sensex================#
res9<-res_ret(ret9)
res9                                     #==============residuals series of bse.sensex=================#

plot(ret_bse.sensex)                     #==============time series plot for close price of bse.sensex==============#  
plot(ret9)                            #==============time series plot for log-return series of bse.sensex=============#
hist(res9)                            #==============histogram plot of residuals series===============#
qqnorm(res9)                          #==============q-q plot of residuals series============#


jarque.test(res9)                          #===============Jarque-Bera test for residuals series of bse.sensex=============#        
ad.test(res9)                              #===============Anderson-Darling test for residuals series of bse.sensex===========#
Box.test(ret9, lag=15, type="Ljung-Box")   #===============Ljung-BOx test for log-return series of bse.sensex============#
ArchTest(ret9,lags=12,demean=F)            #===============Lagrange-Multiplier test for log-return series of bse.sensex===========#

sgarch_std(ret9)             #==============sGARCH(1,1)-std model for bse.sensex=============#                        
sgarch_nig(ret9)             #==============sGARCH(1,1)-nig model for bse.sensex=============#  
sgarch_ged(ret9)             #==============sGARCH(1,1)-ged model for bse.sensex=============#  
igarch_std(ret9)             #==============iGARCH(1,1)-std model for bse.sensex=============#  
igarch_nig(ret9)             #==============iGARCH(1,1)-nig model for bse.sensex=============#  
igarch_ged(ret9)             #==============iGARCH(1,1)-ged model for bse.sensex=============#  
tgarch_std(ret9)             #==============tGARCH(1,1)-std model for bse.sensex=============#  
tgarch_nig(ret9)             #==============tGARCH(1,1)-nig model for bse.sensex=============#  
tgarch_ged(ret9)             #==============tGARCH(1,1)-ged model for bse.sensex=============#  
egarch_std(ret9)             #==============eGARCH(1,1)-std model for bse.sensex=============#  
egarch_nig(ret9)             #==============eGARCH(1,1)-nig model for bse.sensex=============#  
egarch_ged(ret9)             #==============eGARCH(1,1)-ged model for bse.sensex=============# 

forecasting_volatility1(ret9)       #=========volatility forecasting==========#
mean(ret9)                          #=========mean of return series========#

