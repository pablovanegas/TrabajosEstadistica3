#LIBRERIAS Y PAQUETES ----------------------------------------------------------
rm(list=ls(all=TRUE))
library(forecast)
library(lmtest)
library(TSA)
library(uroot)
library(pdR)
source("https://raw.githubusercontent.com/NelfiGonzalez/Funciones-de-Usuario-Estadistica-III/main/Funciones-Criterios.Informacion-Calidad.Intervalos.R")
source("https://raw.githubusercontent.com/NelfiGonzalez/Funciones-de-Usuario-Estadistica-III/main/Funciones-BP.LB.test-pruebaDW1.R")

#Leer anex-EMC-SeriesIndiceEmpalmados-vtas reales lineas mercancia-may2023.csv, columna 12: 6. Productos de aseo personal, cosm?ticos y perfumer?a
Datos24=read.table(file.choose(),header=T,sep=";",skip=10,dec=",",colClasses=c(rep("NULL",11),"numeric",rep("NULL",10)))
Datos24=ts(Datos24,freq=12,start=c(2013,1))

#LECTURA DE DATOS --------------------------------------------------------------

m=12 
n=length(Datos24)-m
t=1:n

#Serie recortada
yt=ts(Datos24[t],freq=m,start=c(2013,1))

#Datos para la validación cruzada
tnuevo=(n+1):length(Datos24)
ytnuevo=ts(Datos24[tnuevo],freq=m,start=c(2022,6)) 

#PUNTO 1 Analisis descriptivo y test HEGY --------------------------------------
#--PUNTO b ---------------------------------------------------------------------
#serie recortada

win.graph()
plot(Datos24,ylab="Datos24",main="Gráfica de la serie")

win.graph()
plot(yt,ylab="yt",main="Gráfica de la serie recortada")
#ACF
win.graph()
acf(as.numeric(yt),lag.max=36,ci.type="ma",main="ACF yt ")

#primera diferencia regular 
difd1=diff(yt)
win.graph()
plot(difd1,ylab="difd1",main="Gráfica primera diferencia regular")
#ACF
win.graph()
acf(as.numeric(difd1),lag.max=36,ci.type="ma",lwd=2,main="ACF primera diferencia regular") 
abline(v=c(12,24,36),lty=2,col=2)

#primera diferencia estacional
difD12=diff(yt,lag=m)
win.graph()
plot(difD12,ylab="difD12",main="Gráfica primera diferencia estacional")
#ACF
win.graph()
acf(as.numeric(difD12),lag.max=36,ci.type="ma",lwd=2,main="ACF primera diferencia estacional") 
abline(v=c(12,24,36),lty=2,col=2)

#serie diferenciada por tendencia y estacionalidad
difdD12=diff(diff(yt,lag=m))
win.graph()
plot(difdD12,ylab="difdD12",main="Gráfica diferencia regular y estacional (d=D=1)")
#ACF
win.graph()
acf(as.numeric(difdD12),lag.max=36,ci.type="ma",lwd=2,main="ACF diferencia regular y estacional (d=D=1)") 
abline(v=c(12,24,36),lty=2,col=2)

#--PUNTO c ---------------------------------------------------------------------

#Test Hegy sobre la serie recortada
HEGY.test(wts=yt,itsd=c(0,0,c(0)),selectlags=list(mode="aic", Pmax=m))$stats

#PUNTO 2 Identificacion de modelos SARIMA(p,d,q) × (P,D,Q)[12]------------------
#--PUNTO a ---------------------------------------------------------------------

#Identificación por ACF Y PACF
win.graph(width=3.7,height=2.8)
acf(as.numeric(difdD12),ci.type="ma",lag.max=36,lwd=2,main="",cex.lab=0.5,cex.axis=0.5)
title(main="ACF diferencia regular y estacional (d=D=1)",cex.main=0.5)
abline(v=c(12,24,36),lty=2,col=2)
win.graph(width=3.7,height=2.8)
pacf(as.numeric(difdD12),lag.max=36,lwd=2,main="",cex.lab=0.5,cex.axis=0.5)
title(main="PACF diferencia regular y estacional (d=D=1)",cex.main=0.5)
abline(v=c(12,24,36),lty=2,col=2)

#--PUNTO b ---------------------------------------------------------------------
#Identificación con auto.arima
auto.arima(yt,ic="aic",seasonal.test="ocsb")
auto.arima(yt,ic="aic",seasonal.test="ch")
auto.arima(yt,ic="aic",seasonal.test="seas")
auto.arima(yt,ic="bic",seasonal.test="ocsb")
auto.arima(yt,ic="bic",seasonal.test="ch")
auto.arima(yt,ic="bic",seasonal.test="seas")

#--PUNTO c ---------------------------------------------------------------------
#Identificación con armasubsets

win.graph(heigh=5,width=9)
plot(armasubsets(difdD12,nar=18,nma=18,y.name="AR",ar.method="ols"))
#nota: usando el renglon 4 se identifica el modelo ARIMA(9,1,5)(1 ,1,1)[12]
#nota: usando el renglon 6 se identifica el modelo ARIMA(9,1,5)(1,1,1)[12]

#PUNTO 3 Ajuste de modelos con validacion cruzada ------------------------------
#--MODELO 1:ARIMA(0,1,1)(0,1,1)[12]---------------------------------------------

modelo1=Arima(yt,order=c(0,1,1),seasonal=list(order=c(0,1,1)),method="ML")

#número de parámetros del modelo
k1=length(coef(modelo1)[coef(modelo1)!=0]);k1 

#Construya tabla de parámetros estimados 
coeftest(modelo1)

#Gráfico de la serie y su ajuste
ythat1=ts(fitted(modelo1),freq=m,start=start(yt))
win.graph()
plot(yt,ylab="yt",main="Modelo 1")
lines(ythat1,col=2,lwd=2)
legend("topleft",legend=c("yt","Ajuste Modelo 1"),lty=1,col=1:2)

#Gráficos de residuales
win.graph()
plot(residuals(modelo1),ylab="Residuales",main="Residuales modelo 1");abline(h=0)
abline(h=c(-2*sqrt(modelo1$sigma2),2*sqrt(modelo1$sigma2)),lty=2, col=2)
win.graph()
plot(as.numeric(modelo1$fitted),residuals(modelo1),ylab="Residuales",main="Residuos vs. ajustados modelo 1");abline(h=0)
abline(h=c(-2*sqrt(modelo1$sigma2),2*sqrt(modelo1$sigma2)),lty=2, col=2)

#Cálculo de pseudo residuales
Res.orig.modelo1=yt-ythat1 
Criteriosmodelo1=exp.crit.inf.resid(residuales= Res.orig.modelo1,n.par=k1); Criteriosmodelo1

#VALIDACION DE SUPUESTOS 

#ACF sobre residuales de ajuste en el modelo. Use valor para m=36 
win.graph()
acf(as.numeric(residuals(modelo1)),ci.type="ma",lag.max=36,main="ACF modelo 1",ci.col=2)

#PACF sobre residuales de ajuste en el modelo. Use valor para m=36 
win.graph()
pacf(as.numeric(residuals(modelo1)),lag.max=36,main="PACF modelo 1",ci.col=2)

#test Ljung-Box m=36 igual al de ACF y PACF
BP.LB.test(residuals(modelo1),maxlag=36,type="Ljung") 

#Normalidad sobre residuales de ajuste en el modelo. Sólo si no se rechaza supuesto de ruido blanco
shapiro.test(residuals(modelo1))
win.graph()
qqnorm(residuals(modelo1),main="Gráfico de normalidad residuos de ajuste modelo 1")
qqline(residuals(modelo1),col=2) 

#PRONOSTICOS PARA LA VALIDACION CRUZADA

#Cálculo del pronóstico con I.P del 95%, en escala original
predmodelo1=ts(as.data.frame(forecast(modelo1,h=m,level=95)),freq=frequency(ytnuevo),start=start(ytnuevo))
predmodelo1=ts(predmodelo1,freq=m,start=c(2022,6));predmodelo1
ytpronmodelo1=predmodelo1[,1];ytpronmodelo1

#Medidas precisión pronósticos
accuracy(ytpronmodelo1,ytnuevo)
amplcobmodelo1=amplitud.cobertura(real=ytnuevo,LIP=predmodelo1[,2],LSP=predmodelo1[,3]);amplcobmodelo1

#--MODELO 2:ARIMA(3,0,0)(2,1,0)[12]---------------------------------------------

modelo2=Arima(yt,order=c(3,0,0),seasonal=list(order=c(2,1,0)),include.drift=TRUE,method="ML")

#número de parámetros del modelo
k2=length(coef(modelo2)[coef(modelo2)!=0]);k2 

#Construya tabla de parámetros estimados 
coeftest(modelo2) 

#Gráfico de la serie y su ajuste
ythat2=ts(fitted(modelo2),freq=m,start=start(yt))
win.graph()
plot(yt,ylab="yt",main="Modelo 2")
lines(ythat2,col=2,lwd=2)
legend("topleft",legend=c("yt","Ajuste Modelo 2"),lty=1,col=1:2)

#Gráficos de residuales
win.graph()
plot(residuals(modelo2),ylab="Residuales",main="Residuales modelo 2");abline(h=0)
abline(h=c(-2*sqrt(modelo2$sigma2),2*sqrt(modelo2$sigma2)),lty=2, col=2)
win.graph()
plot(as.numeric(modelo2$fitted),residuals(modelo2),ylab="Residuales",main="Residuos vs. ajustados modelo 2");abline(h=0)
abline(h=c(-2*sqrt(modelo2$sigma2),2*sqrt(modelo2$sigma2)),lty=2, col=2)

#Cálculo de pseudo residuales
Res.orig.modelo2=yt-ythat2 
Criteriosmodelo2=exp.crit.inf.resid(residuales= Res.orig.modelo2,n.par=k2); Criteriosmodelo2

#VALIDACION DE SUPUESTOS

#ACF sobre residuales de ajuste en el modelo. Use valor para m=36 
win.graph()
acf(as.numeric(residuals(modelo2)),ci.type="ma",lag.max=36,main="ACF modelo 2",ci.col=2)

#PACF sobre residuales de ajuste en el modelo. Use valor para m=36 
win.graph()
pacf(as.numeric(residuals(modelo2)),lag.max=36,main="PACF modelo 2",ci.col=2)

#test Ljung-Box m=36 igual al de ACF y PACF
BP.LB.test(residuals(modelo2),maxlag=36,type="Ljung") 

#Normalidad sobre residuales de ajuste en el modelo. Sólo si no se rechaza supuesto de ruido blanco
shapiro.test(residuals(modelo2))
win.graph()
qqnorm(residuals(modelo2),main="Gráfico de normalidad residuos de ajuste modelo 2")
qqline(residuals(modelo2),col=2) 

#PRONOSTICOS PARA LA VALIDACION CRUZADA

#Cálculo del pronóstico con I.P del 95%, en escala original
predmodelo2=ts(as.data.frame(forecast(modelo2,h=m,level=95)),freq=frequency(ytnuevo),start=start(ytnuevo))
predmodelo2=ts(predmodelo2,freq=m,start=c(2022,6));predmodelo2
ytpronmodelo2=predmodelo2[,1];ytpronmodelo2 

#Medidas precisión pronósticos
accuracy(ytpronmodelo2,ytnuevo)
amplcobmodelo2=amplitud.cobertura(real=ytnuevo,LIP=predmodelo2[,2],LSP=predmodelo2[,3]);amplcobmodelo2

#--MODELO 3:ARIMA(9,1,5)(1,1,1)[12],𝜙1𝜙9 ɵ1 ɵ3 ɵ5	Φ1 Θ1-----------------------

modelo3=Arima(yt,order=c(9,1,5),seasonal=list(order=c(1,1,1)),fixed=c(NA,0,0,0,0,0,0,0,NA,NA,0,NA,0,NA,NA,NA),method="ML")

#número de parámetros del modelo
k3=length(coef(modelo3)[coef(modelo3)!=0]);k3 

#Construya tabla de parámetros estimados 
coeftest(modelo3) 

#Gráfico de la serie y su ajuste
ythat3=ts(fitted(modelo3),freq=m,start=start(yt))
win.graph()
plot(yt,ylab="yt",main="Modelo 3")
lines(ythat3,col=2,lwd=2)
legend("topleft",legend=c("yt","Ajuste Modelo 3"),lty=1,col=1:2)

#Gráficos de residuales
win.graph()
plot(residuals(modelo3),ylab="Residuales",main="Residuales modelo 3");abline(h=0)
abline(h=c(-2*sqrt(modelo3$sigma2),2*sqrt(modelo3$sigma2)),lty=2, col=2)
win.graph()
plot(as.numeric(modelo3$fitted),residuals(modelo3),ylab="Residuales",main="Residuos vs. ajustados modelo 3");abline(h=0)
abline(h=c(-2*sqrt(modelo3$sigma2),2*sqrt(modelo3$sigma2)),lty=2, col=2)

#Cálculo de pseudo residuales
Res.orig.modelo3=yt-ythat3 #Cálculo de pseudo residuales
Criteriosmodelo3=exp.crit.inf.resid(residuales=Res.orig.modelo3,n.par=k3); Criteriosmodelo3 

#VALIDACION DE SUPUESTOS

#ACF sobre residuales de ajuste en el modelo. Use valor para m=36
win.graph()
acf(as.numeric(residuals(modelo3)),ci.type="ma",lag.max=36,main="ACF modelo 3",ci.col=2) 

#PACF sobre residuales de ajuste en el modelo. Use valor para m=36
win.graph()
pacf(as.numeric(residuals(modelo3)),lag.max=36,main="PACF modelo 3",ci.col=2)

#test Ljung-Box m=36 igual al de ACF y PACF
BP.LB.test(residuals(modelo3),maxlag=36,type="Ljung") 

#Normalidad sobre residuales de ajuste en el modelo. Sólo si no se rechaza supuesto de ruido blanco
shapiro.test(residuals(modelo3))
win.graph()
qqnorm(residuals(modelo3),main="Gráfico de normalidad residuos de ajuste modelo 3")
qqline(residuals(modelo3),col=2) 

#PRONOSTICOS PARA LA VALIDACION CRUZADA

#Cálculo del pronóstico con I.P del 95%, en escala original
predmodelo3=ts(as.data.frame(forecast(modelo3,h=m,level=95)),freq=frequency(ytnuevo),start=start(ytnuevo))
predmodelo3=ts(predmodelo3,freq=m,start=c(2022,6));predmodelo3
ytpronmodelo3=predmodelo3[,1];ytpronmodelo3

#Medidas precisión pronósticos
accuracy(ytpronmodelo3,ytnuevo)
amplcobmodelo3=amplitud.cobertura(real=ytnuevo,LIP=predmodelo3[,2],LSP=predmodelo3[,3]);amplcobmodelo3

#--MODELO 4:ARIMA(9,1,5)(1,1,1)[12],𝜙1𝜙9 ɵ2	ɵ3 ɵ5	Φ1 Θ1-----------------------

modelo4=Arima(yt,order=c(9,1,5),seasonal=list(order=c(1,1,1)),fixed=c(NA,0,0,0,0,0,0,0,NA,0,NA,NA,0,NA,NA,NA),method="ML",optim.method="Nelder")

#número de parámetros del modelo
k4=length(coef(modelo4)[coef(modelo4)!=0]);k4 

#Construya tabla de parámetros estimados 
coeftest(modelo4) 

#Gráfico de la serie y su ajuste
ythat4=ts(fitted(modelo4),freq=m,start=start(yt))
win.graph()
plot(yt,ylab="yt",main="Modelo 4")
lines(ythat4,col=2,lwd=2)
legend("topleft",legend=c("yt","Ajuste Modelo 4"),lty=1,col=1:2)

#Gráficos de residuales
win.graph()
plot(residuals(modelo4),ylab="Residuales",main="Residuales modelo 4");abline(h=0)
abline(h=c(-2*sqrt(modelo4$sigma2),2*sqrt(modelo4$sigma2)),lty=2, col=2)
win.graph()
plot(as.numeric(modelo4$fitted),residuals(modelo4),ylab="Residuales",main="Residuos vs. ajustados modelo 4");abline(h=0)
abline(h=c(-2*sqrt(modelo4$sigma2),2*sqrt(modelo4$sigma2)),lty=2, col=2)

#Cálculo de pseudo residuales
Res.orig.modelo4=yt-ythat4 #Cálculo de pseudo residuales
Criteriosmodelo4=exp.crit.inf.resid(residuales=Res.orig.modelo4,n.par=k4); Criteriosmodelo4 

#VALIDACION DE SUPUESTOS

#ACF sobre residuales de ajuste en el modelo. Use valor para m=36
win.graph()
acf(as.numeric(residuals(modelo4)),ci.type="ma",lag.max=36,main="ACF modelo 4",ci.col=2) 

#PACF sobre residuales de ajuste en el modelo. Use valor para m=36
win.graph()
pacf(as.numeric(residuals(modelo4)),lag.max=36,main="PACF modelo 4",ci.col=2)

#test Ljung-Box m=36 igual al de ACF y PACF
BP.LB.test(residuals(modelo4),maxlag=36,type="Ljung") 

#Normalidad sobre residuales de ajuste en el modelo. Sólo si no se rechaza supuesto de ruido blanco
shapiro.test(residuals(modelo4))
win.graph()
qqnorm(residuals(modelo4),main="Gráfico de normalidad residuos de ajuste modelo 4")
qqline(residuals(modelo4),col=2) 

#PRONOSTICOS PARA LA VALIDACION CRUZADA

#Cálculo del pronóstico con I.P del 95%, en escala original
predmodelo4=ts(as.data.frame(forecast(modelo4,h=m,level=95)),freq=frequency(ytnuevo),start=start(ytnuevo))
predmodelo4=ts(predmodelo4,freq=m,start=c(2022,6));predmodelo4
ytpronmodelo4=predmodelo4[,1];ytpronmodelo4

#Medidas precisión pronósticos
accuracy(ytpronmodelo4,ytnuevo)
amplcobmodelo4=amplitud.cobertura(real=ytnuevo,LIP=predmodelo4[,2],LSP=predmodelo4[,3]);amplcobmodelo4

#PUNTO 4 Analisis de residuales y validacion de supuestos-----------------------

#Analisis de residuales
win.graph()
layout(cbind(c(1,3),c(2,4)))
plot(residuals(modelo1),main="Modelo 1");abline(h=0)
abline(h=c(-2*sqrt(modelo1$sigma2),2*sqrt(modelo1$sigma2)),lty=2, col=2)
plot(residuals(modelo2),main="Modelo 2");abline(h=0)
abline(h=c(-2*sqrt(modelo2$sigma2),2*sqrt(modelo2$sigma2)),lty=2, col=2)
plot(residuals(modelo3),main="Modelo 3");abline(h=0)
abline(h=c(-2*sqrt(modelo3$sigma2),2*sqrt(modelo3$sigma2)),lty=2, col=2)
plot(residuals(modelo4),main="Modelo 4");abline(h=0)
abline(h=c(-2*sqrt(modelo4$sigma2),2*sqrt(modelo4$sigma2)),lty=2, col=2)
win.graph()
layout(cbind(c(1,3),c(2,4)))
plot(as.numeric(modelo1$fitted),residuals(modelo1),main="Modelo 1");abline(h=0)
abline(h=c(-2*sqrt(modelo1$sigma2),2*sqrt(modelo1$sigma2)),lty=2, col=2)
plot(as.numeric(modelo2$fitted),residuals(modelo2),main="Modelo 2");abline(h=0)
abline(h=c(-2*sqrt(modelo2$sigma2),2*sqrt(modelo2$sigma2)),lty=2, col=2)
plot(as.numeric(modelo3$fitted),residuals(modelo3),main="Modelo 3");abline(h=0)
abline(h=c(-2*sqrt(modelo3$sigma2),2*sqrt(modelo3$sigma2)),lty=2, col=2)
plot(as.numeric(modelo4$fitted),residuals(modelo4),main="Modelo 4");abline(h=0)
abline(h=c(-2*sqrt(modelo4$sigma2),2*sqrt(modelo4$sigma2)),lty=2, col=2)

#validacion de supuestos
win.graph()
layout(cbind(c(1,3),c(2,4)))
acf(as.numeric(residuals(modelo1)),ci.type="ma",lag.max=36,main="ACF modelo 1",ci.col=2)
acf(as.numeric(residuals(modelo2)),ci.type="ma",lag.max=36,main="ACF modelo 2",ci.col=2)
acf(as.numeric(residuals(modelo3)),ci.type="ma",lag.max=36,main="ACF modelo 3",ci.col=2) 
acf(as.numeric(residuals(modelo4)),ci.type="ma",lag.max=36,main="ACF modelo 4",ci.col=2) 
win.graph()
layout(cbind(c(1,3),c(2,4)))
pacf(as.numeric(residuals(modelo1)),lag.max=36,main="PACF modelo 1",ci.col=2)
pacf(as.numeric(residuals(modelo2)),lag.max=36,main="PACF modelo 2",ci.col=2)
pacf(as.numeric(residuals(modelo3)),lag.max=36,main="PACF modelo 3",ci.col=2)
pacf(as.numeric(residuals(modelo4)),lag.max=36,main="PACF modelo 4",ci.col=2)

#tabla resumen Test Ljung-box
tabla.LjungBox=cbind(BP.LB.test(residuals(modelo1),maxlag=36,type="Ljung"),BP.LB.test(residuals(modelo2),maxlag=36,type="Ljung"),BP.LB.test(residuals(modelo3),maxlag=36,type="Ljung"),BP.LB.test(residuals(modelo4),maxlag=36,type="Ljung"))[,c(1,3,4,6,7,9,10,12)]
colnames(tabla.LjungBox)=c("QLB-M1","vp-M1","QLB-M2","vp-M2","QLB-M3","vp-M3","QLB-M4","vp-M4")
tabla.LjungBox

#normalidad
win.graph()
layout(cbind(c(1,3),c(2,4)))
qqnorm(residuals(modelo1),main="modelo 1")
qqline(residuals(modelo1),col=2) 
qqnorm(residuals(modelo2),main="modelo 2")
qqline(residuals(modelo2),col=2) 
qqnorm(residuals(modelo3),main="modelo 3")
qqline(residuals(modelo3),col=2) 
qqnorm(residuals(modelo4),main="modelo 4")
qqline(residuals(modelo4),col=2) 

#tabla resumen test shapiro
tabla.Shapiro=rbind(shapiro.test(residuals(modelo1)),shapiro.test(residuals(modelo2)),shapiro.test(residuals(modelo3)),shapiro.test(residuals(modelo4)))[,c(1,2)]
rownames(tabla.Shapiro)=c("Modelo 1","Modelo 2","Modelo 3","Modelo 4")
tabla.Shapiro

#PUNTO 5 Pronosticos para la validacion cruzada --------------------------------

#Grafica de los pronosticos
win.graph()
plot(ytnuevo,type="b",ylab="Datos24",col=1,pch=19,ylim=c(min(ytnuevo,ytpronmodelo1,ytpronmodelo2,ytpronmodelo3,ytpronmodelo4),max(ytnuevo,ytpronmodelo1,ytpronmodelo2,ytpronmodelo3,ytpronmodelo4)),lwd=2,xaxt="n", main="Gráfica comparativa de los pronósticos de los 4 modelos")
axis(1,at=time(ytnuevo),labels=c("22-VI","22-VII","22-VIII","22-IX","22-X","22-XI","22-XII","23-I","23-II","23-III","23-IV","23-V"),cex.axis=0.7)
lines(ytpronmodelo1,col=2,pch=1,type="b",lwd=2)
lines(ytpronmodelo2,col=3,pch=2,type="b",lwd=2)
lines(ytpronmodelo3,col=4,pch=3,type="b",lwd=2)
lines(ytpronmodelo4,col=5,pch=4,type="b",lwd=2)
legend("topleft",legend=c("Real","Modelo 1","Modelo 2","Modelo 3","Modelo 4"),pch=c(19,1:4),col=c(1:5),lwd=2)

#tabla resumen de medidas de pronosticos
tabla.pronosticos=cbind(ytpronmodelo1,ytpronmodelo2,ytpronmodelo3,ytpronmodelo4)
colnames(tabla.pronosticos)=c("Modelo 1","Modelo 2","Modelo 3","Modelo 4")
tabla.pronosticos

#tabla resumen de medidas de precision de pronosticos
precision.puntuales=rbind(accuracy(ytpronmodelo1,ytnuevo),accuracy(ytpronmodelo2,ytnuevo),accuracy(ytpronmodelo3,ytnuevo),accuracy(ytpronmodelo4,ytnuevo))[,c(2,3,5)]
precision.intervalos=rbind(amplcobmodelo1,amplcobmodelo2,amplcobmodelo3,amplcobmodelo4)
tabla.precision=cbind(precision.puntuales,precision.intervalos)
rownames(tabla.precision)=c("Modelo 1","Modelo 2","Modelo 3","Modelo 4")
tabla.precision

#RESUMEN PROGRAMACION-----------------------------------------------------------

#tabla resumen de medidas de ajuste AIC Y BIC 
tabla.ajuste=rbind(Criteriosmodelo1,Criteriosmodelo2,Criteriosmodelo3,Criteriosmodelo4)
rownames(tabla.ajuste)=c("Modelo 1","Modelo 2","Modelo 3","Modelo 4")
tabla.ajuste

#tabla resumen de medidas de precision de pronosticos
precision.puntuales=rbind(accuracy(ytpronmodelo1,ytnuevo),accuracy(ytpronmodelo2,ytnuevo),accuracy(ytpronmodelo3,ytnuevo),accuracy(ytpronmodelo4,ytnuevo))[,c(2,3,5)]
precision.intervalos=rbind(amplcobmodelo1,amplcobmodelo2,amplcobmodelo3,amplcobmodelo4)
tabla.precision=cbind(precision.puntuales,precision.intervalos)
rownames(tabla.precision)=c("Modelo 1","Modelo 2","Modelo 3","Modelo 4")
tabla.precision


#FINAL
#-------------------------------------------------------------------------------------------------------
# Modelo 2 polinomial de grado 4 con trigonometricas
#--------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------
# Definicion de variables necesarias
#-------------------------------------------------------------------------------------------------------------------------------------------

#definicion variable para el ajuste
m = 12
n=length(Datos24)-m

#indices de tiempo
t=1:n
t2=t^2
t3=t^3
t4=t^4


#serie a ajustar
yt=ts(Datos24[t], freq=12,start=c(2013,1))

#trigonometricas para ondas en las frecuencias j/12, para j= 2, 3, 4, 5

sen2=sin(pi*t/3)
cos2=cos(pi*t/3)
sen3=sin(pi*t/2)
cos3=cos(pi*t/2)
sen4=sin(2*pi*t/3)
cos4=cos(2*pi*t/3)
sen5=sin(5*pi*t/6)
cos5=cos(5*pi*t/6)

#Variables para los pronosticos
tnuevo=c((n+1):(n+m))
t2nuevo=tnuevo^2
t3nuevo=tnuevo^3
t4nuevo=tnuevo^4

sen2n=sin(pi*tnuevo/3)
cos2n=cos(pi*tnuevo/3)
sen3n=sin(pi*tnuevo/2)
cos3n=cos(pi*tnuevo/2)
sen4n=sin(2*pi*tnuevo/3)
cos4n=cos(2*pi*tnuevo/3)
sen5n=sin(5*pi*tnuevo/6)
cos5n=cos(5*pi*tnuevo/6)


#Matriz de diseño para los pronosticos

X2nuevo=data.frame(t=tnuevo,t2=t2nuevo,t3=t3nuevo,t4=t4nuevo,sen2=sen2n,cos2=cos2n,sen3=sen3n,cos3=cos3n,sen4=sen4n,cos4=cos4n,sen5=sen5n,
                   cos5=cos5n)


X2=data.frame(t,t2,t3,t4,sen2,cos2,sen3,cos3,sen4,cos4,sen5,cos5)
modelo2=lm(yt~.,data=X2)
summary(modelo2)

#Calculo valores ajustados del modelo 2
ythatmodelo2=ts(fitted(modelo2),freq=12,start=start(yt))

#Calculo de los criterios de informacion usando exp(C^*_n(p))
nparmodelo2=length(coef(modelo2)[coef(modelo2)!=0]); nparmodelo2#numero parametros modelo2 
Criterios2=exp.crit.inf.resid(residuales=residuals(modelo2),n.par=nparmodelo2);Criterios2

#Grafico de la serie y su ajuste
plot(Datos24)
lines(ythatmodelo2,col=2,lwd=2)
legend("topleft",legend=c("Original","Ajuste modelo 2"),lty=1,col=c(1,2))

#pronosticos puntuales y por Intervalos del 95%
pronmodelo2=predict(modelo2,newdata=X2nuevo,interval="prediction",level=0.95)
pronmodelo2=ts(pronmodelo2,freq=12,start=start(ytnuevo)); pronmodelo2


ytpronmodelo2=pronmodelo2[,1]; ytpronmodelo2 #serie de los pronosticos puntuales

accuracy(ytpronmodelo2,ytf) #Calculando exactitud de los pronosticos puntuales

#precision pronosticos por I.P modelo 2
amplcobmodelo2=amplitud.cobertura(real=ytf,LIP=pronmodelo2[,2],LSP=pronmodelo2[,3]);amplcobmodelo2

#-------------------------------------------------------------------
#Modelo 3 Holt-Winters aditivo
#------------------------------------------------------------------
source("https://raw.githubusercontent.com/NelfiGonzalez/Funciones-de-Usuario-Estadistica-III/main/Funciones-Criterios.Informacion-Calidad.Intervalos.R")
source("https://raw.githubusercontent.com/NelfiGonzalez/Funciones-de-Usuario-Estadistica-III/main/Funcion-regexponencial.R")
source("https://raw.githubusercontent.com/NelfiGonzalez/Funciones-de-Usuario-Estadistica-III/main/Funcion-Descomp.Loess.R")
source("https://raw.githubusercontent.com/NelfiGonzalez/Funciones-de-Usuario-Estadistica-III/main/Funcion-SuavizamientoEstacional.R")
source("https://raw.githubusercontent.com/NelfiGonzalez/Funciones-de-Usuario-Estadistica-III/main/Funciones-Criterios.Informacion-Calidad.Intervalos.R")

modelo3 = SuavizamientoEstacional(yt ,seasonal="additive", h=m, beta=1e-5)
str(modelo3)

ythat3=fitted(modelo3) #valores ajustados. Ya tienen formato de serie de tiempo

#Gráfica del ajuste

plot(yt)
lines(ythat3,col=2)
legend("topleft",legend=c("Original","Ajustada SEHW"),col=c(1,2),lty=1)

et3=residuals(modelo3) #residuales. tienen formato de serie de tiempo

#Calculando AIC y BIC usando exp(C*n(p))
npar3=(12-1)+2; npar3 #Aprox. del número de parámetros del suavmiento
Criterios3=exp.crit.inf.resid(residuales=et3,n.par=npar3) 
Criterios3

#Gráficos de residuos
MSE3=modelo3$MSE #MSE aproximado del ajuste total del Suavizamiento
MSE3
plot(et3,ylim=c(min(-2*sqrt(MSE3),et3),max(2*sqrt(MSE3),et3)))
abline(h=c(-2*sqrt(MSE3),0,2*sqrt(MSE3)),col=2)
legend("topleft",legend="modelo3")


plot(as.numeric(ythat3),et3,ylim=c(min(-2*sqrt(MSE3),et3),max(2*sqrt(MSE3),et3)))
abline(h=c(-2*sqrt(MSE3),0,2*sqrt(MSE3)),col=2)
legend("topleft",legend="Modelo 3: SEHW")

#Predicciones puntuales y por I.P del 95%
predicciones3=modelo3$forecast
predicciones3

ytpron3=predicciones3[,1] #Separando los pronosticos puntuales
ytpron3

#MODELO 4:ARMA()
library(forecast)
library(lmtest)
X1=data.frame(t,t2,t3,t4,sen2,cos2,sen3,cos3,sen4,cos4,sen5,cos5)
Xnuevo=data.frame(t=tnuevo,t2=t2nuevo,t3=t3nuevo,t4=t4nuevo,sen2=sen2n,cos2=cos2n,sen3=sen3n,cos3=cos3n,sen4=sen4n,cos4=cos4n,sen5=sen5n,
                  cos5=cos5n)
mod1=lm(yt~.,data=X1)
nparmod1=length(coef(mod1)[coef(mod1)!=0]); nparmod1#numero parametros mod1 

modelo4<- Arima(yt,order=c(15,0,11),
                fixed = c(NA,rep(0,13),NA,rep(0,3),NA,NA,rep(0,3),NA,0,NA,rep(NA,nparmod1))
                ,xreg = as.matrix(X1),method = "ML")
k4=length(coef(modelo4)[coef(modelo4)!=0]);k4 #n?mero de par?metros del modelo
df4=n-k4 #n-Total de par?metros 
coeftest(modelo4,df=df4)

#Calculando valores ajustados
yhat4=modelo4$fitted

win.graph()
plot(Datos24)
lines(yhat4,col=2)
legend("topleft",legend=c("Serie real","Ajuste modelo 4"),col=c(1,2),lty=1)

Criterios4=exp.crit.inf.resid(residuales=residuals(modelo4),n.par=k4);Criterios4

#Gr?fico de residuales de ajuste vs tiempo
win.graph(width=4.8,height=4.8,pointsize=8) 
plot(residuals(modelo4))
abline(h=c(-2*sqrt(modelo4$sigma2),0,2*sqrt(modelo4$sigma2)),col=2)

#Gr?fico de residuales de ajuste vs ajustados
win.graph(width=4.8,height=4.8,pointsize=8)
plot(as.numeric(yhat4), residuals(modelo4), type="p")
abline(h=c(-2*sqrt(modelo4$sigma2),0,2*sqrt(modelo4$sigma2)),col=2)

#FAC sobre residuales de ajuste modelo4
win.graph(width=4.8,height=4.8,pointsize=8) 
acf(as.numeric(residuals(modelo4)),ci.type="ma",lag.max=36,main="ACF modelo 4",ci.col=2)

#PACF sobre residuales de ajuste modelo4
win.graph(width=4.8,height=4.8,pointsize=8) 
pacf(as.numeric(residuals(modelo4)),lag.max=36,main="PACF modelo 4",ci.col=2)

#Box-pierce & Ljung-Box
BP.LB.test(residuals(modelo4),maxlag=36,type="Box")
BP.LB.test(residuals(modelo4),maxlag=36,type="Ljung")

shapiro.test(residuals(modelo4))
win.graph(width=4.8,height=4.8,pointsize=8)
qqnorm(residuals(modelo4),main="Grafico de normalidad residuos modelo 4")
qqline(residuals(modelo4),col=2)

#Pron?sticos e I.P del 95%

pred4=ts(as.data.frame(forecast(modelo4,xreg=as.matrix(Xnuevo),h=m,level=95)),freq=frequency(ytnuevo),start=start(ytnuevo))
pred4=ts(pred4,freq=m,start=c(2022,6));pred4
ytpred4=pred4[,1];ytpronmodelo1
#Precisi?n pron?sticos puntuales
accuracy(pred4[,1],ytnuevo)

#precisi?n pron?sticos por I.P modelo 4
amplcobmod4=amplitud.cobertura(real=ytf,LIP=pred4[,2],LSP=pred4[,3]);amplcobmod4
#--MODELO 1:ARIMA(0,1,1)(0,1,1)[12]---------------------------------------------

modelo1=Arima(yt,order=c(0,1,1),seasonal=list(order=c(0,1,1)),method="ML")

#número de parámetros del modelo
k1=length(coef(modelo1)[coef(modelo1)!=0]);k1 

#Construya tabla de parámetros estimados 
coeftest(modelo1)

#Gráfico de la serie y su ajuste
ythat1=ts(fitted(modelo1),freq=m,start=start(yt))
win.graph()
plot(yt,ylab="yt",main="Modelo 1")
lines(ythat1,col=2,lwd=2)
legend("topleft",legend=c("yt","Ajuste Modelo 1"),lty=1,col=1:2)



win.graph()
predicciones3=modelo3$forecast
predicciones3

ytpron3=predicciones3[,1] #Separando los pronosticos puntuales
ytpron3


win.graph()
plot(ytnuevo,type="b",ylab="Datos24",col=1,pch=19,ylim=c(min(ytnuevo,ytpronmodelo2,ytpron3,ytpred4,ytpronmodelo1),max(ytnuevo,ytpronmodelo2,ytpron3,ytpred4,ytpronmodelo1)),lwd=2,xaxt="n", main="Gráfica comparativa de los pronósticos de los 4 modelos")
axis(1,at=time(ytnuevo),labels=c("22-VI","22-VII","22-VIII","22-IX","22-X","22-XI","22-XII","23-I","23-II","23-III","23-IV","23-V"),cex.axis=0.7)
lines(ytpronmodelo2,col=2,pch=1,type="b",lwd=2) #Modelo global trabajo 1
lines(ytpron3,col=3,pch=2,type="b",lwd=2) #Modelo local trabajo1
lines(ytpred4,col=4,pch=3,type="b",lwd=2) #Mejor modelo trabajo 2
lines(ytpronmodelo1,col=5,pch=4,type="b",lwd=2) #Mejor modelo trabajo 3
legend("topleft",legend=c("Real","Mejor Modelo GT1","Mejor Modelo LT1","Mejor Modelo T2","Mejor modelo T3"),pch=c(19,1:4),col=c(1:5),lwd=2,cex=0.8)


