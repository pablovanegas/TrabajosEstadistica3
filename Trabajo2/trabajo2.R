rm(list=ls(all=TRUE)) 

#Cargar librerias 
library(forecast)
library(fANCOVA)
#library(FitAR)
library(TSA)
library(car)
library(lmtest)

#Cargar funciones de usuario necesarias
source("https://raw.githubusercontent.com/NelfiGonzalez/Funciones-de-Usuario-Estadistica-III/main/Funcion-SelectModel.R") 
source("https://raw.githubusercontent.com/NelfiGonzalez/Funciones-de-Usuario-Estadistica-III/main/Funcion-regexpo.ErrorARMA.R")
source("https://raw.githubusercontent.com/NelfiGonzalez/Funciones-de-Usuario-Estadistica-III/main/Funciones-Criterios.Informacion-Calidad.Intervalos.R")
source("https://raw.githubusercontent.com/NelfiGonzalez/Funciones-de-Usuario-Estadistica-III/main/Funcion-regexponencial.R")
source("https://raw.githubusercontent.com/NelfiGonzalez/Funciones-de-Usuario-Estadistica-III/main/Funcion-Descomp.Loess.R")
source("https://raw.githubusercontent.com/NelfiGonzalez/Funciones-de-Usuario-Estadistica-III/main/Funcion-SuavizamientoEstacional.R")
source("https://raw.githubusercontent.com/NelfiGonzalez/Funciones-de-Usuario-Estadistica-III/main/Funciones-BP.LB.test-pruebaDW1.R")
source("https://raw.githubusercontent.com/NelfiGonzalez/Funciones-de-Usuario-Estadistica-III/main/Funcion-regexponencial.R")

#Lectura de los datos con la base de datos Anex_ISE_9actividades_sep_2022.csv, col 8: Información y comunicaciones
Datos24=read.table(file.choose(),header=T,sep=";",skip=10,dec=",",colClasses=c(rep("NULL",11),"numeric",rep("NULL",10)))
Datos24=ts(Datos24,freq=12,start=c(2013,1))


#Grafica de la serie, falta cambiar los nombres de los ejes 
plot(Datos24, ylab="Productos de aseo personal, cosméticos y perfumería")

#Tendencia de la descomposición aditiva del log natural de la serie
Tendencia=decompose((Datos24))$trend
plot(Tendencia,ylim=c(min((Datos24)),max((Datos24))))

#Estacionalidad con boxplots 
boxplot((Datos24)~cycle(Datos24),names=month.abb)

#periodograma
x=diff((Datos24))
plot(x,ylab=expression((Y[t]-Y[t-1])));abline(h=mean(x))
#periodogram(x);abline(v=c(1:6)/12,col=2,lty=2) 

#errores para la comparacion del patron de varianza usando diferentes descomposiciones
plot(decompose(Datos24, type="additive")$random, ylab="error");abline(h=0)
plot(decompose(Datos24, type="multiplicative")$random, ylab="error");abline(h=1)

#ACF sobre la serie 
acf(as.numeric((Datos24)), lag.max = 36, ci.type = "ma", col=4, ci.col=4, main= "ACF", ylab= "Lo que quieran poner")



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


#Matriz de diseño de modelos; tiene los valores de los predictores en sus columnas
X1=data.frame(t,t2,t3,t4,sen2,cos2,sen3,cos3,sen4,cos4,sen5,cos5)

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

Xnuevo=data.frame(t=tnuevo,t2=t2nuevo,t3=t3nuevo,t4=t4nuevo,sen2=sen2n,cos2=cos2n,sen3=sen3n,cos3=cos3n,sen4=sen4n,cos4=cos4n,sen5=sen5n,
                  cos5=cos5n)
ytf=ts(Datos24[tnuevo], freq=12,start=c(2022,6))

#-------------------------------------------------------------------------------------------------#
##Modelo global

X1=data.frame(t,t2,t3,t4,sen2,cos2,sen3,cos3,sen4,cos4,sen5,cos5)

mod1=lm(yt~.,data=X1)
summary(mod1)



#Calculo valores ajustados del modelo 
ythatmod1=ts(fitted(mod1),freq=12,start=start(yt))

#Calculo de los criterios de informacion usando exp(C^*_n(p))
nparmod1=length(coef(mod1)[coef(mod1)!=0]); nparmod1#numero parametros mod1 
Criterios1=exp.crit.inf.resid(residuales=residuals(mod1),n.par=nparmod1);Criterios2

#Grafico de la serie y su ajuste
plot(Datos24, ylab= "Serie y Ajuste")

lines(ythatmod1,col=2,lwd=2)
legend("topleft",legend=c("Original","Ajuste modelo global"),lty=1,col=c(1,2))

#pronosticos puntuales y por Intervalos del 95%
pronmod1=predict(mod1,newdata=Xnuevo,interval="prediction",level=0.95)
pronmod1=ts(pronmod1,freq=12,start=start(ytf)); pronmod1

ytpronmod1=pronmod1[,1]; ytpronmod1 #serie de los pronosticos puntuales

accuracy(ytpronmod1,ytf) #Calculando exactitud de los pronosticos puntuales

#precision pronosticos por I.P modelo 1
amplcobmod1=amplitud.cobertura(real=ytf,LIP=pronmod1[,2],LSP=pronmod1[,3]);amplcobmod1


#Graficos de residuales
#residuales vs tiempo

plot.ts(residuals(mod1),ylim=c(min(residuals(mod1),-2*summary(mod1)$sigma,2*summary(mod1)$sigma),max(residuals(mod1),-2*summary(mod1)$sigma,2*summary(mod1)$sigma)))
abline(h=c(-2*summary(mod1)$sigma,0,2*summary(mod1)$sigma),col=2)
legend("topleft",legend=c("modelo global"),lty=1,col=1,lwd=2)

#residuales vs valores ajustados
plot(fitted(mod1),residuals(mod1),ylim=c(min(residuals(mod1),-2*summary(mod1)$sigma,2*summary(mod1)$sigma),max(residuals(mod1),-2*summary(mod1)$sigma,2*summary(mod1)$sigma)), xlab="valores ajustados")
abline(h=c(-2*summary(mod1)$sigma,0,2*summary(mod1)$sigma),col=2)
legend("topleft",legend=c("modelo global"),lty=1,col=1,lwd=2)

# tabla comparativa de los pronosticos puntuales ------------------------------------------------------------------------------------
plot(ytf,type="b",pch=19,lty=1,col=1,lwd=2,ylab="Indice x",
     ylim=c(min(ytf,pronmod1),
            max(ytf,pronmod1)),xaxt="n")
lines(ytpronmod1,col=2,pch=2,lty=2,type="b",lwd=2)

legend("topleft",legend=c("Real","CAMBIAR"),bty="n",col=1:6,
       pch=c(19,2:6),lty=1:6,lwd=2)
#Etiquetando valores en eje x con las fechas de los m=13 pronÃ³sticos
axis(1,at=time(ytf),
     labels=c("2021-X","2021-XI","2021-XII","2022-I","2022-II","2022-III","2022-IV","2022-V","2022-VI","2022-VII","2022-VIII","2022-IX"))

# Valide supuestos sobre el error de ajuste at
#ACF sobre residuales de ajuste en el modelo. Use valor para m el que se indica en la guía del trabajo 

acf(as.numeric(residuals(mod1)),ci.type="ma",lag.max=36,ci.col=2) 

#PACF sobre residuales de ajuste en el modelo. Use valor para m el que se indica en la guía del trabajo 
win.graph(width=4.875,height=3.5,pointsize=8) 
pacf(as.numeric(residuals(mod1)),lag.max=36,ci.col=2) 

BP.LB.test(residuals(mod1),maxlag=36,type="Ljung") #test Ljung-Box use máximo m igual al de ACF y PACF 
BP.LB.test(residuals(mod1),maxlag=36,type="Box")

# TEST DURBIN WATSON
pruebaDW1(mod1)

#Normalidad sobre residuales de ajuste en el modelo. Sólo si no se rechaza supuesto de ruido blanco 
shapiro.test(residuals(mod1)) 

qqnorm(residuals(mod1),main="Gráfico de normalidad residuos de ajuste modelo") 
qqline(residuals(mod1),col=2)
##################################################################################################

#Identificación con EACF 
eacf(residuals(mod1),ar.max=24, ma.max=24)

# Tablero
plot(armasubsets(residuals(mod1),nar=18,nma=18,y.name='AR',ar.method='ml'))
#ARMA(1,4)
#ARMA(15,4)

#identificación de modelos AR(p) con SelectModel 
SelectModel(residuals(mod1),lag.max=36,Criterion="AIC",ARModel ="AR")

SelectModel(residuals(mod1),lag.max=36,Criterion="BIC",ARModel ="AR")


#Identificación con auto.arima sobre vector de residuales sin fechas
auto.arima(residuals(mod1),ic="aic")
auto.arima(residuals(mod1),ic="bic")

#Identificación con auto.arima sobre vector de residuales con fechas
serieEt=ts(residuals(mod1),freq=m,start=c(2013,1))
auto.arima(serieEt,ic="aic")
auto.arima(serieEt,ic="bic")

#Identificación con armasubsets
#tablero 18x18
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Agregar que?????????????????
plot(armasubsets(residuals(mod1),nar=18,nma=18,y.name="AR",ar.method="ml"))


#------------------------------------Modelo 1-----------------------------------#
# AR(15)
modelo11=Arima(yt,order=c(15,0,0),xreg=as.matrix(X1),method="ML") 
k11=length(coef(modelo11)[coef(modelo11)!=0]);k11 #numero de parametros del modelo
df11=n-k11 #n-Total de par?metros 
coeftest(modelo11,df=df11)

yhat11=modelo11$fitted #para obtener valores ajustados

plot(Datos24)
lines(yhat11,col=2) 
legend("topleft",legend=c("Serie real","Ajuste modelo 1"),col=c(1,2),lty=1)

Criterios11=exp.crit.inf.resid(residuales=residuals(modelo11),n.par=k11);Criterios11

#Grafico de residuales de ajuste vs tiempo
plot(residuals(modelo11))
abline(h=c(-2*sqrt(modelo11$sigma2),0,2*sqrt(modelo11$sigma2)),col=2)


#Grafico de residuales de ajuste vs ajustados 
plot(as.numeric(yhat11), residuals(modelo11), type="p") ##verificar
abline(h=c(-2*sqrt(modelo11$sigma2),0,2*sqrt(modelo11$sigma2)),col=2)
legend("topright",legend=c("modelo 1"),lty=1,col=1,lwd=2)

#ACf sobre residuales de ajuste modelo11
acf(as.numeric(residuals(modelo11)),ci.type="ma",lag.max=36,main="ACF modelo 1",ci.col=2)


#PACF sobre residuales de ajuste modelo11
pacf(as.numeric(residuals(modelo11)),lag.max=36,main="PACF modelo 1",ci.col=2)

#Box-pierce & Ljung-Box
BP.LB.test(residuals(modelo11),maxlag=36,type="Box")
BP.LB.test(residuals(modelo11),maxlag=36,type="Ljung")

shapiro.test(residuals(modelo11))
qqnorm(residuals(modelo11),main="Grafico de normalidad residuos modelo 1")
qqline(residuals(modelo11),col=2)

#Pron?sticos e I.P del 95%
pred11=ts(as.data.frame(forecast(modelo11,xreg=as.matrix(Xnuevo),level=95)),freq=12,start=c(2022,6)); pred11 

#Precisi?n pron?sticos puntuales
accuracy(pred11[,1],ytf)

#precisi?n pron?sticos por I.P modelo 11
amplcobmod11=amplitud.cobertura(real=ytf,LIP=pred11[,2],LSP=pred11[,3]);amplcobmod11


yhat11=modelo11$fitted #para obtener valores ajustados


#Modelo 2 
# ARMA(9,5)

modelo2=Arima(yt,order=c(9,0,5),xreg=as.matrix(X1),method="ML")
k2=length(coef(modelo2)[coef(modelo2)!=0]);k2 #n?mero de par?metros del modelo
df2=n-k2 #n-Total de par?metros 
coeftest(modelo2,df=df2)

#C?lculo valores ajustados
yhat2=modelo2$fitted


plot(Datos24)
lines(yhat2,col=2)
legend("topleft",legend=c("Serie real","Ajuste modelo 2"),col=c(1,2),lty=1)

Criterios2=exp.crit.inf.resid(residuales=residuals(modelo2),n.par=k2);Criterios2

#Grafico de residuales de ajuste vs tiempo

plot(residuals(modelo2))
abline(h=c(-2*sqrt(modelo2$sigma2),0,2*sqrt(modelo2$sigma2)),col=2)

#Grafico de residuales de ajuste vs ajustados


plot(as.numeric(yhat2), residuals(modelo2), type="p")
abline(h=c(-2*sqrt(modelo2$sigma2),0,2*sqrt(modelo2$sigma2)),col=2)
legend("topright",legend=c("modelo 2"),lty=1,col=1,lwd=2)

#FAC sobre residuales de ajuste modelo2

acf(as.numeric(residuals(modelo2)),ci.type="ma",lag.max=36,main="ACF modelo 2",ci.col=2)

#PACF sobre residuales de ajuste modelo2

pacf(as.numeric(residuals(modelo2)),lag.max=36,main="PACF modelo 2",ci.col=2)

#Box-pierce & Ljung-Box
BP.LB.test(residuals(modelo2),maxlag=48,type="Box")
BP.LB.test(residuals(modelo2),maxlag=36,type="Ljung")

shapiro.test(residuals(modelo2))

qqnorm(residuals(modelo2),main="Grafico de normalidad residuos modelo 2")
qqline(residuals(modelo2),col=2)

#Pron?sticos e I.P del 95%
pred2=ts(as.data.frame(forecast(modelo2,xreg=as.matrix(Xnuevo),level=95)),freq=12,start=c(2022,6)) #matriz de regresi?n en pron?sticos es Xnuevo
pred2

#Precisi?n pron?sticos puntuales
accuracy(pred2[,1],ytf)

#precisi?n pron?sticos por I.P modelo 2
amplcobmod2=amplitud.cobertura(real=ytf,LIP=pred2[,2],LSP=pred2[,3]);amplcobmod2

#MODELO 3: Modelo con errores ARMA(2,1)xARMA(0,2)[12]
#ARMA(1,0)(2,0)[12]
modelo3=Arima(yt,order=c(1,0,0),seasonal=list(order=c(2,0,0)),xreg=as.matrix(X1),method="ML")
k3=length(coef(modelo3)[coef(modelo3)!=0]);k3 #n?mero de par?metros del modelo
df3=n-k3 #n-Total de par?metros 
coeftest(modelo3,df=df3)

#C?lculo valores ajustados
yhat3=modelo3$fitted


plot(Datos24)
lines(yhat3,col=2)
legend("topleft",legend=c("Serie real","Ajuste modelo 3"),col=c(1,2),lty=1)

Criterios3=exp.crit.inf.resid(residuales=residuals(modelo3),n.par=k3);Criterios3

#Grafico de residuales de ajuste vs tiempo
plot(residuals(modelo3))
abline(h=c(-2*sqrt(modelo3$sigma2),0,2*sqrt(modelo3$sigma2)),col=2)
legend("topright",legend=c("modelo 3"),lty=1,col=1,lwd=2)

#Grafico de residuales de ajuste vs ajustados
plot(as.numeric(yhat3), residuals(modelo3), type="p")
abline(h=c(-2*sqrt(modelo3$sigma2),0,2*sqrt(modelo3$sigma2)),col=2)

#FAC sobre residuales de ajuste modelo3

acf(as.numeric(residuals(modelo3)),ci.type="ma",lag.max=36,main="ACF modelo 3",ci.col=2)

#PACF sobre residuales de ajuste modelo3

pacf(as.numeric(residuals(modelo3)),lag.max=36,main="PACF modelo 3",ci.col=2)

#Box-pierce & Ljung-Box
BP.LB.test(residuals(modelo3),maxlag=36,type="Box")
BP.LB.test(residuals(modelo3),maxlag=36,type="Ljung")

shapiro.test(residuals(modelo3))

qqnorm(residuals(modelo3),main="Grafico de normalidad residuos modelo 3")
qqline(residuals(modelo3),col=2)

#Pron?sticos e I.P del 95%
pred3=ts(as.data.frame(forecast(modelo3,xreg=as.matrix(Xnuevo),level=95)),freq=12,start=c(2022,6)) #matriz de regresi?n en pron?sticos es Xnuevo
pred3

#Precisi?n pron?sticos puntuales
accuracy(pred3[,1],ytf)

#precisi?n pron?sticos por I.P modelo 3
amplcobmod3=amplitud.cobertura(real=ytf,LIP=pred3[,2],LSP=pred3[,3]);amplcobmod3


#MODELO 4:ARMA()
library(forecas)
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
pred4=ts(as.data.frame(forecast(modelo4,xreg=as.matrix(Xnuevo),level=95)),freq=12,start=c(2022,6)) 
pred4

#Precisi?n pron?sticos puntuales
accuracy(pred4[,1],ytf)

#precisi?n pron?sticos por I.P modelo 4
amplcobmod4=amplitud.cobertura(real=ytf,LIP=pred4[,2],LSP=pred4[,3]);amplcobmod4

#COBERTURA 100%


#Gr?fico comparativo de pron?sticos ex-post
win.graph()
plot(ytf,type="b",pch=19,col=1,lwd=2,xaxt="n",ylim=c(min(ytf,pred11[,1],pred2[,1],pred3[,1],pred4[,1]),max(ytf,pred11[,1],pred2[,1],pred3[,1],pred4[,1])))
axis(1,at=time(ytf),labels=c("dic-20","ene-21","feb-21","mar-21","abr-21","may-21","jun-21","jul-21","ago-21","sep-21","oct-21","nov-21"),cex.axis=0.7)
lines(pronmod1[,1],col=2,type="b",pch=2,lwd=2)
lines(pred11[,1],col=4,type="b",pch=4,lwd=2)
lines(pred2[,1],col=5,type="b",pch=5,lwd=2)
lines(pred3[,1],col=6,type="b",pch=6,lwd=2)
lines(pred4[,1],col=7,type="b",pch=7,lwd=2)
legend("topleft",legend=c("Real","pron. modelo global","pron. modelo1","pron. modelo2","pron. modelo3","pron. modelo4"),col=c(1,2,4,5,6,7),pch=c(19,2,4,5,6,7),lwd=2)

#Tabla con todas las medidas de ajuste
criterios=rbind(Criterios1,Criterios11,Criterios2,Criterios3,Criterios4)
rownames(criterios) = c("modelo global","modelo1","modelo2","modelo3","modelo4")
criterios

#Tabla de medidas de precisi?n de pron?sticos
comp=rbind(accuracy(pronmod1[,1],ytf),accuracy(pred11[,1],ytf),accuracy(pred2[,1],ytf),accuracy(pred3[,1],ytf),accuracy(pred4[,1],ytf))[,c(2,3,5)]
otros=rbind(amplcobmod1,amplcobmod11,amplcobmod2,amplcobmod3,amplcobmod4)

tablaprecis=cbind(comp,otros)
rownames(tablaprecis)=c("Modelo global","Modelo1","Modelo2","Modelo3","Modelo4")
tablaprecis

#---------------------------------------------------------------------------------------------------------#
source("https://raw.githubusercontent.com/NelfiGonzalez/Funciones-de-Usuario-Estadistica-III/main/Funcion-Descomp.Loess.R")

ajusteDLL4=Descomp.Loess(serie.ajuste=yt,h=m,tipo.descomp="additive",grado=1,criterio="aicc")

acf(as.numeric(residuals(ajusteDLL4)),ci.type="ma",lag.max=36,ci.col=2) 

pacf(as.numeric(residuals(ajusteDLL4)),lag.max=36,ci.col=2) 



