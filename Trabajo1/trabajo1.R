#-------------------------------------------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
#Leer anex-EMC-SeriesIndiceEmpalmados-vtas reales lineas mercancia-may2023.csv, columna 12: 6. Productos de aseo personal, cosméticos y perfumería
Datos24=read.table(file.choose(),header=T,sep=";",skip=10,dec=",",colClasses=c(rep("NULL",11),"numeric",rep("NULL",10)))
Datos24=ts(Datos24,freq=12,start=c(2013,1))
#plot(Datos24)
#---------------------------------------------------------------------------------------------------------
#Cargar librerias 
library(forecast)
library(TSA)
library(fANCOVA)

#Cargar funciones de usuario necesarias
source("https://raw.githubusercontent.com/NelfiGonzalez/Funciones-de-Usuario-Estadistica-III/main/Funciones-Criterios.Informacion-Calidad.Intervalos.R") 
source("https://raw.githubusercontent.com/NelfiGonzalez/Funciones-de-Usuario-Estadistica-III/main/Funcion-regexponencial.R")
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
X1=data.frame(t,t2,t3,sen2,cos2,sen3,cos3,sen4,cos4,sen5,cos5)
X2=data.frame(t,t2,t3,t4,sen2,cos2,sen3,cos3,sen4,cos4,sen5,cos5)

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
X1nuevo= data.frame(t=tnuevo,t2=t2nuevo,t3=t3nuevo,sen2=sen2n,cos2=cos2n,sen3=sen3n,cos3=cos3n,sen4=sen4n,cos4=cos4n,sen5=sen5n,
                    cos5=cos5n)
ytf=ts(Datos24[tnuevo], freq=12,start=c(2022,6))

#----------------------------------------------------------------------------------------------------------------------------------

#Grafica de la serie, falta cambiar los nombres de los ejes 
plot(Datos24, ylab="Productos de aseo personal, cosmeticos y perfumeria")


#Grafica de la serie en escala log
#plot(log(Datos24), ylab = "log(Productos de aseo personal, cosm´eticos y perfumeria)")


#Tendencia 
Tt = decompose(Datos24, type = "additive")$trend
plot(Tt,ylim=c(min(Datos24),max(Datos24)))
#Estacionalidad con boxplots 
boxplot((Datos24)~cycle(Datos24),names=month.abb)

#periodograma
x=diff((Datos24))
plot(x,ylab=expression((Y[t]-Y[t-1])));abline(h=mean(x))
periodogram(x);abline(v=c(1:6)/12,col=2,lty=2)

#-----------------------------------------------------------------
# Modelo 1 polinomial de grado 3 con trigonometricas
#-----------------------------------------------------------------
modelo1=lm(yt~.,data=X1)
summary(modelo1)

#Calculo valores ajustados del modelo 1
ythatmodelo1=ts(fitted(modelo1),freq=12,start=start(yt))

#Calculo de los criterios de informacion usando exp(C^*_n(p))
nparmodelo1=length(coef(modelo1)[coef(modelo1)!=0]); nparmodelo1 #numero parametros modelo1 
Criterrios2=exp.crit.inf.resid(residuales=residuals(modelo1),n.par=nparmodelo1);Criterrios2

#Grafico de la serie y su ajuste
plot(Datos24)
lines(ythatmodelo1,col=2,lwd=2)
legend("topleft",legend=c("Original","Ajuste modelo 1"),lty=1,col=c(1,2))

#pronosticos puntuales y por Intervalos del 95%
pronmodelo1=predict(modelo1,newdata=X1nuevo,interval="prediction",level=0.95)
pronmodelo1=ts(pronmodelo1,freq=12,start=start(ytf)); pronmodelo1

ytpronmodelo1=pronmodelo1[,1]; ytpronmodelo1 #serie de los pronosticos puntuales

accuracy(ytpronmodelo1,ytf) #Calculando exactitud de los pronosticos puntuales

#precision pronosticos por I.P modelo 1
amplcobmodelo1=amplitud.cobertura(real=ytf,LIP=pronmodelo1[,2],LSP=pronmodelo1[,3]);amplcobmodelo1

#Graficos de residuales

#resikduales vs tiempo
plot.ts(residuals(modelo1),ylim=c(min(residuals(modelo1),-2*summary(modelo1)$sigma,2*summary(modelo1)$sigma),max(residuals(modelo1),-2*summary(modelo1)$sigma,2*summary(modelo1)$sigma)))
abline(h=c(-2*summary(modelo1)$sigma,0,2*summary(modelo1)$sigma),col=2)
legend("topleft",legend=c("Modelo 1"),lty=1,col=1,lwd=2)

#residuales vs valores ajustados
plot(fitted(modelo1),residuals(modelo1),ylim=c(min(residuals(modelo1),-2*summary(modelo1)$sigma,2*summary(modelo1)$sigma),max(residuals(modelo1),-2*summary(modelo1)$sigma,2*summary(modelo1)$sigma)), xlab="valores ajustados")
abline(h=c(-2*summary(modelo1)$sigma,0,2*summary(modelo1)$sigma),col=2)
legend("topleft",legend=c("Modelo 1"),lty=1,col=1,lwd=2)

#-------------------------------------------------------------------------------------------------------
# Modelo 2 polinomial de grado 4 con trigonometricas
#--------------------------------------------------------------------------
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
pronmodelo2=ts(pronmodelo2,freq=12,start=start(ytf)); pronmodelo2

ytpronmodelo2=pronmodelo2[,1]; ytpronmodelo2 #serie de los pronosticos puntuales

accuracy(ytpronmodelo2,ytf) #Calculando exactitud de los pronosticos puntuales

#precision pronosticos por I.P modelo 2
amplcobmodelo2=amplitud.cobertura(real=ytf,LIP=pronmodelo2[,2],LSP=pronmodelo2[,3]);amplcobmodelo2


#Graficos de residuales
#residuales vs tiempo

plot.ts(residuals(modelo2),ylim=c(min(residuals(modelo2),-2*summary(modelo2)$sigma,2*summary(modelo2)$sigma),max(residuals(modelo2),-2*summary(modelo2)$sigma,2*summary(modelo2)$sigma)))
abline(h=c(-2*summary(modelo2)$sigma,0,2*summary(modelo2)$sigma),col=2)
legend("topleft",legend=c("modelo 2"),lty=1,col=1,lwd=2)

#residuales vs valores ajustados
plot(fitted(modelo2),residuals(modelo2),ylim=c(min(residuals(modelo2),-2*summary(modelo2)$sigma,2*summary(modelo2)$sigma),max(residuals(modelo2),-2*summary(modelo2)$sigma,2*summary(modelo2)$sigma)), xlab="valores ajustados")
abline(h=c(-2*summary(modelo2)$sigma,0,2*summary(modelo2)$sigma),col=2)
legend("topleft",legend=c("modelo 2"),lty=1,col=1,lwd=2)

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

accuracy(predicciones3,ytf) #Calculando exactitud de los pronósticos

#Amplitud media y cobertura de los I.P
Amplcobmodelo3=amplitud.cobertura(real=ytf,LIP=predicciones3[,2],LSP=predicciones3[,3])
Amplcobmodelo3

#-----------------------------------------------------------------------------------------------------------------------------------------
# Modelo 4: Filtro de descomposicion clasica loess GCV
#----------------------------------------------------------------------------------------------------------------------------------------

source("https://raw.githubusercontent.com/NelfiGonzalez/Funciones-de-Usuario-Estadistica-III/main/
Funcion-Descomp.Loess.R")
source("https://raw.githubusercontent.com/NelfiGonzalez/Funciones-de-Usuario-Estadistica-III/main/
Funcion-SuavizamientoEstacional.R")
source("https://raw.githubusercontent.com/NelfiGonzalez/Funciones-de-Usuario-Estadistica-III/main/
Funciones-Criterios.Informacion-Calidad.Intervalos.R")
source("https://raw.githubusercontent.com/NelfiGonzalez/Funciones-de-Usuario-Estadistica-III/
main/Funcion-regexponencial.R")

ajusteDLL4=Descomp.Loess(serie.ajuste=yt,h=m,tipo.descomp="additive",grado=1,criterio="aicc")

plot(ajusteDLL4$St,ylab=expression(hat(S)[t]),lwd=2) #Gráfico estimación de St por el filtro de descomposición. 
#Es el mismo obtenido en la primera ejecución de la función Descomp.Loess

#Gráfico serie desestacionalizada y su ajuste loess
plot(ajusteDLL4$ytd,lwd=2)
lines(ajusteDLL4$Tt,col=2,lwd=2)
legend("topleft",legend=c("Serie ajustada estacionalmente","Tendencia LOESS lineal, criterio GCV"),col=c(1,2),lty=1,lwd=2)

#Gráfico de la serie y su ajuste final

plot(Datos24,lwd=2)
lines(fitted(ajusteDLL4),col=2,lwd=2)
legend("topleft",legend=c("Original","Ajuste DLL(GCV)"),col=c(1,2),lty=1,lwd=2)

#Cálculo AIC y BIC aproximados versión exp(Cn*(p))
Criterios4=exp.crit.inf.resid(residuales=residuals(ajusteDLL4),n.par=ajusteDLL4$p);Criterios4

#Tabla con pronósticos de la tendencia, la estacionalidad y de la serie
ytpron4=ajusteDLL4$ytpron
ajusteDLL4$tablapron

#Precisión pronósticos puntuales
accuracy(ajusteDLL4$ytpron,ytf)

#Gráficos de residuos de ajuste
plot(residuals(ajusteDLL4),lwd=2,ylim=c(min(-2*sqrt(ajusteDLL4$MSE),residuals(ajusteDLL4)),max(2*sqrt(ajusteDLL4$MSE),residuals(ajusteDLL4))))
abline(h=c(-2*sqrt(ajusteDLL4$MSE),0,2*sqrt(ajusteDLL4$MSE)),col=2)
legend("topleft",legend="Modelo 4: DLL(GCV)",lwd=2)

plot(as.numeric(fitted(ajusteDLL4)),residuals(ajusteDLL4),cex=1.5,ylim=c(min(-2*sqrt(ajusteDLL4$MSE),residuals(ajusteDLL4)),max(2*sqrt(ajusteDLL4$MSE),residuals(ajusteDLL4))))
abline(h=c(-2*sqrt(ajusteDLL4$MSE),0,2*sqrt(ajusteDLL4$MSE)),col=2)
legend("topright",legend="Modelo 4: DLL(GCV)",lwd=2)

#Grafica comparación de ajustes
plot(ytf,type="b",
     ylab="Indice información y comunicaciones",col=1,pch=
       ,ylim=c(min(ytf,ytpronmodelo1,ytpronmodelo2,ytpron3,ytpron4),max(ytf,ytpronmodelo1,ytpronmodelo2,ytpron3,ytpron4)),
     lwd=2,xaxt="n")
axis(1,at=time(ytf),labels=c("2021-X","2021-XI","2021-XII","2022-I","2022-II","2022-III","2022-IV","2022-V","2022-VI","2022-VII","2022-VIII","2022-IX"),cex.axis=0.7)
lines(ytpronmodelo1,col=2,pch=1,type="b",lwd=2)
lines(ytpronmodelo2,col=3,pch=2,type="b",lwd=2)
lines(ytpron3,col=4,pch=3,type="b",lwd=2)
lines(ytpron4,col=5,pch=4,type="b",lwd=2)
legend("topleft",legend=c("Real","Modelo 1","Modelo 2","Modelo 3","Modelo 4"),pch=c(19,1:4),col=c(1:5),lwd=2)

