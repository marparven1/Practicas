# Ejercicio Diciembre 21: Analizar el dataset qlecheitalia, contenido en la librería TSA, que contiene una 
# serie de tiempo

# Paso 1. Lectura y representación gráfica de los datos
library(TSA); library (tseries); library(forecast) #cargamos todas las libreras que vamos a usar
x<-read.table("qlecheitalia.txt")
serie <- ts(x,frequency = 12,start = c(2000,1))
serie
plot (serie, ylab="Cantidad de leche producida en Italia",
      xlim=c(2000,2008),ylim=c(700,950))

## a) Homogeneidad de la varianza de los datos datos
# Del gráfico de la serie parece que la varianza es constante
# Observamos los datos vemos que no hay tendencia, los datos no van creciendo poco a poco
## Sí observamos estacionalidad, ya que dentro de cada año hay cierto patrón, que aunque no exacto, 
## se repite cada año
## Homogeneidad de la varianza: diríamos que la varianza es, en principio, estable. 



# Paso 2. Transformaciones para que la varianza sea estable en el tiempo Del gráfico de la serie, no parece
#  que haya que aplicar alguna trasnformación
bc=BoxCox.ar(y=serie); bc

# IC ( -1.8 , 2.0)
# Sugiere transformación con lambda=1.3
# Como en el IC está el 1, no es necesario la transformación de los datos, es decir, 
# confiamos en que los datos son homogéneos y podemos seguir adelante.

## b) Homogeneidad de la media de los datos o alguna diferenciación
# Paso 3. Transformaciones para que la media sea estable en el tiempo.
acf(serie, lag=50)
# Se ve mucha estacionalidad, mucha dependencia de la estación, pero no mucha tendencia regular
# Las autocorrelaciones estacionales desciende lentamente, aplicamos una diferencia estacional.

serieDif2=diff(serie, lag=12) # de orden 12, datos anuales

acf(serieDif2, lag=50)
## Si los datos son estacionarios, puedo empezar a ver el orden
## # Vemos un comportamiento de tipo ordinario de orden 1, pero antes vamos a ver si datos ya 
## preparados.

plot(serieDif2, main="Datos leche tras una diferencia estacional", ylab=" ")
# Muelle
# Contraste para ver la estacionariedad

# c) Contraste adecuado para la serie transformada
# Paso 4. Contrastar estacionariedad Aplicar test de la raíz unitaria
adf.test(serieDif2)
# p-valor = 0.0216 = Rechazo 
# Los datos transformados, rechazamos, ese rechazo implica estacionariedad. 
# He acabado de diferenciar, ajusto un ARMA(p,q)x(P,Q)s

### No es necesario, pero esto si es muy interesante
# Introduzco los datos originales
ndiffs(serie) # num dif regulares necesarias en la serie para que sea estacionaria
nsdiffs(serie) # num dif estarionarias necesarias en la serie para que sea estacionaria
###


## Apartado d)

# Datos estacionarios regularmente y estacionariamente

# Paso 5. Identificar estructura ARIMA

acf(serieDif2, main="FAS tras una diferencia  estacional", lag=50)
# Miro la parte MA de la parte regular y estacional
# Propuesta:
# MA(2)xMA(1)12 
# AR(1)xMA(1)

pacf(serieDif2, main="FAP tras una diferencia estacional", lag=50)
# Delimita la parte AR
# AR(1)xAR(1)

eacf(serieDif2)

## Modelos propuestos 
# MA(2)xMA(1)12 
# AR(1)xMA(1)
# AR(1)xAR(1)

## Ajuste 1: MA(2)xMA(1)12  ####

# Vamos a probar esos tres modelos y ver cuál es mejor.
# Pasos 6 y 7. Estimación de parámetros y diagnóstico
(ajuste1=arima(serie, order=c(0,0,2), seasonal=list(order=c(0,1,1), period=12)))
confint(ajuste1)

# MA(2) en laparte regular sobra. MA(1)xMA(1)12
(ajuste1=arima(serie, order=c(0,0,1), seasonal=list(order=c(0,1,1), period=12)))
confint(ajuste1)
# aic = 651.87


# ¿RB en los resíduos?
checkresiduals(ajuste1)
# Parece bastante claro que es un RB, la ACF no se salen datos, solo uno porque es un IC
# La normalidad de los resíduos es para un IC
# df = 16, p-value = 0.3573. Acepto el test de ljiun box. Acepto la incorrelación
# Es un RB. 18 lag y como hay dos parámetros le quito 2 

(ajuste1=arima(serie, order=c(0,0,1), seasonal=list(order=c(0,1,1), period=12)))
confint(ajuste1)


## Ajuste 2: AR(1)xMA(1) ####

(ajuste2=arima(serie, order=c(1,0,0), seasonal=list(order=c(0,1,1), period=12)))
confint(ajuste2)
# aic = 648.57


# ¿RB en los resíduos?
checkresiduals(ajuste2)
# Parece bastante claro que es un RB, la ACF no se salen datos, solo uno porque es un IC
# La normalidad de los resíduos es para un IC. Si parecen datos normales.
# Q* = 11.145, df = 16, p-value = 0.8005. Acepto el test de ljiun box. Acepto la incorrelación
# Es un RB. 18 lag y como hay dos parámetros le quito 2 







## Ajuste 3: AR(1)xAR(1) ####

(ajuste3=arima(serie, order=c(1,0,0), seasonal=list(order=c(1,1,0), period=12)))
confint(ajuste3)
# aic = 650.04


# ¿RB en los resíduos?
checkresiduals(ajuste3)
# Parece bastante claro que es un RB, la ACF no se salen datos, solo uno porque es un IC
# La normalidad de los resíduos es para un IC. Si parecen datos normales.
# Q* = 12.499, df = 16, p-value = 0.709. Acepto el test de ljiun box. Acepto la incorrelación
# Es un RB. 18 lag y como hay dos parámetros le quito 2 






#### Ajuste 4. Función autoArima####
(ajuste4=auto.arima(serie, d=0, D=1, step=FALSE))

## Series: serie 
## ARIMA(1,0,0)(0,1,1)[12] 
## 
## Coefficients:
##   ar1     sma1
## 0.3902  -0.4620
## s.e.  0.1034   0.1201
## 
## sigma^2 = 224.3:  log likelihood = -322.28
## AIC=650.57   AICc=650.89   BIC=657.64

# Modelo: AR(1)xMA(1)12
# AIC=650.57



## Comparación y elección del mejor modelo ####

# 1
# Modelo: MA(1)xMA(1)
# aic = 651.87

# 2
# Modelo: AR(1)xMA(1)
# aic = 648.57

# 3
# Modelo: AR(1)xAR(1)
# aic = 650.04

# 4 
# Modelo: AR(1)xMA(1)12
# AIC = 650.57

# Mejor modelo: el que tiene menor valor del AIC 
# 2
# Modelo: AR(1)xMA(1)
# aic = 648.57

ajuste2
# yt= alpha_t + 0.3696 y_{t-1} + 0.4620 theta_{t-1}^12 

# Apartado 5

# Paso 8. Predicción de resultados
pred=predict(ajuste2,n.ahead=24)
plot(serie, xlim=c(2000, 2009), ylim=c(770,930))
lines(pred$pred, col="red")

c(pred$pred - 1.96 * pred$se,pred$pred + 1.96 * pred$se)

inf <- pred$pred - qnorm(0.95) * pred$se
sup <- pred$pred + qnorm(0.95) * pred$se

cbind.data.frame(pred$pred,inf,sup)

# Apartado 6 Metodo aditivo para la tendencia y comp estacional. Método basado en MMóviles
dec=decompose(serie, type="additive")
y_des=serie-dec$seasonal #serie desestacionalizada
#---------------------------------------------
# ajustamos una recta a la serie desestacionalizada
#-----------------------------------------
Time=time(y_des)
tendencia=lm(y_des~Time)
summary(tendencia)
#-------------------------
cc=1:24/24; cc=Time[90]+cc; cc=data.frame(Time=cc)
pred_tend=predict(tendencia, cc); pred_tend
dec$figure
season=c(dec$figure[7:12], dec$figure, dec$figure[1:6])
(p2=ts(pred_tend+season, freq=12, start=c(2007,7)))




# Apartado 7 Metodo aditivo para la tendencia y comp estacional. Método basado en suavización exponencial
y_suav=HoltWinters(serie)
(p3=predict(y_suav, n.ahead=24))

# Apartado 8. Representar las 3 predicciones y comentar
plot(serie, xlim=c(2000, 2010), lwd=2)
p1=pred$pred
lines(p1, col="red", lwd=2) #graficoIT5.pdf
lines(p2,  col="blue", lwd=2)
lines(p3,  col="green", lwd=2)
# Prácticamente coinciden