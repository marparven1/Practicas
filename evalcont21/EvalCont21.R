# Ejercicio Evaluación contínua 20/21: Analizar el dataset hipcFR.txt, contienen datos del 
# IPC armonizado en FR desde enero de 2000 hasta diciembre de 2008

# Paso 1. Lectura y representación gráfica de los datos
library(TSA); library (tseries); library(forecast) #cargamos todas las libreras que vamos a usar


x<-read.table("hipcFR.txt")
serie <- ts(x,frequency = 12,start = c(2000,1))
serie
plot (serie, ylab="IPC armonizado en Francia",
     # xlim=c(2000,2008),ylim=c(70,100)
     )
plot(window(serie,2004,2007))

### No es necesario, pero esto si es muy interesante
# Introduzco los datos originales
ndiffs(serie) # num dif regulares necesarias en la serie para que sea estacionaria
nsdiffs(serie) # num dif estarionarias necesarias en la serie para que sea estacionaria
###


## a) Homogeneidad de la varianza de los datos datos
# Del gráfico de la serie parece que la varianza es constante
# Observamos los datos vemos que hay tendencia creciente
## Sí observamos estacionalidad, ya que dentro de cada año hay cierto patrón, que aunque no exacto, 
## se repite cada año
## Homogeneidad de la varianza: diríamos que la varianza es, en principio, estable. 



# Paso 2. Transformaciones para que la varianza sea estable en el tiempo Del gráfico de la serie, no parece
#  que haya que aplicar alguna trasnformación
##
bc=BoxCox.ar(y=serie,method = c("mle")); bc
# Sugiere transformación con lambda=1.9
# Como en el IC está el 1, no es necesario la transformación de los datos, es decir, 
# confiamos en que los datos son homogéneos y podemos seguir adelante.

## b) Homogeneidad de la media de los datos o alguna diferenciación
# Paso 3. Transformaciones para que la media sea estable en el tiempo.
acf(serie, lag=50)

# pero si mucha tendencia regular
# Las autocorrelaciones descienden lentamente, aplicamos una diferencia estacional.

serieDif=diff(serie, lag=1,differences = 1) 

acf(serieDif, lag=50)
# Se ve mucha estacionalidad, mucha dependencia de la estación, hago una dif estacional
serieDif2=diff(serieDif, lag=12,differences = 1) 
acf(serieDif2, lag=50)
## Si los datos son estacionarios, puedo empezar a ver el orden
## # Vemos un comportamiento de tipo ordinario de orden 1, pero antes vamos a ver si datos ya 
## preparados.

plot(serieDif2, main="Datos IPCh tras una diferencia regular", ylab=" ")
# Muelle
# Contraste para ver la estacionariedad

# c) Contraste adecuado para la serie transformada
# Paso 4. Contrastar estacionariedad Aplicar test de la raíz unitaria
adf.test(serieDif2)
# p-valor =  0.04418 = Rechazo 
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

acf(serieDif2, main="FAS tras una diferencia regular y otra estacional", lag=50)
# Miro la parte MA de la parte regular y estacional
# Propuesta:
# MA(1)xMA(1)12 

pacf(serieDif2, main="FAP tras una diferencia regular y otra estacional", lag=50)
# Delimita la parte AR
# AR(1)xAR(1)

eacf(serieDif2)

## Modelos propuestos 
# MA(1)xMA(1)12 
# AR(1)xAR(1)

## Ajuste 1: MA(2)xMA(1)12  ####

# Vamos a probar esos tres modelos y ver cuál es mejor.
# Pasos 6 y 7. Estimación de parámetros y diagnóstico
(ajuste1=arima(serie, order=c(0,1,1), seasonal=list(order=c(0,1,1), period=12)))
confint(ajuste1)
# aic= -22.38
# ¿RB en los resíduos?
checkresiduals(ajuste1)
# Parece bastante claro que es un RB, la ACF no se salen datos, solo uno porque es un IC
# La normalidad de los resíduos es para un IC
# df = 16, p-value = 0.776. Acepto el test de ljiun box. Acepto la incorrelación
# Es un RB. 18 lag y como hay dos parámetros le quito 2 


## Ajuste 2: AR(1)xAR(1) ####

(ajuste2=arima(serie, order=c(1,1,0), seasonal=list(order=c(1,1,0), period=12)))
confint(ajuste2)
# aic = -7.43


# ¿RB en los resíduos?
checkresiduals(ajuste2)
# Parece bastante claro que es un RB, la ACF no se salen datos, solo uno porque es un IC
# La normalidad de los resíduos es para un IC. Si parecen datos normales.
# Q* = 20.999, df = 20, p-value = 0.3972. Acepto el test de ljiun box. Acepto la incorrelación
# Es un RB. 18 lag y como hay dos parámetros le quito 2 



#### Ajuste 3. Función autoArima####
(ajuste3=auto.arima(serie, d=1, D=1, step=FALSE))

## Series: serie 
## ARIMA(2,1,0)(1,1,1)[12] 
## 
## Coefficients:
##   ar1      ar2     sar1     sma1
## 0.2584  -0.1713  -0.1080  -0.8449
## s.e.  0.1033   0.1124   0.2027   0.3180
## 
## sigma^2 = 0.03935:  log likelihood = 13.6
## AIC=-17.2   AICc=-16.52   BIC=-4.43

# Modelo: AR(2)xARMA(1,1)12
# SARIMA(2,1,0)x(1,1,1)12
# AIC=-17.2



## Comparación y elección del mejor modelo ####

# 1
# Modelo: MA(2)xMA(1)
# aic= -22.38

# 2
# Modelo: AR(1)xAR(1)
# aic = -7.43

# 3 
# Modelo: AR(2)xARMA(1,1)12
# SARIMA(2,1,0)x(1,1,1)12
# AIC = -17.2

# Mejor modelo: el que tiene menor valor del AIC 
# 3
# 3 
# Modelo: AR(2)xARMA(1,1)12
# SARIMA(2,1,0)x(1,1,1)12
# AIC = -17.2
# 3 
# Modelo: AR(2)xARMA(1,1)12
# SARIMA(2,1,0)x(1,1,1)12
# AIC = -17.2

ajuste3
# yt= alpha_t + 0.2584 y_{t-1} - 0.1713 y_{t-2} -0.1080 y_{t-12}+0.8449 alpha_{t-12}

# Apartado 5

# Paso 8. Predicción de resultados
pred=predict(ajuste3,n.ahead=12)
plot(serie, xlim=c(2000, 2010),lws=2)
lines(pred$pred, col="red")


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
##### coef <- as.vector(tendencia$coefficients)
##### a<-coef[1];b<-coef[2]
##### frec<-1:12
##### tsecular <- a + b * cc
##### pred = tsecular*dec$figure[frec]
#-------------------------
cc=1:12/12; cc=Time[108]+cc; cc=data.frame(Time=cc)
pred_tend=predict(tendencia, cc); pred_tend
dec$figure
season=c(dec$figure[1:6], dec$figure[7:12])
(p2=ts(pred_tend+season, freq=12, start=c(2009,1)))




# Apartado 7 Metodo aditivo para la tendencia y comp estacional. Método basado en suavización exponencial
y_suav=HoltWinters(serie)
(p3=predict(y_suav, n.ahead=12))

# Apartado 8. Representar las 3 predicciones y comentar
plot(serie, xlim=c(2000, 2010), lwd=2)
p1=pred$pred
lines(p1, col="red", lwd=2) #graficoIT5.pdf
lines(p2,  col="blue", lwd=2)
lines(p3,  col="green", lwd=2)
# Prácticamente coinciden
