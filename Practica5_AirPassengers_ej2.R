# Paso 1. Lectura y representación gráfica de los datos
library(TSA); library (tseries); library(forecast) #cargamos todas las libreras que vamos a usar
data(AirPassengers)
?AirPassengers
plot(AirPassengers)
# Observamos los datos
## vemos que hay tendencia, va creciendo poco a poco
## Tambien estacionalidad, ya que dentro de cada año hay cierto patrón, que aunque no exacto, se repite cada año
## Homogeneidad de la varianza: diríamos que la varianza es, en principio, creciente


# Paso 2. Transformaciones para que la varianza sea estable en el tiempo Del gr ́afico de la serie, no parece
#  que haya que aplicar alguna trasnformación
bc=BoxCox.ar(y=AirPassengers); bc
# Error
bc=BoxCox.ar(y=AirPassengers, method = "ols") ; bc
# IC ( 0.1 ,0.6)
# Sugiere transformación con lambda=0.3



## Con otra librería, no es necesario saber hacer la transformación así
BoxCox.lambda(AirPassengers,method = "loglik") #funcion del paquete forecast
# Sugiere transformación con lambda=0.2
(lambda=bc$mle)
co2_BoxCo=BoxCox(co2,lambda)
co2_Inv_BoxCox=InvBoxCox(co2_BoxCo,lambda)
(dif=co2_Inv_BoxCox)
##

# Dado que los valores sugeridos son pequeños cercanos a 0 vamos a usar la transformación logarítmica.

# Podríamos afinar con 0.2 pero no es lo usual. El modelo será ALGO mejor


# Vamos a intentar la transformación logarítmica
y=log(AirPassengers)
plot(y, main="Transformacin logarmica")
# Al parecer, la varianza de los datos es ahora estable en el tiempo


# Paso 3. Transformaciones para que la media sea estable en el tiempo.
acf(y, lag=50, main="FAS de log(Num Pasajeros)")
# Valores que decrecen muy lentamente, NECESITO DIF ORDINARIA
# Se ve mucha estacionalidad, mucha dependencia de la estación
air.d1=diff(y, lag=1, diff=1)
plot(air.d1, main="Datos log(AirPassengers) tras una diferencia regular", ylab=" ")
# Ya no hay tendencia, pero si estacionalidad
acf(air.d1, lag=50)
# Las autocorrelaciones estacionales desciende lentamente, aplicamos una diferencia estacional.
air.d=diff(air.d1, lag=12,diff=1) # de orden 12, datos anuales

acf(air.d, lag=50)
# Esta FAC puede ser estacionaria en la ACP miro la parte MA 
# En la FACP miro la parte AR
## Si los datos son estacionarios, puedo empezar a ver el orden
## # Vemos un comportamiento de tipo ordinario de orden 1, pero antes vamos a ver si datos ya preparados.

plot(air.d, main="Datos log(AirPassengers) tras una diferencia regular y otra estacional", ylab=" ")
# Muelle
# Contraste para ver la estacionariedad
# Paso 4. Contrastar estacionariedad Aplicar test de la raíz unitaria
adf.test(air.d)
# p-value = 0.01
# Los datos transformados, rechazamos, ese rechazo implica estacionariedad. He acabado de diferenciar, ajusto
# un ARMA(p,q)x(P,Q)s

### No es necesario, pero esto si es muy interesante
# Introduzco los datos originales
ndiffs(y) # num dif regulares necesarias en la serie para que sea estacionaria
nsdiffs(y) # num dif estarionarias necesarias en la serie para que sea estacionaria
###


# Homog en varianza
# Estacionarios regularmente y estacionariamente

# Paso 5. Identificar estructura ARIMA
acf(air.d, main="FAS tras una diferencia regular y otra estacional", lag=50)
# Miro la parte MA de la parte regular y estacional
# Propuesta:
# ARMA(0,2)xARMA(0,1)12 

# En realidad, será un MA(1), será un MA(3) en todo caso, pero la bara 2 es 0.
# ¡¡¡ CUIDADO CON LO QUE ME HA PASADO!!!
# Como luego lo voy a quitar, no cambio el código pero cuidado.

pacf(air.d, main="FAP tras una diferencia regular y otra estacional", lag=50)
# Podríamos probar AR(1) y AR(3), a mi AR(3) me parece un orden gigante
# En la parte estacional AR(1)12

eacf(air.d) #FACE
# Me sugiere también ARMA(1,1)

# Vamos a probar esos dos modelos y ver cuál es mejor.


# Pasos 6 y 7. Estimación de parámetros y diagnóstico

(ajuste1=arima(y, order=c(0,1,2), seasonal=list(order=c(0,1,1), period=12)))
confint(ajuste1)
# IC del coef de MA(2) regular contiene al 0, lo quito 
#  aic = -483.62

(ajuste1=arima(y, order=c(0,1,1), seasonal=list(order=c(0,1,1), period=12)))
confint(ajuste1)
# No contienen al 0
# aic = -485.4
checkresiduals(ajuste1)
# p-value = 0.233. Acepto la incorrelación de los datos. Mirando el gráfico, parace que si, en la FAS
# se sale un dato, pero únicamente uno y es normal porque es un IC 
# Los resíduos siguien un RB, el modelo es adecuado MA(1)xMA(1)12

(ajuste2=auto.arima(y, d=1, D=1, step=FALSE))
## Propone el mismo modelo que teníamos

## Series: y 
## ARIMA(0,1,1)(0,1,1)[12] 
## 
## Coefficients:
##   ma1     sma1
## -0.4018  -0.5569
## s.e.   0.0896   0.0731
## 
## sigma^2 estimated as 0.001371:  log likelihood=244.7
## AIC=-483.4   AICc=-483.21   BIC=-474.77


(ajuste3=arima(y, order=c(1,1,1), seasonal=list(order=c(1,1,1), period=12)))
confint(ajuste3)
# El AR1 de la parte regular y de la estacional sobran, es un MA(1)xMA(1)12

# Paso 8. Predicción de resultados
pred=predict(ajuste1,n.ahead=24)
plot(y, xlim=c(1949, 1962), ylim=c(4.5, 7))
lines(pred$pred, col="red")

# Predicción forecast
forecast(ajuste1,h=24)
plot(forecast(ajuste1,h=24))


plot(AirPassengers, xlim=c(1949, 1962), ylim=c(100,750))
lines(exp(pred$pred), col="red")



(ajuste2=auto.arima(AirPassengers, max.p=0, d=1, max.P=0, D=1, step=FALSE, lambda=0))
forecast(ajuste2,h=24)
plot(forecast(ajuste2,h=24))


