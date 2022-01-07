# Ejercicio 1: Analizar el dataset co2, contenido en la librería TSA, que contiene una serie de tiempo de 132
#  observaciones mensuales de 1994 a 2005, relativa a la concentración de CO2 en Alert (Canadá).

# Paso 1. Lectura y representación gráfica de los datos
library(TSA); library (tseries); library(forecast) #cargamos todas las libreras que vamos a usar
data(co2)
str(co2)
plot (co2, ylab="CO2")
# Observamos los datos
## vemos que hay tendencia, va creciendo poco a poco
## Tambien estacionalidad, ya que dentro de cada año hay cierto patrón, que aunque no exacto, se repite cada año
## Homogeneidad de la varianza: diríamos que la varianza es, en principio, estable. 


## Instrucciones no necesarias
# win.graph(width=4.875,heigth=3, pointsize=8)
plot(co2,ylab="CO2")
plot(window(co2,start=c(2000,1)),ylab="CO2")
Month=c("E","F","M","A","M","J","J","J","S","O","N","D")
points(window(co2,start=c(2000,1)),pch=Month)
##


# Paso 2. Transformaciones para que la varianza sea estable en el tiempo Del gr ́afico de la serie, no parece
#  que haya que aplicar alguna trasnformación
bc=BoxCox.ar(y=co2); bc
# IC ( 0.2, 2.0)
# Sugiere transformación con lambda=2
# Como en el IC está el 1, no es necesario la transformación de los datos, es decir, 
# confiamos en que los datos son homogéneos y podemos seguir adelante.

## Con otra librería, no es necesario saber hacer la transformación así
BoxCox.lambda(co2,method = "loglik")
(lambda=bc$mle)
co2_BoxCo=BoxCox(co2,lambda)
co2_Inv_BoxCox=InvBoxCox(co2_BoxCo,lambda)
(dif=co2_Inv_BoxCox)
##


# Paso 3. Transformaciones para que la media sea estable en el tiempo.
acf(co2, lag=50)
# Valores que decrecen muy lentamente, DIF ORDINARIA
# Se ve mucha estacionalidad, mucha dependencia de la estación
co2.d1=diff(co2)
plot(co2.d1, main="Datos co2 tras una diferencia regular", ylab=" ")
# Ya no hay tendencia, pero si mucha estacionalidad
acf(co2.d1, lag=50)
# Las autocorrelaciones estacionales desciende lentamente, aplicamos una diferencia estacional.
co2.d=diff(co2.d1, lag=12) # de orden 12, datos anuales

acf(co2.d, lag=50)
# Esta FAC puede ser estacionaria en la ACP miro la parte MA 
# En la FACP miro la parte AR
## Si los datos son estacionarios, puedo empezar a ver el orden
## # Vemos un comportamiento de tipo ordinario de orden 1, pero antes vamos a ver si datos ya preparados.

plot(co2.d, main="Datos co2 tras una diferencia regular y otra estacional", ylab=" ")
# Muelle
# Contraste para ver la estacionariedad
# Paso 4. Contrastar estacionariedad Aplicar test de la raíz unitaria
adf.test(co2.d)
# Los datos transformados, rechazamos, ese rechazo implica estacionariedad. He acabado de diferenciar, ajusto
# un ARMA(p,q)x(P,Q)s

### No es necesario, pero esto si es muy interesante
# Introduzco los datos originales
ndiffs(co2) # num dif regulares necesarias en la serie para que sea estacionaria
nsdiffs(co2) # num dif estarionarias necesarias en la serie para que sea estacionaria
###


# Homog en varianza
# Estacionarios regularmente y estacionariamente
# Paso 5. Identificar estructura ARIMA
 
acf(co2.d, main="FAS tras una diferencia regular y otra estacional", lag=50)
# Miro la parte MA de la parte regular y estacional
# Propuesta:
# MA(1)xMA(1)12 

pacf(co2.d, main="FAP tras una diferencia regular y otra estacional", lag=50)
# Delimita la parte AR
# AR(2)xAR(1)


# Vamos a probar esos dos modelos y ver cuál es mejor.
# Pasos 6 y 7. Estimación de parámetros y diagnóstico
(ajuste1=arima(co2, order=c(0,1,1), seasonal=list(order=c(0,1,1), period=12)))
confint(ajuste1)
# No contienen al 0
# AIC = 283.08
# ¿RB en los resíduos?
checkresiduals(ajuste1)
# Parece bastante claro que es un RB, la ACF no se salen datos, solo uno porque es un IC
# Sospechamos que no siguien unos datos normales, pero solo hay que ver si es RC
# La normalidad de los resíduos es para un IC
# df = 22, p-value = 0.2564. Acepto el test de ljiun box. Acepto la incorrelación
# Es un RB. 24 lag y como hay dos parámetros le quito 2 
(ajuste2=auto.arima(co2, d=1, D=1, step=FALSE))
## Propone el mismo modelo que teníamos
## Series: co2 
## ARIMA(0,1,1)(0,1,1)[12] 
## 
## Coefficients:
##   ma1     sma1
## -0.5792  -0.8206
## s.e.   0.0791   0.1137
## 
## sigma^2 estimated as 0.5683:  log likelihood=-139.54
## AIC=285.08   AICc=285.29   BIC=293.41



(ajuste3=arima(co2, order=c(2,1,0), seasonal=list(order=c(1,1,0), period=12)))
confint(ajuste3)
checkresiduals(ajuste3)
# AIC = 305.37 Es bastante peor modelo. Los resíduos no siguen un RB
# p-value = 0.04146. Rechazo, los datos no son incorrelados, por lo que no son un RB
# Subo a 2 el orden retardo estacional
(ajuste4=arima(co2, order=c(2,1,0), seasonal=list(order=c(2,1,0), period=12)))
confint(ajuste4)
# Me sugiere que quite el 2 y lo convierta en un 1 y haga un AR(1) en la parte regular
checkresiduals(ajuste4)
(ajuste5=arima(co2, order=c(1,1,0), seasonal=list(order=c(2,1,0), period=12)))
confint(ajuste5)
checkresiduals(ajuste5)
# El AIC = 297.54 no es mejor




# Paso 8. Predicción de resultados
pred=predict(ajuste1,n.ahead=24)
plot(co2, xlim=c(1994, 2006), ylim=c(350,400))
lines(pred$pred, col="red")

# Predicción forecast
forecast(ajuste2,h=24)
plot(forecast(ajuste2,h=24))
