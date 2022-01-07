# Practica 5
# 17/12/21

# 1. Lectura y representación gráfica de los datos
matriculaciones_ini=read.table("matricul.dat")
matriculaciones_ini=ts(matriculaciones_ini,  frequency = 12, start = c(1960, 1))
plot(matriculaciones_ini, xlab="tiempo", ylab="matriculaciones")


# en el gráfico vemos que la varianza no es constante porque las 
# oscilaciones van aumentando. Por tanto, tendremos que hacer alguna
# transformación para que la varianza sea constante


# 2. Transformación de la varianza para que sea estable en el tiempo
library(TSA)
?BoxCox.ar
bc=BoxCox.ar(y=matriculaciones_ini); bc
# Rejilla de lambdas y vamos evaluando la LOG verosimilitud para 
# ver el óptimo
bc$mle # est max verosimilitud
# corresponde a la observación 22 1584.3671
bc$ci # Tenemos IC


# Podemos solicitar la busqueda en una rejilla mas fina (por 
# defecto va de -2 a 2 con saltos de 0.1):
bc.bis=BoxCox.ar(y=matriculaciones_ini, lambda = seq(0, 0.25, 0.01) ); bc.bis
bc.bis$mle # est max verosimilitud
# corresponde a la observación 14 1584.620
bc.bis$ci # Tenemos IC


# Si proximo a 0 -> LOG
# Si póroximo a 1/2 -> raiz cuadrada
# 1/3 -> raiz cúbica
# Si lambda es distinto de 0, la hago
# Tengo que redondear, para buscar una transformación conocida y simple

# Vamos a hacer las transformaciones, para ver que ambos resultados son similares
matriculaciones=log(matriculaciones_ini)
plot(matriculaciones, xlab="tiempo", ylab="matriculaciones")

Transfmatriculaciones_ini=matriculaciones_ini^(0.13)
plot(Transfmatriculaciones_ini, xlab="tiempo", ylab="matriculaciones")

# La varianza es constante: las oscilaciones son de longitud similar. Nos quedamos con la transformación
# IMPORTANTE: PARA LAS PREDICCIONES HAY QUE DESHACER EL CAMBIO.
# HAGO LA PREDICCIÓN Y LUEGO DESHAGO EL CAMBIO

#  Paso 3. Transformaciones para que la media sea estable en el tiempo.
acf(matriculaciones, main="FAS de ln(matriculaciones)")
# la FAS decrece muy lentamente, lo que indica que se trata de un modelo integrado. 
# Vamos a hacer una diferencia. Volvemos a represenatar la FAS
matri_ini=diff(matriculaciones, lag=1, # orden de la diferencia
               diff=1) # número de diferencias
acf(matri_ini, main="FAS de primera diferencia de ln(matriculaciones)", lag=50)
# Observamos una componente estacional.
# Obsercamos autocorrelaciones que descienden lentamente de 12 en 12 meses. Necesitamos una diferencia
# estacional, no regular, ya que las correlaciones se meten en las bandas
#  Cada año vemos 
# d=1 y D=1 . Una diferencia regular y otra estacional (respectivamente)
matri=diff(matri_ini, 
           lag=12,  # orden de la diferencia: Enero con enero, Feb bon feb
           diff=1)
acf(matri, main="FAS tras una diferencia regular y otra estacional", lag=50)
# Autocorrelaciones que descienden rápidamente, ya hemos acabado. Está determinado el d, orden de integración, que es 1
# FAS tiene la forma de un estacionario


# Ahora tenemos que ver p,P,q,Q. Esto se ve en la FAS y la FAP

# Vamos a dibujar el proceso estacionario, que tiene que ser un muelle
plot(matri)
# muelle. Estacionariedad.
#  Vamos a comprobar la estacionariedad con el TEST de Dikey-FUller para estar seguros

# 4.  Contrastar estacionariedad. Aplicar test de la raız unitaria (si rechazara tendría que buscar otra diferencia)
# H0: el pol autoregresivo tiene una raiz unitaria
# H1: todas las raices del pol autoregresivo son estacionarias, en módulo son mayores que 1
library(tseries)
adf.test(matri)

# Warning message:
# In adf.test(matri) : p-value smaller than printed p-value
## Esto es muy bueno. p-valor muy pequeño



# Paso 5. Identificar estructura ARIMA
# 

acf(matri, main="FAS tras una diferencia regular y otra estacional", lag=50)
# palo único
# Miro Parte estacional
# # palitos que van decreciendo exponencialmente. Solo un palito fuera de la banda
#  en el 1 Candidato-> MA(1). Palito verde en aptes
#  Otro candidato admisible MA(2) hay un palito que se sale un poquito (palo rojo apuntes)
#  P,Q

# Parte regular (mirar al ppio)
# Miro el palo largo (Autocorrelación negativa) del retardo 1. A cada lado de la parte estacionaria
# aparece una autocorrelación (palito) significativamente no nula. Las demás están metidas en las bandas
# Parece tambien un MA(1)
# p,q


pacf(matri, main="FAP tras una diferencia regular y otra estacional", lag=50) 




# La parte regular
# esto era para matri INI
# Palos decrecen lentamente -> no es AR
# Una serie de palos (autocorr parciales) que van decreciendo rápidamente pero que
# no son nulos. Ahora miro en el uno el positivo y a la derecha vuelven a 
# aparecer palitos que decrecen rápidamente
# orden del MA: miro número de autocorrelaciones simples significativamente no nulas

# herramienta para deperminar el orden ARMA si no está claro en la FAS y FAP
# la face. Es de aplicabilidad si NO hay componente estacional



# Vamos a probar modelos
# reg/est
# reg  stac
# MA(1)XMA(1)
# MA(1)XMA(2)
# AR(1)XMA(1)
# AR(1)XMA(2)
# AR(2)XMA(1)
# AR(2)XMA(2)
# ARMA(1,1)XMA(1)
# ARMA(1,1)XMA(2)

# Lo que tengo que comprobar:
###### coef no nulos: si hay los quito
###### pasa la diagnosis

# Pasos 6 y 7. Estimacio ́n de parámetros y diagnóstico
# reg X stac
# MA(1)xMA(1)
ajuste1=arima(matri, order=c(0,0,1), seasonal=list(order=c(0,0,1), period=12)); ajuste1
confint(ajuste1)
###### coef no nulos: si hay los quito
# El 0 NO tiene que estar en el IC. Quito la media
ajuste1.1=arima(matri, order=c(0,0,1), seasonal=list(order=c(0,0,1), period=12),include.mean=FALSE,); ajuste1.1
confint(ajuste1.1)
###### pasa la diagnosis?
library(forecast)
checkresiduals(ajuste1.1)
# si lo pasa el p-valor tiene que ser > que alpha
# p-value = 0.006921 No pasa la diagnosis
# NO ME SIRVE.
# 
# reg X stac
# MA(1)xMA(2)
ajuste2=arima(matri, order=c(0,0,1), seasonal=list(order=c(0,0,2), period=12)); ajuste2
confint(ajuste2)
# El 0 NO tiene que estar en el IC. Quito la media
ajuste2.1=arima(matri, order=c(0,0,1), seasonal=list(order=c(0,0,2), period=12),include.mean=FALSE,); ajuste2.1
confint(ajuste2.1)
###### pasa la diagnosis?
checkresiduals(ajuste2.1)
# si lo pasa el p-valor tiene que ser > que alpha
# p-value = 0.006921 SI pasa la diagnosis
# puede ser UN OFICIAL
# 

# reg X stac
# AR(1)xMA(1)
ajuste3=arima(matri, order=c(1,0,0), seasonal=list(order=c(0,0,1), period=12)); ajuste3
confint(ajuste3)
# El 0 NO tiene que estar en el IC. Quito la media
ajuste3.1=arima(matri, order=c(1,0,0), seasonal=list(order=c(0,0,1), period=12),include.mean=FALSE,); ajuste3.1
confint(ajuste3.1)
###### pasa la diagnosis?
checkresiduals(ajuste3.1)
# si lo pasa el p-valor tiene que ser > que alpha
# p-value =  5.261e-09 NO pasa la diagnosis


# reg X stac
# AR(1)xMA(2)
ajuste4=arima(matri, order=c(1,0,0), seasonal=list(order=c(0,0,2), period=12)); ajuste4
confint(ajuste4)
# El 0 NO tiene que estar en el IC. Quito la media
ajuste4.1=arima(matri, order=c(1,0,0), seasonal=list(order=c(0,0,2), period=12),include.mean=FALSE,); ajuste4.1
confint(ajuste4.1)
###### pasa la diagnosis?
checkresiduals(ajuste4.1)
# si lo pasa el p-valor tiene que ser > que alpha
# p-value =  3.782e-06 NO pasa la diagnosis


# reg X stac
# AR(2)xMA(1)
ajuste5=arima(matri, order=c(2,0,0), seasonal=list(order=c(0,0,1), period=12)); ajuste5
confint(ajuste5)
# El 0 NO tiene que estar en el IC. Quito la media
ajuste5.1=arima(matri, order=c(2,0,0), seasonal=list(order=c(0,0,1), period=12),include.mean=FALSE,); ajuste5.1
confint(ajuste5.1)
###### pasa la diagnosis?
checkresiduals(ajuste5.1,plot=FALSE)
# si lo pasa el p-valor tiene que ser > que alpha
# p-value =  0.02225 NO pasa la diagnosis


# reg X stac
# AR(2)xMA(2)
ajuste6=arima(matri, order=c(2,0,0), seasonal=list(order=c(0,0,2), period=12)); ajuste6
confint(ajuste6)
# El 0 NO tiene que estar en el IC. Quito la media
ajuste6.1=arima(matri, order=c(2,0,0), seasonal=list(order=c(0,0,2), period=12),include.mean=FALSE,); ajuste6.1
confint(ajuste6.1)
###### pasa la diagnosis?
checkresiduals(ajuste6.1,plot=FALSE)
# si lo pasa el p-valor tiene que ser > que alpha
# p-value =  0.1411 SI pasa la diagnosis


# reg X stac
# ARMA(1,1)xMA(1)
ajuste7=arima(matri, order=c(1,0,1), seasonal=list(order=c(0,0,1), period=12)); ajuste7
confint(ajuste7)
# El 0 NO tiene que estar en el IC. Quito la media y quito AR1, porque el 0 está en el IC.
#  Se queda un MA(1)xMA(1) que ya lo he rechazado. Este modelo adios


# reg X stac
# ARMA(1,1)xMA(2)
ajuste8=arima(matri, order=c(1,0,1), seasonal=list(order=c(0,0,2), period=12)); ajuste8
confint(ajuste8)
# El 0 NO tiene que estar en el IC. Quito la media y quito AR1, porque el 0 está en el IC.
#  Se queda un MA(1)xMA(2) que ya lo he aceptado. Este modelo adios
#  
#  



# Tenemos dos modelos
# AIC2: -630.26
# AIC: -621.77

# Nos quedamos con el modelo 2, que es el de menor AIC MA(1)XMA(2)

auto <- auto.arima(matri,step=FALSE)
# MA(1)xARMA(1,1)


confint(auto)
checkresiduals(auto)
# p-value = 0.1615 pasa el test

# AIC=-628.95 
# El mio tenía menor AIC


autoConStep <- auto.arima(matri)
autoConStep 
# ARMA(1,2)xARMA(2,1)

# AIC=-AIC=-624.08
# El mio tenía menor AIC
confint(autoConStep)
# ma2  -0.06060482  0.8771745 el ma lo tengo que sacar
# Hacemos este modelo ARMA(1,1)xARMA(1,1)
arima(matri, order=c(1,0,1), seasonal=list(order=c(1,0,1), period=12))
confint(arima(matri, order=c(1,0,1), seasonal=list(order=c(1,0,1), period=12)))
# quito AR1
# MA(1)xARMA(1,1) . Es el de arriba. Está hecho




# Nos quedamos con el modelo 2 y el omdelo 9
# Tienen un AIC muy próximo, coeficientes no nulos y pasan el test de lbox
# Nos van a dar una predicción muy similar.
# Vamos a ajustar los modelos completos para poder hacer predicciones


# pones D=d=1 para no deshacer logaritmos de diferencias, que es un toston. Para
# predecir incluyo LAS DIFERENCIAS, PARA QUE HAGA LAS PREDICCIONES CON LAS DIFERENCIAS
# SINO, TENDRÍA QUE SER A MANO. POR DEFECTO LA MEDIA SE LA QUITA SI AJUSTO UN MODELO INTEGRADO



AJUSTE1<- arima(matriculaciones, # LOG Yt
                order=c(0,1,1), seasonal=list(order=c(0,1,2), period=12))
AJUSTE1
#  aic = -630.26
#  es m
AJUSTE2 <- arima(matriculaciones, # LOG Yt
                 order=c(0,1,1), seasonal=list(order=c(1,1,1), period=12))
AJUSTE2
# aic = -630.95

# Mejor modelo: ajuste 1


# PREDICCIÓN: 
# con predict
# con paquete forecast

## Paso 8. Predicción de resultados
## hay que deshacer el cambio tomando exponencial



pred1=predict(AJUSTE1,n.ahead=24) # da prediccin + error estndar de la prediccin
plot(matriculaciones, xlim=c(1960, 2005), ylim=c(8,12))
lines(pred1$pred, col="red")

plot(matriculaciones_ini, xlab="tiempo", ylab="matriculaciones", xlim=c(1960, 2005), ylim=c(0,190000))
lines(exp(pred1$pred), col="red")


pred2=predict(AJUSTE2,n.ahead=24) # da prediccin + error estndar de la prediccin
plot(matriculaciones, xlim=c(1960, 2005), ylim=c(8,12))
lines(pred2$pred, col="red")

plot(matriculaciones_ini, xlab="tiempo", ylab="matriculaciones", xlim=c(1960, 2005), ylim=c(0,190000))
lines(exp(pred2$pred), col="red")



# DA ERROR
# plot((forecast(AJUSTE1,h=24))) #muy similar al anterior

# AJUSTAR EL MODELO CON EL PAQUETE FORECAST 
# FUNCIÓN: ARIMA

# AJUSTE1F<- ARIMA(matriculaciones, # LOG Yt
#                 order=c(0,1,1), seasonal=list(order=c(0,1,2), period=12))
# AJUSTE1F


y=arima.sim(list(order=c(0,0,8), ma=c(0,0,0,0.5,0,0,0,0.7)),n=10000)
acf(y, lwd=2, main=" ")
