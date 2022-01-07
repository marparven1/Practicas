#   Práctica 3. Suavizado exponencial.

#   La función HoltWinters()
#   ________________________
#   
#   gamma estacionalidad
#   beta tendencia
#   gamma media
#   
#   simple (no beta y no gamma)
#   doble hay no gamma
#   triple si hay todo

?HoltWinters 

#   Suavizado exponencial simple
#   ____________________________

y=c(409,416,423,430,437,444,452,458,459,461,461)
y=ts(y, start=2002) # Es importante convertir los datos en serie temporal
Pb3.simple=HoltWinters(y,alpha=0.8,beta=F,gamma=F)
str(Pb3.simple)
Pb3.simple$fitted

plot(Pb3.simple) #representa valores observados y ajustados
Pb3.simple.440=HoltWinters(y,alpha=0.8,beta=F,gamma=F, l.start=440)
Pb3.simple.min=HoltWinters(y,beta=F,gamma=F)

Pb3.simple$SSE
Pb3.simple.440$SSE
Pb3.simple.min$SSE
predict(Pb3.simple, n.ahead=5)

0.9999226 # mejor valor de alfa, predict ya le da los mejores parámetros
# n.ahead es el número de parámtros que voy a predecir, los 5 siguientes

#   Ejemplo 2 Suavizado exponencial simple
#   Predecir la tendencia de la serie de paro de los años 2003 a 2012 en el
#   año 2013, usando un suavizado exponencial simple
#   ______________________________________

paro <- read.csv("paro.csv", header=T, sep=";",dec=",")
paro$Total <- gsub(".", "", paro$Total)
paro$Total <- gsub("\\.", "", paro$Total) # le quitamos el .
paro$Total <- gsub(",", ".", paro$Total) # le quito la coma y la transformo en punto, que es lo que quiere R para tratar con números 
paro$Total=as.numeric(paro$Total,2)
serie1=ts(data=paro$Total,frequency=4,start=c(2003,1))
serie1
paro.hw=HoltWinters(serie1,beta=F,gamma=F)
plot(serie1)
paro.hw$fitted
lines(paro.hw$fitted[,1],col="red")
predict(paro.hw,n.ahead=4)
plot(serie1,xlim=c(2003,2013))
lines(paro.hw$fitted[,1],col=2)
lines(predict(paro.hw,n.ahead=4),col=2)

#   Suavizado exponencial doble de Holt
#   ___________________________________

Pb3.doble=HoltWinters(y,alpha=0.5, beta=0.1,gamma=F)
Pb3.doble$fitted
Pb3.doble.440.1=HoltWinters(y,alpha=0.5,beta=0.1,gamma=F, l.start=440, b.start=1)
Pb3.doble.min=HoltWinters(y,gamma=F)
Pb3.doble$SSE
Pb3.doble.440.1$SSE
Pb3.doble.min$SSE
predict(Pb3.doble, n.ahead=3)
plot(Pb3.simple.min)

#   Suavizado exponencial triple de Holt-Winters (co2)
#   __________________________________________________

help(co2) #para informacion sobre este dataset
co2
m=HoltWinters(co2)
m$SSE
plot(m)             #plot de valores ajustados y observados
plot(fitted(m))
plot(co2,xlim=c(1959,1998)) 
# parece aditividad
# Si no lo tengo claro, hago ambos y busco el que tenga mejor sc_e
lines(m$fitted[,1],col=2)
prediccion=predict(m, n.ahead=12)
lines(prediccion,col=4)

#   Suavizado exponencial triple de Holt-Winters (paro)
#   ___________________________________________________

paro <- read.csv("paro.csv", header=T, sep=";",dec=",")
paro$Total <- gsub("\\.", "", paro$Total)
paro$Total <- gsub(",", ".", paro$Total)
paro$Total=as.numeric(paro$Total,2)
serie1=ts(data=paro$Total,frequency=4,start=c(2003,1))
serie1
plot(serie1)
paro.hw=HoltWinters(serie1,seasonal = c("add"))
plot(serie1)
paro.hw$fitted
lines(paro.hw$fitted[,1],col="red")
predict(paro.hw,n.ahead=4)
plot(serie1,xlim=c(2003,2013))
lines(paro.hw$fitted[,1],col=2)
lines(predict(paro.hw,n.ahead=12),col=2)

#   Problema 9 del tema 1
#   _____________________

y=c(3490,2883,3939,4434,4454,4564,3600,4328,3826,2727,4032,2666)
y=ts(y, start=1983)
plot(y)
Pb9.simple=HoltWinters(y,alpha=0.2,beta=F,gamma=F,l.start=3490)
Pb9.simple$fitted
predict(Pb9.simple,n.ahead=1)
lines(Pb9.simple$fitted[,1],col=2)
lines(predict(Pb9.simple,n.ahead=12),col=2)
Pb9.simple$SSE

#   Problema 11 del tema 1, apartado b)
#   ___________________________________

y=c(50.179,50.650,51.061,52.572,54.727,55.947,56.776,57.558)
y=ts(y, start=1985)
plot(y)

Pb11.simple=HoltWinters(y,alpha=0.2,beta=0.1,gamma=F,l.start=50.650, b.start=0.471)
Pb11.simple$fitted

lines(Pb11.simple$fitted[,1],col=2)
lines(predict(Pb11.simple,n.ahead=12),col=2)
predict(Pb11.simple,n.ahead=4)

