
# Ejemplo extra: Problema 8 del tema 1
# ____________________________________

datos2=c(2456,2683,2515,2591,2455,2598,2480,2583,2460,2652,2474,2580)
serie2=ts(data=datos2,frequency=4,start=c(2011,3))
serie2

------------------------------------------------------------------
# Criterio 1 para decidir modelo
plot(serie2)

# Criterio 2 para decidir modelo
x <- c(rep(0,3))
y <- c(rep(0,3))
for(i in 1:3){
  j=(i-1)*4+1
  k=(i-1)*4+4
  x[i] <- mean(datos2[j:k])
  y[i] <- sd(datos2[j:k])
}
plot(x,y)

# Criterio 3 para decidir el modelo

serie_est_post=serie2[5:12]
serie_est_ant=serie2[1:8]
dif_est=serie_est_post-serie_est_ant
coc_est=serie_est_post/serie_est_ant
(CVd=(sd(dif_est)/abs(mean(dif_est))))
(CVc=(sd(coc_est)/abs(mean(coc_est))))


plot(serie2,type="l",ylim=c(min(serie2),max(serie2)))
componentes=decompose(serie2,type=c("multiplicative"))
mmcent=componentes$trend
mmcent
varest1=componentes$figure # Índice para cada estación
varest1
varest2=componentes$seasonal
#Índice para cada estación repetidos para cada año
varest2

serie_des=serie2/varest2 #serie desestacionalizada
serie_des

Time=time(serie_des)
#Proporciona los tiempos en los que ha sido medida la serie
Time


rTendT=lm(serie_des~Time) #recta de regresión
rTendT
coef=as.vector(rTendT$coefficients)
#extraigo los coeficientes
coef
a=coef[1]
b=coef[2]
a
b

frec=3
t=2014.5
(tsecular=a+b*t) #calculo la tendencia secular
(prediccion=tsecular*varest1[frec]) #predicción (modelo multiplicativo)

(Time = 1:12)
rTendT=lm(serie_des~Time) #recta de regresión
rTendT
(srTendT = summary(rTendT))
(Rcuadrado = srTendT$r.squared)
coef=as.vector(rTendT$coefficients) #se extraen los coeficientes
coef
a=coef[1]
b=coef[2]
a
b
(varest1bien = varest1[c(3,4,1,2)])
frec=c(1:4)
t=13:16
(tsecular=a+b*t) #cálculo la tendencia secular
(prediccion=tsecular*varest1bien[frec]) #predicción
---------------------------------------------------------------------------

