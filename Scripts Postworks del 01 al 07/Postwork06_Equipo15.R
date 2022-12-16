"POSTWORK SESION 06: REGRESION LINEAL Y CLASIFICACION
Desarrollo
Requisitos
Tener instalado R y RStudio
Haber trabajado con el Prework y el Work
Desarrollo
Supongamos que nuestro trabajo consiste en aconsejar a un cliente sobre
como mejorar las ventas de un producto particular, y el conjunto de datos
con el que disponemos son datos de publicidad que consisten en las ventas
de aquel producto en 200 diferentes mercados, junto con presupuestos de
publicidad para el producto en cada uno de aquellos mercados para tres
medios de comunicación diferentes: TV, radio, y periódico. No es posible
para nuestro cliente incrementar directamente las ventas del producto. Por
otro lado, ellos pueden controlar el gasto en publicidad para cada uno de
los tres medios de comunicación. Por lo tanto, si determinamos que hay una
asociación entre publicidad y ventas, entonces podemos instruir a nuestro
cliente para que ajuste los presupuestos de publicidad, y así
indirectamente incrementar las ventas.

En otras palabras, nuestro objetivo
es desarrollar un modelo preciso que pueda ser usado para predecir las
ventas sobre la base de los tres presupuestos de medios de comunicación.
Ajuste modelos de regresión lineal múltiple a los datos advertisement.csv y elija el modelo más adecuado siguiendo los procedimientos vistos

Considera:

 Y: Sales (Ventas de un producto)
X1: TV (Presupuesto de publicidad en TV para el producto)
X2: Radio (Presupuesto de publicidad en Radio para el producto)
X3: Newspaper (Presupuesto de publicidad en Periódico para el producto)"

# datos por tipo de publicidad con su presupuesto, y las ventas de ese producto
adv <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-06/data/advertising.csv")


# Mejorar las ventas de un producto en particular
# podemos instruir a nuestro cliente para que ajuste los presupuestos de publicidad, y así
# indirectamente incrementar las ventas.

summary( adv )

hist( adv$TV)
hist( adv$Radio)
hist( adv$Newspaper)

str( adv )
summary( adv )



#1) Validar que la variable dependiente sea continua
#2) Existir correlación entre variables: Positiva, NO negativa
library( dplyr )
adv.select <- select (adv, Sales, TV, Radio, Newspaper)
round( cor( adv.select ), 4 )    #Matriz de correlaciones
      #Los números son positivos, por lo que indica una correlación
      # TV tiene la mayor correlacion y Newspaper la menor

#Las gráficas muestran la correlacion
pairs(~ Sales + TV + Radio + Newspaper,
      data = adv.select, gap = 0.4, cex.labels = 1.5)


attach(adv.select)
# Nuestra hipotesis es 
# Ho: beta_i   = 0       
# Ha: beta_i != 0

# utilizaremos el nivel de confianza de 95% y por tanto 5% de nivel de significancia


#Modelo 1
m1 <- lm(Sales ~ TV + Radio + Newspaper )
summary( m1 )

#Podemos ver que el coeficiente de la variable Newspaper no es significativo
#mediante el intercepto y el PValue (no es significativo para explicar el precio)

#Modelo 2: quitamos la variable Newspaper
# para evaluar TV vs Radio
m2 <- update( m1,  ~.-Newspaper )
summary(m2)

# El mejor modelo es m2, sube un poco el adjusted r squared



# MODELO DE REGRESION
#desarrollar un modelo preciso que pueda ser usado para predecir las
#ventas sobre la base de los tres presupuestos de medios de comunicación

StanRes2 <- rstandard(m2)

par(mfrow = c(2, 2))

# Vamos a calcular los residuos estandarizados
plot(TV, StanRes2, ylab = "Residuales Estandarizados")
plot(Radio, StanRes2, ylab = "Residuales Estandarizados")


qqnorm(StanRes2) 
qqline(StanRes2) 
# La gráfica tiene una inclinación de 45 grados, con la línea se observa mejor
# Distribuye como una normal

#  Ahora vamos a comprobar con la prueba de Shapiro : 2a comprobacion
shapiro.test( StanRes2 )
    #nuestro p-value es de 0.001365 
    # Se rechaza la Ho a nivel de confianza standard (de 90%, 95% y 99%)
    # concluimos que distribuye como la normal
    
# Para hacer la predicción vamos a:
#1) Hacer un modelo para TV
  
    min(adv.select$TV); max( adv.select$TV);      #  Minimo=0.7,  Maximo=296.4
    min(adv.select$Radio); max( adv.select$Radio);   # Minimo=0, Maximo=49.6
  #escenario 1: incrementar presupuesto de TV con respecto a Radio
  newData <- data.frame (
    TV = c( 50, 150, 250, 296),
    Radio = c ( 0, 10, 25,30 )
  )
  
  predict <- predict(m2, newdata = newData, interval = 'confidence')
  predict  
  
  plot(TV, Sales, xlab = "TV", 
       ylab = "Sales", pch = 16)
  points( newData$TV, predict[,1], xlab = "TV", 
         ylab = "Sales", pch = 16, col = "red")
  abline(lsfit(TV, Sales))
  
  
  
  # RESULTADO PARA TV
  # Sales esta entre 7.353328 y 23.963009
  
  #escenario 2: incrementar presupuesto de Radio
  newData <- data.frame (
    TV = c( 0.7, 80,  190, 270),
    Radio = c ( 65, 70, 75,80 )
  )
  
  
  predict <- predict(m2, newdata = newData, interval = 'confidence')
  predict  
  
  plot(TV, Sales, xlab = "TV", 
       ylab = "Sales", pch = 16)
  points( newData$TV, predict[,1], xlab = "TV", 
          ylab = "Sales", pch = 16, col = "red")
  abline(lsfit(TV, Sales))
  
  
  
  # RESULTADO PARA RADIO
  # Sales esta entre 11.63534 y 27.90606
  
  
  #CONCLUSION
  # Nos conviene meter más presupuesto a la RADIO porque 
  # las predicciones en ventas son mayores
  
