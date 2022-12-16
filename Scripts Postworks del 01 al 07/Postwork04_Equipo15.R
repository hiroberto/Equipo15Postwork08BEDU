"Utilizando la variable total_intl_charge de la base de datos telecom_service.csv 
de la sesión 3, realiza un análisis probabilístico. 
Para ello, debes determinar la función de distribución de probabilidad que más
se acerque el comportamiento de los datos.
Hint: Puedes apoyarte de medidas descriptivas o técnicas de visualización.

Una vez que hayas seleccionado el modelo, realiza lo siguiente:
# Se cargan las librerias para trabajar


# Se cargan las librerias para trabajar

library(DescTools)
library(ggplot2)

# Se cargan los datos desde el archivo 
df.p4 <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-03/Data/telecom_service.csv")

# Revisamos las medidas de tendencia central

Mode(df.p4$total_intl_charge)[1]   # Moda

mean(df.p4$total_intl_charge)     # Promedio

median(df.p4$total_intl_charge)   # Mediana

# El resultado de estos valores es casi el mismo.

# determinar la función de distribución de probabilidad  
# Mostramos histograma de cargos internacionales.

# ******** No se que valor y de donde toma "T"" * *******

hist(df.p4$total_intl_charge,prob = T, main = "Histograma Total de Cargos internacionales")

###
media.var <- mean(df.p4$total_intl_charge)    # Guardamos la media en una variable

ds <- sd(df.p4$total_intl_charge)   #Calculamos la desviacion estandar

###1.Grafica la distribución teórica de la variable aleatoria total_intl_charge
## FUNCION DE LA DISTRIBUCION NORMAL.
curve(dnorm(x,mean = media.var, sd = ds),from = 0, to = 5,
      col = 'blue', main= "Densidad de Probabilidad Normal",
      ylab = "f(x)",xlab = "X")

# LA grafica se muestra BIEN con la forma esperada.
##2.¿Cuál es la probabilidad de que el total de cargos internacionales sea menor a 1.85 usd?
## Funcion de la densidad de la distribucion

 pnorm(1.85, mean = media.var, sd = ds, lower.tail = T)

 #3.¿Cuál es la probabilidad de que el total de cargos internacionales sea mayor a 3 usd? 
 
 pnorm(3, mean = media.var, sd = ds, lower.tail = F)
 
 ##4.¿Cuál es la probabilidad de que el total de cargos internacionales esté entre 2.35usd y 4.85 usd?
 ## PAra este punto se resta la funcion de probabilidad de 4.85 a la de 2.35 para obtener el resultado final
 
  pnorm(4.85, mean = media.var, sd = ds) - pnorm(2.35, mean = media.var, sd=ds)
 
##5.Con una probabilidad de 0.48, ¿cuál es el total de cargos internacionales más alto que podría esperar?

    qnorm(p=0.48, mean = media.var, sd= ds)
    
##6.¿Cuáles son los valores del total de cargos internacionales que dejan exactamente al centro el 80% de probabilidad?"  
    
  ## No entendi como calculan para el 80 % con estos valores.
    
    qnorm(p=0.10, mean = media.var,sd = ds)
    qnorm(p=0.90, mean = media.var, sd = ds)
    