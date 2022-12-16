"Postwork05 TEOREMA DEL LÍMITE CENTRAL E INFERENCIA ESTADÍSTICA
El data frame iris contiene información recolectada por Anderson sobre 
50 flores de 3 especies distintas (setosa, versicolor y virginca), 
incluyendo medidas en centímetros del largo y ancho del sépalo así como 
de los pétalos.

Utilizando pruebas de inferencia estadística, concluye si existe evidencia
suficiente para concluir que los datos recolectados por Anderson están en
línea con los nuevos estudios.

Utiliza 99% de confianza para todas las pruebas, en cada caso realiza el
planteamiento de hipótesis adecuado y concluye"

library(ggplot2)

View(iris)
str(iris)
names(iris)
dim(iris)

df <- na.omit(iris)  # Se validan los datos
dim(df)              # No hay registros con datos faltantes (valores NA)

"Estudios recientes sobre las mismas especies muestran que:
  
I. En promedio, el largo del sépalo de la especie setosa (Sepal.Length)
es igual a 5.7 cm"

# Planteamiento de hipótesis:
# Ho: prom_sepal_length_setosa == 5.7
# Ha: prom_sepal_length_setosa =! 5.7

t.test(iris[iris$Species == 'setosa', "Sepal.Length"], 
       alternative = 'two.sided', m = 5.7, conf.level = 0.99)

# Con un nivel de confianza del 99% y un p-value < 2.2e-16
# existe evidencia estadística para rechazar Ho, es decir, 
# en promedio, el largo del sépalo de la especie setosa (Sepal.Length)
# es diferente a 5.7 cm 


"II. En promedio, el ancho del pétalo de la especie virginica (Petal.Width) 
es menor a 2.1 cm"

# Planteamiento de hipótesis:
# Ho: prom_petal_width_virginica >= 2.1
# Ha: prom_petal_width_virginica  < 2.1

t.test(iris[iris$Species == 'virginica', "Petal.Width"], 
       alternative = 'less', mu = 2.1, conf.level = 0.99)

# Con un nivel de confianza del 99% y un p-value = 0.03132
# no existe evidencia estadística para rechazar Ho, es decir, 
# en promedio, el ancho del pétalo de la especie virginica (Petal.Width)
# es mayor o igual a 2.1 cm 

"III. En promedio, el largo del pétalo de la especie virgínica es 1.1 cm 
más grande que el promedio del largo del pétalo de la especie versicolor."

# Planteamiento de hipótesis:
# Ho: prom_petal_width_virginica == 1.1 + prom_petal_width_versicolor
# Ha: prom_petal_width_virginica != 1.1 + prom_petal_width_versicolor"

# Primero se Identifica si las varianzas de cada grupo, son iguales o 
# diferentes en la población para ver cómo es su distribución.
# Ho: Varianzas son iguales (razon = 1)   ** No se rechaza (p-value= 0.2637) ***
# Ha: Varianzas son diferentes (razon != 1)


var.test(iris[iris$Species == "virginica", "Petal.Length"], 
         iris[iris$Species == "versicolor", "Petal.Length"], 
         ratio = 1, alternative = "two.sided", conf.level = 0.99)

t.test(iris[iris$Species == "virginica", "Petal.Length"],
       iris[iris$Species == "versicolor", "Petal.Length"],
       alternative = "greater", mu = 1.1, var.equal = TRUE, conf.level = 0.99)


# Con un nivel de confianza del 99% y un p-value = 0.03202
# no existe evidencia estadística para rechazar Ho, es decir, 
# en promedio, el largo del pétalo de la especie virginica es 1.1 cm más grande
# que el promedio dedel largo del pétalo de la especie versicolor.


"IV. En promedio, no existe diferencia en el ancho del sépalo entre las 
3 especies."

# Planteamiento de hipótesis:
# Ho: prom_sepal_width_virginica == prom_sepal_width_versicolor == prom_sepal_width_setosa
# Ha: prom_sepal_width_virginica != prom_sepal_width_versicolor != prom_sepal_width_setosa

# Se grafica para visualizar los promedios de cada especie. Se ven diferentes.

boxplot(Sepal.Width ~ Species, data = iris, 
        main = "ANCHO DEL SÉPALO. Se observa: Promedios no iguales", col="blue")

# Se calculan las medias del ancho del sépalo

iris_mean_sepal_width <- iris %>% 
       mutate(Species = factor(Species, labels = 
          c("PROM_setosa", "PROM_versicolor","PROM_virginica"))) %>%
       group_by(Species) %>%
       summarize(Sepal.Width = mean(Sepal.Width)) 
iris_mean_sepal_width

# Se obtiene el siguiente resultado en el que son diferentes los promedios.
# PROMEDIO.setosa_______ 3.43
# PROMEDIO.versicolor___ 2.77
# PROMEDIO.virginica____ 2.97

# También se realiza un análisis del modelo de varianzas

anova <- aov(Sepal.Width ~ Species, data = iris)

summary(anova)

# Con la Tabla de descomposición de la varianza y un p-value < 2e-16
# existe evidencia estadística para rechazar Ho, es decir, 
# en promedio, si existe diferencia en el ancho del sépalo entre las 
# 3 especies y coincide con lo visualizado y calculado.