# Postwork Sesión 2.

#### Objetivo

"- Conocer algunas de las bases de datos disponibles en `R`
- Observar algunas características y manipular los DataFrames con `dplyr`
- Realizar visualizaciones con `ggplot`
#### Requisitos

1. Tener instalado R y RStudio
2. Haber realizado el prework y estudiado los ejemplos de la sesión."

#### Desarrollo

"1) Inspecciona el DataSet iris disponible directamente en la librería de ggplot. 
Identifica las variables que contiene y su tipo, asegúrate de que no hayan datos 
faltantes y que los datos se encuentran listos para usarse."


"iris data set gives the measurements in centimeters of the variables sepal length 
    and width and petal length and width, respectively, for 50 flowers from each of 3 
    species of iris. The species are Iris setosa, versicolor, and virginica."

iris.var <- iris
dim( iris.var )
str ( iris.var )
class( iris.var )
View( iris.var)

#verificar si hay vacios con is.na = funcion que trae TRUE si es NA - ‘Not Available’
sum( is.na(iris.var)  )  # el resultado es cero, por lo que indica que tenemos todos los datos






"2) Crea una gráfica de puntos que contenga `Sepal.Lenght` en el eje horizontal, 
`Sepal.Width` en el eje vertical, que identifique `Species` por color y que el tamaño 
de la figura está representado por `Petal.Width`. 
Asegúrate de que la geometría contenga `shape = 10` y `alpha = 0.5`."
#para cargar una biblioteca, se usa el comando library
library(ggplot2)


ggplot(iris.var, aes(x = Sepal.Length, y = Sepal.Width, color = Species,
                 size = Petal.Width)) +
  geom_point(shape = 10, alpha = 1.0) +
  labs( title = "Muestra de 50 flores de Iris de 3 especies",
        x = "Longitud del sépalo (cm)",
        y = "Anchura del sépalo (cm)") + 
        theme_classic()



View( iris.var)

"3) Crea una tabla llamada `iris_mean` que contenga el promedio de todas 
las variables agrupadas por `Species`."
library(dplyr)
iris_mean <-  iris.var %>%
    select(Species,Petal.Length, Petal.Width, Sepal.Length, Sepal.Width) %>%
    group_by( Species) %>%
    summarize( Petal.Length = mean(Petal.Length),
             Petal.Width = mean(Petal.Width), 
             Sepal.Length = mean(Sepal.Length), 
             Sepal.Width =mean(Sepal.Width) )
str(iris_mean)
class(iris_mean)
View( iris_mean)





"4) Con esta tabla, agrega a tu gráfica anterior otra geometría de puntos para agregar 
los promedios en la visualización. Asegúrate que el primer argumento de la geometría 
sea el nombre de tu tabla y que los parámetros sean `shape = 23`, `size = 4`, 
`fill = 'black'` y `stroke = 2`. También agrega etiquetas, temas y los cambios 
necesarios para mejorar tu visualización."

ggplot(iris.var, aes(x = Sepal.Length, y = Sepal.Width, color = Species,
                     size = Petal.Width)) +
  geom_point(shape = 10, alpha = 1.0) +
  geom_point( data = iris_mean, shape = 23, size=4, fill="black", stroke=2) +
  labs( title = "Muestra de 50 flores de Iris de 3 especies",
        x = "Longitud del sépalo (cm)",
        y = "Anchura del sépalo (cm)",
        subtitle = "Se incluyen promedios representados con rombos negros",
        ) + 
  theme_classic()

