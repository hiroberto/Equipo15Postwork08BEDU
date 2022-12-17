 ## Programación y estadística con R
### Equipo 15: Postwork 08
Este es el repositorio del Postwork 08 del Modulo Programación y estadística con R.

**Encontrarás:**
1. Planteamiento del problema del caso
2. Análisis descriptivo de la información
3. Uso de probabilidades para entender el problema en México
4. Planteamiento de hipótesis estadísticas y conclusiones para entender el problema en México
5. Modelo de regresión para identificar los determinantes de la inseguridad alimentaria en México
6. Análisis 

A continuación se muestra el detalle de cada apartado:
### 1. Planteamiento del problema del caso

### 2. Análisis descriptivo de la información
## Descripción de datos

Se cuenta con 20280 observaciones con 10 variables descritas en la siguiente tabla:

![Tabla1_02](Imagenes/02_Img1.jpg?raw=true)

Para cada uno de los niveles socioeconómicos se tiene un aproximado del 20%, excepto para el nivel socioeconómico Bajo que corresponde a un 17% de las observaciones.

![Nivel_socioeconómico_Img2](Imagenes/02_Img2.jpg?raw=true)

La inseguridad alimentaria en los hogares observados está presente en poco más del 70%.

![Inseguridad_Alim_Img3](Imagenes/02_Img3.jpg?raw=true)

De todas de las observaciones el 68% corresponde a hogares ubicados en una Zona geográfica urbana.

![Zona_Geografica_Img4](Imagenes/02_Img4.jpg?raw=true)

El 80% de los hogares observados, no cuenta con un recurso financiero distinto al ingreso laboral.

![Recursos_Financieros_Img5](Imagenes/02_Img5.jpg?raw=true)

Para los gastos en alimentos saludables y no saludables, obtenemos las siguientes medidas de tendencia central y de dispersión:

![Tabla2_02](Imagenes/02_Img6.png?raw=true)

De las cuales observamos que para el gasto en alimentos saludables la media, mediana y moda son cercanas, teniendo una distribución normal de los datos sesgada a la izquierda, así como una dispersión baja respecto a la media. Mientras que, para el gasto en alimentos no saludables, tenemos una media y mediana con valores cercanos, con una distribución normal, no simétrica.

![Gasto_Als](Imagenes/02_Img7.jpg?raw=true)

![Gasto_Alns](Imagenes/02_Img8.jpg?raw=true)

Considerando las medidas de tendencia central y dispersión para otras variables tenemos que, para los hogares observados:

![Tabla3_02](Imagenes/02_Img9.jpg?raw=true)

* Más frecuentemente, los hogares están conformados por 4 personas, pero se registran hogares desde 1 hasta 19 personas y solo el 25% está conformado por más de 5 personas. Se tiene una distribución sesgada a la derecha. 
* La edad promedio del jefe de familia es de 46 años, sin embargo, la edad más frecuente es de 38 años, el 50% de los jefes de familia tiene una edad entre 36 y 57 años.
* Los años de educación del jefe de familia se observan desde 0 hasta 24 años de estudio, con un promedio de 10.9 años de educación, sin embargo, los datos se encuentran muy dispersos, con una distribución no normal, donde el 75% de los jefes de familia tienen igual o menos de 12 años de educación.

![Educacion](Imagenes/02_Img10.jpg?raw=true)

Nivel socioeconómico y gastos en alimentos

![NS_GastosAls](Imagenes/02_Img11.jpg?raw=true)

![NS_GastosAlns](Imagenes/02_Img12.jpg?raw=true)

## R
-  [Código R](https://github.com/luisalegria/Equipo15Postwork08BEDUPostwork8_02_ok.R)








### 3. Uso de probabilidades para entender el problema en México
Hemos decidido tomar la distribución binomial para entender los gastos económicos de la población entre el uso del dinero en alimentos saludables y no saludables.
Para esto tuvimos que encontrar para cada uno de los tipos de gastos (Saludables y No Saludables) las siguientes variables:
* Total de muestra
* Probabilidad
* Número de casos en los que si hubo gastos en el tipo de alimento

Por lo cual para ambas distribuciones tomamos un total de datos sin limpiar de 40,809. 

Pero a partir de ahí tomamos tamaños diferentes de muestras tanto para saludables (40, 022) como para NO saludables (23,305), descontando los datos N/A (Donde no hubo registro o es inexistente).
Las probabilidades fueron calculadas dividiendo el total de casos N/A entre el total de registros y a eso lo restamos a 1 para obtener ambas. Para Saludable fue 0.980715% y para NO Saludable fue 0.571075%.

A partir de ahí obtuvimos la media y la desviación estándar para Saludables y como para NO Saludables.
Una vez obtenidos los datos, graficamos la distribución binomial para los Saludables donde La concentración logarítmica de gasto sobre alimentos SI saludables nos dice que la gente tiene una probabilidad arriba .014% de 39,244 en el gasto de este tipo alimentos SI saludables.

![alt text](Imagenes/03_01_als.JPG?raw=true)

Y para terminar graficaremos también la distribución binomial para los NO Saludables donde la concentración logarítmica de gasto sobre alimentos NO saludables nos dice que la gente tiene una probabilidad arriba  .005% de 13,293 en el gasto de este tipo alimentos NO saludables.
![alt text](Imagenes/03_02_alns.JPG?raw=true)




### 4. Planteamiento de hipótesis estadísticas y conclusiones para entender el problema en México

#### Diferencia entre medias muéstrales.

*Calculamos la diferencia de medias para dos poblaciones:* 
1.  Nivel socioeconómico: medio, medio alta y alto, 
2. Nivel socioeconómico medio bajo y bajo 
3. considerando el logaritmo natural del gasto en alimentos saludables. Para esto propusimos la siguiente Hipótesis: 
### Ho: No hay diferencia entre las medias poblaciones en base al algoritmo natural de alimentos saludables.
### Ha: Si hay diferencia entre las medias poblacionales en base al algoritmo natural de alimentos saludables
```
 ns_Alto <- df.clean %>% filter(nse5f >=3) %>% pull(ln_als)
 ns_Bajo <- df.clean %>% filter(nse5f < 3) %>% pull(ln_als)
 Res1.ln_als <- mean(ns_Alto) - mean(ns_Bajo)
 Res1 = 0.4265841
```
 En base a este resultado se **rechaza la hipótesis nula**, por lo tanto no se rechaza la hipótesis alternativa.

*Calculamos la diferencia de medias de dos poblaciones (las mismas del punto anterior), pero esta vez en base al logaritmo natural del gasto en alimentos NO saludables.*

### Ho: No hay diferencia entre las medias poblacionales considerando el logaritmo de alimentos No saludables = 0
### Ha: Si hay diferencia entre las medias poblacionales el logaritmo de alimentos No saludables != 0 

```
 Nns_Alto <- df.clean %>% filter(nse5f >=3) %>% pull(ln_alns)
 Nns_BAjo <- df.clean %>% filter(nse5f >=3) %>% pull(ln_alns)
 Res2.ln_alns <- mean(Nns_Alto) - mean(Nns_BAjo)
```
El Resultado es **Cero**.  En base a este resultado **no se rechaza la hipótesis nula propuesta**.



### Se verifica la varianza con var.test, para después calcular t.tes. en base a las hipótesis.
### Ho : Nivel Alto >= Nivel Bajo   ** El Gasto en alimentos saludables es mayor en la clase Alta que en la clase baja
### Ha : Nivel Alto < Nivel Bajo  ** El Gasto en alimentos saludables es menor en la clase Alta que en la baja.

```
 var.test(df.clean[df.clean$nse5f >= 3, "ln_als"],
 df.clean[df.clean$nse5f <= 2, "ln_als"],
 ratio = 1, alternative = "two.sided",
 conf.level = 0.95)
```

> F test to compare two variances
> data:  df.clean[df.clean$nse5f >= 3, "ln_als"] and df.clean[df.clean$nse5f <= 2, "ln_als"]
> F = 0.71634, num df = 12799, denom df = 7479, p-value < 2.2e-16
> alternative hypothesis: true ratio of variances is not equal to 1
> 95 percent confidence interval:
>  0.6879413 0.7457528
> sample estimates:
> ratio of variances 
>          0.7163415

```
t.test( x = df.clean[df.clean$nse5f >= 3, "ln_als"],
        y = df.clean[df.clean$nse5f <= 2, "ln_als"],
        alternative = "greater", mu=0, var.equal = FALSE  ,conf.level = 0.95)
```
> data:  df.clean[df.clean$nse5f >= 3, "ln_als"] and df.clean[df.clean$nse5f <= 2, "ln_als"]
> t = 42.713, df = 13653, p-value < 2.2e-16
> alternative hypothesis: true difference in means is greater than 0
> 95 percent confidence interval:
>  0.4101555       Inf
> sample estimates:
> mean of x mean of y 
>  6.349331  5.922747 

Para esta hipótesis p-value < 2.2e-16 que es menor a 0.05 por lo tanto se ** RECHAZA la hipótesis Nula**.


### Ho: Nivel Alto >= Nivel Bajo   ** El Gasto en alimentos NO saludables es mayor o igual en la clase Alta que en la clase baja
### Ha: Nivel Alto < Nivel Bajo  ** El Gasto en alimentos NO saludables es menor en la clase Alta que en la baja.
```
var.test(df.clean[df.clean$nse5f >= 3, "ln_alns"],
         df.clean[df.clean$nse5f <= 2, "ln_alns"],
         ratio = 1, alternative = "two.sided",
         conf.level = 0.95)
```

> F test to compare two variances
> data:  df.clean[df.clean$nse5f >= 3, "ln_alns"] and df.clean[df.clean$nse5f <= 2, "ln_alns"]
> F = 1.2108, num df = 12799, denom df = 7479, p-value < 2.2e-16
> alternative hypothesis: true ratio of variances is not equal to 1
> 95 percent confidence interval:
>  1.162810 1.260527
> sample estimates:
> ratio of variances 
>           1.210814 

```
t.test( x = df.clean[df.clean$nse5f >= 3, "ln_alns"],
        y = df.clean[df.clean$nse5f <= 2, "ln_alns"],
        alternative = "greater", mu=0, var.equal = FALSE  ,conf.level = 0.95)
```

> data:  df.clean[df.clean$nse5f >= 3, "ln_alns"] and df.clean[df.clean$nse5f <= 2, "ln_alns"]
> t = 34.606, df = 16871, p-value < 2.2e-16
> alternative hypothesis: true difference in means is greater than 0
> 95 percent confidence interval:
>  0.4741434       Inf
> sample estimates:
> mean of x mean of y 
>  4.302453  3.804648 

Para esta hipótesis obtuvimos un p-value < 2.2e-16 menor al 0.05 por lo cual la hipótesis nula se ** RECHAZA**.





### 5. Modelo de regresión para identificar los determinantes de la inseguridad alimentaria en México



### 6. Análisis
De acuerdo a la información analizada en los puntos anteriores, y retomando esto:
> La mayoría de las personas afirman que los hogares con menor nivel socioeconómico tienden a gastar más en productos no saludables que las personas con mayores niveles socioeconómicos y que esto, entre otros determinantes, lleva a que un hogar presente cierta inseguridad alimentaria.

* Llegamos a lo siguiente en este postwork:
  * Vimos con algunos estadísticos sobre el gasto en alimentos saludables VS no saludables, que la muestra gasta más en los primeros.
  * Además con la regresión logística, vimos que esta afirmación no aplica a la muestra, ya que los factores que más determinan la inseguridad son el número de personas en el hogar y la edad del jefe de familia

* Sobre el trabajo en equipo y la aplicación de los conocimientos adquiridos vimos:
  * Aunque comprendemos de manera general conceptos y realizamos ejercicios en las sesiones, llevar a cabo este postwork ocupaba un entendimiento mayor.
    * Tanto de las herramientas como de los conceptos, así como unificar ambas cosas
    * Hemos intentado trabajar con lo que conocíamos y llegamos a los resultados mostrados en el script y la presentación
  * Atacamos la situación así:
    * Quien tenía un mejor entendimiento tomaba el rol de hacer el punto del postwork08 o de los otros postworks
    * Explicaba la situación y de cierto modo aprendíamos, viendo la situación desde otra perspectiva
    * De manera individual, revisamos los videos de las sesiones para una mejor comprensión
    * Nos apoyamos del material actual así como de información de otros sitios
