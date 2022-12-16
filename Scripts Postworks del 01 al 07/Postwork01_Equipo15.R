# Postwork Sesión 1.
"Equipo 15"
#### Objetivo

"El Postwork tiene como objetivo que practiques los comandos básicos aprendidos 
durante la sesión, de tal modo que sirvan para reafirmar el conocimiento. Recuerda 
que la programación es como un deporte en el que se debe practicar, habrá caídas, 
pero lo importante es levantarse y seguir adelante. Éxito"

#### Requisitos
#- Concluir los retos
#- Haber estudiado los ejemplos durante la sesión

#### Desarrollo

"El siguiente postwork, te servirá para ir desarrollando habilidades como si se 
tratara de un proyecto que evidencie el progreso del aprendizaje durante el módulo, 
sesión a sesión se irá desarrollando.
A continuación aparecen una serie de objetivos que deberás cumplir, es un ejemplo 
real de aplicación y tiene que ver con datos referentes a equipos de la liga española 
de fútbol (recuerda que los datos provienen siempre de diversas naturalezas), en 
este caso se cuenta con muchos datos que se pueden aprovechar, explotarlos y generar 
análisis interesantes que se pueden aplicar a otras áreas.
Siendo así damos paso a las instrucciones:" 

#1. Del siguiente enlace, descarga los datos de soccer de la temporada 2019/2020 de
#   la primera división de la liga española: https://www.football-data.co.uk/spainm.php

#2. Importa los datos a R como un Dataframe. NOTA: No olvides cambiar tu dirección de
#   trabajo a la ruta donde descargaste tu archivo

install.packages("data.table")
library(data.table)

setwd("~/BEDU_Estadística_R_Python") # ***** Cambiar a Dirección de trabajo particular ******
sp1 <- read.csv("SP1.csv")
View(sp1)

#3. Del dataframe que resulta de importar los datos a `R`, extrae las columnas que contienen
#   los números de goles anotados por los equipos que jugaron en casa (FTHG) y los goles
#   anotados por los equipos que jugaron como visitante (FTAG); guárdalos en vectores separados

(FTHG <- c(sp1$FTHG))
(FTAG <- sp1$FTAG)     # Otra forma de hacer lo mismo



#4. Consulta cómo funciona la función `table` en `R`. Para ello, puedes ingresar los comando
#   `help("table")` o `?table` para leer la documentación.

help("table")      # Para Crear una tabla


(marcadores <- table(FTHG, FTAG, dnn = list("FTHG", "FTHG")))
class(marcadores)
(x <- (dim(marcadores)))   # Para visualizar dimensiones de tabla


#5. Responde a las siguientes preguntas:
#  a) ¿Cuántos goles tuvo el partido con mayor empate?

marcadores
(i = min(dim(marcadores)))
while( i>0 & marcadores[i,i]==0) {
#  print(marcadores[i,i])  Imprime la cantidad de empates
  i <- i-1  
}
(i <- i-1)
(maxGoles = i*2)


(mayor <- paste("Mayor número de goles en empate:", (maxGoles), "... Marcador:", i,"-",i))



#  b) ¿En cuántos partidos ambos equipos empataron 0 a 0?

partidos.empate.cero <- paste("No. de partidos con empate a cero (0-0):", marcadores[1,1])
print(partidos.empate.cero[1])


#  c) ¿En cuántos partidos el equipo local (HG) tuvo la mayor goleada sin dejar que
#      el equipo visitante (AG) metiera un solo gol?


(i <- max(dim(marcadores)))
(result <-marcadores [i,1])
while (result == 0) {
  i <- 1 -1
  result <-marcadores [i,1] 
}

(resultado.final <- paste("Número de partidos con máxima victoria Local con blanqueada:", 
                          result, "... Marcador:", i-1, "-", 0))


#  __Notas para los datos de soccer:__ https://www.football-data.co.uk/notes.txt
