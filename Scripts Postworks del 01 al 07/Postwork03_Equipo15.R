# Postwork Sesión 3

#### Objetivo

#- Realizar un análisis descriptivo de las variables de un dataframe

#### Requisitos

#1. R, RStudio
#2. Haber realizado el prework y seguir el curso de los ejemplos de la sesión
#3. Curiosidad por investigar nuevos tópicos y funciones de R
###############################################################################
library(dplyr)
install.packages("dplyr")
library(tibble)
install.packages("tibble")
library(DescTools)
library(ggplot2)
library(moments)
install.packages("modeest")
library(modeest)

#### Desarrollo

"Utilizando el dataframe `boxp.csv` realiza el siguiente análisis descriptivo. No olvides excluir los missing values y transformar las variables a su
tipo y escala correspondiente."
dfp <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-03/Data/boxp.csv")
str(dfp)
summary(dfp)
view(dfp)
dfp


dfp.clean <- dfp[complete.cases(dfp), ] 
dim(dfp.clean)
dfp <- (dfp.clean) # quedaron solo 591 datos completos de 615. Se eliminaron 24

dfp$Categoria <- factor(dfp$Categoria)
dfp$Grupo <- factor(dfp$Grupo, labels = c("Grupo 1","Grupo 2"))
#1) Calcula e interpreta las medidas de tendencia central de la variable `Mediciones`


mean (dfp$Mediciones)
mean(dfp$Mediciones, trim = 0.25)
median(dfp$Mediciones)
mode(dfp$Mediciones)
Mode(dfp$Mediciones)[1]
kdfp = ceiling(sqrt(length(dfp$Mediciones))) # 25 brackets para la medicion del histograma
acdfp = (max(dfp$Mediciones)-min(dfp$Mediciones))/kdfp # ancho del backet debe ser de 11.512


#2) Con base en tu resultado anterior, ¿qué se puede concluir respecto al sesgo de `Mediciones`?
binsdfp <- seq(min(dfp$Mediciones),max(dfp$Mediciones), by = acdfp)
binsdfp
Mediciones.clases <- cut(dfp$Mediciones, breaks = binsdfp, include.lowest=TRUE, dig.lab = 5)
Mediciones.clases
dist.freq.dfp <- table(Mediciones.clases)
dist.freq.dfp "se visualiza cunatos valores caen en cada medicion, donde la clase mas alta con valores 
interiores es la(14.312,25.824] con 98"

my_hist.dfp <- hist(dfp$Mediciones, breaks =binsdfp, main ="Histograma Mediciones") "se confirma que el
valor mayor es de 92 en el 2do bracket"
my_hist.dfp # Se viusaliza que tiene un Sesgo a la derecha

my_hist.dfp$counts <- cumsum(my_hist.dfp$counts)
my_hist.dfp$counts # realiza la acumulación de valores hasta llegar a los 591
plot(my_hist.dfp, main = "Histograma acumulado Mediciones", xlab = "Medidas") "Se realiza la acumulación en un
histograma"

#3) Calcula e interpreta la desviación estándar y los cuartiles de la distribución de `Mediciones`

var(dfp$Mediciones)
sd(dfp$Mediciones)
IQR(dfp$Mediciones)
cuartiles.dfp <- quantile(dfp$Mediciones, probs = c(0.25,.5,.75,1))
cuartiles.dfp # las mediciones se concentran en el último cuartil entre 75% y 100%


"4) Con ggplot, realiza un histograma separando la distribución de `Mediciones` por `Categoría`
¿Consideras que sólo una categoría está generando el sesgo?" 



ggplot(dfp, aes(Mediciones)) +
  geom_histogram(by = binsdfp) + 
  labs(title = "Histograma de Mediciones", 
       x = "Mediciones",
       y = "Frequency") + 
  theme_classic()
#SI, en la primer clase de 4 brackets  o en la 3era de 26 clases con binsfdp

ggplot(dfp, aes(Mediciones)) +
  geom_histogram(bins = 4) + 
  labs(title = "Histograma de Mediciones", 
       x = "Mediciones",
       y = "Frequency") + 
  theme_classic()
#SI, en la primer clase de 4 brackets  o en la 3era de 26 clases con binsfdp

ggplot(dfp, aes(Mediciones)) +
  geom_histogram(aes(y = cumsum(..count..)), bins = 4) + 
  labs(title = "Histograma acumulado de Mediciones", 
       x = "Mediciones",
       y = "Frequency") + 
  theme_classic()


"5) Con ggplot, realiza un boxplot separando la distribución de `Mediciones` por `Categoría` 
y por `Grupo` dentro de cada categoría. ¿Consideras que hay diferencias entre categorías? ¿Los grupos al interior de cada categoría 
podrían estar generando el sesgo?"


dfpmean <- dfp %>% 
  group_by(dfp$Categoria) %>%
  summarize(dfp$Mediciones)
            
dfpmean

ggplot(dfpmean, aes(x = dfp$Mediciones, y = dfp$Categoria)) +
  geom_point(shape = 23, alpha = 0.5, size = 4, fill = 'black', stroke = 2) +
  labs( title = "Meidiciones promedio",
        x = "Promedio longitud mediciones",
        y = "Frecuencia de ocurrencia") + 
  theme_classic()
