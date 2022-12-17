"POSTWORK 8
Desarrollo
Un centro de salud nutricional está interesado en analizar estadísticamente
y probabilísticamente los patrones de gasto en alimentos saludables y
no saludables en los hogares mexicanos con base en su nivel socioeconómico,
en si el hogar tiene recursos financieros extras al ingreso y en si 
presenta o no inseguridad alimentaria. 
Además, está interesado en un modelo que le permita identificar los 
determinantes socioeconómicos de la inseguridad alimentaria.

La base de datos es un extracto de la Encuesta Nacional de Salud y Nutrición
(2012) levantada por el Instituto Nacional de Salud Pública en México. 
La mayoría de las personas afirman que los hogares con menor nivel socioeconómico
tienden a gastar más en productos no saludables que las personas con mayores
niveles socioeconómicos y que esto, entre otros determinantes, lleva a que
un hogar presente cierta inseguridad alimentaria.

La base de datos contiene las siguientes variables:"

# nse5f (Nivel socioeconómico del hogar):    
#       1 "Bajo", 2 "Medio bajo", 3 "Medio", 4 "Medio alto", 5 "Alto"
#            CUALITATIVA ORDINAL    
# area  (Zona geográfica):                   
#       0 "Zona urbana", 1 "Zona rural"
#            CUALITATIVA NOMINAL
# numpeho (Número de persona en el hogar)    
#       (1 - 19)
#            CUANTITATIVA DISCRETA
# refin (Recursos financieros distintos al ingreso laboral): 
#       0 "no", 1 "sí"
#            CUALITATIVA NOMINAL
# edadjef (Edad del jefe/a de familia)
#       (18 - 111)
#            CUANTITATIVA DISCRETA 
# sexoje (Sexo del jefe/a de familia):
#       0 "Hombre", 1 "Mujer"
#            CUALITATIVA NOMINAL
# añosedu (Años de educación del jefe de familia)  
#       (0 - 24)
#            CUANTITATIVA DISCRETA 
# ln_als (Logaritmo natural del gasto en alimentos saludables)
#       (0.6931 - 8.9699)
#            CUANTITATIVA CONTINUA
# ln_alns (Logaritmo natural del gasto en alimentos no saludables)  
#       (0 - 8.403)        
#             CUANTITATIVA CONTINUA
# IA (Inseguridad alimentaria en el hogar): 
#        0 "No presenta IA", 1 "Presenta IA"
#            CUALITATIVA NOMINAL


# LIBRERIAS A INSTALAR
# install.packages(dplyr)
# install.packages(DescTools)
# install.packages(ggplot2)
# install.packages(moments)
library(dplyr)
library(DescTools)
library(ggplot2)
library(moments)


"1) Plantea el problema del caso"

"analizar estadísticamente y probabilísticamente los 
patrones de gasto en alimentos saludables y no saludables en los hogares mexicanos
con base en su nivel socioeconómico,

en si el hogar tiene recursos financieros extras al ingreso y 

en si presenta o no inseguridad alimentaria. 

Modelo que le permita identificar los determinantes socioeconómicos de la inseguridad alimentaria."


"La mayoría de las personas afirman que los hogares con menor nivel socioeconómico
tienden a gastar más en productos no saludables que las personas con mayores
niveles socioeconómicos y que esto, entre otros determinantes, lleva a que
un hogar presente cierta inseguridad alimentaria."



"2) Realiza un análisis descriptivo de la información"

df <- read.csv("/Users/lunna/Documents/GitHub/Programacion-R-Santander-2022/Sesion-08/Postwork/inseguridad_alimentaria_bedu.csv")
str(df)
summary(df)
class(df)
head(df)

df$nse5f <- factor( df$nse5f, labels = c("Bajo","Medio bajo", "Medio","Medio alto", "Alto"), ordered = TRUE )
df$area <- factor( df$area, labels = c("Zona urbana", "Zona rural") )
df$refin <- factor( df$refin, labels = c("No", "Si") )
df$sexojef <- factor( df$sexojef, labels = c("Hombre", "Mujer") )
df$IA <- factor( df$IA, labels = c("No presenta IA","Presenta IA") )

summary(df)
View(df)

df.clean <- df[complete.cases(df),]
# dim(df.clean)

# df.select <- select(df.clean, nse5f, refin, ln_als, ln_alns, IA)

## Rangos de valores para variables cuantitativas
(min(df.clean$numpeho)); (max(df.clean$numpeho))
(min(df.clean$edadjef)); (max(df.clean$edadjef))
(min(df.clean$añosedu)); (max(df.clean$añosedu)) 

## VARIABLES CUALITATIVAS
## Nivel Socioeconómico
transform(table(df.clean$nse5f), FreqRelativa=prop.table(Freq))       # Obtenemos frecuencias y se grafica
ggplot(df.clean, aes(x = nse5f, fill=nse5f, )) + geom_bar() +
  labs(title = "Nivel socioeconómico", x = "Nivel", y = "Numero de hogares")

## Area
transform(table(df.clean$area), FreqRelativa=prop.table(Freq))
ggplot(df.clean, aes(x = area, fill=area)) + geom_bar() + labs(title = "Zona geográfica", x = "Zona", y = "Numero de hogares")

## Ingresos extra
transform(table(df.clean$refin), FreqRelativa=prop.table(Freq))
ggplot(df.clean, aes(x = refin, fill=refin)) + geom_bar() + labs(title = "Recursos financieros", x = "Ingreso", y = "Numero de hogares")

## Presenta Inseguridad Alimentaria
transform(table(df.clean$IA), FreqRelativa=prop.table(Freq))
(g <- ggplot(df.clean, aes(x = IA, fill=IA)) + geom_bar() + labs(title = "Inseguridad Alimentaria", x = "IA"))
# ggsave("IA.jpg", plot=g)


## VARIABLES CUANTITATIVAS 

## Logarítmo natural del gasto en alimentos saludables
k = ceiling(1 + 3.3*log10(length(df.clean$ln_als)))          
ac = (max(df.clean$ln_als)-min(df.clean$ln_als))/k
bins <- seq(min(df.clean$ln_als), max(df.clean$ln_als), by = ac)    
ln_als.clases <- cut(df.clean$ln_als, breaks = bins, include.lowest=TRUE, dig.lab = 8)
transform(table(ln_als.clases),  rel.freq=prop.table(Freq),  cum.freq=cumsum(prop.table(Freq)))
{
  hist_als <- hist( df.clean$ln_als, breaks = bins)
  plot(hist_als, main = "Gasto en Alimentos Saludables", col="blue" ,
       xlab = "Logaritmo Gasto en Alimentos Saludables")  
}
# Medidas de tendencia central y de dispersión
mean(df.clean$ln_als); median(df.clean$ln_als); Mode(df.clean$ln_als)[1]
var(df.clean$ln_als); sd(df.clean$ln_als)
IQR(df.clean$ln_als) #Dispersión alrededor a la mediana
# (iqr = quantile(df.clean$ln_als, probs = 0.75) - quantile(df.clean$ln_als, probs = 0.25))


## Logarítmo natural del gasto en alimentos NO saludables
k = ceiling(1 + 3.3*log10(length(df.clean$ln_alns)))          
ac = (max(df.clean$ln_alns)-min(df.clean$ln_alns))/k           
bins <- seq(min(df.clean$ln_alns), max(df.clean$ln_alns), by = ac)    
ln_alns.clases <- cut(df.clean$ln_alns, breaks = bins, include.lowest=TRUE, dig.lab = 8)
transform(table(ln_alns.clases),  rel.freq=prop.table(Freq),  cum.freq=cumsum(prop.table(Freq)))
hist_alns <- hist( df.clean$ln_alns, breaks = bins)
plot(hist_alns, main = "Gasto en Alimentos NO Saludables", col="green" ,
     xlab = "Logaritmo Gasto en Alimentos NO Saludables")  
# Medidas de tendencia central y de dispersión
mean(df.clean$ln_alns); median(df.clean$ln_alns); Mode(df.clean$ln_alns)[1]
var(df.clean$ln_alns); sd(df.clean$ln_alns)
IQR(df.clean$ln_alns) #Dispersión alrededor a la mediana


## Número de personas por hogar
# Medidas de tendencia central y de dispersión
mean(df.clean$numpeho); median(df.clean$numpeho); Mode(df.clean$numpeho)[1]
var(df.clean$numpeho); sd(df.clean$numpeho)
(iqr = quantile(df.clean$numpeho, probs = 0.75) - quantile(df.clean$numpeho, probs = 0.25))
(cuartiles <- quantile( df.clean$añosedu, probs = c(0.25, 0.5,0.75 ) ))

## Edad Jefe de Familia
# Medidas de tendencia central y de dispersión
mean(df.clean$edadjef); median(df.clean$edadjef); Mode(df.clean$edadjef)[1]
var(df.clean$edadjef); sd(df.clean$edadjef)
IQR(df.clean$ln_alns) #Dispersión alrededor a la mediana
(iqr = quantile(df.clean$edadjef, probs = 0.75) - quantile(df.clean$edadjef, probs = 0.25))
(cuartiles <- quantile( df.clean$edadjef, probs = c(0.25, 0.5,0.75 ) ))


## Años de Educación del Jefe de Familia
# Medidas de tendencia central y de dispersión
mean(df.clean$añosedu); median(df.clean$añosedu); Mode(df.clean$añosedu)[1]
var(df.clean$añosedu); sd(df.clean$añosedu)
(iqr = quantile(df.clean$añosedu, probs = 0.75) - quantile(df.clean$añosedu, probs = 0.25))
(cuartiles <- quantile( df.clean$añosedu, probs = c(0.25, 0.5,0.75 ) ))


## Boxplot
boxplot(ln_als ~ nse5f, data = df.clean,
        main = "Nivel socioeconómico y gasto en alimentos saludables", col="green") 
(cuartiles <- quantile( df.clean$ln_als, probs = c(0.25, 0.5,0.75 ) ))


boxplot(ln_alns ~ nse5f, data = df.clean, 
        main = "Nivel socioeconómico y gasto en alimentos NO saludables", col="red")
(cuartiles <- quantile( df.clean$ln_alns, probs = c(0.25, 0.5,0.75 ) ))









" CARLOS 3) Calcula probabilidades que nos permitan entender el problema en México"


"N=total de muestra
p= Probabilidad
size = casos"

n.alns <- count(df)  # 40809

p.alns <- 1-(sum(is.na(df$ln_alns)) /length (df$ln_alns))# 0.571075
size.alns <- count(df) - sum(is.na(df$ln_alns)) # 23305

help(rbinom)

binom.alns <- rbinom( n = 40809, size = 23305, prob = 0.571075 )


binom.alns.mean <- meanN(binom.alns)
binom.alns.sd <- sdN(binom.alns)


barplot(table(binom.alns)/length(binom.alns),
        main = "Distribución Binomial", 
        xlab = "# de encuestados que realizan un gasto en alNs")



"La concentración logarítmica de gasto sobre alimentos NO saludables nos  dice que
la gente tiene una probabilidad arriba  .005% de 13,293 en el gasto de este tipo 
alimentos NO saludables."

n.als <- count(df)  # 40809

p.als <- 1-(sum(is.na(df$ln_als)) /length (df$ln_als))# 0.980715
p.als
size.als <- count(df) - sum(is.na(df$ln_als)) # 40022



binom.als <- rbinom( n = 40809, size = 40022, prob = 0.980715 )

mean(binom.als)
sd(binom.als)

binom.als.mean <- mean(binom.als)
binom.als.sd <- sd(binom.als)


barplot(table(binom.als)/length(binom.als),
        main = "Distribución Binomial", 
        xlab = "# de encuestados que realizan un gasto en als")

"La concentración logarítmica de gasto sobre alimentos SI saludables nos  dice que
la gente tiene una probabilidad .014% de 39,244 en el gasto de este tipo de alimentos."







" 4) Plantea hipótesis estadísticas y concluye sobre ellas para entender el problema
en México"
##  Se verifican las varianas para saber si son o No iguales:

## Diferencia entre medias muestrales :
"Ho: No hay diferencia entre las medias poblacionales considerando el logaritmo de alimentos saludables =0
 Ha: Si hay diferencia entre las medias poblacionales el logaritmo de alimentos saludables != 0"

ns_Alto <- df.clean %>% filter(nse5f >=3) %>% pull(ln_als)
ns_Bajo <- df.clean %>% filter(nse5f < 3) %>% pull(ln_als)

(Res1.ln_als <- mean(ns_Alto) - mean(ns_Bajo))  #REsultado 0.4265841

"Ho: No hay diferencia entre las medias poblacionales considerando el logaritmo de alimentos No saludables = 0
 Ha: Si hay diferencia entre las medias poblacionales el logaritmo de alimentos No saludables != 0 "

Nns_Alto <- df.clean %>% filter(nse5f >=3) %>% pull(ln_alns)
Nns_BAjo <- df.clean %>% filter(nse5f >=3) %>% pull(ln_alns)
(Res2.ln_alns <- mean(Nns_Alto) - mean(Nns_BAjo)) ## El Resultado es Cero.

### Planteamiento de la Hipotesis :
# Ho : Nivel Alto >= Nivel Bajo   ** El Gasto en alimentos saludables es mayor en la clase Alta que en la clase baja
# Ha : Nivel Alto < Nivel Bajo  ** El Gasto en alimentos saludables es menor en la clase Alta que en la baja.


var.test(df.clean[df.clean$nse5f >= 3, "ln_als"],
         df.clean[df.clean$nse5f <= 2, "ln_als"],
         ratio = 1, alternative = "two.sided",
         conf.level = 0.95)


var.test(df.clean[df.clean$nse5f >= 3, "ln_alns"],
         df.clean[df.clean$nse5f <= 2, "ln_alns"],
         ratio = 1, alternative = "two.sided",
         conf.level = 0.95)

# Con el resultado de las varianzas corremos el t.test 


t.test( x = df.clean[df.clean$nse5f >= 3, "ln_als"],
        y = df.clean[df.clean$nse5f <= 2, "ln_als"],
        alternative = "greater", mu=0, var.equal = FALSE  ,conf.level = 0.95)


t.test( x = df.clean[df.clean$nse5f >= 3, "ln_alns"],
        y = df.clean[df.clean$nse5f <= 2, "ln_alns"],
        alternative = "greater", mu=0, var.equal = FALSE  ,conf.level = 0.95)          







" 5) Estima un modelo de regresión, lineal o logístico, para identificar los 
determinantes de la inseguridad alimentaria en México"

  ##### REGRESION LOGÍSTICA ( para variables categoricas )

df<-read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-08/Postwork/inseguridad_alimentaria_bedu.csv")
df.clean <- df[complete.cases(df),]

dim(df.clean)
str(df.clean)

df.select <- select(df.clean, ln_als, nse5f, numpeho, edadjef, añosedu, ln_alns, IA)

attach( df.select)

# Vamos a calcular la razon de probabilidad de que se presente la Inseguridad alimentaria
#  para cada variable. Usaremos la regresión logística y momios,
# tomando las  variables de :
#   - Numero de personas en el hogar
#   - Edad del Jefe de familia
#   - Años de educación
#   - Nivel socioeconómico, agrupando los niveles Bajo y medio bajo; y de Medio en adelante
# Veremos la probabilidad de que se presente la inseguridad alimentaria

# Inseguridad alimentaria relacionada con numpeho
logistic.1 <- glm( IA ~ numpeho, 
                   data = df.select, family = binomial)
summary(logistic.1)
exp( coef( logistic.1 ) )


# Inseguridad alimentaria relacionada con edadjef
logistic.1 <- glm( IA ~ edadjef, 
                   data = df.select, family = binomial)
summary(logistic.1)
exp( coef( logistic.1 ) )

# Inseguridad alimentaria relacionada con añosedu
logistic.1 <- glm( IA ~ añosedu, 
                   data = df.select, family = binomial)
summary(logistic.1)
exp( coef( logistic.1 ) )


# Inseguridad alimentaria relacionada con variable de nivel socioeconomico
logistic.1 <- glm( IA ~ nse5f, 
                   data = df.select, family = binomial)
summary(logistic.1)
exp( coef( logistic.1 ) )

library( dplyr)
# Inseguridad alimentaria relacionada con nse5f: de bajo y medio bajo
df.bajo <- select (df.select, IA, nse5f )  %>%
    filter( nse5f <= 2 )

logistic.1 <- glm( df.bajo$IA ~ df.bajo$nse5f, 
                   data = df.bajo, family = binomial)
summary(logistic.1)
exp( coef( logistic.1 ) )


# Inseguridad alimentaria relacionada con nse5f: de Medio en adelante
df.medioymas <- select (df.select, IA, nse5f )  %>%
  filter( nse5f > 2 )

logistic.1 <- glm( df.medioymas$IA ~ df.medioymas$nse5f, 
                   data = df.medioymas, family = binomial)
summary(logistic.1)
exp( coef( logistic.1 ) )






"6) Escribe tu análisis en un archivo README.MD y tu código en un script de R y 
publica ambos en un repositorio de Github."

# https://github.com/luisalegria/Equipo15Postwork08BEDU

