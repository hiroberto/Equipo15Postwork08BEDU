" POSTWORK 7. PREDICCIONDE LA TEMPERATURA GLOBAL

OBJETIVO: Estimar modelos ARIMA y realizar predicciones

Utilizando el siguiente vector numérico, realiza lo que se indica: "

url = "https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-07/Data/global.txt"
Global <- scan(url, sep="")
head(Global)

" 1. Crea un objeto ts con los datos de Global. La serie debe ser mensual 
 comenzado en Enero de 1856 "
Global.ts = ts(Global, start =c(1856,1), freq=12 )


" 2. Realiza una gráfica de la serie de tiempo anterior de 2005 "
plot( Global.ts, xlab = "Tiempo", main = "Serie de tiempo Global", 
      sub = "Enero de 1858 - Diciembre de 2005")


" 3. Ahora realiza una gráfica de la ts anterior, transformando a la primera diferencia:"
Global.dif = diff(Global.ts)
plot(Global.dif, main = "Global - Primera diferencia", xlab = "Tiempo",
     sub = "Enero de 1858 - Diciembre de 2005")

" 4. ¿Consideras que la serie es estacionaria en niveles o en primera diferencia? " 
## En primera diferencia

" 5. Con base en tu respuesta anterior, obten las funciones de autocorrelación y autocorrelación parcial"
acf(Global.dif, main="Global - Autocorrelación")
pacf(Global.dif,  main="Global - Autocorrelación parcial")


