#Paquetes necesarios:
library(dplyr)
library(readxl)
library(dplyr)
library(ggpubr)

#Importamos la serie del DAX30 y de futuros sobre el DAX30 para junio 2020 
#sobre la que vamos a trabajar
DAX30_FUT <- read_excel("YOUR PATH/DAX30 + FUT.xlsx")
#Importamos también la serie histórica del VIX para sacar la volatilidad
VIX_DAX30 <- read_excel("YOUR PATH VIX DAX30.xlsx")
#Invertimos el orden de los datos (ordenados por fecha).
datos <- arrange(DAX30_FUT, date) 


##PARÁMETROS##

r <- 0.005 #Tipo de interés medio del bono alemán a 10 años (31DIC18-31DIC19)
q <- 0.0379859 #Dividendos del DAX30 (proporcionado por Thomson Reuters 
#a día 11 de abril de 2020)
sd <- mean(VIX_DAX30$vix)/100 #Desviación típica sacada del VIX histórico.
X <- first(datos$dax) #Strike = primer valor del DAX30
S <- datos$dax #Precio cierre DAX30 (subyacente)
f <-  datos$futdax #Precio de los futuros sobre el DAX30 para JUN20
n <- 1000*S #Nominal del fondo
k1 <-  n/S #Veces que la cartera contiene al índice
k2 <- 25 #Multiplicador del futuro
datos$t_f <- as.Date(rep("2020-06-19", times = length(datos$date)))
- as.Date(datos$date) #Días hasta vencimiento del futuro
t_f <- as.numeric(datos$t_f/250) #Anualizamos los días y los convertimos en números
datos$t <- as.Date(rep("2020-04-09", times = length(datos$date))) 
- as.Date(datos$date) #Días hasta vencimiento de la opción
t <- as.numeric(datos$t/250) #Anualizamos los días y los convertimos en números

##FUNCIONES##
d1 <- (log(S/X) + (r - q + sd^2/2)*t)/(sd*sqrt(t))
delta <- pnorm(d1) - 1 #Cobertura delta
posc <-  round(exp(-(r-q)*t_f) *exp(-q*t)*delta*(k1/k2))  #Posición en futuros
ldpg <- 0:(length(datos$futdax)-1) #Creamos un vector ldpg
for (i in 2:71) {
  ldpg[1] <- 0
  ldpg[i] <- (f[i]-f[i-1])*k2*(posc[i])
} #Liquidación diaria de pérdidas y ganancias
perd <- 0:(length(datos$futdax)-1) #Creamos un vector de pérdidas
for (i in 2:71) {
  perd[1] <- 0
  perd[i] <- n[i]-n[i-1]
}#Pérdida del fondo día a día.
rdo <- perd+ldpg #Resultado día a día de las ganancias y pérdidas.


###Para calcular el valor de la put (p)
d2 <- d1 - sd*sqrt(t)
nd1 <- pnorm(-d1)
nd2 <- pnorm(-d2)
p <- 1000*(X*exp(-r*t)*nd2 - S*exp(-q*t)*nd1) #Valoración de la put.
#Multiplicamos por 1000 porque el nominal es 1000 veces X
track_err <- 0:(length(datos$futdax)-1) #Creamos un vector track_err
for (i in 2:length(datos$futdax)) {
  track_err[1] <- 0
  track_err[i] <- ((p[i-1]-p[i]+ldpg[i])*100) /(ldpg[i])
} #Tracking error en porcentaje.


##OTROS##
#Gráfico de delta sobre el subyacente.
plot(S, delta, xlab = "Índice", ylab = "Delta")
abline(v=X, col="blue") 

#Gráfico para observar la posición en el mercado de futuros.
plot(posc, xlab = "Días", ylab = "Posición") 

#Gráfico para observar el resultado de la cobertura.
plot(rdo, xlab = "Días", ylab = "Resultado") 
abline(h=0, col = "blue")

#Gráfico del tracking error.
ggdensity(track_err, xlab = "Tracking error", ylab = "Densidad") 
