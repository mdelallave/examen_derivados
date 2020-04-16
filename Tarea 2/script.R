#Parámetros:
t <- 1 #Intervalo temporal, 1 año.
n <- 250 #Pasos en los que se divide el intervalo (lo pasamos a días)
m <- 10000 #Cantidad de simulaciones realizadas.
dt <- t/n #Aproximación del diferencial de t.
S_1 <- 13385.93 #Valor inicial del subyacente.
K <- 13500 #Strike de la opción.
r <- -0.00214$ #Tipo de interés.
sd <- 0.230479 #Volatilidad.

#Dummies para almacenar información en el bucle:
ddelta <- 0 #Dummy delta.
dcall<- 0 #Dummy call.
payoff <- rep(0, m) #Vector que contendrá los pagos de la opción.
z <- rep(0, n) #Vector que contendrá al browniano.
S <- rep(S_1, n) #Vector que contendrá el subyacente. 

#Primero creamos el bucle perteneciente a las simulaciones
for (j in 1:m) {
  #Dentro, metemos otro bucle que nos va a dar el vector
  #S, desde S[1] hasta S[n=250].
  for (i in 2:n){
    z[i] <- rnorm(1, mean = 0, sd = 1) #Browniano.
    S[i] <- S[i-1]*exp((r-(sd^2)/2)*dt +sd*sqrt(dt)*z[i])#Black-Scholes
  } 
  #Una vez acabado el bucle, hallamos la función de pagos
  #mediante el siguiente condicional, que nos dará el pago
  #para cada vector S, m veces. Cada S es diferente gracias
  #a la aleatoriedad del browniano, por lo que cada pago
  #también será diferente. Tendremos 100 pagos diferentes en total.
  if (mean(S) > K) {payoff[j] <- mean(S) - K;
                    ddelta <- ddelta + mean(S)} #Acumulamos el valor medio de S.
  else {payoff[j] <- 0} #El condicional hace el papel de función indicatriz.
  dcall <- dcall + payoff[j] #Acumulamos el pago.
}

delta <- (ddelta/(m*S_1))*exp(-r*t) #Valor de delta.
call <- (dcall/m)*exp(-r*t) #Precio de la call.

