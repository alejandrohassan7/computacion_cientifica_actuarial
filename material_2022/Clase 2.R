
## COMPUTACI?N CIENT?FICA ACTUARIAL
## Facultad de Ciencias Econ?micas - UBA
## Docente: Rodrigo Del Rosso
## Introducci?n a Gr?ficos B?sicos en R

################################
####### SETEO DE CARPETA #######
################################

getwd()

dir()

path = "C:/Users/rdelr/OneDrive - Facultad de Ciencias Econ?micas - Universidad de Buenos Aires/751 - 1C2019/RStudio/Clases/Clase 2 - 27-03/"

setwd(path)

getwd() ## verifico si me modific? la ruta

dir()

######################
####### PLOTEO #######
######################

# Leemos el archivo

Obras <- read.csv2(file.choose())  ## En ingl?s es sin "2"
Obras <- read.csv2("Obras.csv")

plot(Obras$Materiales)

?plot

# Cambiamos el formato de los puntos

plot(Obras$Materiales, col="blue", pch = 20)

# Gr?fico de dispersi?n

x <- Obras$Materiales
y <- Obras$Mano.de.obra   ######ojo aca que el script decia "obra con O mayuscula"
plot(x,y) 

#Editamos el gr?fico

plot(x,y,col = "green",
         pch = 19,
         xlab = "Materiales",
         ylab = "Mano de obra",
         xlim = c(360,420),
         ylim = c(350,500))

# Si queremos unir con lineas, sin cerrar el gr?fico usamos lines:

lines(x,y)

# Con lty podemos elegir el estilo de la l?nea, y con col el color de la misma

plot(x,y,
       col="green",
       pch=19,
       xlab="Materiales",
       ylab="Mano de obra",
       xlim=c(360,420),
       ylim=c(350,500))

lines(x,y,
        lty=2,
        col="green")

# Ponemos un t?tulo al gr?fico
title("Materiales vs Mano de Obra")


plot(x,y,
     col="green",
     pch=19,
     xlab="Materiales",
     ylab="Mano de obra",
     xlim=c(360,420),
     ylim=c(350,500),
     main = "Materiales vs Mano de Obra")

##############################################
####### DISTRIBUCIONES DE PROBABILIDAD #######
##############################################

# Distribuci?n Binomial
# Ejemplo:  Sea X~Bi(5,0.1) 

dbinom(3,5,0.1)               # probabilidad puntual (Funci?n de Probabilidad Puntual)

pbinom(3,5,0.1)               # probabilidad acumulada a izquierda (Funci?n de Distribuci?n)

pbinom(3,5,0.1,lower.tail=F)  # probabilidad acumulada a derecha (Funci?n de Supervivencia)

pbinom(3,5,0.1,F)


#.k/P(x<=k)=0,9999

qbinom(0.9999,5,0.1)

#Comprobamos P(x<=4)

pbinom(4,5,0.1)

# Si queremos generar "n?meros aleatorios" con distribuci?n  binomial 

rbinom(10,5,0.1)        #genera valores "al azar", pseudoaleatorios 

rbinom(10,5,0.2)

rbinom(10,5,0.5)

set.seed(120)
rbinom(10,5,0.9)

set.seed(34)
x <- rbinom(10,5,0.9)
x
table(x)  ## tabla de frecuencias absolutas simples

#Analogamente para las distribuciones discretas help(distributions)

help(distributions)

###############
## Discretas ##
###############

dbinom    ## Binomial (Bernoulli)
dgeom     ## Geom?trica
dpois     ## Poisson
dnbinom   ## Binomial Negativa
dmultinom ## Multinomial
dhyper    ## Hipergeom?trica

###############
## Continuas ##
###############

dbeta     ## Beta
dchisq    ## Chi Cuadrado
dexp      ## Exponencial
df        ## F de Snedecor
dgamma    ## Gamma
dlnorm    ## Lognormal
dnorm     ## Normal
dt        ## T de Student
dunif     ## Uniforme
dweibull  ## Weibull

## m?s distribuciones en el paquete "distr"

########################
####### GR?FICOS #######
########################

# Grafiquemos la funci?n de probabilidad puntual

n <-5
x <-seq(0,n)

par(mex = 0.7)
par(mgp = c(2,0.3,0))
par(cex = 0.65)  

p <-0.1
y <- dbinom(x,n,p)

plot(x,y,ylab="p(x)",type="h",lwd = 2,col="green")
points(x,y,pch = 16,col="green")
mtext("p = 0.10",3,-2)

# Sea X~Bi(20,p) , grafiquemos la funci?n de probabilidad puntual para distintos valores de p

n<-20
x<-seq(0,n)
par(mfrow=c(3,2))
par(mex=0.7)
par(mgp=c(2,0.3,0))
par(cex=0.65)

p<-0.1
y<-dbinom(x,n,p)
plot(x,y,ylab="p(x)",type="h",lwd = 2,col="green")
points(x,y,pch = 16,col="green")
mtext("p=0.10",3,-2)

p <- 0.25
y <- dbinom(x,n,p)
plot(x,y,ylab="p(x)",type="h",lwd = 2,col="green")
points(x,y,pch = 16,col="green")
mtext("p=0.25",3,-2)

p<-0.50
y<-dbinom(x,n,p)
plot(x,y,ylab="p(x)",type="h",lwd = 2,col="green")
points(x,y,pch = 16,col="green")
mtext("p=0.50",3,-2)

p<-0.60
y<-dbinom(x,n,p)
plot(x,y,ylab="p(x)",type="h",lwd = 2,col="green")
points(x,y,pch = 16,col="green")
mtext("p=0.60",3,-2)

p<-0.75
y<-dbinom(x,n,p)
plot(x,y,ylab="p(x)",type="h",lwd = 2,col="green")
points(x,y,pch = 16,col="green")
mtext("p=0.75",3,-2)

p<-0.90
y<-dbinom(x,n,p)
plot(x,y,ylab="p(x)",type="h",lwd = 2,col="green")
points(x,y,pch = 16,col="green")
mtext("p=0.90",3,-2)

# Distribuci?n Normal

# Ejemplo:  Sea X~N(mu, sigma) 

# dnorm(x,mean,sd) evalua la funci?n de densidad en x
# pnorm(q,mean,sd) P(x<=q)
# qnorm(p,mean,sd) q/P(x<=q)=p
# rnorm(n,mean,sd) genera n valores pseudoaleatorios normales con la media y el desv?o dado.

# X~N(0,1)

#.f(0)
dnorm(0,0,1)

1/sqrt(2*pi)

#.P(X<=0)
pnorm(0,0,1)

#.P(X<=2)
pnorm(2,0,1)

#.P(X 2)

pnorm(2,0,1,F)

qnorm(0.99,0,1)

qnorm(0.99,0,1,F)

set.seed(34)
rnorm(10,0,1)

# Gr?fico de densidades

x <- seq(-8,8,0.01)
plot(x,dnorm(x),type="l",ylim=c(0,1),ylab="")#normal estandar
lines(x,dnorm(x,0,2),type="l",ylim=c(0,1),col="red")#media 0 y desv?o 2
lines(x,dnorm(x,0,0.5),type="l",ylim=c(0,1),col="blue") #media 0 y desv?o 0.5
lines(x,dnorm(x,2,1),type="l",ylim=c(0,1),col="green") #media 2 y desv?o 1
lines(x,dnorm(x,-2,0.5),type="l",ylim=c(0,1),col="brown") #media -2 y desv?o 0.5
legend(1.7,1, legend = c("Media=0 Desv?o=1","Media=0 Desv?o=2","Media=0 Desv?o=0.5","Media=2 Desv?o=1","Media=-2 Desv?o=0.5"),
       col = c(1,"red","blue","green","brown"),lty = 1, cex = .8, y.intersp = 1)
title("Funciones de densidad  de la distribuci?n Normal")



#######################################
####### ESTAD?STICA DESCRIPTIVA #######
#######################################

DatosEPH <- read.csv2("DatosEPH2016.csv", header=T)  # Leemos el archivo de datos

edit(DatosEPH) #Para ver el conjunto de datos le?do

# Cerramos el editor para poder continuar

table(DatosEPH$AGLOMERADO) #Para graficar una diagrama circular 

tabulate(DatosEPH$AGLOMERADO)

Aglomerado.frec <- table(DatosEPH$AGLOMERADO)

pie(Aglomerado.frec) 

# Gr?fico editado

pie(Aglomerado.frec,
    labels = c("CABA","GBA"),
    col=c("red","blue"),
    main="Gr?fico circular para la variable Aglomerado")

# Para graficar un gr?fico de barras

barplot(Aglomerado.frec)

# Gr?fico editado

barplot(Aglomerado.frec,
        col=c("red","blue"),
        main="Gr?fico de barras para la variable Aglomerado")

barplot(Aglomerado.frec,
        col=c("red","blue"),
        main="Gr?fico de barras para la variable Aglomerado",
        axis.lty = 1)

# Otra forma
# Si x es el factor Aglomerado  en el data frame DatosEPH

plot(DatosEPH$AGLOMERADO)

################################
####### BARRAS AGRUPADAS #######
################################

Ejemplo <- read.csv2("Datos barras agrupadas.csv", header=T)
Sex_Nivel <- table(Ejemplo$Nivel.Educativo,Ejemplo$Sexo)
Sex_Nivel

barplot(Sex_Nivel,legend=rownames(Sex_Nivel))

#Si queremos el gr?fico con las frecuencias relativas porcentuales por g?nero:

totalNivel <- apply(Sex_Nivel,2,sum)
SexNivel.porcentual <- t(Sex_Nivel)/totalNivel*100
t(SexNivel.porcentual)

#Si queremos las barras al costado y cambiamos el color
barplot(t(SexNivel.porcentual),
        legend=colnames(SexNivel.porcentual),
        beside=T,
        col=c("red","green","blue"))

Ingresos <- DatosEPH$ITF
mean(Ingresos)

# Si hubiera valores faltantes dar?a NA, una forma de calcular la media descartando los valores faltantes es con na.rm=T

median(Ingresos,na.rm=T)
min(Ingresos,na.rm=T)
max(Ingresos,na.rm=T)
quantile(Ingresos,na.rm=T)

# lo mismo calcula fivenum
fivenum(Ingresos,na.rm=T)

#si queremos otros percentiles

quantile(Ingresos,na.rm=T,probs=c(0.10,0.30,0.90,0.95))


summary(Ingresos)

range(Ingresos)
diff(range(Ingresos))
#Otra forma
max(Ingresos)-min(Ingresos)

var(Ingresos)

sd(Ingresos)  #desvio estandar
IQR(Ingresos) #rango intercuartil
mad(Ingresos) #desvio absoluto medio

100*sd(Ingresos)/mean(Ingresos)

#Si queremos solo dos cifras decimales

round(100*sd(Ingresos)/mean(Ingresos),2)


Región<-DatosEPH$AGLOMERADO

tapply(Ingresos, Región, summary)
tapply(Ingresos, Región, sd)
tapply(Ingresos, Región, mad) 

#tapply aplica una funci?n a un vector en los subvectores que define otro vector m?scara
#en este caso calcula el ingreso medio, por region

100*tapply(Ingresos, Región, sd)/tapply(Ingresos, Región, mean)

#Que hace si le aplico una funci?n a todo el data.frame?
summary(DatosEPH)


#Gr?ficos

hist(Ingresos)
histo.ingresos <- hist(Ingresos)
names(histo.ingresos) 

histo.ingresos$breaks
histo.ingresos$mids
histo.ingresos$counts 

par(mfrow=c(1,2))# es para dividir la pantalla gr?fica en 1 fila y dos columnas para poder visualizar mejor.
tapply(Ingresos,Región,hist)

Ingresos
IngresosCABA<-DatosEPH$ITF[DatosEPH$AGLOMERADO=="CABA"]
IngresosGBA<-DatosEPH$ITF[DatosEPH$AGLOMERADO=="GBA"]

#Histograma editado (fijamos la longitud de cada intervalo, los l?mites, elegimos color y ponemos t?tulos)

# Para poder comparar ambos histogramas trabajamos para ambas variables con los mismos intervalos de clase # y la misma escala para las frecuencias.
par(mfrow=c(1,2)) # es para dividir la pantalla gr?fica en 1 fila y dos columnas para poder visualizar mejor.

# Longitud de cada intervalo 4000 (elegimos esta longitud de acuerdo a los datos observados)
hist(IngresosCABA,breaks=seq(0,48000,4000),col="blue",ylim=c(0,60), main="Histograma de Ingresos CABA")
hist(IngresosGBA,breaks=seq(0,48000,4000),col="green",ylim=c(0,60), main="Histograma de Ingresos GBA")

hist(IngresosCABA,breaks=seq(0,48000,4000),freq=F,col="blue",ylim=c(0,0.0001), main="Histograma de Ingresos CABA")
hist(IngresosGBA,breaks=seq(0,48000,4000),labels=T,freq=F,col="green",ylim=c(0,0.0001), main="Histograma de Ingresos GBA")


# Otros Ejemplos
Ejemplos <- read.table("Ejemplo.csv",header=T)
hist(Ejemplos$N, 
     col="blue",
     main=" ",
     xla="",
     sub="Distribuci?n Sim?trica")

hist(Ejemplos$AS, 
     col="blue",
     main=" ",
     xla="",
     sub="Distribuci?n Asim?trica a Derecha")

# Generamos un dato at?pico

Out <- c(Ejemplos$AS,36)

hist(Out,
     col="blue",
     main=" ",
     xla="",
     sub="Distribuci?n Asim?trica a Derecha")


#Tallo-Hoja

#Otro ejemplo

stem(Ejemplos$N)
stem(Out)

stem(Ingresos)

#Tallo-Hoja por aglomerado

tapply(Ingresos,Regi?n,stem)


#Boxplot
par(mfrow=c(1,1))

boxplot(Ingresos,col="green",ylab="Ingresos")

#Horizontal

x11()   ### para que no me pise los gr?ficos y tenerlos abiertos

boxplot(Ingresos,col="green",xlab="Ingresos",horizontal=T)

#Para identificar valores at?picos

boxplot(Ingresos,col="green",ylab="Ingresos")

identify(rep(1,length(Ingresos)),Ingresos,rownames(DatosEPH))

boxplot(Ingresos~Regi?n)


boxplot(Ingresos~Regi?n,col=c("red","blue"), main="Ingresos por hogar")

# Vamos a generar otra variable, Cant=n?mero de integrantes del hogar

cant<-DatosEPH$ITF/DatosEPH$IPCF
cant

DatosEPH2<-cbind(DatosEPH,cant)
data.class(DatosEPH2)
edit(DatosEPH2)

# Guardamos la nueva base

write.csv2(DatosEPH2,paste0(path,"Ejemplo.csv"))

## En ingl?s es sin "2" 
## En espa?ol es con "2"

rm(list = ls())  # para limpiar el entorno

