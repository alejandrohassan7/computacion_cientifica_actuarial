#1------------------------------------------------------------------------------

#Ejercicio 1

library(UsingR)
data(galton)

#a)
mean(galton$child)
mean(galton$parent)
sd(galton$child)
sd(galton$parent)
cor(galton$child,galton$parent)

#b)
centered.galton<-as.data.frame(scale(galton,scale=FALSE))<
mean(centered.galton$child) #aproximadamente 0
mean(centered.galton$parent) #aproximadamente 0

#c)
normaliced.galton<-as.data.frame(scale(galton))
sd(normaliced.galton$child)
sd(normaliced.galton$parent)
cor(normaliced.galton$parent,centered.galton$child) #la correlación sigue igual

#2------------------------------------------------------------------------------

#Ejercicio 2

#a)
lm.father.son<-lm(sheight~fheight,data=father.son)

#b)
coef(lm.father.son)["(Intercept)"] #intercepto
coef(lm.father.son)["fheight"] #pendiente

#c)
plot(father.son$fheight,father.son$sheight,main="Dispersion Diagram",xlab ="Father's Height",ylab ="Son's Height")
abline(lm.father.son,col="blue")

#d)
centered.father.son<-as.data.frame(scale(father.son,scale=FALSE))
lm.centered.father.son<-lm(sheight~fheight-1,data=centered.father.son)
coef(lm.centered.father.son)["fheight"]

#e)
???

#f)
normaliced.father.son<-as.data.frame(scale(father.son))
lm.normaliced.father.son<-lm(sheight~fheight,data=normaliced.father.son)
coef(lm.normaliced.father.son)["fheight"]
cor(normaliced.father.son$fheight,normaliced.father.son$sheight)#la pendiente del modelo normalizado es la misma que la correlación

#g)
FH<-data.frame(fheight=63)
predict(lm.father.son,FH,interval="prediction")

#h)
plot(fitted(lm.father.son),resid(lm.father.son),xlab ="Son's Height",ylab ="Residuals")
plot(lm.father.son$fitted.values,lm.father.son$residuals,xlab ="Son's Height",ylab ="Residuals")
abline(0,0)

#i)
summary(lm.father.son)$r.squared

#3------------------------------------------------------------------------------

#Ejercicio 3

(0.3*2) # (r*Sy)/Sx = b1 = (0.3*2m)/m

#a)
1-.5*.3 # intercepto = 1-0,5*pendiente

#b)
(0.3*1/2) # (r*Sy)/Sx = b1 = (0.3*m)/2m

#4------------------------------------------------------------------------------

#Ejercicio 4

#Verdadero: intercepto = yraya-(xraya*pendiente)

#5------------------------------------------------------------------------------

#Ejercicio 5

lm.father.son<-lm(sheight~fheight,data=father.son)
summary(lm.father.son)$coefficients[2,4]
attach(lm.father.son)
cor.test(fheight,sheight)

#a)
#con un intervalo de confianza del 95%, podemos asegurar que rho se encuentra entre 0.4552586 y 0.5447396
#centrar intercepto???

#b)
predict(lm.father.son,data.frame(fheight=80),interval="prediction")
#no recomendaría la predicción, pues la correlación entre las variables es baja (solo 50%)

#6------------------------------------------------------------------------------

#Ejercicio 6

lm.mtcars<-lm(mpg~hp,data=mtcars)
attach(mtcars)

#a)
plot(hp,mpg,main="Scatter-Plot",xlab="HPG",ylab="MPG",pch=19,frame=FALSE)
lines(lowess(hp,mpg),col="red")

#b)

#c)
predict(lm.mtcars,data.frame(hp=111),interval="prediction")

#7------------------------------------------------------------------------------

#Ejercicio 7

States<-read_csv2("States.csv")

#a)
States.lm<-lm(msat~expense,data=States)

#b)
#los supuestos son: media de errores (residuos) nula, errores sin correlación y homocedasticidad de errores
sum(residuals(States.lm)) #aproximadamente cero
library(car)
vif(States.lm)

#8------------------------------------------------------------------------------

#Ejercicio 8

Data<-read_csv2("ceosal2.csv")
attach(Data)

#a)
mean(comten)
mean(age)

#b)
sum(Data[,6]<6)-sum(Data[,6]<1)

#c)
lm.CEO.sallary<-lm(lsalary~ceoten)
coef(lm.CEO.sallary)["ceoten"] #variación del salario ante un aumento de un año en el cargo

#d)
lm.CEO.years<-lm(ceoten~comten)

#9------------------------------------------------------------------------------

#Ejercicio 9

sleep.work<-read.csv2("sleep75.csv")
lm.sleep.work<-lm(sleep~totwork,data=sleep.work)

#a)
coef(lm.sleep.work)["totwork"] #coeficiente b1 / pendiente
coef(lm.sleep.work)["(Intercept)"] #coeficiente b0 / intercepto / cantidad de horas de sueño si no se trabaja
summary(lm.sleep.work)$r.squared

#b)
coef(lm.sleep.work)["totwork"]*2

#10-----------------------------------------------------------------------------

#Ejercicio 10

wage.IQ<-read.csv("wage2.csv")
wage.IQ.lm<-lm(wage~IQ,data=wage.IQ)

#a)
mean(wage.IQ$wage)
mean(wage.IQ$IQ) #aproximadamente 100
sd(wage.IQ$IQ) #aproximadamente 15

#b)

