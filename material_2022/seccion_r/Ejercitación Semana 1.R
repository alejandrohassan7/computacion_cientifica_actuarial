#1------------------------------------------------------------------------------

#Ejercicio 1

#a)
Edad<-c(51,44,55,47,56,54,53,45,48,55)
Sexo<-c("M","F","F","F","M","F","M","F","M","M")
Sueldo<-c(7200,6700,5000,5200,7500,6300,8700,6100,9200,9000)
Antiguedad<-c(5,6,4,5,7,4,8,6,10,9)

#b)
Datos.Empleados<-rbind(Edad,Sexo,Sueldo,Antiguedad) 
colnames(Datos.Empleados)<-c(1,2,3,4,5,6,7,8,9,10)

#c1)
View(Datos.Empleados)
#c3)
Datos.Empleados[3,5]
#c4)
Datos.Empleados[,8]
#c5)
Datos.Empleados[3,Datos.Empleados[2,]=="M"]
#c6)
Datos.Empleados[4,Datos.Empleados[2,]=="F"]
#c7)
Datos.Empleados[3,Datos.Empleados[1,]>50]
#c8)
"Sueldo/Antiguedad"<-c(Datos.Empleados[3,],Datos.Empleados[4,])

#d)
data.frame(Datos.Empleados)

#e)
write.csv(Datos.Empleados,"Datos.Empleados.csv",row.names = FALSE)

#2------------------------------------------------------------------------------

#Ejercicio 2

Edades<-c(27,23,29,24,31)
sink("Datos.txt")
mean(Edades)
sink()
mean(Edades)
source("Datos.txt")

#3------------------------------------------------------------------------------

#Ejercicio 3

y<-c(1,3,5,7)
y<-seq(1,7,2)

#4------------------------------------------------------------------------------

#Ejercicio 4

x<-1:5
x<-c(1,2,3,4,5)
x<-seq(1,5)

#5------------------------------------------------------------------------------

#Ejercicio 5

x<-c(8:5)
x<-c(3,3,3,3,3,3,3,3,2,2)
x<-seq(1,4,0.75)

#6------------------------------------------------------------------------------

#Ejercicio 6

profesores<-c("Alberto", "Daniel", "Pablo", "Eduardo")

#7------------------------------------------------------------------------------

#Ejercicio 7

x<-c(2,-5,4,6,-2.8)

#a)
y<-c(x[x>0])
#b)
z<-c(x[x<0])
#c)
v<-c(x[-1])
#d)
w<-c(x[(x%%2)==0])

#8------------------------------------------------------------------------------

#Ejercicio 8

x<-c(1:6)
y<-c(7,8)
z<-seq(9,12)

#a)
x+x
#b)
x+y # R repite los elementos del segundo vector hasta completar la suma
#c)
x+z # R completó la suma "repitiendo" los primeros dos elementos de Z

#9------------------------------------------------------------------------------

#Ejercicio 9

x<-c(1:6)

m1<-matrix(x,nrow=2,ncol=3)
m2<-matrix(x,nrow=3,ncol=2)
m3<-matrix(x,ncol=3,nrow=2,byrow=TRUE)
m4<-matrix(x,ncol=3,nrow=3)

#10------------------------------------------------------------------------------

#Ejercicio 10

matrix(nrow=3,ncol=3) # R llena sus elementos con NA

matrix(1:6,nrow=2) # R añade filas o columnas hasta completarla
matrix(1:6,nrow=4) # Los datos no alcanzan para completar una segunda columna
matrix(1:6,nrow=4,ncol=4) # Los datos no alcanzan para completar 4 columnas

#11------------------------------------------------------------------------------

#Ejercicio 11

A<-matrix(c(2,3,1,4),nrow=2)
B<-matrix(c(3,8),ncol=1)

A%*%B # Realiza la operación solicitada, la salida es un matriz
A*B # No arroja nada, la salida es un vector

#12------------------------------------------------------------------------------

#Ejercicio 12

A<-matrix(1:6,nrow=2,ncol=3)
B<-matrix(1:12,nrow=3,ncol=4)
C<-matrix(1:6,nrow=2,ncol=3)

#a)
A*B
#b)
outer(A,B)
#c)
A+2
#d)
A%*%B
#e)
exp(B)
#f)
A*C # multiplica aritméticamente elemento a elemento de la matriz
#g)
A%*%C # ncol A distinto de nrow B, no se puede multiplicarlas
#13------------------------------------------------------------------------------

#Ejercicio 13

A<-matrix(1:6,nrow=2)
B<-matrix(1:6,nrow=3)
C<-matrix(1:3,nrow=3,ncol=3)
D<-matrix(seq(2,6,2),nrow=3)

as.vector(A)
as.vector(B)

#14------------------------------------------------------------------------------

#Ejercicio 14

#a)
p0<-c(0.4,0.6)
#b)
P<-matrix(c(0.5,0.75,0.5,0.25),nrow=2)
#c)
t1<-p0%*%P
t2<-p0%*%(P%*%P)
t3<-p0%*%(P%*%P%*%P%*%P%*%P)

#d)
sum(t1)
sum(t2)
sum(t3)
apply(P,1,sum)

#15------------------------------------------------------------------------------

#Ejercicio 15

 P<-matrix(c(0.4,0.1,0,0,0.3,0.1,0,0,0.2,0.6,1,0,0.1,0.2,0,1),nrow=4)

#a)
p0s1<-c(1,0,0,0)%*%P
p0s2<-c(0,1,0,0)%*%P
p0s3<-c(0,0,1,0)%*%P
p0s4<-c(0,0,0,1)%*%P
#b)
sum(p0s1)
sum(p0s2)
sum(p0s3)
sum(p0s4)
apply(P,1,sum)
#c)
G<-matrix(c(0.2,0.6,0.1,0.2),nrow=2)
H<-matrix(c(0.4,0.1,0.3,0.1),nrow=2)
Q<-solve(diag(2)-H)
#d)
R<-Q%*%G