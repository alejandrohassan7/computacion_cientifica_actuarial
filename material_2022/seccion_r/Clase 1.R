##########################################################
#         COMPUTACI?N CIENTIFICA ACTUARIAL (746)         #
#           CATEDRA DEPARTAMENTO DE MATEMATICA           #
#	                CARRERA DE ACTUARIO                    #
#            FACULTAD DE CIENCIAS ECON?MICAS             #
#              UNIVERSIDAD DE BUENOS AIRES               #
##########################################################

## Asignatura: COMPUTACI?N CIENTIFICA ACTUARIAL 
## 1er Cuatrimestre de 2019
## Docente: Rodrigo Del Rosso
## Basado en una gu?a desarrollada por las Profs. GIRIMONTE Y MOLINARI

################################
####### SETEO DE CARPETA #######
################################

getwd()

dir()

directorio <- choose.dir()

setwd(directorio)

setwd("C:/Users/Rodri/Desktop/Estad?stica Actuarial/R/Clase 1")  ## ac? coloquen la ruta donde poseen los archivos de trabajo

getwd() ## verifico si me modific? la ruta

## permite cambiar el directorio donde estamos trabajando (working directory)
## Hay que tener en cuenta que los datos se guardar?n en ese directorio

##############################################
####### OPERACIONES CON N?MEROS REALES #######
##############################################
   
2+5

2-3

2*5

2/5

2^5

9^(1/2)

#################################
####### ALGUNAS FUNCIONES #######
#################################

sqrt(9)

pi

sin(pi/2)

cos(pi)

exp(1)

abs(2)

abs(-3)

round(1/3,4)

## probar con "ceiling, floor, trunc y signif" ##

##########################
####### SUCESIONES #######
##########################

1:5              

-1:2

-100:0

# La funci?n "seq" produce secuencias equi-espaciadas, con paso que se puede determinar. Por default usa paso 1.

seq(1,5)

seq(1,5,0.5)

# Con la funci?n rep replica un valor dado la cantidad de veces que se le indique: rep(valor, longitud)

rep(2,5)

rep(NA,10)

##########################
####### ASIGNACI?N #######
##########################

## Operador de asignaci?n ?<-?: nos permite asignar un nombre/valor/formato a un objeto
 
x <- seq(1,5)

x

print(x)

## Se distinguen may?sculas de min?sculas. Por ejemplo, si ahora le pedimos que nos muestre X

X

## R es CASE SENSITIVE 

########################
####### VECTORES #######
########################

x <- c(1,3,5,7)
x

x <- seq(1,5)
x

y <- seq(1,10,2)
y
 
x*y    # Multiplica componente a componente

x%*%y   # Realiza el producto escalar entre vectores.

w <- c(1,2,3)
  
x + w

# Ejemplo de vector l?gico
 
x <- c(T,F,F,T) # un vector l?gico contiene True o False en cada componente.
x

# Ejemplo de caracteres 

x <- "Hoy es lunes" # vector de caracteres de longitud 1
y <- c("Juan","Tomas","Sol") # vector de caracteres de longitud 3

#3.3 Algunas funciones sobre vectores
  
x <- seq(1,5)

length(x)  # devuelve la dimension del vector

dim(x) ## ?Porqu? da Null?

is.vector(x) #preguntamos si el objeto que generamos es un vector

rev(x)		# invierte el orden de los elementos del vector

sort(c(6,9,2,7,1))        # ordena  los elementos en orden ascendente
	
sort(c("Juan", "Tomas", "Sol"))
     
order((c("Juan", "Tomas", "Sol")))  # indica la componente del vector original que queda en cada lugar al ordenar.

x <- c("Juan", "Tomas", "Sol")


x[2]

x[order(x)] # genera el mismo resultado que  sort(x)


x <- c(1,4,TRUE,"hola")
x

data.class(x)


y <- c(1,4,TRUE)  
y

data.class(y)

w <- c(1,4,FALSE)       
w
      
########################
####### MATRICES #######
########################

# A partir de un vector dado generar una Matriz # 

x <- 1:12

A <- matrix(x,nrow=3,ncol=4)
A
   
A <- matrix(x,nrow=3)
A

A <- matrix(x,ncol=4)
A

B <- matrix(x,nrow=3,byrow=T)
B

# Con la funci?n scan #

M <- matrix(scan(),ncol=3)
M
     
# Podemos crear el vector en la misma sentencia #

M <- matrix(c(2,3,1,2,4,5,2,4,3),ncol=3)
M

# Con las funciones cbind rbind #

col1 <- c(1,2,3)
col2 <- c(0,1,1)
A <- cbind(col1,col2)
A
  
B <- rbind(col1,col2)
B
     
# Operaciones algebraicas.

A <- matrix(1:10,ncol=5)          
A
            
A + c(0,1)
	     
#Si se quiere multiplicar en forma matricial se debe usar %*%:

A <- matrix(1:12,ncol=4)
A
     
B <- matrix(seq(1,24,3),ncol=2)
B
  
A%*%B
 
M    
t(M)     

det(M)

solve(M)

# Para obtener elementos de una matriz #

A

A[1,3]  # devuelve el elemento de la fila 1 y columna 3

A[1,]   # devuelve la primera fila de la matriz

A[,4]   # devuelve la cuarta columna de la matriz.

A[7]    # devuelve el septimo elemento de la matriz recorri?ndola por columnas.

# Operaciones por filas o columnas #

A <- matrix(1:20,ncol=5)
A
    
apply(A,2,sum)  #Sumamos las columnas de la matriz A

apply(A,1,sum)  #Sumamos las filas de la matriz A

apply(A,2,mean)

apply(A,1,sd)

# Nombres de las filas y columnas de una matriz #

notas <- c(10,9,10,9,8,10)
notas <- matrix(notas,3,2,byrow = T)
notas
     
rownames(notas) <- c("Juan","Ana","Pedro")
notas
     
colnames(notas) <- c("Parcial","Final")
notas
      
dimnames(notas)


notas["Juan",]

notas[,"Final"]
 

x <- 1:12  
x
dim(x)

x <- as.matrix(x)  # transformamos a x en una matriz

dim(x)             # dim nos devuelve las dimensiones de la matriz

is.matrix(x)       # preguntamos si es una matriz

data.class(x)      # preguntamos qu? tipo de objeto es

############################
####### DATAS FRAMES #######
############################

# Ejemplos de construcción de un data frame #

# Ejercicio 1 # 

x1 <- c("Juan","Sol","Tom?s")
x2 <- c(10,9,8)
x3 <- c(9,9,7)
x4 <- c(TRUE,FALSE,TRUE)
Notas <- data.frame(x1,x2,x3,x4)
Notas

colnames(Notas) <- c("Nombre","Primer parcial","Segundo Parcial","Tutor?a")
Notas

# Ejercicio 2 # 

x <- c(1,1,1,1)
y <- c(2,2,2)
z <- data.frame(x,y)

# Ejercicio 3 # 

y <- c(2,2,2,NA)
z <- data.frame(x,y)
z
  
# Acceso a los elementos de un Data Frame #

Notas[,1] 

Notas$Nombre

Notas$Primer parcial    ## Qu? error posee esta l?nea de c?digo ##

Notas$Primer 

	
apply(Notas[,2:3],2, mean)

apply(Notas[,2:4],2,mean)
 
apply(Notas,2,mean)
       
# Podemos transformar una matriz en un Data Frame #

A <- matrix(1:10,2,5)
 
data.class(A)

Adf <- data.frame(A)
 
data.class(Adf)

B <- matrix(1:15,ncol = 3)
data.class(B)

Bdf <- as.data.frame(B)
data.class(Bdf)

#5.3 C?lculos sobre subgrupos de datos

sexo <- c("M","F","M")

Notas2 <- data.frame(x1,x2,x3,sexo)

colnames(Notas2) <- c("Nombre","Primer parcial","Segundo Parcial","G?nero")

# Calculemos el promedio en cada uno de los parciales por g?nero

tapply(Notas2[,2],Notas2[,4],mean)

tapply(Notas2[,2:3],Notas2[,4],mean)


######################
####### LISTAS #######
######################

# Generando una lista #

x <- matrix(1:4, 4, 7)
y <- "Hoy es jueves"
z <- c(T, F, T, NA)
lista <- list(x,y,z)

lista

lista[[2]]

# Se asigna un  nombre con names(): 

names(lista) <- c("Matriz", "Leyenda","Vector Logico")
lista

# Para referirse a un elemento de la lista, adem?s del uso de los corchetes,  tenemos dos formas m?s si hay nombres asignados:

lista$Leyenda

lista[["Leyenda"]]

# las listas tienen asociada longitud y no dimensi?n. 
  
length(lista)

names(lista)


# Podemos agregar o eliminar elementos a una lista

lista2 <- lista[-1] #Eliminamos el primer elemento
lista2

lista[[4]] <- c(1,2,3)  # Agregamos una nueva componente a nuestra lista de 3 elementos
lista

# Una lista particular: dimnames

#Podemos agregar nombres a las filas y columnas de una matriz (no de  un data frame) creando esa lista y asign?ndosela a  dimnames(A). 

notas <- c(10,9,10,9,8,10)
notas <- matrix(notas,3,2,byrow=T)
notas
   
dimnames(notas) <- list(c("Juan","Ana","Pedro"),c("Parcial","Final"))

notas
     
#Si se quiere dar nombres unicamente a las columnas ? a las filas utilice NULL: 

dimnames(notas) <- list(NULL,c("Parcial", "Final"))
notas

dimnames(notas) <- list(c("Juan","Ana","Pedro"),NULL)
notas
      
###############################
####### OBJETOS CREADOS #######
###############################

ls()   # f?jese cu?les son los objetos que ha creado.     

objects() ## otra forma de listar todos los objetos creados
         
rm("nombre objeto") ## para eliminar un objeto creado

rm("nombre objeto 1","nombre objeto 2")

rm(list=ls(all=TRUE)) ## elimina todos los objetos creados

