#1------------------------------------------------------------------------------

#Ejercicio 1

dias<-list(c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo"))
for(dia in dias) {
  print(paste("Hoy es",dia))
}

#2------------------------------------------------------------------------------

#Ejercicio 2

mayor_a_5<-function(x){
  sum(x>=5)}

#3------------------------------------------------------------------------------

#Ejercicio 3

media_filas<-function(x){
  while(is.numeric(x)==TRUE)
  print(colMeans(x))
  break
}
break

media_filas<-function(x){
  i<-ncol(x)
  for(x[,i] in 1:i){
    while(is.numeric(x)==TRUE){
    print(mean(x))
    }
  } else{
    print("Not Numeric")
  }
}

#4------------------------------------------------------------------------------

#Ejercicio 4

z<-c(3.585507,4.690701,4.369169,5.185066,4.980251,3.957385,4.885074,4.991899,5.286277,3.090430,4.401031,3.894821,4.849371,4.771315,4.490711,5.230589,3.891744,4.727699,4.325100,4.117442)

test_hipótesis<-function(x){
  if(t.test(x,mu=5)$p.value>0.1){
    print("No se rechaza H0")
  } else{
    print("Se rechaza H0")
  }
}

#5------------------------------------------------------------------------------

#Ejercicio 5

producto<-function(n,m){
  total=0
  for(i in 1:m){
    total=total+n
  }
  print(total)
}  
potencia<-function(n,m){
  n^m
}

#6------------------------------------------------------------------------------

#Ejercicio 6

función_factorial<-function(x){
  for(i in 1:x){
    factorial<-factorial(i)
  } 
  print(factorial)
}
