#1------------------------------------------------------------------------------

#Ejercicio 1

media<-4
if(rnorm(1,media,sqrt(5))<media){
  print("inferior")
} else{
  print("superior")
} 

#2------------------------------------------------------------------------------

#Ejercicio 2

v<-round(as.vector(c(rnorm(10))),2)
a<-NULL ; x<-NULL
mean(v)
for(i in 1:10){
  a<-if(v[i]<mean(v)){
    print("0")
  } else{
    print("1")
  }
  x<-c(x,a)
}

#3------------------------------------------------------------------------------

#Ejercicio 3

a<-NULL;w<-NULL
for(i in 1:10){
  a<-if(v[i]<mean(v)){
    print("i")
  } else{
    print("S")
  }
  w<-c(w,a)}

#4------------------------------------------------------------------------------

#Ejercicio 4

est<-481.712
b<-rbinom(17,77,0.368)
if(sum(b)<est){
  print("0") 
 } else if(sum(b)==est){
  print("1")
 } else{
  print("2")}

#5------------------------------------------------------------------------------

#Ejercicio 5

A<-matrix(rnorm(12,37,9),3,4)
A;a=NULL;w=NULL
for(i in 1:12){
  a<-if(A[i]>=35){
    print("1")
  } else{
    print("0")
  }
  w<-c(w,a)
}
w<-matrix(w,3,4)
w

#6------------------------------------------------------------------------------

#Ejercicio 6

B<-round(matrix(rexp(180,0.007),20,9),2)
B ; a=NULL; w=NULL ; x=NULL ; z=NULL
for(i in 1:9){
  a<-if(sum(B[,i])>1800){
    print("1")
  } else{
    print("0")
  }
  w<-c(w,a)
}

sum(B[,9])
for(i in 1:9){
  x<-if(sum(B[,i])>9000){
    print("1")
  } else{
    print("0")
  }
  z<-c(z,x)
}

#7------------------------------------------------------------------------------

#Ejercicio 7

k<-runif(1,76,245)
suma<-0 ; counter<-0
while(suma<k){
  suma<-runif(1)+suma
  counter<-counter+1
}

#8------------------------------------------------------------------------------

#Ejercicio 8

k<-runif(1,76,245)
suma<-0 ; counter<-0 ; pun.value<-0
while(suma<k){
  pun.value<-runif(1)
  if(pun.value>0.7){
    suma<-pun.value+suma
    counter<-counter+1
  } else if(0.7>pun.value>0.5){
    suma<-0.45+suma
    counter<-counter+1
  } else {
    suma<-suma
    counter<-counter+1
  }
}

