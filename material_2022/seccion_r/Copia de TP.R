
#Ejercicio 1

install.packages("lubridate") #antes de comenzar instalamos y cargamos paquetes que nos serán útiles
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")

library(lubridate)
library(tidyverse)
library(dplyr)
library(ggplot2)

ordenes<-read.csv("Ordenes.csv")
ordenes_agrupadas<-read.csv("ordenes_agrupadas.csv")

#A)

ordenes_2<-filter(ordenes,as.Date(creation_time)=="2021-11-01") #filtramos las órdenes del 1/11/2021
ordenes_3<-merge(ordenes_2,ordenes_agrupadas) #fusionamos los dataframes en base a los order_id presentes en ambos

porcentaje_agrupadas_GLV<-nrow(filter(ordenes_3,city_code=="GLV",is_unbundled=="FALSE"))/nrow(filter(ordenes_2,city_code=="GLV"))
porcentaje_agrupadas_PLY<-nrow(filter(ordenes_3,city_code=="PLY",is_unbundled=="FALSE"))/nrow(filter(ordenes_2,city_code=="PLY"))
#solo consideramos como órdenes agrupadas las que tienen "FALSE" en is_unbundled, según indicado por el ayudante

if(porcentaje_agrupadas_GLV>porcentaje_agrupadas_PLY){
  print(paste("El porcentaje de órdenes agrupadas en GLV es",porcentaje_agrupadas_GLV/porcentaje_agrupadas_PLY*100,"%","mayor"))
} else if(porcentaje_agrupadas_GLV==porcentaje_agrupadas_PLY){
  print(paste("El porcentaje de órdenes agrupadas no difiere entre las ciudades"))
} else {
  print(paste("El porcentaje de órdenes agrupadas en PLY es",porcentaje_agrupadas_PLY/porcentaje_agrupadas_GLV*100,"%","mayor"))
} #notar que R no nos tira error si alguna de las ciudades no tiene órdenes agrupadas (es decir, si dividimos por cero)

#B)

#asumimos que cada fila corresponde a un repartidor diferente (pues no hay manera de relacionarlas) y que todos los campos del data frame corresponden a los últimos 30 días (no sabemos qué día es hoy)

ordenes_4<-filter(ordenes,final_status=="DeliveredStatus") #eliminamos las ordenes canceladas

pickup_time<-ymd_hms(ordenes_4$pickup_time)
enters_delivery<-ymd_hms(ordenes_4$enters_delivery)
time_diff<-as.vector(difftime(enters_delivery,pickup_time,units="secs"))
avg_speed<-ordenes_4$pd_dist/time_diff
ordenes_5<-cbind(ordenes_4,avg_speed) #medimos la velocidad en metros por segundo (suponiendo distancia en metros)

ordenes_agrupadas_2<-merge(ordenes_5,ordenes_agrupadas,all=TRUE) #fusionamos ambos data.frames, manteniendo las ordenes no agrupadas
ordenes_agrupadas_2<-ordenes_agrupadas_2[order(ordenes_agrupadas_2$bundle_id,ordenes_agrupadas_2$pd_dist),] #ordenadas primero por bundle_id y luego por distancia de menor a mayor

ordenes_agrupadas_3<-ordenes_agrupadas_2%>% 
  group_by(bundle_id) %>% #realizamos la operación por cada bundle
  mutate(keep = case_when(
    is.na(bundle_id)~T, #dejamos las órdenes no agrupadas
    row_number()==1~T, #dejamos la primer orden de cada bundle (recordar que estaban ordenadas por distancia de menor a mayor, es decir, dejamos la de menor distancia)
    T~F))

ordenes_agrupadas_3<-filter(ordenes_agrupadas_3,keep==T)%>%select(-keep)%>%ungroup() #eliminamos la columna agregada y deshacemos la agrupación

#conseguimos un data.frame que considera solo la trayectoria al primer punto de entrega en caso de ser orden agrupada

ordenes_agrupadas_3<-ordenes_agrupadas_3[order(ordenes_agrupadas_3$city_code),]

x=0
for(city in ordenes_agrupadas_3$city_code){
  if(is.na(city)=="FALSE"){
    x=x+1
    print(paste("La velocidad promedio del repartidor",x,"de la ciudad",city,"es",sum((filter(ordenes_5,city_code==city)$avg_speed)/nrow(filter(ordenes_5,city_code==city))),"m/s"))} 
  else{
    next}
} #velocidad promedio por repartidor de cada ciudad

previous_city=NA
for(city in ordenes_agrupadas_3$city_code){
  if(city %in% previous_city=="FALSE"){
  print(paste("La velocidad promedio en",city,"es",sum((filter(ordenes_agrupadas_3,city_code==city)$avg_speed)/nrow(filter(ordenes_agrupadas_3,city_code==city))),"m/s"))
  previous_city<-c(city,previous_city)}
  else{
    next
  }
} #velocidad promedio por ciudad

#Ejercicio 2

datos<-read.csv("dataset_ejercicio2.csv")
c<-datos$id
for (i in 1:nrow(datos)) {               
  if(is.na(c[i])=="TRUE"){
    datos<-datos[-i,]
  }
} # eliminamos los id con NA
attach(datos)

#A)

#1)
summary(Total.Time) #summary nos otorga info básica sobre la variable

hist(Total.Time,main="Tiempo de Espera Total",xlab="Tiempo",ylab="Cantidad de clientes",col="light blue")

mean(datos[datos[,"transport"]=="BICYCLE","Total.Time"],na.rm = TRUE)
sd(datos[datos[,"transport"]=="BICYCLE","Total.Time"],na.rm = TRUE)

mean(datos[datos[,"transport"]=="CAR","Total.Time"],na.rm = TRUE)
sd(datos[datos[,"transport"]=="CAR","Total.Time"],na.rm = TRUE)

mean(datos[datos[,"transport"]=="MOTORBIKE","Total.Time"],na.rm = TRUE)
sd(datos[datos[,"transport"]=="MOTORBIKE","Total.Time"],na.rm = TRUE)

#no se detecta variabilidad significativa en el tiempo de según el tipo de transporte
#graficar sobre el histograma

datos_sin_cancelados<-datos[final_status=="DeliveredStatus",]
hist(datos_sin_cancelados$Total.Time,main="Tiempo de Espera Entregados",xlab="Tiempo",ylab="Cantidad de clientes",col="light blue")
summary(datos_sin_cancelados$Total.Time)

datos_cancelados<-datos[final_status=="CanceledStatus",]
hist(datos_cancelados$Total.Time,main="Tiempo de Espera Cancelados",xlab="Tiempo",ylab="Cantidad de clientes",col="light blue")
summary(datos_cancelados$Total.Time)

#algunos cancelaron porque se arrepintieron, otros porque tardó mucho. En el medio no hay mucha densidad de datos

#sin cancelados no se detecta variabilidad significativa

#2)
summary(total_real_distance)
hist(total_real_distance,main="Distancia Total",xlab="Distancia",ylab="Cantidad de clientes",col="light blue")
sd(total_real_distance,na.rm = TRUE)

summary(datos[datos[,"transport"]=="BICYCLE","total_real_distance"],na.rm = TRUE)#debajo de la media poblacional
sd(datos[datos[,"transport"]=="BICYCLE","total_real_distance"],na.rm = TRUE) 
summary(datos[datos[,"transport"]=="CAR","total_real_distance"],na.rm = TRUE)#por encima de la media poblacional
sd(datos[datos[,"transport"]=="CAR","total_real_distance"],na.rm = TRUE)
summary(datos[datos[,"transport"]=="MOTORBIKE","total_real_distance"],na.rm = TRUE) #en el medio de las otras dos
sd(datos[datos[,"transport"]=="MOTORBIKE","total_real_distance"],na.rm = TRUE)

par(mfrow=c(2,2))
y<-lm(Total.Time~total_real_distance)
plot(y)
summary(y)
cor.test(total_real_distance,Total.Time) #prácticamente no hay correlación
#recordar analizar las conclusiones

#3)
for(i in 1:5){
  if(i==1){
    print(paste("la cantidad de reasignados",i,"vez es",nrow(datos[datos[,"number_of_assignments"]>i,])))
  } else {
    print(paste("la cantidad de reasignados",i,"veces es",nrow(datos[datos[,"number_of_assignments"]>i,])))
  }
}

for(i in 1:5){
  if(i==1){
    print(paste("la probabilidad de que sea reasignado",i,"vez es",nrow(datos[datos[,"number_of_assignments"]>i,])/nrow(datos)))
  } else {
    print(paste("la probabilidad de que sea reasignado",i,"veces es",nrow(datos[datos[,"number_of_assignments"]>i,])/nrow(datos)))
  }
}

#B)
datos_reasignaciones<-datos[datos[,"number_of_assignments"]>1,]
datos_reasignaciones$number_of_assignments<-as.numeric(lapply(datos_reasignaciones$number_of_assignments,function(x)x-1)) #convertimos la columna "numero de asignaciones" en "número de reasignaciones"

#evaluamos la relación entre reasignaciones y tiempo total
mean(datos_reasignaciones$Total.Time,na.rm=TRUE) #notar que es mayor a la media sin discriminar por reasignaciones
for(i in 1:5){
  print(paste("el tiempo promedio en caso de",i,"reasignaciones es",mean(datos_reasignaciones[datos_reasignaciones[,"number_of_assignments"]==i,"Total.Time"],na.rm = TRUE)))
} #notar que con una reasignación el tiempo medio aumenta. La baja a partir de dos reasignaciones puede estar explicada por la disminución considerable del tamaño de muestra (no es suficientemente representativa para realizar inferencia estadística)
cor.test(datos_reasignaciones$Total.Time,datos_reasignaciones$number_of_assignments) #ver si dejar

#evaluamos la relación entre reasignaciones y medio de transporte
datos_reasignaciones_2<-datos_reasignaciones
for (i in 1:nrow(datos_reasignaciones)) { 
  if(datos_reasignaciones_2[i,"transport"]=="CAR"){
    datos_reasignaciones_2[i,"transport"]<-1
  } else if(datos_reasignaciones_2[i,"transport"]=="BICYCLE"){
    datos_reasignaciones_2[i,"transport"]<-2
  } else{
  datos_reasignaciones_2[i,"transport"]<-3}
}
cor.test(as.numeric(datos_reasignaciones_2$transport),datos_reasignaciones_2$number_of_assignments) #ver si dejar

mean(datos_reasignaciones[datos_reasignaciones[,"transport"]=="CAR","number_of_assignments"],na.rm = TRUE)
mean(datos_reasignaciones[datos_reasignaciones[,"transport"]=="BICYCLE","number_of_assignments"],na.rm = TRUE)
mean(datos_reasignaciones[datos_reasignaciones[,"transport"]=="MOTORBIKE","number_of_assignments"],na.rm = TRUE)

#evaluamos la relación entre reasignaciones y distancia total
mean(datos_reasignaciones$total_real_distance,na.rm=TRUE) #notar que es mayor a la media sin discriminar por reasignaciones
for(i in 1:5){
  print(paste("la distancia promedio en caso de",i,"reasignaciones es",mean(datos_reasignaciones[datos_reasignaciones[,"number_of_assignments"]==i,"total_real_distance"],na.rm = TRUE)))
} #notar que con una a tres reasignaciones la distancia media aumenta. La baja a partir de cuatro reasignaciones puede estar explicada por la disminución considerable del tamaño de muestra (no es suficientemente representativa para realizar inferencia estadística)
cor.test(datos_reasignaciones$total_real_distance,datos_reasignaciones$number_of_assignments) #ver si dejar

#C)
llegada_pedido<-ymd_hms(courier_enters_pickup_time_local)
retiro_pedido<-ymd_hms(pickup_time_local)
waiting_time_at_pickup<-as.vector(round(difftime(retiro_pedido,llegada_pedido,units="mins"),2)) #Armamos un vector de tiempo de espera
datos_2<-cbind(datos,waiting_time_at_pickup)

d<-datos_2$waiting_time_at_pickup
for (i in 1:nrow(datos_2)) {               
  if(is.na(d[i])=="TRUE"){
  datos_2[i,"waiting_time_at_pickup"]<-""}
} #Eliminamos los NA

datos_2$waiting_time_at_pickup<-as.numeric(datos_2$waiting_time_at_pickup)

if(is.na(id)=="TRUE"){
  datos_2_sin_NA<-datos[-i,]
} #ver como sacar los NA

summary(waiting_time_at_pickup)
sd(waiting_time_at_pickup,na.rm=TRUE) #observamos la alta variabilidad mencionada

#correlaciones posibles: store_address_id - is_food - 

cor.test(datos_2$store_address_id,datos_2$waiting_time_at_pickup)
cor.test(datos_2$number_of_assignments,datos_2$waiting_time_at_pickup)

