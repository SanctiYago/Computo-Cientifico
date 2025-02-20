require(insuranceData)
require(tidyverse)
require(dplyr)
require(skimr)
require(ggplot2)

datos<-data("dataCar")
datos

table(dataCar$numclaims)

# Porcenyaje de polizas con reclamaciones 
reclamaciones<-sum(dataCar$numclaims>=0)
reclamaciones1<-sum(dataCar$numclaims>=1)

Porcentaje<-reclamaciones1/reclamaciones
Porcentaje*100

# TOP 5 vehiculos con mayor numero de reclamaciones 
sedan_coches<- subset(dataCar, veh_body== 'SEDAN')
count(sedan_coches)

num_sedan <- sum(dataCar$veh_body == "SEDAN")
num_sedan

reclamaciones_sedan<-num_sedan>=1

table(dataCar$numclaims)


bus_coches<- subset(dataCar, veh_body== 'BUS')
count(bus_coches)

convt_coches<- subset(dataCar, veh_body== 'CONVT')
count(convt_coches)


num_sedan1 <- (dataCar$veh_body == "SEDAN")
num_sedan1



# En esta parte, filtramos los vehiculos que tienen las reclamaciones.
sedan<-filter(dataCar, num_sedan1)
sum(sedan$numclaims)

# Ahora, podemos sacar el top 5 de todos los vehiculos.

# Agrupar por tipo de vehículo y sumar el número de reclamaciones
top_vehiculos <- aggregate(numclaims ~ veh_body, data = dataCar, sum)

# Ordenar de mayor a menor número de reclamaciones
top_vehiculos <- top_vehiculos[order(-top_vehiculos$numclaims), ]

# Seleccionar el Top 5
top_5_vehiculos <- head(top_vehiculos, 5)

# Mostrar el Top 5
top_5_vehiculos



# Codigo Maestra 
claims_tipo <- dataCar%>%
  group_by(veh_body)%>%
  summarise(totclaims=sum(numclaims))%>%
  arrange(desc(totclaims))
# Para mostrar el top 5 que tenemos es
head(claims_tipo,5)

# Ahora, por el tipo de poliza que nos muestra, vamos a tener todos los coches.
pol_tipo <- dataCar%>%
  group_by(veh_body)%>%
  summarise(totpol=n())%>%
  arrange(desc(totpol))

# Mostramos el resultado
pol_tipo

# Pasemos a ggplot2, jugamos con los elementos
library(ggplot2)

ggplot(claims_tipo, aes(x = reorder(veh_body, -totclaims), y = totclaims)) +
  geom_bar(stat = "identity", fill = "#F0E68C", color = "#CD1076") +
  labs(title = "Grafica por el numero de reclamaciones", x = "Tipo de vehículo", y = "Total de Reclamaciones") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Top vehiculos con el mayor monto de reclamaciones
monto_coches <- dataCar%>%
  group_by(veh_body)%>%
  summarise(monto_total=sum(claimcst0))%>%
  arrange(desc(monto_total))

# Mostramos el resultado
monto_coches


# Ahora graficamos
ggplot(monto_coches, aes(x = reorder(veh_body, -monto_total), y = monto_total)) +
  geom_bar(stat = "identity", fill = "#F0E68C", color = "#CD1076") +
  labs(title = "Grafica por el numero de reclamaciones", x = "Tipo de vehículo", y = "Total de Reclamaciones") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Mejoramos nuestra grafica

library(ggplot2)
library(viridis)  

ggplot(monto_coches, aes(x = reorder(veh_body, -monto_total), y = monto_total, fill = veh_body)) + 
  geom_bar(stat = "identity", color = "black") +  
  geom_text(aes(label = monto_total), vjust = -0.5, size = 4, fontface = "bold") +  
  labs(title = "Número de Reclamaciones por Tipo de Vehículo",
       x = "Tipo de Vehículo",
       y = "Total de Reclamaciones",
       fill = "Tipo de Vehículo") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "none") +  
  scale_fill_viridis_d(option = "plasma")  


# Ahora lo hacemos por genero y el numero de reclamaciones 
genero <- dataCar%>%
  group_by(gender)%>%
  summarise(genero_siniestros=sum(numclaims))%>%
  arrange(desc(genero_siniestros))
# Mostramos el resultado 
genero

# Graficamos 
library(ggplot2)

ggplot(genero, aes(x = gender, y = genero_siniestros, fill = gender)) +  
  geom_bar(stat = "identity", color = "black") +  
  labs(title = "Número de reclamaciones según género", 
       x = "Género", 
       y = "Total de Reclamaciones") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  scale_fill_manual(values = c("#FFFF00", "#66CDAA")) 

# Mejoramos nuestras gráficas

library(ggplot2)
library(viridis)

ggplot(genero, aes(x = gender, y = genero_siniestros, fill = gender)) + 
  geom_bar(stat = "identity", color = "black", position = "dodge") +  
  geom_text(aes(label = genero_siniestros), vjust = -0.5, size = 5, fontface = "bold") +  
  labs(title = "Número de Reclamaciones según Género",
       x = "Género",
       y = "Total de Reclamaciones",
       fill = "Género") +  
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12, face = "bold"),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "top") +  
  scale_fill_manual(values = c("#9A32CD", "#EE6A50")) 





# Ahora clasificamos por coches y el genero
genero_auto <- dataCar%>%
  group_by(gender, veh_body)%>%
  summarise(genero_coche=sum(numclaims))%>%
  arrange(desc(genero_coche))

# Mostramos el resultado
genero_auto


# Graficamos 
ggplot(genero_auto, aes(x = gender, y = genero_coche, fill = veh_body)) + 
  geom_bar(stat = "identity", color = "black", position = "dodge") +  # <- Aquí está la clave
  labs(title = "Gráfica por el número de reclamaciones según género",
       x = "Género",
       y = "Total de Reclamaciones") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("#FFFF00", "#66CDAA", "#CD3333", "#458B00", "#EE6A50", 
                               "#CD950C", "#9A32CD", "#00CD00", "#FF00FF", "#00008B", 
                               "#EE4000", "#FF0000", "#4EEE94"))

