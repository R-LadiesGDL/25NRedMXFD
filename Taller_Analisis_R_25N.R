# Instalar paqueterias (en caso de no tenerlas instaladas, se ejecuta solo una vez)
install.packages("dplyr")
install.packages("ggplot2")
install.packages("plotly")
install.packages("scale")

# Cargar libreria
library(dplyr)    #Transformar dataframe
library(ggplot2)  #Geneación de gráficos
library(plotly)   #Interacción de gráficos
library(scales)   #Propiedades de etiquetas y escalas
library(tidyverse)#Paquetería para la manipulación y transformación

# Importar base de datos de víctimas del Secretariado
victimas<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQJiaSZ5Q0rzKdUREJFscR0xrDy3-2HrOQMfp89L7qoRtkO5haKeQBgNTzKB3vywMyvrE7LBNA0N1mS/pub?gid=50735521&single=true&output=csv", encoding = "UTF-8")

# Dimensión número de filas y columnas
dim(victimas)

# Nombre de columnas
colnames(victimas)

# Ver que delitos integran la columna "subtipo de delitos"
unique(victimas$Subtipo.de.delito)

# Sólo analizaremos Feminicidios y Homicidios dolosos, así que haremos un vector con estos delitos
delitos<- c("Feminicidio", "Homicidio doloso")

#Hacemos sumatoria anual por los delitos que corresponden a mujeres
victimas %>% 
  pivot_longer(cols = Enero:Diciembre,
               names_to = "Mes",
               values_to = "Total") %>% 
  filter(Sexo=="Mujer",
         Subtipo.de.delito%in%delitos) %>% 
  group_by(Año, Subtipo.de.delito) %>% 
  summarise(Total=sum(Total, na.rm = T))->victimas_anual

#Visualización anual de líneas y puntos de muertes violentas
victimas_anual %>% 
  mutate(
    Año=factor(Año,
               levels=c(2015, 2016, 2017, 2018, 
                        2019, 2020, 2021, 2022))) %>% #Factor
  ggplot() + #Gráfico
  geom_line(aes(x=Año, y=Total, group=Subtipo.de.delito, color=Subtipo.de.delito), size=2) +
  geom_point(aes(x=Año, y=Total, fill=Subtipo.de.delito, color=Subtipo.de.delito), size=12)+
  geom_text(aes(x=Año, y=Total, label=comma(Total), hjust=.5, vjust=.5), colour="White")+
  scale_color_manual(values = c("#3aa69d", "#66349e"))+
  scale_y_continuous(labels = comma)+
  labs(title = "Total de muertes violentas de mujeres de 2015 a septiembre 2022",
       x= "",
       y="",
       fill="Delito", colour="Delito",
       caption = "Fuente: Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SESNSP).")+
  theme(axis.text.x = element_text(angle = 45), legend.position = "bottom") +
  theme_minimal() + 
  theme(text=element_text(size=11),
        legend.position='bottom',
        plot.title = element_text(size = 14, hjust = 0, color = "black"),
        plot.caption = element_text(size = 10, hjust = 0, face = "italic"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=11))

#Víctimas de mujeres por día
victimas_anual %>% group_by(Año) %>% 
  pivot_wider(names_from = Año, values_from = Total) %>% 
  summarise(`2015`=sum(`2015`/365),
            `2016`=sum(`2016`/365),
            `2017`=sum(`2017`/365),
            `2018`=sum(`2018`/365),
            `2019`=sum(`2019`/365),
            `2020`=sum(`2020`/365),
            `2021`=sum(`2021`/365),
            `2022`=sum(`2022`/272)) %>% 
  pivot_longer(cols = `2015`:`2022`, names_to = "Año", values_to = "Total")


########### Generación de mapas ##########
# Para la instalación
if (!require("devtools")) {
  install.packages("devtools", force=TRUE)}
devtools::install_github("diegovalle/mxmaps")

# Cargar la paquetería
library(mxmaps)

# Explorar las funciones y datos que contiene para el año 2020.
# Base de datos que almacena información al censo 2020.
data("df_mxstate_2020") 

head(df_mxstate_2020) 

# Visualización simple de las distintas capas.
# Parte de las características de "mxmaps" recae en su base, para poder georreferenciar
# con esta paquetería es necesario que los datos que queremos pintar se encuentrre en la variable:
# de df_mxstate_2020$value.

# Ejemplo 
df_mxstate_2020$value <- df_mxstate_2020$pop

mxstate_choropleth(df_mxstate_2020, 
                   num_colors = 3,
                   title = "Ejemplo: Poblacion",
                   legend = "Poblacion total")

mxhexbin_choropleth(df_mxstate_2020, num_colors=3,
                    legend = "Poblacion total")+ 
  labs(title = "Ejemplo: Poblacion total.",
       subtitle = "Mapa hexagonal")


mxhexbin_choropleth(df_mxstate_2020, 
                    legend = "Población total")+ 
  labs(title = "Ejemplo 3: Población total.",
       subtitle = "Mapa hexagonal",
       fill = "Value",
       caption = "Elaboración propia con base a 'mxmaps' de Diego Valle- Jones")+
  scale_fill_manual("Total de pobllación", 
                    values = c("mediumorchid4", "lightsalmon3",
                               "pink4", "slateblue3", "yellow2",
                               "palegreen3", "honeydew3"))+
  theme_minimal()

# MxMaps con Feminicidios
victimas%>% 
  pivot_longer(cols = Enero:Diciembre,
               names_to = "Mes",
               values_to = "Total") %>% 
  filter(Sexo=="Mujer",
         Subtipo.de.delito=="Feminicidio",
         Año=="2022") %>% 
  group_by(Entidad) %>% 
  summarise(Total=sum(Total, na.rm=T))->feminicidios_entidad

# Merge2 para población en Mxmaps con Victimas de mi base Merge_Feminicidios

unique(feminicidios_entidad$Entidad)
unique(df_mxstate_2020$state_name_official)

merge(feminicidios_entidad, df_mxstate_2020, 
      by.x="Entidad", by.y="state_name_official", all.x = TRUE)->merge

# Creación de la variable que podenere las víctimas de feminicidios por cada 100 mil habitantes.
merge %>% 
  group_by(Entidad) %>% 
  summarise(Tasa= (Total/pop)*100000)->merge_tasa

head(merge_tasa)

merge(df_mxstate_2020, merge_tasa, 
      by.x="state_name_official", by.y="Entidad", all.x = TRUE)->merge_final

head(merge_final)

merge_final$value<-NULL #Eliminar value

library(data.table)
setnames(merge_final, old = "Tasa", new="value") #La transformación de las tasas a "value".

mxhexbin_choropleth(merge_final) +  
  labs(title="Feminicidios por cada 100 mil habitantes por entidad en México, 2020",
       fill="Tasa") +
  theme_minimal() 

#Añadir propiedades de diseño
mxhexbin_choropleth(merge_final, num_colors = 1) +  
  labs(title="Feminicidios por cada 100 mil habitantes por entidad en México, 2022", 
       caption="Elaborado por R-Ladies GDL con base al SESNSP, 2022.",
       fill="Tasa") +
  scale_fill_gradient(
    low = "#bfafe0", 
    high = "#921ef7",
    guide = "colourbar")+
  theme_minimal()->TasaMapa

#Mapa final
ggplotly(TasaMapa)            




  
