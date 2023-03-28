
#Cargar libreria
rm(list=ls())
library(tidyverse)
library(grid)
library(gridExtra)
library(readxl)
library(ggrepel)
library(sf)
# Definamos el directorio de trabajo 
setwd("D:/CURSOS EXTENSIVOS/CURSO DE R/EXAMEN/EXAMEN_R")
#https://datos.minedu.gob.pe/dataset/obras-ejecutadas-2017-2018-por-proyecto
#cargar la base de datos
# extraemos de la carpeta ·
df <- read.xlsx("obrasejecutadas2017-2018.xlsx")

# Definamos un directorio para guardar los output de mi analisis:
output <- paste0(getwd() , "/output/")

## Preprocesamiento de los datos #
# Nombres de las columnas
colnames(df)
# Observemos los elementos 
head(x = df, 58)
# Las columnas que mas nos interesan son :
# [2] "INSTITUCION_EDUCATIVA"                             
# [3] "CARTERA DE PROYECTO"      
# [4] "Monto Contratado S/._Ob"
# [5] "Monto Contractual(vigente)S/._Sup"                          
# [6] "Contratista_Sup"                       
# [7] "Nº DE ALUMNOS" 
# [8] "DEPARTAMENTO"
# [9] "Estado_Proyecto"
# [10] "Inicio Obra"
# 
# Veamos los valores faltantes
colSums(is.na(df))

#### Analisis descriptivo ####
# Estructura del dataframe 
str(df)
#tranformamos la fecha
df$Inicio.Obra <-as.data(df$Inicio.Obra, format = "%y, %m,$a"  )
head(df$Inicio.Obra)
#posibles valores del estado de py
unique(df$Estado_Proyecto)
#filtramos  el estado de los py del 2017 a 2018
filter(Inicio.Obra >= as.date("2017-01-01") & Inicio.Obra<= as.date("2018-12-31"),
       Estado_Proyecto %in% c("EN EJECUCION", "OBRA RECEPCIONADA", "IBRA TERMINADA") )

#graficamos
graf2 <- df %>% count(Inicio.Obra, Estado_Proyecto) %>% ggplot(aes(x  = Inicio.Obra, y = n))+
  geom_line(aes(col = Estado_Proyecto))+scale_x_date(date_labels = "%b")+
  labs(y = "Numero de Alumnos", x = "")+
#mod leyenda 
  theme(legend.position = "bottom",
        legend.title = element_blank())

# Numero de instituciones educativas
NumINST <- df %>% group_by(DEPARTAMENTO) %>% 
summarise(NumINST = n()) %>% 
# Ordenamos en forma descendente
  arrange(desc(NumINST))
View(NumINST)
# NUmero de valores diferentes de la columna intituciones
unique(df$INSTITUCION_EDUCATIVA)
# graf en Barras en orden creciente
NumINST %>% 
  mutate(DEPARTAMENTO = fct_reorder(DEPARTAMENTO, desc(NumINST))) %>% 
  ggplot(mapping = aes(x = DEPARTAMENTO, y = NumINST))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90))

# Separemos los datos de las instituciones que tienen una empresa supervisora 
unique(df$Contratista_Sup)
Inst_contr <- df %>% filter(is.na(df["Contratista_Sup"])==FALSE)

#que instituciones les correponde un monto contraactual  
MontoContra <- df %>% 
  group_by(DEPARTAMENTO) %>% 
  summarize(PromedioMonto = mean(`Monto.Contractual(vigente)S/._Sup`)) %>% 
# ordenamos de forma descendente
  arrange(PromedioMonto)







