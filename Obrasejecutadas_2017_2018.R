
#Cargar tidyverse
library(tidyverse)
library(readxl)
# Definamos el directorio de trabajo 
setwd("D:/CURSOS EXTENSIVOS/CURSO DE R/EXAMEN")

##cargar la base de datos
# extraemos de la carpeta ·
Obraejec17_18 <- read_excel(path = "obrasejecutadas2017-2018.xlsx")
## Preprocesamiento de los datos #
# Nombres de las columnas
colnames(Obraejec17_18)
# Observemos los elementos 
head(x = Obraejec17_18, 56)

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
# [11] "FECHA DE TÉRMINO DE LA OBRA"
# [12] "FECHA DE RECEPCIÓN DE LA OBRA"
# Valores faltantes por columnas 
colSums(is.na(Obraejec17_18))

# Separemos los datos de las instituciones que tienen un monto contractual 
Inst_mntcont <- Obraejec17_18 %>% 
  filter(is.na(Obraejec17_18["Contratista_Sup"]) == FALSE)

#procedemos a eliminar esa columna del dataframe
# Rcta
Obraejec17_18 <- Obraejec17_18[, -5]
# 
# Veamos los valores faltantes
colSums(is.na(Obraejec17_18))

# Dada la cantidad de datos faltantes (comparandolas con el total)
# decidimos eliminar esas observacion NA
Obraejec17_18 <-  Obraejec17_18 %>% 
  drop_na()

#### Analisis descriptivo ####
# Estructura del dataframe 
str(Obraejec17_18)

# Numero de empresas por departamento 
# 
# NUmero de valores diferentes de la columna departamento 
unique(Obraejec17_18$INSTITUCION_EDUCATIVA)






# P1.Qué py de instituciones educativas demandaron mas de S/10 millones en contrato
# Solucion 1:Usando los verbos de dplyr como funciones 
x <- filter(obrasejecutadas2017_2018, Nº DE ALUMNOS <1500, Monto Contratado S/._Ob>10000000.0)
# Modelos
select(x, INSTITUCION_EDUCATIVA)
# Juntemos el select con el filter  
select(filter(obrasejecutadas2017_2018,  Nº DE ALUMNOS <1500, Monto Contratado S/._Ob>10000000.0), INSTITUCION_EDUCATIVA)
# Solucion 2:Usemos el operador pipe 
obrasejecutadas2017_2018%>% 
  filter(Nº DE ALUMNOS <1500, Monto Contratado S/._Ob>10000000.0) %>% 
  select(INSTITUCION_EDUCATIVA)

#abrimos el paquete microbenchmark
library(microbenchmark)
help("microbenchmark")

solucion1 <- function(){select(filter(obrasejecutadas2017_2018, Nº DE ALUMNOS <1500,
                        Monto Contratado S/._Ob>10000000.0), INSTITUCION_EDUCATIVA)}
solucion2 <- function(){obrasejecutadas2017_2018 %>% filter(Nº DE ALUMNOS <1500, 
                        Monto Contratado S/._Ob>10000000.0) %>% select(INSTITUCION_EDUCATIVA)}
microbenchmark(solucion1(),solucion2(),times = 100)

#p.2 a que instituciones les correponde un monto contraactual mayor a los S/10 mil 
#P.3 cuales son las empresas de consultoria que pagaran ese monto por inclumiento.
#p.4 que instituciones tienen mas de 1000 estuadiantes
#p.5 de estas intituciones cuales ya culminaron su ejecución
#p.6 cuales son los py que se encuentran en ejecución 
#P.7 que instituciones forman parte de la cartera de obra por impuestos
#P.8 que instituciones forman parte de la cartera emblematica
#p.9 que monto de contratacion demandaron las instituciones por cartera emblematica
#p.10 en el 2018 cuales fueron los py que se culmiaron y entregaron
#p.11 en el 2017 que py iniciaron su ejecucion con un monto de inversion mayor a los S/5 millones y menor de S/15 millones
#p.12 que intituciones recepcionaron la culminacion de la obra.







