# ---- Parte 0: Identificación y descripción general ---- 

---
  # Título: "Trabajo 2 EM Código de Preparción"
  # Tema: Legitimidad política y posición social
  # Cátedra: Estadística Multivariada
  # Sección: 1
  # Base de datos: CEP
  # Autores: Mayron Vasquez, Ignacio Núñez, Nicolás Tobar, Alexis Ibáñez y Dafne Jaime Vargas.
  # Docente: Juan Castillo V.
  # Docente auxiliar: Valentina Andrade
  # Ayudante: 
  # Fecha: ""
  ---
  
  # ---- Parte 1: Librerías ----
install.packages("pacman")
library(pacman)


pacman::p_load(dplyr, # ajuste general de datos
               sjmisc, # descripción y exploración de base de datos
               car, # principalmente la función recode para recodificar/agrupar valores de variable
               sjlabelled, #
               stargazer,# para tabla descriptiva
               summarytools,
               fastDummies) #

rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica

# ---- Parte 2: cargamos la base de datos ---- 


load("input/data/original/Encuesta_CEP.RData")


# ---- Parte 3: selección de variables a utilizar ---- 

CEP <- Encuesta_CEP %>% select(MB_P14, #posición política
                               MB_12_1, #Legitimidad
                               MB_12_2,
                               MB_12_3,
                               MB_12_4,
                               MB_12_5,
                               MB_12_6,
                               MB_12_7,
                               MB_12_8,
                               MB_P20,
                               MB_P21,
                               MB_P13A,#Nivel social
                               FACTOR, #ponderador
                               DS_P1, #Sexo
                               DS_P4, #Nivel educacional
                               DS_P2_EXACTA) #Edad


names(CEP)

CEP <- sjlabelled::remove_all_labels(CEP)

# ---- Parte 4: procesamiento de variables ---- 

# El flujo de trabajo es el siguiente: 

#a. Descriptivo general

#b. Recodificación: de casos perdidos y otros valores (en caso necesario)

#c. Etiquetado: cambio de nombres de variables y valores (en caso necesario)

#d. Otros ajustes


## ---- 4.1 Variable Posición política ----

# 14. 	PASE TARJETA CIRCULAR “14” MÓDULO 1. Como Ud. sabe, tradicionalmente en nuestro país 
#la gente define las posiciones políticas como más cercanas a la izquierda, al centro o a la derecha.  
#En esta tarjeta representamos las distintas posiciones políticas.  
#Por favor, indíqueme, ¿con cuál Ud. se identifica o simpatiza más?

#1.	Derecha	
#2.	Centro derecha	
#3.	Centro	
#4.	Centro izquierda	
#5.	Izquierda	
#6.	Independiente (no leer)	
#7.	Ninguno (no leer)	


# ---- A. Descriptivo  general ----

frq(CEP$MB_P14)

# ---- B. Recodificación ----

CEP$MB_P14 <- car::recode(CEP$MB_P14, "c(8, 9)=NA")

# ---- C. Etiquetado ----

CEP <- CEP %>% rename("posicion_pol"=MB_P14) #Posición Política

CEP$posicion_pol <- set_label(x = CEP$posicion_pol,label = "Posición política") # Renombrar

get_label(CEP$posicion_pol) # Visualizar


CEP <- dummy_cols(CEP,select_columns = "posicion_pol")

head(CEP)#revisión



# ---- Revisión final ----

frq(CEP$posicion_pol)


## ---- 4.2 Variable Legitimidad política ----
#12. Entre 1 y 7, ¿qué nota le pondría usted al Gobierno por su gestión en…?
# Delincuencia, Pensiones, Salud, Educación, Empleo, Crecimiento económico, Transporte público y Inmigración

# ---- A. Descriptivo  general de las variables a utilizar en el índice ----

frq(CEP$MB_12_1)
frq(CEP$MB_12_2)
frq(CEP$MB_12_3)
frq(CEP$MB_12_4)
frq(CEP$MB_12_5)
frq(CEP$MB_12_6)
frq(CEP$MB_12_7)
frq(CEP$MB_12_8)


# ---- B. Recodificación de las variables a utilizar en el índice  ----

CEP$MB_12_1 <- car::recode(CEP$MB_12_1, "c(8, 9)=NA")
CEP$MB_12_2 <- car::recode(CEP$MB_12_2, "c(8, 9)=NA")
CEP$MB_12_3 <- car::recode(CEP$MB_12_3, "c(8, 9)=NA")
CEP$MB_12_4 <- car::recode(CEP$MB_12_4, "c(8, 9)=NA")
CEP$MB_12_5 <- car::recode(CEP$MB_12_5, "c(8, 9)=NA")
CEP$MB_12_6 <- car::recode(CEP$MB_12_6, "c(8, 9)=NA")
CEP$MB_12_7 <- car::recode(CEP$MB_12_7, "c(8, 9)=NA")
CEP$MB_12_8 <- car::recode(CEP$MB_12_8, "c(8, 9)=NA")


CEP$legi <- rowSums(CEP [,c('MB_12_1', 'MB_12_2', 'MB_12_3', 'MB_12_4', 'MB_12_5', 'MB_12_6', 'MB_12_7', 'MB_12_8')], na.rm=TRUE)

CEP$legitimidad_pol <- (CEP$legi/ 8)

# ---- C. Etiquetado ----

CEP$legitimidad_pol <- set_label(x = CEP$legitimidad_pol,label = "Legitimidad política") # Renombrar

get_label(CEP$legitimidad_pol) # Visualizar


# ---- Revisión final ----

frq(CEP$legitimidad_pol)


## ---- 4.3 Variable Posición social ----
#13A.	En nuestra sociedad, hay grupos que tienden a ubicarse en los niveles más altos y 
#grupos que tienden a ubicarse en los niveles más bajos. Usando la siguiente escala 
#PASAR TARJETA “13” MÓDULO 2 que va desde el nivel más alto al más bajo, ¿dónde se ubicaría Ud.?

# ---- A. Descriptivo  general ----

frq(CEP$MB_P13A)

# ---- B. Recodificación ----

CEP$MB_P13A <- car::recode(CEP$MB_P13A, "c(77, 88, 99)=NA")

# ---- C. Etiquetado ----

CEP <- CEP %>% rename("posicion_soc"=MB_P13A) #Posición Social

CEP$posicion_soc <- set_label(x = CEP$posicion_soc,label = "Posición Social") # Renombrar

get_label(CEP$posicion_soc) # Visualizar


# ---- Revisión final ----

frq(CEP$posicion_soc)


## ---- 4.4 Variable Sexo ----

# ---- A. Descriptivo  general ----

frq(CEP$DS_P1)

# ---- B. Recodificación ----

#no es necesario

# ---- C. Etiquetado ----

CEP <- CEP %>% rename("sexo"=DS_P1) #Sexo

CEP$sexo <- set_label(x = CEP$sexo,label = "Sexo") # Renombrar

get_label(CEP$sexo) # Visualizar


# ---- Revisión final ----

frq(CEP$sexo)


## ---- 4.5 Variable Nivel educacional ----

# ---- A. Descriptivo  general ----

frq(CEP$DS_P4)

# ---- B. Recodificación ----
CEP$DS_P4 <- set_labels(CEP$DS_P4,
                        labels=c( "No estudio"=0,
                                  "Educación básica incompleta"=1,
                                  "Educación básica completa"=2,
                                  "Educación media incompleta"=3,
                                  "Educación media completa"=4,
                                  "Educación sup no universitaria incompleta"=5,
                                  "Educación superior no universitaria completa"=6,
                                  "Educación superior universitaria incompleta"=7,
                                  "Educación superior universitaria completa"=8,
                                  "Estudios de posgrado, magister, doctorado"=9))



CEP$DS_P4 <- car::recode(CEP$DS_P4, "c(99)=NA")

# ---- C. Etiquetado ----

CEP <- CEP %>% rename("nivel_educ"=DS_P4) #nivel educacional

CEP$nivel_educ <- set_label(x = CEP$nivel_educ,label = "Nivel Educacional") # Renombrar

get_label(CEP$nivel_educ) # Visualizar

class("nivel_educ")
levels(CEP$nivel_educ)
# ---- D. Otros ----
#Transformación a variable Dummy para el análisis

CEP <- dummy_cols(CEP,select_columns = "nivel_educ")

head(CEP)#revisión

CEP <- dummy_cols(CEP,select_columns = c("nivel_educ") %>% select(-c("nivel_educ")))

# ---- Revisión final ----

frq(CEP$nivel_educ)


## ---- 4.6 Variable Edad ----

# ---- A. Descriptivo  general ----

frq(CEP$DS_P2_EXACTA)

# ---- B. Recodificación ----

#no es necesario

# ---- C. Etiquetado ----

CEP <- CEP %>% rename("Edad"=DS_P2_EXACTA) #Edad

CEP$Edad <- set_label(x = CEP$Edad,label = "Edad") # Renombrar

get_label(CEP$Edad) # Visualizar

# ---- Revisión final ----

frq(CEP$Edad)

## ---- 4.7 Variable percepción responsabilidad ingresos ----

# ---- A. Descriptivo  general ----

frq(CEP$MB_P20)

# ---- B. Recodificación ----

CEP$MB_P20 <- car::recode(CEP$MB_P20, "c(88, 99)=NA")

# ---- C. Etiquetado ----

CEP <- CEP %>% rename("perc_resp_ing"=MB_P20) #Percepción responsabilidad ingresos

CEP$perc_resp_ing <- set_label(x = CEP$perc_resp_ing,label = "Percepción Responsabilidad Ingresos") # Renombrar

get_label(CEP$perc_resp_ing) # Visualizar

# ---- Revisión final ----

frq(CEP$perc_resp_ing)

## ---- 4.8 Variable percepción responsabilidad sustento ----

# ---- A. Descriptivo  general ----

frq(CEP$MB_P21)

# ---- B. Recodificación ----

CEP$MB_P21 <- car::recode(CEP$MB_P21, "c(88, 99)=NA")

# ---- C. Etiquetado ----

CEP <- CEP %>% rename("perc_resp_sust"=MB_P21) #Percepción responsabilidad sustento

CEP$perc_resp_sust <- set_label(x = CEP$perc_resp_sust,label = "Percepción Responsabilidad sustento") # Renombrar

get_label(CEP$perc_resp_sust) # Visualizar

# ---- Revisión final ----

frq(CEP$perc_resp_sust)


# ---- Parte 5: generación de base de datos procesada ---- 

#Antes de guardar la base procesada, revisamos nuevamente todas las variables con una tabla descriptiva general 

# mediante la función stargazer.

stargazer(CEP, type="text")


#Guardar base de datos procesada: en nube de datos

CEP_base_proc <- CEP
CEP_original <- Encuesta_CEP


save.image("/cloud/project/CEP_base_proc.RData")


load("/cloud/project/CEP_base_proc.RData")





