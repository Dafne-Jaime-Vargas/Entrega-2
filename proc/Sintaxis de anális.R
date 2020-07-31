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
  # Ayudante: Sebastián Cortínez Rojas
  # Fecha: "31 de julio"
  ---
  
  
  # ---- Parte 1: Librerías principales a utilizar en el análisis ---- 

pacman::p_load(dplyr,  #Manipulacion de datos
               stargazer, ggplot2, #Tablas
               sjmisc, # Tablas
               summarytools,  # Tablas
               kableExtra, #Tablas
               sjPlot, #Tablas y gráficos
               corrplot, # Correlaciones
               sessioninfo, # Información de la sesión de trabajo
               devtools,
               webshot,
               ggpubr, #graficos
               gridExtra, #unir graficos
               texreg, #mostrar regresion multiple
               fastDummies, # Crear variable dummy
               sjlabelled, #etiquetas variables
               coefplot) # graficos de coeficientes



# ---- Parte 2: Datos (base procesada) ---- 

# Cargar base de datos y fijarnos en nuestro espacio de trabajo (Enviroment). 

load("/cloud/project/input/data/proc/CEP_base_proc.RData") 

# Exploramos los nombres de las variables  

names(CEP_base_proc) # Muestra los nombres de las variables de la base de datos
dim(CEP_base_proc) # Dimensiones

# ---- Parte 3: Descripción de variables ---- 

# ---- Parte 3.1: Exploración ---- 

# Seleccionamos las variables que queremos incluir en el modelo

CEP_base_proc <- CEP_base_proc %>% select(ind_nota_gob, #Dependiente
                                          identidad_pol,
                                          identidad_pol_Derecha,
                                          identidad_pol_Centro,
                                          identidad_pol_Izquierda,
                                          identidad_pol_Ninguno,
                                          ind_disp_reg_eco,
                                          posicion_soc,
                                          sexo, #control
                                          sexo_1,
                                          sexo_2,
                                          nivel_educ, #control
                                          `nivel_educ_Básica incom`,
                                          `nivel_educ_Bas. com y Media incom`,
                                          `nivel_educ_Med. com y Ed. superior incom`,
                                          `nivel_educ_Sup. com y postgrado`,
                                          Edad) #control





# ---- Parte 3.2: tablas bivariadas  ---- 

# Gráfico de nube de puntos / scatter de ambas (respecto a la variable dependiente) 

#Grafico x1 = posicion social
graf_1 <- ggscatter(CEP_base_proc, x = "posicion_soc", y = "ind_nota_gob",
                    shape = 21, size = 3, # Forma y tamaño de puntos
                    title = "Gráfico 1. Relación entre Evaluación al desempeño del gobierno y posición social",
                    add = "reg.line", #Agregar recta de regresion
                    cor.coef = TRUE)# Agregar coeficiente correlacion

png("/cloud/project/output/graphs/graf_1.png")
plot(graf_1)
dev.off()


#Grafico x2 = identidad política
graf_2 <- plot_grpfrq(CEP_base_proc$ind_nota_gob,CEP_base_proc$identidad_pol,
                      title = "Gráfico 2. Relación entre Evaluación al desempeño del gobierno e Identidad política",
                      type = "box")

png("/cloud/project/output/graphs/graf_2.png")
plot(graf_2)
dev.off()



#Grafico x3 = índice disposición regularización económica
graf_3 <- ggscatter(CEP_base_proc, x = "ind_disp_reg_eco", y = "ind_nota_gob",
                    shape = 21, size = 3,
                    title = "Gráfico 3. Relación entre Evaluación al desempeño del gobierno y disposición de regularización económica",
                    add = "reg.line",
                    cor.coef = TRUE)

png("/cloud/project/output/graphs/graf_3.png")
plot(graf_3)
dev.off()


# ---- Parte 4: Regresión lineal ----

# ---- Parte 4.1: Análisis de datos ----

stargazer(CEP_base_proc %>% select(ind_nota_gob, ind_disp_reg_eco, posicion_soc, sexo, Edad) , type = "text")


# ---- 4.2 Tabla Regresión Múltiple con variables independientes ----

reg_1 <-lm((ind_nota_gob ~ identidad_pol_Centro + identidad_pol_Izquierda + identidad_pol_Ninguno + ind_disp_reg_eco + posicion_soc), data = CEP_base_proc)
reg_1


sjPlot::tab_model(list(reg_1), show.ci=FALSE, p.style = "stars",string.pred = "Predictores", string.est = "β",digits = 3,
                  dv.labels = c("Modelo 1"))

# ---- 4.3 Tabla Regresión Múltiple con variables de control ----

reg_2 <-lm((ind_nota_gob ~ identidad_pol_Centro + identidad_pol_Izquierda + identidad_pol_Ninguno + `nivel_educ_Bas. com y Media incom` + `nivel_educ_Med. com y Ed. superior incom` + `nivel_educ_Sup. com y postgrado` + ind_disp_reg_eco + posicion_soc + sexo_2 + Edad), data = CEP_base_proc)
reg_2


sjPlot::tab_model(list(reg_2), show.ci=FALSE, p.style = "stars",string.pred = "Predictores", string.est = "β",digits = 3,
                  dv.labels = c("Modelo 4"))

# ---- 4.3 Tabla Regresión Múltiple para presentar (con y sin variables de control) ----


tabla_2 <- sjPlot::tab_model(list(reg_1, reg_2),
                             show.se=TRUE,
                             show.ci=FALSE,
                             digits=3,
                             p.style = "stars",
                             dv.labels = c("Modelo 1", "Modelo 2"),
                             string.pred = "Predictores",
                             string.est = "β")


tab_df(tabla_2, alternate.rows = TRUE, title = "Tabla 2. Modelo de regresión múltiple", show.se=TRUE,
       show.ci=FALSE,
       digits=3,
       p.style = "stars",
       dv.labels = c("Modelo 1", "Modelo 2"),
       string.pred = "Predictores",
       string.est = "β", 
       file = "/cloud/project/output/tables/tabla_2.html")

webshot("/cloud/project/output/tables/tabla_2.html", "/cloud/project/output/tables/tabla_2.png")

# ---- Parte 5: Tabla de error ----

graf_4 <- sjPlot::plot_model(reg_2,ci.lvl = c(0.95), title = "Gráfico 5. Modelo 2, coeficiente de regresión e intervalos de confianza",vline.color = "grey",line.size = 1)

png("/cloud/project/output/graphs/gráfico_4.png")
plot(graf_4)
dev.off()

save.image("/cloud/project/input/data/proc/CEP_base_proc.RData")

# ---- Nota final: Información de la sesión de R ----
sessionInfo()


