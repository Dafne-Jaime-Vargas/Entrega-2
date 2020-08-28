# ---- Parte 0: Identificación y descripción general ---- 

---
  # Título: "Trabajo 2 EM Código de Preparción"
  # Tema: Legitimidad política y posición social
  # Cátedra: Estadística Multivariada
  # Sección: 1
  # Base de datos: CEP N°83 mayo del 2019
  # Autores: Mayron Vasquez, Ignacio Núñez, Nicolás Tobar, Alexis Ibáñez y Dafne Jaime Vargas.
  # Docente: Juan Castillo V.
  # Docente auxiliar: Valentina Andrade
  # Ayudante: Sebastián Cortínez Rojas
  # Fecha: "31 de julio, 2020"
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
               coefplot, # graficos de coeficientes
               texreg,
               lmtest,
               sandwich, rms, kableExtra) 


webshot::install_phantomjs()

# ---- Parte 2: Datos (base procesada) ---- 

# Cargar base de datos y fijarnos en nuestro espacio de trabajo (Enviroment). 

load("../project/input/data/proc/CEP_base_proc.RData") 

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

M <- CEP_base_proc %>% # se especifica la base de datos
  select(ind_nota_gob, ind_disp_reg_eco, posicion_soc, Edad)  # se seleccionan las variables


tab_corr(M,                                       #dependiendo de la versión puede ser sjt.corr
         triangle = "lower",   
         title = "Tabla 2. Correlación", 
         file = "output/tables/tabla_corr.html")


webshot("output/tables/tabla_corr.html", "output/tables/tabla_corr.png") 


# Gráfico de nube de puntos / scatter de ambas (respecto a la variable dependiente) 

#Grafico x1 = posicion social
graf_1 <- ggscatter(CEP_base_proc, x = "posicion_soc", y = "ind_nota_gob",
                    shape = 21, size = 3, # Forma y tamaño de puntos
                    title = "Gráfico 1. Relación entre Evaluación al desempeño del gobierno 
                    y posición social",
                    add = "reg.line", #Agregar recta de regresion
                    cor.coef = TRUE)# Agregar coeficiente correlacion

png("../project/output/graphs/graf_1.png")
plot(graf_1)
dev.off()


#Grafico x2 = identidad política
graf_2 <- plot_grpfrq(CEP_base_proc$ind_nota_gob,CEP_base_proc$identidad_pol,
                      title = "Gráfico 2. Relación entre Evaluación al desempeño del gobierno e Identidad política",
                      type = "box")

png("../project/output/graphs/graf_2.png")
plot(graf_2)
dev.off()



#Grafico x3 = índice disposición regularización económica
graf_3 <- ggscatter(CEP_base_proc, x = "ind_disp_reg_eco", y = "ind_nota_gob",
                    shape = 21, size = 3,
                    title = "Gráfico 3. Relación entre Evaluación al desempeño del gobierno 
                    y disposición de regularización económica",
                    add = "reg.line",
                    cor.coef = TRUE)

png("../project/output/graphs/graf_3.png")
plot(graf_3)
dev.off()


# ---- Parte 4: Regresión lineal ----

# ---- Parte 4.1: Análisis de datos ----

stargazer(CEP_base_proc %>% select(ind_nota_gob, ind_disp_reg_eco, posicion_soc, sexo, Edad) , type = "text")


# ---- 4.2 Tabla Regresión Múltiple con variables independientes ----

reg_1 <-lm((ind_nota_gob ~ identidad_pol_Centro + identidad_pol_Izquierda + identidad_pol_Ninguno + ind_disp_reg_eco + posicion_soc), data = CEP_base_proc)
reg_1


sjPlot::tab_model(list(reg_1), show.ci=FALSE, 
                  p.style = "stars", #dependiendo de la versión puede ser asterisk
                  string.pred = "Predictores", string.est = "β",digits = 3,
                  dv.labels = c("Modelo 1"))

# ---- 4.3 Tabla Regresión Múltiple con variables de control ----

reg_2 <-lm((ind_nota_gob ~ identidad_pol_Centro + identidad_pol_Izquierda + identidad_pol_Ninguno + `nivel_educ_Bas. com y Media incom` + `nivel_educ_Med. com y Ed. superior incom` + `nivel_educ_Sup. com y postgrado` + ind_disp_reg_eco + posicion_soc + sexo_2 + Edad), data = CEP_base_proc)
reg_2


sjPlot::tab_model(list(reg_2), show.ci=FALSE, 
                  p.style = "stars", #dependiendo de la versión puede ser asterisk
                  string.pred = "Predictores", string.est = "β",digits = 3,
                  dv.labels = c("Modelo 2"))

# ---- 4.3 Tabla Regresión Múltiple para presentar (con y sin variables de control) ----


tabla_2 <- sjPlot::tab_model(list(reg_1, reg_2),
                             show.se=TRUE,
                             show.ci=FALSE,
                             digits=3,
                             p.style = "stars", #dependiendo de la versión puede ser asterisk
                             dv.labels = c("Modelo 1", "Modelo 2"),
                             string.pred = "Predictores",
                             string.est = "β")


tab_df(tabla_2, alternate.rows = TRUE, title = "Tabla 2. Modelo de regresión múltiple", show.se=TRUE,
       show.ci=FALSE,
       digits=3,
       p.style = "stars", #dependiendo de la versión puede ser asterisk
       dv.labels = c("Modelo 1", "Modelo 2"),
       string.pred = "Predictores",
       string.est = "β", 
       file = "../project/output/tables/tabla_2.html")

webshot("output/tables/tabla_2.html", "output/tables/tabla_2.png")

# ---- Parte 5: Tabla de error ----

graf_4 <- sjPlot::plot_model(reg_2,ci.lvl = c(0.95), title = "Gráfico 4. Modelo 2, coeficiente de regresión e intervalos de confianza",vline.color = "grey",line.size = 1)

png("../project/output/graphs/gráfico_4.png")
plot(graf_4)
dev.off()


# ---- Parte 6: Revisión de supuestos ----

# ---- 6.1 Casos atípicos ----
n<- nobs(reg_2) #n de observaciones
k<- length(coef(reg_2)) # n de parametros
dcook<- 4/(n-k-1) #punto de corte


final <- broom::augment_columns(reg_2,data = CEP_base_proc)
final$id <- as.numeric(row.names(final))

# identify obs with Cook's D above cutoff

graf_5 <- ggplot(final, aes(id, .cooksd))+
  ggtitle ("Gráfico 5. Identificación de casos influyentes") + # Título del gráfico +
  geom_bar(stat="identity", position="identity")+
  xlab("Obs. Number")+ylab("Cook's distance")+
  geom_hline(yintercept=dcook)+
  geom_text(aes(label=ifelse((.cooksd>dcook),id,"")),
            vjust=-0.2, hjust=0.5)

png("../project/output/graphs/gráfico_5.png")
plot(graf_5)
dev.off()

#Modificar modelo
ident<- final %>% filter(.cooksd>dcook)
CEP_base_proc_2 <- final %>% filter(!(id %in% ident$id))

reg_3<- lm((ind_nota_gob ~ identidad_pol_Centro + identidad_pol_Izquierda + identidad_pol_Ninguno + `nivel_educ_Bas. com y Media incom` + `nivel_educ_Med. com y Ed. superior incom` + `nivel_educ_Sup. com y postgrado` + ind_disp_reg_eco + posicion_soc + sexo_2 + Edad), data = CEP_base_proc_2)

sjPlot::tab_model(list(reg_3), show.ci=FALSE, 
                  p.style = "stars", #dependiendo de la versión puede ser asterisk
                  string.pred = "Predictores", string.est = "β",digits = 3,
                  dv.labels = c("Modelo 3"))

tabla_3 <- sjPlot::tab_model(list(reg_2, reg_3),
                             show.se=TRUE,
                             show.ci=FALSE,
                             digits=3,
                             p.style = "stars", #dependiendo de la versión puede ser asterisk
                             dv.labels = c("Modelo 2", "Modelo 3"),
                             string.pred = "Predictores",
                             string.est = "β")


tab_df(tabla_3, alternate.rows = TRUE, title = "Tabla 3. Modelo de regresión múltiple, sin casos influyentes", show.se=TRUE,
       show.ci=FALSE,
       digits=3,
       p.style = "stars", #dependiendo de la versión puede ser asterisk
       dv.labels = c("Modelo 2", "Modelo 3"),
       string.pred = "Predictores",
       string.est = "β", 
       file = "../project/output/tables/tabla_3.html")

#Save
webshot("output/tables/tabla_3.html", "output/tables/tabla_3.png")

# ---- 6.2 Linealidad ----

graf_dr <- ggplot(reg_3, aes(.fitted, .resid)) +
  ggtitle ("Gráfico 4. Distribución de residuos") + # Título del gráfico
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = TRUE)

png("../project/output/graphs/gráfico_dr.png")
plot(graf_dr)
dev.off()

#Edad

CEP_base_proc_2$Eddad <- CEP_base_proc_2$Edad^2
reg_4 <- lm((ind_nota_gob ~ identidad_pol_Centro + identidad_pol_Izquierda + identidad_pol_Ninguno + `nivel_educ_Bas. com y Media incom` + `nivel_educ_Med. com y Ed. superior incom` + `nivel_educ_Sup. com y postgrado` + ind_disp_reg_eco + posicion_soc + sexo_2 + Edad + Eddad), data = CEP_base_proc_2)


#gráfico

Eddad<- reg_4$model$Eddad

fit<- reg_4$fitted.values

data01 <- as.data.frame(cbind(Eddad,fit))


graf_edad <- ggplot(data01, aes(x = Eddad, y = fit)) +
  ggtitle ("Gráfico 5. Efecto cuadrátio en Edad") + # Título del gráfico
  theme_bw() +
  geom_point()+
  geom_smooth()

#save
png("../project/output/graphs/graf_edad.png")
plot(graf_edad)
dev.off()

#dejar para paper
labs03 <- c("Intercepto", "Identidad política: Centro", "Identidad política: Izquierda", "Identidad política: Ninguno", "Niv. Educacional: Básica com y Media incom", 
            "Niv. Educacional: Media com y Ed. Superior incom", "Niv. Educacional: Superior com y Postgrado",
            "Índice de disposición a la regularización económica", "Posición Social", "Sexo (mujer=2)","Edad","Edad²")

screenreg(list(reg_3, reg_4), doctype = FALSE, p.style = "stars",
        custom.model.names = c("Modelo 3", "Modelo 4"), 
        custom.coef.names = labs03)


tabla_4 <- sjPlot::tab_model(list(reg_3, reg_4),
                             show.se=TRUE,
                             show.ci=FALSE,
                             digits=3,
                             p.style = "stars", #dependiendo de la versión puede ser asterisk
                             dv.labels = c("Modelo 3", "Modelo 4"),
                             string.pred = "Predictores",
                             string.est = "β",
                             custom.coef.names = labs03)


tab_df(tabla_4, alternate.rows = TRUE, title = "Tabla 4. Modelo de regresión múltiple, Edad polinomica", show.se=TRUE,
       show.ci=FALSE,
       digits=3,
       p.style = "stars", #dependiendo de la versión puede ser asterisk
       dv.labels = c("Modelo 3", "Modelo 4"),
       string.pred = "Predictores",
       string.est = "β", 
       custom.coef.names = labs03,
       file = "../project/output/tables/tabla_edad.html")

webshot("output/tables/tabla_edad.html", "output/tables/tabla_edad.png")

# ---- 6.3 Homocedasticidad ----
car::ncvTest(reg_3)

lmtest::bptest(reg_3)

#prueba de robustez

model_robust <- coeftest(reg_3, vcov=vcovHC)

labs04 <- c("Intercepto", "Identidad política: Centro", "Identidad política: Izquierda", "Identidad política: Ninguno", "Niv. Educacional: Básica com y Media incom", 
            "Niv. Educacional: Media com y Ed. Superior incom", "Niv. Educacional: Superior com y Postgrado",
            "Índice de disposición a la regularización económica", "Posición Social", "Sexo (mujer=2)","Edad")


tabla_Rob<- screenreg(list(reg_3, model_robust), doctype = FALSE, p.style = "stars",  file = "../project/output/tables/tabla_Rob.html",
              custom.model.names = c("Modelo 4","Modelo 4Robust"), custom.coef.names = labs04) 
             
#Save
webshot("output/tables/tabla_Rob.html", "output/tables/tabla_Rob.png")

htmlreg(list(reg_3, model_robust), doctype = FALSE, p.style = "stars", tittle = "Tabla 6.",
        custom.model.names = c("Modelo 4","Modelo 4Robust"), custom.coef.names = labs04)


# ---- 6.4 Multicolinealidad ----
car::vif(reg_2)
car::vif(reg_3)

kable(vif(reg_3), format = "html", caption = "Tabla 7. Prueba de colinealidad", file = "../project/output/tables/tabla_VIF.html", col.names = c("VIF"))

webshot("output/tables/tabla_VIF.html", "output/tables/tabla_VIF.png")

#guardar base de datos

save.image("../project/input/data/proc/CEP_base_proc.RData")
save.image("../project/input/data/proc/CEP_base_proc_2.RData")
save.image("../project/input/data/proc/CEP_base_proc_2.RData")

# ---- Nota final: Información de la sesión de R ----
sessionInfo()


