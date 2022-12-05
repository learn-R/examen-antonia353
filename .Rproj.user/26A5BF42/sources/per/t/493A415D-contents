#Examen Final. 

#Carga de paquetes.
pacman::p_load(dplyr, #Seleccionar  variables dentro de un set 
               tidyverse, #Universo de paquetes
               magrittr, #Para utilizar el operador pipe
               srvyr, #Para trabajar con muestras complejas
               survey,#para estimar los modelos con peso muestral
               sjPlot,#Para visualizar los datos
               haven,#Para cargar base de datos 
               sjmisc) 
install.packages("car")

#Carga de Datos. 

Data<- read_sav("input/Data/Final ENVIF 2020.sav")


#Exploración de la base de datos
view(Data) #ver base completa 
names(Data) #nombres variables 
head(Data) #primeras filas de la base de datos


frq(Data$p2) #Nacionalidad
frq(Data$EDAD_REC) #edad en tramos
frq(Data$p3) #Estado civil actual
frq(Data$p15) #Relación con el (la) principal sostenedor (a) del hogar
frq(Data$p7) #Nivel educacional
frq(Data$p61) #disponibilidad para gastos propios. 
frq(Data$p148_1) #negado el dinero para gastos del hogar, aun cuando hay para otras cosas. 
frq(Data$reg_rec) #región


#aparte de las variables anteriores ocuparemos el ponderador total 

#Selección de variables. 

Envif1<- select(Data, nacionalidad = p2, estado_civil = p3,sostenedorH = p15, 
                nivel_educacional = p7, dinero_gastosP = p61, negación_dinero = p148_1, 
                reg_rec, pond_total, EDAD_REC, factor_exp, SbjNum)

#recodificación variables 
Envif2<- Envif1 %>% 
  mutate(reg_rec = case_when(reg_rec %in% c(8,10,14,6,7,16,8)~ "Zona Sur",
                             reg_rec %in% c(15,1,2,3,4,5)~ "Zona Norte",
                             reg_rec %in% c(13)~ "Zona Central",
                             reg_rec %in% c(11,12)~ "Zona Austral",
                             TRUE ~ NA_character_),) %>% 
  mutate(nacionalidad = case_when(nacionalidad %in% c(1)~ "Chilena",
                                  nacionalidad %in% c(2,3,4,5,6,7,88)~ "Extranjera",
                                  TRUE ~ NA_character_),) %>% 
  mutate(EDAD_REC = case_when(EDAD_REC %in% c(1)~ "Adolescencia",
                              EDAD_REC %in% c(2,3)~ "Juventud",
                              EDAD_REC %in% c(4,5)~ "Adulta",
                              EDAD_REC %in% c(6,7)~ "Adulta Mayor",
                              TRUE ~ NA_character_),) %>% 
  mutate(estado_civil = case_when( estado_civil %in% c(1) ~ "Casada",
                                   estado_civil %in% c(2)~ "Unión Civil",
                                   estado_civil %in% c(3) ~ "anulado",
                                   estado_civil %in% c(4)~ "separada",
                                   estado_civil %in% c(5)~ "divorciada",
                                   estado_civil %in% c(6)~ "viuda",
                                   estado_civil %in% c(7)~ "soltera",
                                   TRUE ~ NA_character_),) %>% 
  mutate(sostenedorH =case_when( sostenedorH %in% c(1)~"Es la sostenedora",
                                 sostenedorH %in% c(2,3,4,5,6,7,8,9,10,11,12)~"Otro",
                                 TRUE~ NA_character_),) %>% 
  mutate(nivel_educacional = case_when(nivel_educacional %in% c(1)~ "sin estudios",
                                       nivel_educacional %in% c(2,4,6,8,10,11)~ "Nivel completo",
                                       nivel_educacional %in% c(3,5,7,9)~ "Nivel no completo",
                                       TRUE ~ NA_character_),) %>% 
  mutate(dinero_gastosP = case_when(dinero_gastosP %in% c(1)~"si",
                                    dinero_gastosP %in% c(2)~" no",
                                    TRUE ~ NA_character_),) %>% 
  mutate(negación_dinero = case_when(negación_dinero %in% c(1)~ "Nunca",
                                     negación_dinero %in% c(2)~ "solo una vez",
                                     negación_dinero %in% c(3,4,5)~ "algunas veces/reiteradas veces",
                                     negación_dinero %in% c(6)~ " No aplica",
                                     TRUE ~ NA_character_),)
  

  
  #Visualización base recodificada.
head(Envif2)

#Guardado base datos 
saveRDS(Envif2, file = "output/Data/datos_proc.rds") # se guarda la base de datos limpia para proceder al análisis.


#Analisis descriptivo
#Univariado

#Gráfico univariado de nacionalidad

plot_frq(Envif2, nacionalidad,
         title = "Gráfico de frecuencias, barras",
         type = "bar",
         save_plot("output/tablas/tab.png", fig = last_plot()))

#Tabla de frecuencia

flat_table(Envif2, nacionalidad, estado_civil, sostenedorH,
           dinero_gastosP, negación_dinero, reg_rec)
  
#Bivariado. 

#N°1
Envif2 %>% 
  group_by(nacionalidad) %>% 
  frq(negación_dinero)
#grafico 
plot_xtab(Envif2$nacionalidad, Envif2$negación_dinero, margin = "row", 
          bar.pos = "stack",
          title = "Negación del dinero para el hogar por parte del sostendor",
          show.summary = TRUE, coord.flip = TRUE)

#creación tabla de contingencia.

sjt.xtab(Envif2$nacionalidad, Envif2$negación_dinero,
         show.col.prc=TRUE,
         show.summary=FALSE, 
         encoding = "UTF-8",
         title = "Tabla de contingencia negación dinero por nacionalidad")



#N°2
Envif2 %>% 
  group_by(EDAD_REC) %>% 
  frq(negación_dinero)

#gráfico

plot_grpfrq(Envif2$EDAD_REC, Envif2$negación_dinero,
            type = "bar", title = "Gráfico de barras")
  
  
#creación tabla de contingencia.

sjt.xtab(Envif2$EDAD_REC, Envif2$negación_dinero,
         show.col.prc=TRUE,
         show.summary=FALSE, 
         encoding = "UTF-8",
         title = "Tabla de contingencia negación dinero
        por tramos de edad")


#N°3
Envif2 %>% 
  group_by(estado_civil) %>% 
  frq(negación_dinero)

#gráfico

plot_grpfrq(Envif2$estado_civil, Envif2$negación_dinero,
            type = "bar", title = "Gráfico de barras")

 #Tabla de contingencia
sjt.xtab(Envif2$estado_civil, Envif2$negación_dinero,
         show.col.prc=TRUE,
         show.summary=FALSE, 
         encoding = "UTF-8",
         title = "Tabla de contingencia negación dinero
        por estado civil")
#N°4
Envif2 %>% 
  group_by(sostenedorH) %>% 
  frq(negación_dinero)

#Gráfico
plot_grpfrq(Envif2$sostenedorH, Envif2$negación_dinero,
            type = "bar", title = "Gráfico de barras")

#tabla de contingencia
sjt.xtab(Envif2$sostenedorH, Envif2$negación_dinero,
         show.col.prc=TRUE,
         show.summary=FALSE, 
         encoding = "UTF-8",
         title = "Tabla de contingencia negación dinero
        por quien es el sostenedor del Hogar")

#N°5
Envif2 %>% 
group_by(dinero_gastosP) %>% 
  frq(negación_dinero)

#Gráfico
plot_grpfrq(Envif2$dinero_gastosP, Envif2$negación_dinero,
            type = "bar", title = "Gráfico de barras")
#tabla de contingencia
sjt.xtab(Envif2$dinero_gastosP, Envif2$negación_dinero,
         show.col.prc=TRUE,
         show.summary=FALSE, 
         encoding = "UTF-8",
         title = "Tabla de contingencia negación dinero
        por quien es el sostenedor del Hogar")

#N°6
Envif2 %>% 
  group_by(reg_rec) %>% 
  frq(negación_dinero)
#Gráfico
plot_grpfrq(Envif2$reg_rec, Envif2$negación_dinero,
            type = "bar", title = "Gráfico de barras")
#tabla de contingencia
sjt.xtab(Envif2$reg_rec, Envif2$negación_dinero,
         show.col.prc=TRUE,
         show.summary=FALSE, 
         encoding = "UTF-8",
         title = "Tabla de contingencia negación dinero
        por quien es el sostenedor del Hogar")
#N°7
Envif2 %>% 
  group_by(nivel_educacional) %>% 
  frq(negación_dinero)
#Gráfico
plot_grpfrq(Envif2$nivel_educacional, Envif2$negación_dinero,
            type = "bar", title = "Gráfico de barras")
#Tabla de contingencia
sjt.xtab(Envif2$nivel_educacional, Envif2$negación_dinero,
         show.col.prc=TRUE,
         show.summary=FALSE, 
         encoding = "UTF-8",
         title = "Tabla de contingencia negación dinero
        por nivel educacional")


#Objeto encuesta


#creación objeto encuesta
EnvifObj <- Envif2 %>% #Creamos un nuevo objeto llamado casen_regional con la información de data
  as_survey_design(ids = SbjNum, #Aplicamos diseño muestral, especificando los ids a partir de varunit
                   weights = pond_total)

  
EnvifObj %>%
  group_by(reg_rec) %>% #Agrupamos por región
  summarise(prop = survey_prop(na.rm = T)) #Y calculamos las proporciones


EnvifObj %>% #Creamos un objeto llamado pobreza_reg con datos de casen_regional
  group_by(EDAD_REC, negación_dinero) %>% #Agrupamos por pobreza y sexo
  summarise(prop = survey_prop(vartype = "ci", na.rm = T), #Calculamos las proporciones con intervalos de confianza
            total = survey_total(vartype = "ci", na.rm=T)) %>% #Así como el total por categoría
  mutate(prop = prop*100)
