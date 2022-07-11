###############################################
############ Problem Set 2     ###############


rm(list = ls())

##########Carga de paquetes

install.packages("car")
library(car)
library(tidyverse)
library(dplyr)
library(skimr)  #estad?sticas descriptivas
library(descr) ###tablas cruzadas
library(tableone) #descriptivas
library(flextable) #tablas
library(huxtable) #regresiones de tablas


################################


###### limpieza y manejo de las variables

#recodificar variable de mujeres


test_personas$P6020[test_personas$P6020 == 1] <- 0
test_personas$P6020[test_personas$P6020 == 2] <- 1



#crear variable para ver si es adulto

test_personas <- test_personas %>%
  mutate(P6240 = ifelse(P6240==1, 1,0),
         P6040 = ifelse(P6040 >= 18, 1,0),
         P6585s1 = ifelse(P6585s1==1, 1, 0),
         P6585s2 = ifelse(P6585s2==1, 1, 0),
         P6585s3 = ifelse(P6585s3==1, 1, 0),
         P6585s4 = ifelse(P6585s4==1, 1, 0))

#para generar la base completa en test

modelo1 <- lm(Ingtot~factor(Oficio), train_personas)

ing_predicho <- predict(modelo1, newdata = test_personas)

head(ing_predicho)

test_personas <- test_personas %>% 
  mutate(ingreso = predict(modelo1, newdata = test_personas))


#volvemos 0 los NA en ingreso para sumar

test_personas$ingreso[is.na(test_personas$ingreso)] <- 0
test_personas$P6585s1[is.na(test_personas$P6585s1)] <- 0
test_personas$P6585s2[is.na(test_personas$P6585s2)] <- 0
test_personas$P6585s3[is.na(test_personas$P6585s3)] <- 0
test_personas$P6585s4[is.na(test_personas$P6585s4)] <- 0


# se hacen algunos mutate con las variables que necesitamos

test_personas <- test_personas %>% group_by(id) %>%
    mutate(ingreso_test = sum(ingreso),
           num_mujeresh = sum(P6020),
           mun_adulth = sum(P6040),
           sub1 = sum(P6585s1),
           sub2 = sum(P6585s2),
           sub3 = sum(P6585s3),
           sub4 = sum(P6585s4),
           subsidio= ifelse(sub1+sub2+sub3+sub4 >0, 1,0))

# Merge de las bases de datos de Test
           
test_incompleta <- test_personas %>%group_by(id) %>%
  summarise_each(funs=mean, ingreso_test, num_mujeresh, mun_adulth, subsidio)

test_completa <- merge(test_hogares, test_incompleta, by.x="id", by.y="id", all.x = TRUE, all.y = FALSE)




##### renombramos el nombre de las variables


test_completa <- test_completa %>%
  rename(l_pob = Lp, 
         T_vivienda = P5090, 
         T_hab = P5000, 
         Dormitorios = P5010 
         )




#### creamos variables de ciudad
test_completa <-  test_completa %>%
  mutate(Mdll = Dominio,
         Cali = Dominio,
         Bqa = Dominio,
         Qbd = Dominio,
         Rioh = Dominio)

#se crean dummies para las variables de departamento

test_completa <- test_completa %>%
  mutate( Mdll = factor(ifelse(Mdll =="MEDELLIN", 1, 0 )),
          Cali = factor(ifelse(Cali =="CALI", 1, 0 )),
          Bqa = factor(ifelse(Bqa =="BARRANQUILLA", 1, 0)),
          Qbd = factor(ifelse(Qbd =="QUIBDO", 1, 0)),
          Rioh = factor(ifelse(Rioh =="RIOHACHA", 1, 0)))

#recodificar variable cabecera

test_completa$Clase[test_completa$Clase == 1] <- 0
test_completa$Clase[test_completa$Clase == 2] <- 1

####################
#Replicamos para train
####################

################################


###### limpieza y manejo de las variables

#recodificar variable de mujeres


train_personas$P6020[train_personas$P6020 == 1] <- 0
train_personas$P6020[train_personas$P6020 == 2] <- 1

#crear variable para ver si es adulto

train_personas <- train_personas %>%
  mutate(P6240 = ifelse(P6240==1, 1,0),
         P6040 = ifelse(P6040 >= 18, 1,0),
         P6585s1 = ifelse(P6585s1==1, 1, 0),
         P6585s2 = ifelse(P6585s2==1, 1, 0),
         P6585s3 = ifelse(P6585s3==1, 1, 0),
         P6585s4 = ifelse(P6585s4==1, 1, 0))


#volvemos 0 los NA en ingreso para sumar


train_personas$P6585s1[is.na(train_personas$P6585s1)] <- 0
train_personas$P6585s2[is.na(train_personas$P6585s2)] <- 0
train_personas$P6585s3[is.na(train_personas$P6585s3)] <- 0
train_personas$P6585s4[is.na(train_personas$P6585s4)] <- 0


# se hacen algunos mutate con las variables que necesitamos

train_personas <- train_personas %>% group_by(id) %>%
  mutate( num_mujeresh = sum(P6020),
         mun_adulth = sum(P6040),
         sub1 = sum(P6585s1),
         sub2 = sum(P6585s2),
         sub3 = sum(P6585s3),
         sub4 = sum(P6585s4),
         subsidio= ifelse(sub1+sub2+sub3+sub4 >0, 1,0))

# Merge de las bases de datos de Test

train_incompleta <- train_personas %>%group_by(id) %>%
  summarise_each(funs=mean, num_mujeresh, mun_adulth, subsidio)

train_completa <- merge(train_hogares, train_incompleta, by.x="id", by.y="id", all.x = TRUE, all.y = FALSE)




##### renombramos el nombre de las variables


train_completa <- train_completa %>%
  rename(l_pob = Lp, 
         T_vivienda = P5090, 
         T_hab = P5000, 
         Dormitorios = P5010,
         ingreso=Ingtotug
  )




#### creamos variables de interacci?n y nuevas variables
train_completa <-  train_completa %>%
  mutate(Mdll = Dominio,
         Cali = Dominio,
         Bqa = Dominio,
         Qbd = Dominio,
         Rioh = Dominio)

#se crean dummies para las variables de departamento

test_completa <- test_completa %>%
  mutate( Mdll = factor(ifelse(Mdll =="MEDELLIN", "si", "no" )),
          Cali = factor(ifelse(Cali =="CALI", 1, 0 )),
          Bqa = factor(ifelse(Bqa =="BARRANQUILLA", 1, 0)),
          Qbd = factor(ifelse(Qbd =="QUIBDO", 1, 0)),
          Rioh = factor(ifelse(Rioh =="RIOHACHA", 1, 0)))

#recodificar variable cabecera

train_completa$Clase[train_completa$Clase == 1] <- 0
train_completa$Clase[train_completa$Clase == 2] <- 1


###########################################
#creamos línea de pobreza para las dos bases

train_completa <- train_completa %>%
  mutate(
    pobreza = ifelse(ingreso<l_pob,1,0)
  )
summary(train_completa$pobreza)



test_completa <- test_completa %>%
  mutate(
    pobreza = ifelse(ingreso_test<l_pob,1,0)
  )
summary(test_completa_1$pobreza)


test_completa_1$T_hab[is.na(test_completa_1$T_hab)] <- 0

sum(is.na(test_completa_1$T_hab))
###

test_completa_1 <- test_completa %>%
  mutate(pobreza = factor(ifelse(test_completa$pobreza == 1, 1, 0 )),
         T_hab=factor(T_hab,levels=c(1:13)),
         Dormitorios=factor(Dormitorios,levels=c(1:9)),
         Clase = factor(ifelse(test_completa$Clase == 1, 1, 0 )),
         num_mujeresh = factor(num_mujeresh, levels=c(1:13)),
         mun_adulth = factor(mun_adulth, levels=c(1:19)),
         subsidio = factor(ifelse(test_completa$subsidio == 1, 1, 0 ))
         )

#Eliminamos NA



train_completa_1 <- train_completa %>%
  mutate(pobreza = factor(pobreza, levels=c(1,0), labels=c("Si", "No")),
         T_hab=factor(T_hab,levels=c(1:13),labels = c("1","2","3","4","5","6","7","8","9","10","11","12","13")),
         Dormitorios=factor(Dormitorios,levels=c(1:9), labels = c("1","2","3","4","5","6","7","8","9")),
         Clase = factor(Clase,levels=c(1,0), labels=c("Si", "No")),
         num_mujeresh = factor(num_mujeresh, levels=c(1:13),labels = c("1","2","3","4","5","6","7","8","9","10","11","12","13")),
         mun_adulth = factor(mun_adulth, levels=c(1:19),labels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19")),
         subsidio = factor(subsidio, levels=c(1,0), labels = c("Si", "No"))
  )

factor((train_completa_1$Pobre), levels = c(0, 1), labels = c("No", "si"))


test_completa_1 <- test_completa %>%
  mutate( Mdll = factor(Mdll =="MEDELLIN",levels=c(1,0), labels = c("Si","No")),
          Cali = factor(Cali =="CALI",levels=c(1,0), labels = c("Si", "No")),
          Bqa = factor(Bqa =="BARRANQUILLA",levels=c(1,0), labels=c("Si", "No")),
          Qbd = factor(Qbd =="QUIBDO",levels=c(1,0), labels=c("Si", "No")),
          Rioh = factor(Rioh =="RIOHACHA",levels=c(1,0), labels=c("Si", "No")))


          
                               
                               data.frame(table(train_completa_1$mun_adulth))

#Fin de armado de bases






##### dicionario de datos


#### p5000 número de cuartos por hogar
## lp  linea de pobreza
## li linea de indigencia
## nper Personas en el hogar 
## npersug  Número de personas en la unidad de gasto 
## p5090 La vivienda ocupada por este hogar 
## P5010 ¿en cuántos de esos cuartos duermen las personas de este hogar? 
## p5100 ¿cuánto pagan mensualmente por cuota de amortización?
## p5130 Si tuviera que pagar arriendo por esta  vivienda, ¿cuánto estima que tendría que pagar mensualmente? valor $
## ¿cuánto pagan mensualmente por arriendo? excluya el pago de administración y/o celaduría valor $
## P6020 sexo 1 Hombre 2 mujer
## p6040 edad
## P6050 relación Jefe de hogar
##  P6090 ¿ ... Está afiliado, es cotizante o es beneficiario de alguna entidad de seguridad social en salud? (instituto de
#  seguros sociales - iss, empresa promotora de salud - eps o administradora de régimen subsidiado - ars) 1 sí 2 no 9 no sabe, no informa
## p6100 ¿A cuál de los siguientes regímenes de seguridad social en salud está afiliado: a. Contributivo (eps)? b. Especial ? (fuerzas
# armadas, ecopetrol, universidades públicas) c. Subsidiado? (eps-s) d. No sabe, no informa 
## p6210 Nivel educativo categórica
## P6210s1 Grado escolar en númerico
#P6240 si est? trabajando o buscando trabajo estudiando
## P6426 c ¿cuanto tiempo lleva ... Trabajando en esta empresa, negocio, industria, oficina, firma o finca de manera continua? 
## P6430 En este trabajo…..es: a. Obrero o empleado de empresa particular b. Obrero o empleado del gobierno c. Empleado
##doméstico d. Trabajador por cuenta propia e. Patrón o empleador f. Trabajador familiar sin remuneración g. Trabajador sin
##remuneración en empresas o negocios de otros hogares h. Jornalero o peón i. Otro
## ¿el mes pasado recibió a. Auxilio o subsidio de alimentación?  1 si 2 no 9 no sabe, no informa
## P6800 horas trabajadas
## P6920 cotiza a pensiones (trabajo formal o informal)

