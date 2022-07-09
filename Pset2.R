###############################################
############ Problem Set 2     ###############


rm(list = ls(df))
rm(df)
##########Carga de paquetes

install.packages("car")
library(car)
library(tidyverse)
library(dplyr)
library(skimr)  #estad�sticas descriptivas
library(descr) ###tablas cruzadas
library(tableone) #descriptivas
library(flextable) #tablas
library(huxtable) #regresiones de tablas


################################


##### Merge de las bases de datos de Test

test_completa <- merge(test_hogares, test_personas, by.x="id", by.y="id", all.x = TRUE, all.y = FALSE)

###### limpieza y manejo de las variables

#recodificar variable de mujeres


test_completa$P6020[test_completa$P6020 == 1] <- 0
test_completa$P6020[test_completa$P6020 == 2] <- 1

#recodificar variaable de subsidios

test_completa$P6585s1[test_completa$P6585s1 == 9] <- 3
test_completa$P6585s2[test_completa$P6585s2 == 9] <- 3
test_completa$P6585s3[test_completa$P6585s3 == 9] <- 3
test_completa$P6585s4[test_completa$P6585s4 == 9] <- 3


#convertir departamentos en factor

test_completa$Dominio.x <- as.factor(test_completa$Dominio.x)

class(test_completa$Dominio.x)

##### renombrear las etiquetas categoricas

test_completa <- test_completa %>%
  mutate(Clase.x = factor(ifelse(Clase.x ==1, "cabecera (1)", "rural (0)" )),
         P6020 = factor(ifelse(P6020 == 1, "Mujer", "Hombre")),
         P6050 = factor(P6050, levels = c(1:9), labels = c("JEFE_H","Pareja_JH", "Hijo(a)", "Nieto", "Otro", "Empleado_S", "Pensionista", "Trabajador", "Otro_NP")),
                        P6210 = factor(P6210, levels = c(1:9), labels = c("Ninguno", "Preescolar","Basica_p","Basica_S","Media", "Superior_OU","","", "No sabe")),
                        P6240 = factor(P6240, levels = c(1:6), labels = c("Trabajando","Buscando_T","Estudiando", "Oficios_H", "Incapacitado_T", "Otra_A")),
                        P6430 = factor(P6430, levels = c(1:9), labels = c("Obrero_Part", "Obrero_Gov", "Emp_dom", "Cuenta_pro","Patron_Emp", "T_familiarnr", "T_NRotrosh", "Jornalero","otro")),
                        P5090 = factor(P5090, levels = c(1:6), labels = c("Propia_T", "Propia_p", "Arriendo", "Usufructo","Ocupante", "otra")),
                        P6100 = factor(P6100, levels = c(1:4), labels = c("Contributivo", "Especial", "subsidiado", "No_sabe")),
                        P6585s1 = factor(P6585s1, levels = c(1:3), labels = c("si","no","no_sabe")),
                        P6585s2 = factor(P6585s2, levels = c(1:3), labels = c("si","no","no_sabe")),
                        P6585s3 = factor(P6585s3, levels = c(1:3), labels = c("si","no","no_sabe")),
                        P6585s4 = factor(P6585s4, levels = c(1:3), labels = c("si","no","no_sabe")))

table(test_completa$P6020)

##### renombramos el nombre de las variables


test_completa <- test_completa %>%
  rename(l_pob = Lp, 
         l_ind = Li, 
         T_vivienda = P5090, 
         T_hab = P5000, 
         Dormitorios = P5010, 
         val_arriendo = P5130, 
         Genero = P6020, 
         Edad = P6040, 
         Relacion_JH = P6050,
         Reg_salud = P6100, 
         Niv_educ = P6210,
         Grado = P6210s1,
         Temp_emp = P6426,
         tipo_of = P6430,
         horas_t =P6800,
         cotiza_p = P6920,
         Sub_Alim = P6585s1,
         Sub_transp = P6585s2,
         Sub_fam = P6585s3,
         Sub_educ= P6585s4)

#### creamos variables de interacci�n y nuevas variables
 test_completa <-  test_completa %>%
   mutate(Mdll = Dominio.x,
          Cali = Dominio.x,
          Bqa = Dominio.x,
          Qbd = Dominio.x,
          Gjr = Dominio.x,
          Edad2 = Edad^2,
          Mujer = Genero)

 #convertir mujer a numerico
 
 test_completa$Mujer <- as.numeric(test_completa$Mujer)
 
 class(test_completa$Mujer)
 
 
 #se crean dummies para las variables de departamento
 
 test_completa <- test_completa %>%
   mutate( Muj_edad = Mujer*Edad
          ,Mdll = factor(ifelse(Mdll =="MEDELLIN", 1, 0 )),
          Cali = factor(ifelse(Cali =="CALI", 1, 0 )),
          Bqa = factor(ifelse(Bqa =="BARRANQUILLA", 1, 0)),
          Qbd = factor(ifelse(Qbd =="QUIBDO", 1, 0)),
          Gjr = factor(ifelse(Gjr =="RIOHACHA", 1, 0)))


##### Dejo solo las variables que voy a utilizar

test_com1 <- subset(test_completa, select = c(l_pob, 
                                              l_ind, 
                                              T_vivienda, 
                                              T_hab, 
                                              Dormitorios, 
                                              val_arriendo, 
                                              Genero, 
                                              Edad, 
                                              Relacion_JH,
                                              Reg_salud, 
                                              Niv_educ,
                                              Grado,
                                              Temp_emp,
                                              tipo_of,
                                              horas_t,
                                              cotiza_p,
                                              Sub_Alim,
                                              Sub_transp,
                                              Sub_fam,
                                              Sub_educ,
                                              id,
                                              Clase.x,
                                              Dominio.x,
                                              Nper,
                                              Npersug,
                                              Orden,
                                              Muj_edad,
                                              Edad2,
                                              Mdll,
                                              Cali,
                                              Bqa ,
                                              Qbd,
                                              Gjr, 
                                              Oficio))


##### Merge de las bases de datos de Train

train_completa <- merge(train_hogares, train_personas, by.x="id", by.y="id", all.x = TRUE, all.y = FALSE)

#####

#recodificar variable de mujeres


train_completa$P6020[train_completa$P6020 == 1] <- 0
train_completa$P6020[train_completa$P6020 == 2] <- 1

#recodificar variaable de subsidios

train_completa$P6585s1[train_completa$P6585s1 == 9] <- 3
train_completa$P6585s2[train_completa$P6585s2 == 9] <- 3
train_completa$P6585s3[train_completa$P6585s3 == 9] <- 3
train_completa$P6585s4[train_completa$P6585s4 == 9] <- 3

#convertir departamentos en factor

train_completa$Dominio.x <- as.factor(train_completa$Dominio.x)


##### renombrear las etiquetas categoricas

train_completa <- train_completa %>%
  mutate(Clase.x = factor(ifelse(Clase.x ==1, "cabecera (1)", "rural (0)" )),
         P6020 = factor(ifelse(P6020 == 1, "Mujer", "Hombre")),
         P6050 = factor(P6050, levels = c(1:9), labels = c("JEFE_H","Pareja_JH", "Hijo(a)", "Nieto", "Otro", "Empleado_S", "Pensionista", "Trabajador", "Otro_NP"  )),
                        P6210 = factor(P6210, levels = c(1:9), labels = c("Ninguno", "Preescolar","Basica_p","Basica_S","Media", "Superior_OU","","", "No sabe")),
                        P6240 = factor(P6240, levels = c(1:6), labels = c("Trabajando","Buscando_T","Estudiando", "Oficios_H", "Incapacitado_T", "Otra_A")),
                        P6430 = factor(P6430, levels = c(1:9), labels = c("Obrero_Part", "Obrero_Gov", "Emp_dom", "Cuenta_pro","Patron_Emp", "T_familiarnr", "T_NRotrosh", "Jornalero","otro")),
                        P5090 = factor(P5090, levels = c(1:6), labels = c("Propia_T", "Propia_p", "Arriendo", "Usufructo","Ocupante", "otra")),
                        P6100 = factor(P6100, levels = c(1:4), labels = c("Contributivo", "Especial", "subsidiado", "No_sabe")),
                        P6585s1 = factor(P6585s1, levels = c(1:3), labels = c("si","no","no_sabe")),
                        P6585s2 = factor(P6585s2, levels = c(1:3), labels = c("si","no","no_sabe")),
                        P6585s3 = factor(P6585s3, levels = c(1:3), labels = c("si","no","no_sabe")),
                        P6585s4 = factor(P6585s4, levels = c(1:3), labels = c("si","no","no_sabe")))

##### renombramos el nombre de las variables


train_completa <- train_completa %>%
  rename(l_pob = Lp, 
         l_ind = Li, 
         T_vivienda = P5090, 
         T_hab = P5000, 
         Dormitorios = P5010, 
         val_arriendo = P5130, 
         Genero = P6020, 
         Edad = P6040, 
         Relacion_JH = P6050,
         Reg_salud = P6100, 
         Niv_educ = P6210,
         Grado = P6210s1,
         Temp_emp = P6426,
         tipo_of = P6430,
         horas_t =P6800,
         cotiza_p = P6920,
         Sub_Alim = P6585s1,
         Sub_transp = P6585s2,
         Sub_fam = P6585s3,
         Sub_educ= P6585s4)

#### creamos variables de interacci�n y nuevas variables
train_completa <-  train_completa %>%
  mutate(Mdll = Dominio.x,
         Cali = Dominio.x,
         Bqa = Dominio.x,
         Qbd = Dominio.x,
         Gjr = Dominio.x,
         Edad2 = Edad^2,
         Mujer = Genero)

#convertir mujer a numerico

train_completa$Mujer <- as.numeric(train_completa$Mujer)

class(train_completa$Mujer)


#se crean dummies para las variables de departamento

train_completa <- train_completa %>%
  mutate( Muj_edad = Mujer*Edad
          ,Mdll = factor(ifelse(Mdll =="MEDELLIN", 1, 0 )),
          Cali = factor(ifelse(Cali =="CALI", 1, 0 )),
          Bqa = factor(ifelse(Bqa =="BARRANQUILLA", 1, 0)),
          Qbd = factor(ifelse(Qbd =="QUIBDO", 1, 0)),
          Gjr = factor(ifelse(Gjr =="RIOHACHA", 1, 0)))



##### Dejo solo las variables que voy a utilizar

train_com1 <- subset(train_completa, select = c(l_pob, 
                                              l_ind, 
                                              T_vivienda, 
                                              T_hab, 
                                              Dormitorios, 
                                              val_arriendo, 
                                              Genero, 
                                              Edad, 
                                              Relacion_JH,
                                              Reg_salud, 
                                              Niv_educ,
                                              Grado,
                                              Temp_emp,
                                              tipo_of,
                                              horas_t,
                                              cotiza_p,
                                              Sub_Alim,
                                              Sub_transp,
                                              Sub_fam,
                                              Sub_educ,
                                              id,
                                              Clase.x,
                                              Dominio.x,
                                              Nper,
                                              Npersug,
                                              Orden,
                                              Ingtotug,
                                              Pobre,
                                              Indigente,
                                              Npobres,
                                              Nindigentes,
                                              Muj_edad,
                                              Edad2,
                                              Mdll,
                                              Cali,
                                              Bqa ,
                                              Qbd,
                                              Gjr,
                                              Oficio,
                                              Ingtotugarr))





########################################################
#### Tablas de estadisticas descriptivas ##############

#### creamos un vector de variables descriptivas


#test

vardesc_test <- c("l_pob", "l_ind", "T_vivienda", "T_hab", "Dormitorios", "val_arriendo", 
                  "Genero", "Edad", "Relacion_JH", "Reg_salud", "Niv_educ", "Grado", "Temp_emp",
                  "tipo_of", "horas_t","cotiza_p","Sub_Alim", "Sub_transp","Sub_fam",
                  "Sub_educ", "id", "Clase.x", "Dominio.x", "Nper", "Npersug", "Orden")

tabl_test <- CreateTableOne(data = test_com1, vars = vardesc_test)


#train

vardesc_train <- c("l_pob", "l_ind", "T_vivienda", "T_hab", "Dormitorios", "val_arriendo", 
            "Genero", "Edad", "Relacion_JH", "Reg_salud", "Niv_educ", "Grado", "Temp_emp",
            "tipo_of", "horas_t","cotiza_p","Sub_Alim", "Sub_transp","Sub_fam",
            "Sub_educ", "id", "Clase.x", "Dominio.x", "Nper", "Npersug", "Orden", "Ingtotug",
            "Pobre", "Indigente","Npobres", "Nindigentes")


tabl_train <- CreateTableOne(data = train_com1, vars = vardesc_train)


#####################################################################


###Graficas que ilustren la poblaci�n que trabajamos



ggplot(test_completa, aes(x= factor(Niv_educ), fill = Genero, colour = Genero)) + geom_bar(position = "dodge") + 
  geom_text(aes(label=..count..), stat='count',position=position_dodge(0.8), vjust= -0.9,  size=5.0) 





data.frame(table(test_completa$P6430))



detectores()


###############################################################################################################


modelo1 <- lm(Ingtotugarr~factor(Dominio), train_hogares)

ing_predicho <- predict(modelo1, newdata = test_hogares)




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
## P6426 c ¿cuanto tiempo lleva ... Trabajando en esta empresa, negocio, industria, oficina, firma o finca de manera continua? 
## P6430 En este trabajo…..es: a. Obrero o empleado de empresa particular b. Obrero o empleado del gobierno c. Empleado
##doméstico d. Trabajador por cuenta propia e. Patrón o empleador f. Trabajador familiar sin remuneración g. Trabajador sin
##remuneración en empresas o negocios de otros hogares h. Jornalero o peón i. Otro
## ¿el mes pasado recibió a. Auxilio o subsidio de alimentación?  1 si 2 no 9 no sabe, no informa
## P6800 horas trabajadas
## P6920 cotiza a pensiones (trabajo formal o informal)

