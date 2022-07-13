###############################################
############ Problem Set 2     ###############

rm(test_completa_1)
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
##Cargamos las bases
test_hogares <- readRDS("C:/Users/pcere/Dropbox/Machine Learning/ml-taller2/Taller-2/test_hogares.Rds")
test_personas <- readRDS("C:/Users/pcere/Dropbox/Machine Learning/ml-taller2/Taller-2/test_personas.Rds")
train_hogares <- readRDS("C:/Users/pcere/Dropbox/Machine Learning/ml-taller2/Taller-2/train_hogares.Rds")
train_personas <- readRDS("C:/Users/pcere/Dropbox/Machine Learning/ml-taller2/Taller-2/train_personas.Rds")


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

train_completa <- train_completa %>%
  mutate( Mdll = factor(ifelse(Mdll =="MEDELLIN", 1, 0 )),
          Cali = factor(ifelse(Cali =="CALI", 1, 0 )),
          Bqa = factor(ifelse(Bqa =="BARRANQUILLA", 1, 0)),
          Qbd = factor(ifelse(Qbd =="QUIBDO", 1, 0)),
          Rioh = factor(ifelse(Rioh =="RIOHACHA", 1, 0)))

summary(train_completa$Cali)

#recodificar variable cabecera

train_completa$Clase[train_completa$Clase == 1] <- 0
train_completa$Clase[train_completa$Clase == 2] <- 1


###########################################
#creamos línea de pobreza para las dos bases

train_completa <- train_completa %>%
  mutate(pobreza = ifelse(ingreso<l_pob,1,0))

summary(train_completa$pobreza)



test_completa <- test_completa %>%
  mutate( pobreza = ifelse(ingreso_test<l_pob,1,0))

summary(train_completa$pobreza)



###Volvemos factor

test_completa <- test_completa %>%
  mutate(pobreza = factor(ifelse(test_completa$pobreza == 1, "Si", "No" )),
         Clase = factor(ifelse(test_completa$Clase == 1, "Si", "No" )),
         subsidio = factor(ifelse(test_completa$subsidio == 1, "Si", "No" ))
         )
train_completa <- train_completa %>%
  mutate(pobreza = factor(ifelse(train_completa$pobreza == 1, "Si", "No" )),
         Clase = factor(ifelse(train_completa$Clase == 1, "Si", "No" )),
         subsidio = factor(ifelse(train_completa$subsidio == 1, "Si", "No" ))
         )

########################################################
#variables de interacción

#### Test

test_completa <-  test_completa %>%
  mutate(ClaseNum = as.numeric(Clase) )

test_completa <- test_completa %>%
  mutate(Dormitorios2 = Dormitorios^2,
         fam_rural = ClaseNum*Nper)

train_completa <-  train_completa %>%
  mutate(ClaseNum = as.numeric(Clase) )

train_completa <- train_completa %>%
  mutate(Dormitorios2 = Dormitorios^2,
         fam_rural = ClaseNum*Nper)
                               

#Fin de armado de bases

###########################
#Clasificación y regresiones
##########################
########################
#clasificación
###################
install.packages("caret")
install.packages("rio")
install.packages("modelsummary")
install.packages("gamlr")
install.packages("class")
install.packages("lmtest")
install.packages("AER")

library(caret)
library(rio)
library(modelsummary)
library(gamlr)
library(class)
library(lmtest)
library(AER)
library(huxtable)
library(tidyverse)
#########


#### Train


vardesc <- c("ClaseNum", "T_hab", "Dormitorios", "Nper", "num_mujeresh", "mun_adulth", "Mdll", "Cali", "Bqa", "Qbd", "Rioh", "Dormitorios2", "fam_rural" )

########################################################
#Creamos una función para obtener todos los resultados que necesitamos de los modelos
FiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
#Creamos grilla para Lasso y Ridge
lambda_grilla <- 10^seq(-4, 0.01, length = 200)
lambda_grilla

#Partimos test para crear evaluación
set.seed(1712)
split2 <- createDataPartition(test_completa$pobreza, p= 1/3) [[1]]
evaluation <- test_completa[split2,]
test <- test_completa[-split2,]

#train_completa_1$Pobre <- factor((train_completa_1$Pobre), levels = c(0, 1), labels = c("No", "si"))
#train_completa_1$subsidio <- factor((train_completa_1$subsidio), levels = c(0, 1), labels = c("No", "si"))


#Logit

#Función#
ctrl_def <- trainControl(method = "cv",
                         number = 10,
                         savePredictions = TRUE,
                         classProbs = TRUE,
                         summaryFunction = FiveStats)
#Modelo 1 Logit Lasso sin sampling
set.seed(1712)
Logit_lasso <- train(
  pobreza ~  ClaseNum + T_hab +  Dormitorios + Nper + num_mujeresh + mun_adulth + Mdll + Cali + Bqa + Qbd + Rioh + Dormitorios2 + fam_rural, data = train_completa,
  method = "glmnet",
  trControl = ctrl_def,
  family = "binomial",
  preProcess = c("center", "scale"),
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=lambda_grilla)
)
Logit_lasso

#Modelo 2 Logit Ridge sin interacciones ni ciudades

set.seed(1712)
Logit_Ridge <- train(
  pobreza ~  T_hab  + Dormitorios + subsidio + Clase + num_mujeresh + mun_adulth, data = train_completa,
  method = "glmnet",
  trControl = ctrl_def,
  family = "binomial",
  preProcess = c("center", "scale"),
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 1,lambda=lambda_grilla)
)
Logit_Ridge

#Modelo 3 Logit Elastic Net sin interacciones

set.seed(1712)
Logit_EN <- train(
  pobreza ~  T_hab  + Dormitorios + subsidio + Clase + num_mujeresh + mun_adulth+ Mdll + Cali + Bqa + Qbd + Rioh, data = train_completa,
  method = "glmnet",
  trControl = ctrl_def,
  family = "binomial",
  preProcess = c("center", "scale"),
  metric = "Sens"
)
Logit_EN



##Modelo 4 Logit Ridge Up sapmle 
set.seed(1712)
USTrain <- upSample(x = train_completa,
                    y = train_completa$pobreza,
                    yname = "pobreza")
set.seed(1712)
USTest <- upSample(x = test,
                   y = test$pobreza,
                   yname = "pobreza")

set.seed(1712)
logit_US <- train(
  pobreza ~ ClaseNum + T_hab +  Dormitorios + Nper + num_mujeresh + mun_adulth + Mdll + Cali + Bqa + Qbd + Rioh + Dormitorios2 + fam_rural, data = USTrain,
  method = "glmnet",
  trControl = ctrl_def,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 1,lambda=lambda_grilla),
  preProcess = c("center", "scale")
)
logit_US
##Corremos predichos
logit_US_test <- train(
  pobreza ~ ClaseNum + T_hab +  Dormitorios + Nper + num_mujeresh + mun_adulth + Mdll + Cali + Bqa + Qbd + Rioh + Dormitorios2 + fam_rural, data = test,
  method = "glmnet",
  trControl = ctrl_def,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 1,lambda=0.192563),
  preProcess = c("center", "scale")
)

pred_log<-predict(logit_US_test)
pred_log
###
##Modelo 5 Logit Lasso Down sapmle sin ciudad
set.seed(1712)
DSTrain <- downSample(x = train_completa,
                      y = train_completa$pobreza,
                      yname = "pobreza")
set.seed(1712)
logit_DS <- train(
  pobreza ~ T_hab + Dormitorios + Clase + num_mujeresh + mun_adulth + subsidio + Dormitorios2 + fam_rural, data = DSTrain,
  method = "glmnet",
  trControl = ctrl_def,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=0.4045618),
  preProcess = c("center", "scale")
)
logit_DS

##########################
##Regresiones##
##################

##Función para regresiones
ctrl <- trainControl(method = "cv",
                     number = 10,
                     savePredictions = TRUE,
                     summaryFunction = FiveStats)

##Modelo 1 Reg downsample completa
set.seed(1712)
Reg_DS <- train(
  ingreso ~ T_hab + Dormitorios + Clase + num_mujeresh + mun_adulth + subsidio + Mdll + Cali + Bqa + Qbd + Rioh + Dormitorios2 + fam_rural, data = DSTrain,
  method = "lm"
)
Reg_DS

Reg_DS_test<-lm(ingreso_test ~ T_hab + Dormitorios + Clase + num_mujeresh + mun_adulth + subsidio + Mdll + Cali + Bqa + Qbd + Rioh + Dormitorios2 + fam_rural, data = test)
Reg_ds_pred<-predict(Reg_DS_test)
Barbara<-data.frame( Reg_ds_pred, test$l_pob )
Barbara_pred<- Barbara %>%
  mutate(pred_reg = ifelse(Reg_ds_pred < test.l_pob, 1,0))
##Modelo 2 Reg upsample completa
set.seed(1712)
Reg_US <- train(
  ingreso ~ T_hab + Dormitorios + Clase + num_mujeresh + mun_adulth + subsidio + Mdll + Cali + Bqa + Qbd + Rioh + Dormitorios2 + fam_rural, data = USTrain,
  method = "lm"
)
Reg_US

##Modelo 3 Reg completa base normal, sin interacciones ni ciudades 
set.seed(1712)
Reg_Normi <- train(
  ingreso ~ T_hab + Dormitorios + Clase + num_mujeresh + mun_adulth + subsidio, data = train_completa,
  method = "lm"
)
Reg_Normi

##Modelo 4 Upsample con pre procesamiento y sin ciudades
set.seed(1712)
Reg_Up_presam <- train(
  ingreso ~ T_hab + Dormitorios + Clase + num_mujeresh + mun_adulth + subsidio + Dormitorios2+fam_rural, data = USTrain,
  method = "lm",
  preProcess = c("center", "scale")  
)
Reg_Up_presam

##Modelo con pre procesamiento sin interacciones
set.seed(1712)
Reg_normi_pre <- train(
  ingreso ~ T_hab + Dormitorios + Clase + subsidio + num_mujeresh + mun_adulth + subsidio + Mdll + Cali + Bqa + Qbd + Rioh + Dormitorios2 + fam_rural, data = train_completa,
  method = "lm",
  preProcess = c("center", "scale")  
)
Reg_normi_pre

#El mejor modelo es el 1, se convertirá el ingreso predicho en valor binario.
Predichos <- data.frame()

Predichos <- data.frame(test$id, pred_log,Barbara_pred$pred_reg)

CSVFINAL <- Predichos %>%
  mutate(id=test.id,
         pobre_clasificacion=ifelse(pred_log=="Si",1,0),
         pobre_ingreso=Barbara_pred.pred_reg)
CerezoDuran <- CSVFINAL[ -c(1:3) ]

#Exportamos el archivo con predicciones
write.csv(CerezoDuran,"C:/Users/pcere/Dropbox/Machine Learning/ml-taller2/Taller-2\\CerezoDuran.csv", row.names = T)

#Fin del script




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

