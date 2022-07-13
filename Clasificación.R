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

