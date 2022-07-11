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
#########
#Partimos test para crear evaluación
set.seed(1712)
split2 <- createDataPartition(test_completa_1$pobreza, p= 1/3) [[1]]
evaluation <- test_completa_1[split2,]
test <- test_completa_1[-split2,]


#Logit Lasso

#Función#
ctrl_def <- trainControl(method = "cv",
                         number = 5,
                         savePredictions = T)

T_hab + Dormitorios+ Clase + num_mujeresh + mun_adulth + subsidio + Mdll + Cali + Bqa + Qbd + Rioh = train_completa_1
##
set.seed(1712)
Log_lasso <- train(
  pobreza ~  Clase + subsidio, data = test_completisima,
  method = "glm",
  trControl = ctrl_def,
  family = "binomial",
  
)
Log_lasso
#probit Elastic Net
summarise(test_completa_1$pobreza)

##########
set.seed(1712)
Log_lasso <- train(
  pobreza ~ T_hab + Dormitorios + Clase + num_mujeresh + mun_adulth + subsidio + Mdll + Cali + Bqa + Qbd + Rioh , data = test_completisima,
  method = "knn",
  trControl = ctrl_def,
)
