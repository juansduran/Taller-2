#clasificación

#Logit Lasso
set.seed(1712)
mylogit lasso downsample <- train(
  Default ~amount+installment+age+ historygood + historypoor + purposeusedcar+ purposegoods.repair + purposeedu + foreigngerman + rentTRdata = downSampledTrain,
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambda grid),
  preProcess = c("center", "scale")
)
#probit Elastic Net

#