##########################################################
#
# Mestrado em Engenharia Informática e de Computadores
# MDLE
# 
# Material de apoio (editado)
#
##########################################################

require(caret)
require(e1071)

# mdle.printConfusionMatrix <- function(sp.pred, method) {
#   pred<-sp.pred %>% select('CLASS', 'prediction')  %>% collect
#   pred$prediction<-round(pred$prediction)
# 
#   cfxmat<-confusionMatrix(table(as.vector(pred$CLASS),as.vector(pred$prediction)),mode = "everything",positive = "1")
#   cat(noquote(paste("\n","Confusion Matrix and Statistics:",method,"\n")))
#   print(cfxmat$table)
# 
#   cat(noquote(paste("False Positive Rate :",format(round(1-cfxmat$byClass[[3]], 3), nsmall = 3),"\n" )))
#   cat(noquote(paste("Accuracy            :",format(round(cfxmat$overall[[1]], 3), nsmall = 3),"\n" )))
#   cat(noquote(paste("Kappa               :",format(round(cfxmat$overall[[2]], 3), nsmall = 3),"\n" )))
#   cat(noquote(paste("Pos Pred Value      :",format(round(cfxmat$byClass[[3]], 3), nsmall = 3),"\n" )))
#   cat(noquote(paste("Neg Pred Value      :",format(round(cfxmat$byClass[[4]], 3), nsmall = 3),"\n" )))
# 
#   return (cfxmat)
# }

metrics_func <- function(predictions, msg) {
  auc <- predictions %>% ml_binary_classification_evaluator(
    label_col = "CLASS",
    raw_prediction_col = "rawPrediction",
    metric_name = "areaUnderROC"
  )
  
  class <- factor(collect(predictions["CLASS"])$CLASS)
  pred <- factor(collect(predictions["prediction"])$prediction, levels = levels(class))
  
  cfxmat <- confusionMatrix(pred, class, mode = "everything")
  
  metrics <- c(Sensitivity = cfxmat$byClass["Sensitivity"],
               Specificity = cfxmat$byClass["Specificity"],
               FNR = 1 - cfxmat$byClass["Sensitivity"],
               AUC = auc)
  metrics <- as.data.frame(t(metrics))
  colnames(metrics) <- c("Sensitivity", "Specificity", "FNR", "AUC")
  return(metrics)
}


mdle.predict<-function(model,test) {
  return (ml_predict(model,test))
}

mdle.printDataDimensions <- function(data, msg) {
  cat("\n", msg, "\n", "Rows =", n_rows <- sparklyr::sdf_nrow(data),
      "Columns =", n_cols <- sparklyr::sdf_ncol(data), "\n"
  )
}

mdle.printDataClassCount <- function(data, msg) {
  cat("\n", msg)
  print(df.counts <- data %>% group_by(CLASS) %>% count() %>% collect())
}
