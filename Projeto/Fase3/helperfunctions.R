##########################################################
#                                                        #
# Mestrado em Engenharia Informática e de Computadores   #
#                                                        #
# MDLE                                                   #
#                                                        #
# Material de apoio (editado)                            #
#                                                        #
##########################################################

require(caret)
require(e1071)

mdle.printConfusionMatrix <- function(sp.pred, msg) {
  auc <- sp.pred %>% ml_binary_classification_evaluator(
    label_col = "CLASS",
    raw_prediction_col = "rawPrediction",
    metric_name = "areaUnderROC"
  )
  
  class <- factor(collect(sp.pred["CLASS"])$CLASS)
  pred <- factor(collect(sp.pred["prediction"])$prediction, levels = levels(class))
  
  cfxmat <- confusionMatrix(pred, class, mode = "everything")
  
  metrics <- c(Sensitivity = cfxmat$byClass["Sensitivity"],
               Specificity = cfxmat$byClass["Specificity"],
               FPR = 1 - cfxmat$byClass["Specificity"],
               FNR = 1 - cfxmat$byClass["Sensitivity"],
               AUC = auc)
  
  metrics <- as.data.frame(t(metrics))
  colnames(metrics) <- c("Sensitivity", "Specificity", "FPR", "FNR", "AUC")
  
  cat(noquote("---------------------------------------------------------------"))
  cat(noquote(paste("\nConfusion Matrix and Statistics:",msg,":\n\n")))
  print(cfxmat$table)
  cat(noquote("\nMectrics:\n\n"))
  cat(noquote(paste("Sensitivity            :",format(round(metrics[1], 3), nsmall = 3),"\n" )))
  cat(noquote(paste("Specificity            :",format(round(metrics[2], 3), nsmall = 3),"\n" )))
  cat(noquote(paste("False Positive Rate    :",format(round(metrics[3], 3), nsmall = 3),"\n" )))
  cat(noquote(paste("False Negative Rate    :",format(round(metrics[4], 3), nsmall = 3),"\n" )))
  cat(noquote(paste("AUC                    :",format(round(metrics[5], 3), nsmall = 3),"\n" )))
  cat(noquote("---------------------------------------------------------------\n"))
}

mdle.predict<-function(model,test) {
  return (ml_predict(model,test))
}

mdle.printDataDimensions <- function(data, msg) {
  cat("\n", msg, "\n", "Rows =", n_rows <- sparklyr::sdf_nrow(data),
      "Columns =", n_cols <- sparklyr::sdf_ncol(data) - 1, "\n"
  )
}

mdle.printDataClassCount <- function(data, msg) {
  cat("\n", msg)
  print(df.counts <- data %>% group_by(CLASS) %>% count() %>% collect())
}
