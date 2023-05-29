##################################################################
#                                                                #
# ISEL - Instituto Superior de Engenharia de Lisboa              #
# MDLE - Mineração de Dados em Larga Escala                      #
#                                                                #
# Projeto - Fase 3                                               #
#                                                                #
# Data - 01/06/2023                                              #
#                                                                #
# Autores - Grupo 08: Pedro Diogo A47573, Gonçalo Fonseca A50185 #
#                                                                #
##################################################################

################# Preparation ################
libs = c('sparklyr', 'dplyr','data.table', 'smotefamily')
lapply(libs, require, character.only = TRUE)

source("Fase3/helperfunctions.R")

set.seed(123)

################# Spark setup ################
spark_disconnect_all() #Just preventive code
sc <- spark_connect('local', version = '3.3.2', hadoop_version = '3', config = list())

################# Load data ################
basepath <- paste(getwd(), "/Influenza-Outbreak-Dataset", sep = '')

train.data <- c("train_data_1.csv") #The training data to use
train.labels <- c("train_labels_1.csv") #The training labels for the data

test.data <- c("test_data_1.csv") #The testing data to use
test.labels <- c("test_labels_1.csv") #The testing labels for the data

fun1 <- function(i, type) { #Read CSV data
  print(paste(basepath, type, i, sep = "/"))
  read.csv(paste(basepath, type, i, sep = "/"), header = FALSE, stringsAsFactors = FALSE)
}

fun2 <- function(i, type) { #Read and transpose CSV data
  read.csv(paste(basepath, type, i, sep = "/"), header = FALSE, stringsAsFactors = FALSE) %>% t %>% as.data.table
}

#Train data
df.train <- do.call(rbind, lapply(train.data, fun1, type = "train")) #Bind csv together
df.train.l <- do.call(rbind, lapply(train.labels, fun2, type = "train")) #Bind class together
names(df.train.l) <- c("CLASS")
df.train.local <- cbind(df.train.l, df.train) #Bind them together (labels in first column)
df.train <- copy_to(sc, df.train.local) #Copy a local data frame to Spark

#Test data
df.test <- do.call(rbind, lapply(test.data, fun1, type = "test")) #Bind csv together
df.test.l <- do.call(rbind, lapply(test.labels, fun2, type = "test")) #Bind class together
names(df.test.l) <- c("CLASS")
df.test.local <- cbind(df.test.l, df.test) #Bind them together (labels in first column)
df.test <- copy_to(sc, df.test.local) #Copy a local data frame to Spark

################# Data visualization ################
mdle.printDataDimensions(df.train, "Original train dataset dimensions:")
mdle.printDataDimensions(df.test, "Original test dataset dimensions:")

mdle.printDataClassCount(df.train, "Original train data class counts:")
mdle.printDataClassCount(df.test, "Original test data class counts:")

################# Feature selection ################
calculate_number_of_features <- function(values, thresholds) {
  # Compute the adequate number of features for each threshold
  n_features <- sapply(thresholds, function(x) sum(cumsum(values) <= sum(values) * x))

  # Print the adequate number of features for each threshold
  cat(paste0("For threshold ", thresholds[1] * 100, "%, use ", n_features[1], " features.\n"))
  cat(paste0("For threshold ", thresholds[2] * 100, "%, use ", n_features[2], " features.\n"))
  cat(paste0("For threshold ", thresholds[3] * 100, "%, use ", n_features[3], " features.\n"))

  return(n_features)
}

svd_result <- svd(df.train.local[, -1])

cat("\n", "Number of features based on SVD:", "\n")
n_features <- calculate_number_of_features(svd_result$d, c(0.9, 0.95, 0.99))

reduced_x_train <- scale(df.train.local[, -1], scale = FALSE) %*% svd_result$v[, 1:n_features[3]]
df.train <- cbind(df.train.l, data.frame(reduced_x_train)) #Bind them together (labels in first column)
df.train <- copy_to(sc, df.train) #Copy a local data frame to Spark

reduced_x_test <- scale(df.test.local[, -1], scale = FALSE) %*% svd_result$v[, 1:n_features[3]]
df.test <- cbind(df.test.l, data.frame(reduced_x_test)) #Bind them together (labels in first column)
df.test <- copy_to(sc, df.test) #Copy a local data frame to Spark

mdle.printDataDimensions(df.train, "SVD reduced train dataset dimensions:")
mdle.printDataDimensions(df.test, "SVD reduced test dataset dimensions:")

################# Instance manipulation ################
df.train.neg <- df.train %>% filter(CLASS == 0) #Filter for negative classes
df.train.pos <- df.train %>% filter(CLASS == 1) #Filter for positive classes

#Undersampling
frac.undersampling <- sdf_nrow(df.train.pos) / sdf_nrow(df.train) #Determine the fraction

df.train.neg.undersampled <- sdf_sample(df.train.neg, fraction = frac.undersampling, replacement = F, seed = 123)
df.train.undersampled <- sdf_bind_rows(df.train.pos, df.train.neg.undersampled)

mdle.printDataClassCount(df.train.undersampled, "Undersampled train data class counts:")

#Oversampling
frac.oversampling <- sdf_nrow(df.train.neg) / sdf_nrow(df.train.pos) #Determine the fraction

df.train.pos.oversampled <- sdf_sample(df.train.pos, fraction = frac.oversampling, replacement = T, seed = 123)
df.train.oversampled <- sdf_bind_rows(df.train.pos.oversampled, df.train.neg)

mdle.printDataClassCount(df.train.oversampled, "Oversampled train data class counts:")

#BL-SMOTE
df.train.Smote.labels <- collect(df.train %>% select(CLASS)) #Filter for labels only
df.train.Smote.data <- collect(df.train  %>% select(-CLASS)) #Filter for data only

df.train.blsmote.local <- BLSMOTE(df.train.Smote.data, df.train.Smote.labels, K = 7, C = 5, method = "type1")

df.train.blsmote <- copy_to(sc, df.train.blsmote.local$data) #Copy a local data frame to a remote source

mdle.printDataClassCount(df.train.oversampled, "BL-SMOTE train data class counts:")

################# Data classification ################

#Create a pipelines
svc_pipeline <- ml_pipeline(sc) %>%
  ft_r_formula("CLASS ~ .") %>%
  ml_linear_svc()

rf_pipeline <- ml_pipeline(sc) %>%
  ft_r_formula("CLASS ~ .") %>%
  ml_random_forest_classifier()

#Baseline
rf_baseline_fit <- ml_fit(rf_pipeline, df.train)
rf_baseline_preds <- ml_predict(rf_baseline_fit, df.test)
print(metrics_func(rf_baseline_preds, "SVD | Baseline | Random Forest"))

svc_baseline_fit <- ml_fit(svc_pipeline, df.train)
svc_baseline_preds <- ml_predict(svc_baseline_fit, df.test)
print(metrics_func(svc_baseline_preds, "SVD | Baseline | SVC"))

#Undersample
rf_undersample_fit <- ml_fit(rf_pipeline, df.train.undersampled)
rf_undersample_preds <- ml_predict(rf_undersample_fit, df.test)
print(metrics_func(rf_undersample_preds, "SVD | Undersample | Random Forest"))

svc_undersample_fit <- ml_fit(svc_pipeline, df.train.undersampled)
svc_undersample_preds <- ml_predict(svc_undersample_fit, df.test)
print(metrics_func(svc_undersample_preds, "SVD | Undersample | SVC"))

# #Oversample
rf_oversample_fit <- ml_fit(rf_pipeline, df.train.oversampled)
rf_oversample_preds <- ml_predict(rf_oversample_fit, df.test)
print(metrics_func(rf_oversample_preds, "SVD | Oversample | Random Forest"))

svc_oversample_fit <- ml_fit(svc_pipeline, df.train.oversampled)
svc_oversample_preds <- ml_predict(svc_oversample_fit, df.test)
print(metrics_func(svc_oversample_preds, "SVD | Oversample | SVC"))

# #BL-SMOTE TODO
# svm_model <- ml_linear_svc(df.train.blsmote, formula = "class ~ .")
# predictions <- mdle.predict(svm_model, df.test)
# mdle.printConfusionMatrix(predictions, "Support vector classification  - BL-SMOTE")
# 
# rf_model <- ml_random_forest(df.train.blsmote, formula = "class ~ .", seed = 123, type = 'classification')
# predictions <- mdle.predict(rf_model, df.test)
# mdle.printConfusionMatrix(predictions, "Random forest  - BL-SMOTE")

# ################# Spark cleanup ################
# # spark_disconnect(sc)
