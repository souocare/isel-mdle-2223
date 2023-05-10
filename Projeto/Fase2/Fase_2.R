##################################################################
#                                                                #
# ISEL - Instituto Superior de Engenharia de Lisboa              #
# MDLE - Mineração de Dados em Larga Escala                      #
#                                                                #
# Projeto - Fase 2                                               #
#                                                                #
# Data - 11/05/2023                                              #
#                                                                #
# Autores - Grupo 08: Pedro Diogo A47573, Gonçalo Fonseca A50185 #
#                                                                #
##################################################################

################# Preparation ################
library(dplyr) #Data manipulation
library(sparklyr) #Spark 
library(smotefamily) #For SMOTE sampling
library(data.table) #More efficient data.frame

if (!exists("printConfusionMatrix", mode = "function"))
  source("Fase2/helperfunctions.R")

################# Spark setup ################
spark_disconnect_all() #Just preventive code
sc <- spark_connect('local', version = '3.3.2', hadoop_version = '3', config = list())

################# Load data ################
basepath <- paste(getwd(), "/Influenza-Outbreak-Dataset", sep = '')

train.data <- c("train_data_1.csv") #The training data to use
train.labels <- c("train_labels_1.csv") #The training labels for the data

test.data <- c("test_data_1.csv") #The testing data to use
test.labels <- c("test_labels_1.csv") #The testing labels for the data

# keywords <- readLines(paste(basepath, "/flu_keywords.txt", sep = ''))

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
df.train <- copy_to(sc, df.train.local) #Copy a local data frame to a remote source

#Test data
df.test <- do.call(rbind, lapply(test.data, fun1, type = "test")) #Bind csv together
df.test.l <- do.call(rbind, lapply(test.labels, fun2, type = "test")) #Bind class together
names(df.test.l) <- c("CLASS")

df.test.local <- cbind(df.test.l, df.test) #Bind them together (labels in first column)
df.test <- copy_to(sc, df.test.local) #Copy a local data frame to a remote source

################# Data visualization ################
mdle.printDataDimensions(df.train, "train")
mdle.printDataDimensions(df.test, "test")

mdle.printDataClassCount(df.train, "train", "Original")
mdle.printDataClassCount(df.test, "test", "Original")

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

calculate_svd <- function(data) {
  svd <- svd(data)
  plot(svd$d, type = 'p', main = 'SVD', xlab = "Singular value", ylab = "Magnitude")
  return(svd)
}

reduce_with_svd <- function(decomposition_values, n_features) {
  reduced_dataset <- decomposition_values$u %*% diag(decomposition_values$d)[, 1:n_features] %*% t(decomposition_values$v[1:n_features, 1:n_features])
  return(reduced_dataset)
}

thresholds <- c(0.9, 0.95, 0.99)

train_svd_result <- calculate_svd(df.train.local[, -1])
test_svd_result <- calculate_svd(df.test.local[, -1])

print("Train data - number of features based on SVD:")
  train_n_features_svd <- calculate_number_of_features(train_svd_result$d, thresholds)
  
  print("Test data - number of features based on SVD:")
  test_n_features_svd <- calculate_number_of_features(test_svd_result$d, thresholds)
  
  reduced_x_train <- reduce_with_svd(train_svd_result, train_n_features_svd[3])
  df.train <- cbind(df.train.l, data.frame(reduced_x_train)) #Bind them together (labels in first column)
  df.train <- copy_to(sc, df.train) #Copy a local data frame to a remote source
  
  reduced_x_test <- reduce_with_svd(test_svd_result, test_n_features_svd[3])
  df.test <- cbind(df.test.l, data.frame(reduced_x_test)) #Bind them together (labels in first column)
  df.test <- copy_to(sc, df.train) #Copy a local data frame to a remote source

################# Instance manipulation ################
df.train.neg <- df.train %>% filter(CLASS == 0) #Filter for negative classes
df.train.pos <- df.train %>% filter(CLASS == 1) #Filter for positive classes

mdle.printDataClassCount(df.train, "train", "Normal")

#Undersampling
frac.undersampling <- sdf_nrow(df.train.pos) / sdf_nrow(df.train) #Determine the fraction

df.train.neg.undersampled <- sdf_sample(df.train.neg, fraction = frac.undersampling, replacement = F, seed = 123)
df.train.undersampled <- sdf_bind_rows(df.train.pos, df.train.neg.undersampled)

mdle.printDataClassCount(df.train.undersampled, "train", "Undersampled")

#Oversampling
frac.oversampling <- sdf_nrow(df.train.neg) / sdf_nrow(df.train.pos) #Determine the fraction

df.train.pos.oversampled <- sdf_sample(df.train.pos, fraction = frac.oversampling, replacement = T, seed = 123)
df.train.oversampled <- sdf_bind_rows(df.train.pos.oversampled, df.train.neg)

mdle.printDataClassCount(df.train.oversampled, "train", "Oversampled")

#BL-SMOTE
df.train.Smote.labels <- collect(df.train %>% select(CLASS)) #Filter for labels only
df.train.Smote.data <- collect(df.train  %>% select(-CLASS)) #Filter for data only

df.train.blsmote.local <- BLSMOTE(df.train.Smote.data, df.train.Smote.labels, K = 7, C = 5, method = "type1")

df.train.blsmote <- copy_to(sc, df.train.blsmote.local$data) #Copy a local data frame to a remote source

mdle.printDataClassCount(df.train.oversampled, "train", "BL-SMOTE")

################# Data classification ################
svm_model <- ml_linear_svc(df.train, formula = "CLASS ~ .")
predictions <- mdle.predict(svm_model, df.test)
mdle.printConfusionMatrix(predictions, "Support vector classification  - Normal")

rf_model <- ml_random_forest(df.train, formula = "CLASS ~ .", seed = 123, type = 'auto')
predictions <- mdle.predict(rf_model, df.test)
mdle.printConfusionMatrix(predictions, "Random forest  - Normal")



svm_model <- ml_linear_svc(df.train.undersampled, formula = "CLASS ~ .")
predictions <- mdle.predict(svm_model, df.test)
mdle.printConfusionMatrix(predictions, "Support vector classification  - Undersample")

rf_model <- ml_random_forest(df.train.undersampled, formula = "CLASS ~ .", seed = 123, type = 'auto')
predictions <- mdle.predict(rf_model, df.test)
mdle.printConfusionMatrix(predictions, "Random forest  - Undersample")

svm_model <- ml_linear_svc(df.train.oversampled, formula = "CLASS ~ .")
predictions <- mdle.predict(svm_model, df.test)
mdle.printConfusionMatrix(predictions, "Support vector classification  - Oversample")

rf_model <- ml_random_forest(df.train.oversampled, formula = "CLASS ~ .", seed = 123, type = 'auto')
predictions <- mdle.predict(rf_model, df.test)
mdle.printConfusionMatrix(predictions, "Random forest  - Oversample")

svm_model <- ml_linear_svc(df.train.blsmote, formula = "class ~ .")
predictions <- mdle.predict(svm_model, df.test)
mdle.printConfusionMatrix(predictions, "Support vector classification  - BL-SMOTE")

rf_model <- ml_random_forest(df.train.blsmote, formula = "class ~ .", seed = 123, type = 'auto')
predictions <- mdle.predict(rf_model, df.test)
mdle.printConfusionMatrix(predictions, "Random forest  - BL-SMOTE")

################# Spark cleanup ################
spark_disconnect(sc)
