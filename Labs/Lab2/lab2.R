### ISEL - Instituto Superior de Engenharia de Lisboa ###
### MDLE - Mineração de Dados em Larga Escala ###
### Prática 2 ###
### Grupo 08 - Pedro Diogo A47573, Gonçalo Fonseca A50185 ###
### Data - 19/04/2023

# Libs for discretization
library(arules)
library(arulesCBA)

### Setup the dataset ###

read_data <- function(path) {
  # Read the dataset
  dataset <- read.csv(path, header = FALSE, sep = ',', skip = 1)
  return(dataset)
}

### Feature selection ###

calculate_variance <- function(data) {
  variances <- apply(data, 2, var)

  for (i in seq_along(variances)) {
    cat("Feature", colnames(data)[i], "variance:", variances[i], "\n")
  }

  # sort the vectors
  sorted_variances <- sort(variances, decreasing = TRUE)

  return(sorted_variances)
}

calculate_mm_diff <- function(data) {
  means <- apply(data, 2, mean)
  medians <- apply(data, 2, median)
  mean_median_diff <- abs(means - medians)

  for (i in seq_along(mean_median_diff)) {
    cat("Feature", colnames(data)[i], "mean-median:", mean_median_diff[i], "\n")
  }

  # Sort the vectors
  sorted_mean_median_diff <- sort(mean_median_diff, decreasing = TRUE)

  return(sorted_mean_median_diff)
}

calculate_n_features <- function(values, thresholds) {
  # Compute the adequate number of features for each threshold
  n_features <- sapply(thresholds, function(x) sum(cumsum(values) <= sum(values) * x))

  # Print the adequate number of features for each threshold
  cat(paste0("For threshold ", thresholds[1] * 100, "%, use ", n_features[1], " features.\n"))
  cat(paste0("For threshold ", thresholds[2] * 100, "%, use ", n_features[2], " features.\n"))
  cat(paste0("For threshold ", thresholds[3] * 100, "%, use ", n_features[3], " features.\n"))

  return(n_features)
}

### Feature reduction ####

calculate_pca <- function(data, name) {
  pca <- prcomp(data)
  plot(pca$sdev, type = 'p', main = paste(name, 'PCA eigenvalues'), xlab = 'Principal component', ylab = 'Eigenvalue')
  return(pca)
}

calculate_svd <- function(data, name) {
  svd <- svd(data)
  plot(svd$d, type = 'p', main = paste(name, 'SVD singular values'), xlab = "Singular value", ylab = "Magnitude")
  return(svd)
}

reduce_with_pca <- function(data, decomposition_values, n_features) {
  reduced_dataset <- scale(data, scale = FALSE) %*% decomposition_values$rotation[, 1:n_features]
  return(reduced_dataset)
}

reduce_with_svd <- function(decomposition_values, n_features) {
  reduced_dataset <- decomposition_values$u %*%
    diag(decomposition_values$d)[, 1:n_features] %*%
    t(decomposition_values$v[1:n_features, 1:n_features])

  return(reduced_dataset)
}

### main function ###

main <- function() {
  diabetes_path <- "Labs/Lab2/R/diabetes.csv"
  influenza_path <- "Labs/Lab2/R/train_data_1.csv"

  diabetes_dataset <- read_data(diabetes_path)
  diabetes_dataset_ex3 <- diabetes_dataset
  # remove labels
  diabetes_dataset <- diabetes_dataset[, -ncol(diabetes_dataset)]
  influenza_dataset <- read_data(influenza_path)

  # 1.a)
  print("Diabetes dataset variance:")
  diabetes_variances <- calculate_variance(diabetes_dataset)
  plot(diabetes_variances, main = "Diabetes - Variance", xlab = "Features", ylab = "Relevance")

  print("Diabetes dataset mean media difference:")
  diabetes_mm_diff <- calculate_mm_diff(diabetes_dataset)
  plot(diabetes_mm_diff, main = "Diabetes - Mean median difference", xlab = "Features", ylab = "Relevance")

  print("Influenza dataset variance:")
  influenza_variances <- calculate_variance(influenza_dataset)
  plot(influenza_variances, main = "Influzenza - Variance", xlab = "Features", ylab = "Relevance")

  print("Influenza dataset mean media difference:")
  influenza_mm_diff <- calculate_mm_diff(influenza_dataset)
  plot(influenza_mm_diff, main = "Influzenza - Mean Median Difference", xlab = "Features", ylab = "Relevance")

  # 1.b)
  thresholds <- c(0.9, 0.95, 0.99)

  print('Number of features for Diabetes dataset using vairance')
  diabetes_n_features_variance <- calculate_n_features(diabetes_variances, thresholds)
  print('Number of features for Diabetes dataset using mm diff')
  diabetes_n_features_mm_diff <- calculate_n_features(diabetes_mm_diff, thresholds)
  print('Number of features for Influenza dataset using vairance')
  influenza_n_features_variance <- calculate_n_features(influenza_variances, thresholds)
  print('Number of features for Influenza dataset using mm diff')
  influenza_n_features_mm_diff <- calculate_n_features(influenza_mm_diff, thresholds)

  # 2.a)
  diabetes_eigenvalues <- calculate_pca(diabetes_dataset, 'Diabetes')
  influenza_eigenvalues <- calculate_pca(influenza_dataset, 'Influenza')

  # 2.b)
  diabetes_singular_values <- calculate_svd(diabetes_dataset, 'Diabetes')
  influenza_singular_values <- calculate_svd(influenza_dataset, 'Influenza')

  # 2.c)
  diabetes_reduced_pca <- reduce_with_pca(diabetes_dataset, diabetes_eigenvalues, 5)
  diabetes_reduced_svd <- reduce_with_svd(diabetes_singular_values, 5)

  influenza_reduced_pca <- reduce_with_pca(influenza_dataset, influenza_eigenvalues, 270)
  influenza_reduced_svd <- reduce_with_svd(influenza_singular_values, 270)

  # 3.
  data_matrix <- as.matrix(diabetes_dataset)
  unsupervised_discretization <- discretize(data_matrix, "frequency", 5)
  View(unsupervised_discretization)

  diabetes_dataset_ex3$V9 <- as.factor(diabetes_dataset_ex3$V9)
  supervised_discretization <- discretizeDF.supervised(V9 ~ ., diabetes_dataset_ex3, 'mdlp')
  View(supervised_discretization)
}

main()
