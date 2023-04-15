

### ISEL - Instituto Superior de Engenharia de Lisboa ###
### MDLE - Mineração de Dados em Larga Escala ###
### Projeto - Fase 1 ###
### Grupo 08 - Pedro Diogo A47573, Gonçalo Fonseca A50185 ###
### Data - 14/04/2023

### Setup the dataset ###

read_data <- function(path) {
  # Read the dataset
  dataset <- read.csv(path, header = FALSE, sep = ',')
  return(dataset)
}

read_keywords <- function(path) {
  # Read the keywords (attributes)
  keywords <- readLines(path)
  return(keywords)
}

join <- function(data, attributes) {
  # Join the dataset and the keywords
  names(data) <- attributes

  # Check if there are any missing column names
  if (any(is.na(names(data)))) {
    na_cols <- which(is.na(names(data))) # Get the indices of the NA column names
    for (i in na_cols) {
      names(data)[i] <- paste0("dynamic_", i) # Name the columns iteratively
    }
  }

  return(data)
}

### Feature selection (unsupervised) ###

calculate_variance <- function(data) {
  variances <- apply(data, 2, var)
  # sort the vectors
  sorted_variances <- sort(variances, decreasing = TRUE)

  for (i in seq_along(variances)) {
    cat("Feature", colnames(data)[i], "variance:",
        sorted_variances[i], "\n")
  }

  return(sorted_variances)
}

calculate_mm_diff <- function(data) {
  means <- apply(data, 2, mean)
  medians <- apply(data, 2, median)
  mean_median_diff <- abs(means - medians)

  # Sort the vectors
  sorted_mean_median_diff <- sort(mean_median_diff, decreasing = TRUE)

  for (i in seq_along(mean_median_diff)) {
    cat("Feature", colnames(data)[i], "mean-median:", sorted_mean_median_diff[i], "\n")
  }

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

### Feature reduction (unsupervised) ####

calculate_pca <- function(data) {
  pca <- prcomp(data)
  plot(pca$sdev, type = 'p', main = 'PCA', xlab = 'Principal component', ylab = 'Eigenvalue')
  return(pca)
}

calculate_svd <- function(data) {
  svd <- svd(data)
  plot(svd$d, type = 'p', main = 'SVD', xlab = "Singular value", ylab = "Magnitude")
  return(svd)
}

reduce_with_pca <- function(data, decomposition_values, n_features) {
  reduced_dataset <- predict(decomposition_values, newdata = data)[, 1:n_features]
  return(reduced_dataset)
}

reduce_with_svd <- function(decomposition_values, n_features) {
  reduced_dataset <- decomposition_values$u[, 1:n_features] %*% diag(decomposition_values$d[1:n_features]) %*% t(decomposition_values$v[, 1:n_features])
  return(reduced_dataset)
}

# test_model <- function(x_train, y_train, x_test, y_test) {
#   # Train the model
#   model <- svm(x_train, factor(unlist(y_train)), kernel = 'linear', scale = FALSE)
#
#   # Test the model
#   model_predictions <- predict(model, x_test)
#
#   # Evaluate the model
#   confusion_matrix <- table(model_predictions, unlist(y_test))
#   print(confusion_matrix)
# }


### main function ###

main <- function() {

  # Flu keywords
  keywords_path <- "~/Documents/Mestrado/2oSemestre/MDLE/isel-mdle-2223/Projeto/Fase1/Influenza-Outbreak-Dataset/flu_keywords.txt"
  keywords <- read_keywords(keywords_path)

  # Train data
  x_train_path <- "~/Documents/Mestrado/2oSemestre/MDLE/isel-mdle-2223/Projeto/Fase1/Influenza-Outbreak-Dataset/train/train_data_1.csv"
  y_train_path <- "~/Documents/Mestrado/2oSemestre/MDLE/isel-mdle-2223/Projeto/Fase1/Influenza-Outbreak-Dataset/train/train_labels_1.csv"

  x_train <- read_data(x_train_path)
  x_train <- join(x_train, keywords)
  y_train <- read_data(y_train_path)

  # Test data
  x_test_path <- "~/Documents/Mestrado/2oSemestre/MDLE/isel-mdle-2223/Projeto/Fase1/Influenza-Outbreak-Dataset/test/test_data_1.csv"
  y_test_path <- "~/Documents/Mestrado/2oSemestre/MDLE/isel-mdle-2223/Projeto/Fase1/Influenza-Outbreak-Dataset/test/test_labels_1.csv"

  x_test <- read_data(x_test_path)
  x_test <- join(x_test, keywords)
  y_test <- read_data(y_test_path)

  variances <- calculate_variance(x_train)
  mm_diff <- calculate_mm_diff(x_train)

  plot(variances, main = "Variance", xlab = "Features", ylab = "Relevance")

  plot(mm_diff, main = "Mean Median Difference", xlab = "Features", ylab = "Relevance")

  thresholds <- c(0.9, 0.95, 0.99)

  print("# of features based on variance:")
  calculate_n_features(variances, thresholds)

  print("# of features based on mean median difference:")
  calculate_n_features(mm_diff, thresholds)

  # PCA
  pca_result <- calculate_pca(x_train)

  print("# of features based on PCA:")
  n_features_pca <- calculate_n_features(pca_result$sdev, thresholds)

  reduced_x_train <- reduce_with_pca(x_train, pca_result, n_features_pca[3]) # Index 3 corresponds to the highest threshold value
  reduced_x_test <- reduce_with_pca(x_test, pca_result, n_features_pca[3])

  # # Test with non-reduced dataset
  # cat("Confusion Matrix for model with non-reduced dataset:")
  # test_model(x_train, y_train, x_test, y_test)
  #
  # # Test with reduced dataset
  # cat("Confusion Matrix for model with reduced dataset using PCA:")
  # test_model(reduced_x_train, y_train, reduced_x_test, y_test)

  # SVD
  svd_result <- calculate_svd(x_train)

  print("# of features based on SVD:")
  n_features_svd <- calculate_n_features(svd_result$d, thresholds)

  reduced_x_train <- reduce_with_svd(svd_result, n_features_svd[3])
  reduced_x_test <- reduce_with_svd(svd_result, n_features_svd[3])

  # # Test with non-reduced dataset
  # cat("Confusion Matrix for model with non-reduced dataset:")
  # test_model(x_train, y_train, x_test, y_test)
  #
  # # Test with reduced dataset
  # cat("Confusion Matrix for model with reduced dataset using SVD:")
  # test_model(reduced_x_train, y_train, reduced_x_test, y_test)
}

main()
