### ISEL - Instituto Superior de Engenharia de Lisboa ###
### MDLE - Mineração de Dados em Larga Escala ###
### Grupo 08 - Pedro Diogo A47573, Gonçalo Fonseca A50185 ###
### Data - 14/04/2023

### Setup the dataset ###

read_data <- function(path) {
  # Read the dataset
  dataset <- read.csv(path)
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

  # check if there are any missing column names
  if (any(is.na(names(data)))) {
    na_cols <- which(is.na(names(data))) # get the indices of the NA column names
    for (i in na_cols) {
      names(data)[i] <- paste0("dynamic_", i) # name the columns iteratively
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
  # sort the vectors
  sorted_mean_median_diff <- sort(mean_median_diff, decreasing = TRUE)

  for (i in seq_along(mean_median_diff)) {
    cat("Feature", colnames(data)[i], "mean-median:",
        sorted_mean_median_diff[i], "\n")
  }

  return(sorted_mean_median_diff)
}

calculate_cumsum <- function(values, thresholds) {
  # Compute the adequate number of features for each threshold
  n_features <- sapply(thresholds, function(x) sum(cumsum(values) <= sum(values) * x))

  # Print the adequate number of features for each threshold
  cat(paste0("For threshold ", thresholds[1] * 100, "%, use ",
             n_features[1], " features.\n"))
  cat(paste0("For threshold ", thresholds[2] * 100, "%, use ",
             n_features[2], " features.\n"))
  cat(paste0("For threshold ", thresholds[3] * 100, "%, use ",
             n_features[3], " features.\n"))
}

### Feature reduction (unsupervised) ####

calculate_pca <- function(data) {
  pca <- prcomp(data, center = FALSE, scale. = FALSE)

  plot(pca$sdev^2, type = 'p', main = 'PCA', xlab = 'Principal component', ylab = 'Eigenvalue', xlim = c(0,20)) b  
  return(pca)
}

calculate_svd <- function(data) {
  svd <- svd(data)

  plot(svd$d^2, type = 'p', main = 'SVD', xlab = "Singular value", ylab = "Magnitude")
  return(svd)
}

reduce_with_pca <- function(data, decomposition_values) {
  mydata_pca_new <- predict(decomposition_values, newdata = data)[, 1:10]
  return(mydata_pca_new)

}

reduce_with_svd <- function(decomposition_values) {


}

### main function ###

main <- function() {
  data_path <- "Projeto/Fase1/Influenza-Outbreak-Dataset/train/train_data_1.csv"
  keywords_path <- "Projeto/Fase1/Influenza-Outbreak-Dataset/flu_keywords.txt"

  dataset <- read_data(data_path)
  keywords <- read_keywords(keywords_path)
  dataset <- join(dataset, keywords)

  variances <- calculate_variance(dataset)
  mm_diff <- calculate_mm_diff(dataset)

  # TODO - fix plots (function maybe?)
  barplot(variances, main = "Variance per Feature", xlab = "Features",
          ylab = "Variance", xlim = c(0, 10), ylim = c(0, 0.8))

  barplot(mm_diff, main = "Mean Median per Feature",
          xlab = "Features", ylab = "Mean Median", xlim = c(0, 10), ylim = c(0, 0.5))

  thresholds <- c(0.1, 0.5, 0.9)

  calculate_cumsum(variances, thresholds)

  calculate_cumsum(mm_diff, thresholds)

  pca_result <- calculate_pca(dataset)
  svd_result <- calculate_svd(dataset)


  x <- reduce_with_pca(dataset, pca_result)
}

main()
