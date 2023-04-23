################# Preparation ################
library(dplyr) #data manipulation
library(sparklyr) #spark 
library(smotefamily) #For SMOTE sampling
library(data.table) #To be used when possible, as a more performant data.frame


if (!exists("printConfusionMatrix", mode = "function"))
  source("Lab3/helperfunctions.R")

################# Spark setup ################
spark_disconnect_all() #just preventive code
sc <- spark_connect('local', version = '3.3.2', hadoop_version = '3', config = list())


################# Load data ################
basepath <- "C:/Users/pedro/OneDrive/Documents/ISEL/Mestrado/Semestre 2/MDLE/Repo/Projeto/Fase1/Influenza-Outbreak-Dataset"
tr.data <- c("train_data_25.csv", "train_data_30.csv") #The data to use
labels <- c("train_labels_25.csv", "train_labels_30.csv") #the labels for the data


fun1 <- function(i) { #read CSV data
  print(paste(basepath, "train", i, sep = "/"))
  read.csv(paste(basepath, "train", i, sep = "/"), header = FALSE, stringsAsFactors = FALSE)
}

fun2 <- function(i) { #read and transpose CSV data
  read.csv(paste(basepath, "train", i, sep = "/"), header = FALSE, stringsAsFactors = FALSE) %>% t %>% as.data.table
}


df <- do.call(rbind, lapply(tr.data, fun1)) #bind csv together
df.l <- do.call(rbind, lapply(labels, fun2)) #bind class together
names(df.l) <- c("CLASS") #rename dependent variable
df.local <- cbind(df.l, df) #bind them together

df <- copy_to(sc, df.local)

################# G2 #######################
#Glimpse of the data set
#Visualise the dataset

# a)
sdf_schema(df)

# b)
head(df)

#c)
num_rows <- sparklyr::sdf_nrow(df)
num_cols <- sparklyr::sdf_ncol(df)
print(num_rows)
print(num_cols)

expected_rows <- 2190
expected_cols <- 546

# Use stopifnot() function to check if the number of rows and columns matches the expected values
print("Check number of rows")
stopifnot(num_rows == expected_rows)
print("Check number of columns")
stopifnot(num_cols == expected_cols)

################# G3 #######################
#Feature Selection
idx <- c(1, 2, 5, 6, 9, 10, 11, 14, 16, 17, 19, 21, 24, 25, 26, 31, 32, 33, 34, 35, 41, 44, 49, 50, 54)

#a)
df.sel <- df %>% select(all_of(idx))

#b)
head(df.sel)

################# G4 #######################
#Generating train and test data

#a)
split_dff <- sdf_random_split(df, weights = c(training = 2/3, test = 1/3), seed = 123)

df.train <- split_dff$training
df.test <- split_dff$test


#b)
print(table(df.train$CLASS))
print(table(df.test$CLASS))


df.train_r <- collect(df.train)
df.test_r <- collect(df.test)

# Apply table() function on R data frames
df.train$CLASS <- as.factor(df.train_r$CLASS)
df.test$CLASS <- as.factor(df.test_r$CLASS)

train_table <- table(df.train_r$CLASS)
test_table <- table(df.test_r$CLASS)

cat("\n\n")
cat("Training dataset counts:\n")
print(train_table)

cat("\n\n")
cat("\nTest dataset counts:\n")
print(test_table)

#Verify results using Spark

cat("\n\n")
cat("Training dataset counts using spark:\n")
print(df_train_counts <- df.train %>%
  group_by(CLASS) %>%
  count() %>%
  collect())

cat("\n\n")
cat("\nTest dataset counts using spark:\n")
print(df_test_counts <- df.test %>%
  group_by(CLASS) %>%
  count() %>%
  collect())

#c) e d)

model <- ml_random_forest(df.train, formula = "CLASS ~ .", seed = 123, type = 'classification')

predictions <- mdle.predict(model, df.test)

cat("\n\n")
mdle.printConfusionMatrix(predictions, "Random Forest Model - Baseline")
cat("\n\n")

################# G5 #######################
#Using imbalanced correcting sampling techniques

# a)

df.neg.train <- df.train %>% filter(CLASS == 0)
df.pos.train <- df.train %>% filter(CLASS == 1)

# Determine the fractions of each class to use in sample
frac.undersample <- sdf_nrow(df.pos.train) / sdf_nrow(df.train)

# Undersample
df.neg.train.undersampled <- sdf_sample(df.neg.train, fraction = frac.undersample, replacement = F, seed = 123)

# Combine
df.train.undersampled <- sdf_bind_rows(df.pos.train, df.neg.train.undersampled)

# Count instances
n_pos <- sdf_nrow(df.pos.train)
n_neg <- sdf_nrow(df.neg.train.undersampled)

cat("Number of instances in positive class after undersampling:", n_pos, "\n")
cat("Number of instances in negative class after undersampling:", n_neg, "\n")

# b)

model <- ml_random_forest(df.train.undersampled, formula = "CLASS ~ .", seed = 123, type = 'classification')
predictions <- mdle.predict(model, df.test)

cat("\n\n")
mdle.printConfusionMatrix(predictions, "Random Forest Model - Undersample")
cat("\n\n")

# c)

# Determine the fractions of each class to use in sample
frac.oversample <- sdf_nrow(df.neg.train) / sdf_nrow(df.pos.train)

# Oversample
df.pos.train.oversampled <- sdf_sample(df.pos.train, fraction = frac.oversample, replacement = T, seed = 123)

# Combine oversampled positive and negative classes
df.train.oversampled <- sdf_bind_rows(df.pos.train.oversampled, df.neg.train)

# Count instances after oversampling
n_pos <- sdf_nrow(df.pos.train.oversampled)
n_neg <- sdf_nrow(df.neg.train)

cat("Number of instances in positive class after oversampling:", n_pos, "\n")
cat("Number of instances in negative class after oversampling:", n_neg, "\n")

# d)

model <- ml_random_forest(df.train.oversampled, formula = "CLASS ~ .", seed = 123, type = 'classification')
predictions <- mdle.predict(model, df.test)

cat("\n\n")
mdle.printConfusionMatrix(predictions, "Random Forest Model - Oversample")
cat("\n\n")

# e)

df.train.labels <- collect(df.train %>% select(CLASS))
df.train.data <- collect(df.train  %>% select(-CLASS))

# B-SMOTE
df.train.blsmote <- BLSMOTE(df.train.data, df.train.labels, K = 7, C = 5, method = "type1")

# f)

df.train.blsmote <- copy_to(sc, df.train.blsmote$data)

model <- ml_random_forest(df.train.blsmote, formula = "class ~ .", seed = 123, type = 'classification')
predictions <- mdle.predict(model, df.test)

cat("\n\n")
mdle.printConfusionMatrix(predictions, "Random Forest Model - Borderline SMOTE")
cat("\n\n")

################# Spark cleanup ################
spark_disconnect(sc)
