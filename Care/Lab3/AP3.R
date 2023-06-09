################# Preparation ################
library(dplyr) #data manipulation
library(sparklyr) #spark 
library(smotefamily) #For SMOTE sampling
library(data.table) #To be used when possible, as a more performant data.frame
library(magrittr)
library(mlflow)
library(mda)
library(smotefamily)

if(!exists("printConfusionMatrix", mode="function")) 
  source("/Users/souocare/Documents/Mestrado/2oSemestre/MDLE/isel-mdle-2223/Lab3/helperfunctions.R")

################# Spark setup ################
spark_disconnect_all() #just preventive code
sc <- spark_connect('local', version = '3.3.2', hadoop_version = '3', config = list())


################# Load data ################
basepath <- "/Users/souocare/Documents/Mestrado/2oSemestre/MDLE/isel-mdle-2223/Projeto/Fase1/Influenza-Outbreak-Dataset"
tr.data <- c("train_data_25.csv", "train_data_30.csv") #The data to use
labels<- c("train_labels_25.csv", "train_labels_30.csv") #the lables for the data


fun1 <- function(i) { #read CSV data
print(paste(basepath,"train",i,sep = "/"))
  read.csv(paste(basepath,"train",i,sep = "/"), header=FALSE,stringsAsFactors = FALSE)
}

fun2 <- function(i) { #read and transpose CSV data
  read.csv(paste(basepath,"train",i,sep = "/"), header=FALSE,stringsAsFactors = FALSE) %>% t %>% as.data.table
}


df<-do.call(rbind, lapply(tr.data, fun1 )) #bind csv together
df.l<-do.call(rbind, lapply(labels, fun2 )) #bind class together
names(df.l) <-c("CLASS") #rename dependent variable
df.local<- cbind(df.l,df) #bind them together

df <- copy_to(sc, df.local)

################# Visualise the dataset #######################
#View(sdf_schema(df))

#View(head(df))


# num_rows <- sparklyr::sdf_nrow(df)
# num_cols <- sparklyr::sdf_ncol(df)
# print(num_rows)
# print(num_cols)

# expected_rows <- 2190
# expected_cols <- 5

# # Use stopifnot() function to check if the number of rows and columns matches the expected values
# print("Check stopifnot number of rows")
# stopifnot(num_rows == expected_rows)
# print("Check stopifnot number of columns")
# stopifnot(num_cols == expected_cols)



################# Feature Selection #######################

# a)
#df.sel <- df %>% select("CLASS", names(df)[c(1, 2, 5, 6, 9, 10, 11, 14, 16, 17, 19, 21, 24, 25, 26, 31, 32, 33, 34, 35, 41, 44, 49, 50, 54)])

# Show the resulting data frame
#View(head(df.sel))


########### Use of generic sampling techniques ################
#split_df <- sdf_random_split(df, c("train" = 2/3, "test" = 1/3), seed = 123)
weights <- c(training = 0.6, test = 0.4)
split_dff <- sdf_random_split(df, weights = weights, seed=123)
df.train <- split_dff$training
df.test <- split_dff$test


# print(table(df.train$CLASS))
# print(table(df.test$CLASS))


df.train_r <- collect(df.train)
df.test_r <- collect(df.test)

# # Apply table() function on R data frames
# df.train$CLASS <- as.factor(df.train_r$CLASS)
# df.test$CLASS <- as.factor(df.test_r$CLASS)

# train_table <- table(df.train_r$CLASS)
# test_table <- table(df.test_r$CLASS)

# cat("Training dataset counts:\n")
# print(train_table)

# cat("\nTest dataset counts:\n")
# print(test_table)


# #VERIFIY USING SPARKS

# cat("Training dataset counts using spark:\n")
# print(df_train_counts <- df.train %>% 
#   group_by(CLASS) %>% 
#   count() %>% 
#   collect())


# cat("nTest dataset counts using spark:\n")
#   print(df_test_counts <- df.test %>% 
#   group_by(CLASS) %>% 
#   count() %>% 
#   collect())


## C) e d)
# # Define the formula for the classification model
# formula <- "CLASS ~ ."
# model <- ml_random_forest(df.train, formula = formula)
# predictions <- mdle.predict(model, df.test)
# mdle.printConfusionMatrix(predictions, "Random Forest Model")

# cat("\n\n")


######## Using imbalanced correcting sampling techniques. #######
# a)

# df.pos.train <- df.train %>% filter(sql("CLASS == '1'"))
# df.neg.train <- df.train %>% filter(sql("CLASS == '0'"))

# # Fraction
# frac <- min(sdf_nrow(df.pos.train) / sdf_nrow(df.neg.train), 1.0)

# # Undersample
# df.pos.train.undersampled <- sdf_sample(df.pos.train, replacement=FALSE, fraction=frac, seed=123)
# df.neg.train.undersampled <- sdf_sample(df.neg.train, replacement=FALSE, fraction=frac, seed=123)

# # Combine
# df.train.undersampled <- sdf_bind_rows(df.pos.train.undersampled, df.neg.train.undersampled)

# # # Count instances
# n_pos <- sdf_nrow(df.pos.train.undersampled)
# n_neg <- sdf_nrow(df.neg.train.undersampled)

# cat("Number of instances in positive class after undersampling:", n_pos, "\n")
# cat("Number of instances in negative class after undersampling:", n_neg, "\n")



# formula <- "CLASS ~ ."
# model <- ml_random_forest(df.train.undersampled, formula = formula)
# predictions <- mdle.predict(model, df.test)
# mdle.printConfusionMatrix(predictions, "Random Forest Model")

# cat("\n\n")


# c)
## the replacement parameter should be set to TRUE, which allows for duplication of instances from the minority class with replacement, whereas in random undersampling, the replacement parameter should be set to FALSE, which prevents duplication of instances from the majority class.


# df.pos.train <- df.train %>% filter(sql("CLASS == '1'"))
# df.neg.train <- df.train %>% filter(sql("CLASS == '0'"))
# # Count instances in each class
# n_pos <- sdf_nrow(df.pos.train)
# n_neg <- sdf_nrow(df.neg.train)

# # Calculate the oversampling ratio for each class
# oversampling_ratio_pos <- max(n_neg / n_pos, 1.0)
# oversampling_ratio_neg <- max(n_pos / n_neg, 1.0)

# # Oversample both positive and negative classes using Random Oversampling
# df.pos.train.oversampled <- sdf_sample(df.pos.train, 
#                                       replacement = TRUE, 
#                                       fraction = oversampling_ratio_pos, 
#                                       seed = 123)

# # Random Oversampling for negative class
# df.neg.train.oversampled <- sdf_sample(df.neg.train, 
#                                       replacement = TRUE, 
#                                       fraction = oversampling_ratio_neg, 
#                                       seed = 123)

# # Combine oversampled positive and negative classes
# df.train.oversampled <- sdf_bind_rows(df.pos.train.oversampled, df.neg.train.oversampled)

# # Count instances after oversampling
# n_pos_oversampled <- sdf_nrow(df.pos.train.oversampled)
# n_neg_oversampled <- sdf_nrow(df.neg.train.oversampled)

# cat("Number of instances in positive class after oversampling:", n_pos_oversampled, "\n")
# cat("Number of instances in negative class after oversampling:", n_neg_oversampled, "\n")


# formula <- "CLASS ~ ."
# model <- ml_random_forest(df.train.oversampled, formula = formula)
# predictions <- mdle.predict(model, df.test)
# mdle.printConfusionMatrix(predictions, "Random Forest Model")

# cat("\n\n")


# e)
# Convert Spark DataFrame to R data.frame
df.train_r <- as.data.frame(df.train)

# Apply Borderline-SMOTE sampling
df.train_oversampled <- BLSMOTE(df.train_r[, -1], df.train_r[, 1])
sink("myTest.dat")
writeLines(unlist(lapply(k, paste, collapse=" ")))
sink()
# # # Convert oversampled data back to Spark DataFrame
df.train_oversampled <- as.data.frame(df.train_oversampled)
# df.train_oversampled <- sdf_copy_to(sc, df.train_oversampled)

# # # Count instances
# n_pos_oversampled <- sdf_nrow(filter(df.train_oversampled, CLASS == "1"))
# n_neg_oversampled <- sdf_nrow(filter(df.train_oversampled, CLASS == "0"))

# cat("Number of instances in positive class after Borderline-SMOTE oversampling:", n_pos_oversampled, "\n")
# cat("Number of instances in negative class after Borderline-SMOTE oversampling:", n_neg_oversampled, "\n")

################# G2 #######################
#Glimpse of the data set
#TODO


################# G3 #######################
#Feature Selection
idx <- c(1,2,5,6,9,10,11,14,16,17,19,21,24,25,26,31,32,33,34,35,41,44,49,50,54)

#TODO


################# G4 #######################
#Generating train and test data

#df.split <- #TODO
#df.train <- #TODO
#df.test <-  #TODO

#TODO Baseline

################# G5 #######################
#Using imbalanced correcting sampling techniques
#df.pos.train<- #TODO
#df.neg.train<- #TODO
#TODO

#Oversampling
#TODO

#BSMOTE
#TODO

################# Spark cleanup ################
spark_disconnect(sc)
