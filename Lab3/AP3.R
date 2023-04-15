################# Preparation ################
library(dplyr) #data manipulation
library(sparklyr) #spark 
library(smotefamily) #For SMOTE sampling
library(data.table) #To be used when possible, as a more performant data.frame
library(magrittr)

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


print(table(df.train$CLASS))


df.train_r <- collect(df.train)
df.test_r <- collect(df.test)

# Apply table() function on R data frames
df.train$CLASS <- as.factor(df.train_r$CLASS)
df.test$CLASS <- as.factor(df.test_r$CLASS)

train_table <- table(df.train_r$CLASS)
test_table <- table(df.test_r$CLASS)

cat("Training dataset counts:\n")
print(train_table)

cat("\nTest dataset counts:\n")
print(test_table)


#VERIFIY USING SPARKS

cat("Training dataset counts using spark:\n")
print(df_train_counts <- df.train %>% 
  group_by(CLASS) %>% 
  count() %>% 
  collect())


cat("nTest dataset counts using spark:\n")
  print(df_test_counts <- df.test %>% 
  group_by(CLASS) %>% 
  count() %>% 
  collect())


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
