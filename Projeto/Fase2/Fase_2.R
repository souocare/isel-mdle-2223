##################################################################
#                                                                #
# ISEL - Instituto Superior de Engenharia de Lisboa              #
# MDLE - Mineração de Dados em Larga Escala                      #
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
basepath <- "C:/Users/pedro/OneDrive/Documents/ISEL/Mestrado/Semestre 2/MDLE/Repo/Projeto/Influenza-Outbreak-Dataset"

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
names(df.train.l) <- c("CLASS") #Rename dependent variable
df.train.local <- cbind(df.train.l, df.train) #Bind them together (labels in first column)
df.train <- copy_to(sc, df.train.local) #Copy a local data frame to a remote source

#Test data
df.test <- do.call(rbind, lapply(test.data, fun1, type = "test")) #Bind csv together
df.test.l <- do.call(rbind, lapply(test.labels, fun2, type = "test")) #Bind class together
names(df.test.l) <- c("CLASS") #Rename dependent variable
df.test.local <- cbind(df.test.l, df.test) #Bind them together (labels in first column)
df.test <- copy_to(sc, df.test.local) #Copy a local data frame to a remote source

################# Data visualization ################
mdle.printDataDimensions(df.train, "train")
mdle.printDataDimensions(df.test, "test")

mdle.printDataClassCount(df.train, "train", "Original")
mdle.printDataClassCount(df.test, "test", "Original")

################# Feature selection ################
#Necessário?

################# Instance manipulation ################
df.train.neg <- df.train %>% filter(CLASS == 0) #Filter for negative classes
df.train.pos <- df.train %>% filter(CLASS == 1) #Filter for positive classes

#Random
#Talvez fazer para comparar?

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
#Como fazer paralelização?
