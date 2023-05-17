#############################################
#                                           #
# ISEL, ND, 2023                            #
#                                           #
# Didactic material to support the          #
# Big Data Mining course                    #
#                                           #
#############################################

#Verbs are dplyr commands for manipulating data. 
#When connected to a Spark DataFrame, dplyr translates the commands into Spark SQL statements. 
#From: https://spark.rstudio.com/guides/dplyr.html

#Initialize 
libs = c('sparklyr', 'dplyr')
lapply(libs, require, character.only = TRUE)

#connect to spark
ss <- spark_connect('local')  

#load_data to spark
system.time(
  ds<- spark_read_parquet(ss,"data/cancer.data.parquet")
)

#Some data manipulation
system.time(
  ds.id<-
    ds %>% 
    filter(F107>0) %>% 
    arrange(IFID) %>%
    mutate(id = row_number())
)  

system.time(
  sdf_describe(ds.id)
)    

spark_disconnect(ss)
