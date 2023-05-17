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
  ds<- spark_read_parquet(ss,"../Labs/Lab5/data/cancer.data.parquet")
)

"
Elapsed Time is the time charged to the CPU(s) for the expression.

User Time is the wall clock time. The time that you as a user experienced.

Usually both times are relatively close. But they may vary in some other situations. For example:

If elapsed time > user time, this means that the CPU is waiting around for some other operations (may be external) to be done.
If elapsed time < user time, this means that your machine has multiple cores and is able to use them
"


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
