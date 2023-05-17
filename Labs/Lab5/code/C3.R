#############################################
#                                           #
# ISEL, ND, 2023                            #
#                                           #
# Didactic material to support the          #
# Big Data Mining course                    #
#                                           #
#############################################


#MLlib standardizes APIs for machine learning algorithms 
# to make it easier to combine multiple algorithms into 
#a single pipeline, or workflow.
#More information: https://spark.rstudio.com/guides/pipelines.html

#Initilize 
libs = c('sparklyr', 'dplyr')
lapply(libs, require, character.only = TRUE)

#connect to spark
ss <- spark_connect('local')  

#load_data to spark
ds<- spark_read_parquet(ss,"../Labs/Lab5/data/cancer.data.parquet")

#create an empty pipeline
def.pipe <- . %>% filter(F107>0) %>% 
  arrange(IFID) %>% mutate(id = row_number()) %>% 
  select('CLASS','F1','F2','F3','F4','F9','F10','F20','F21','MLO','Xloc','Yloc','Xnloc','Ynloc','LB','id')

ds.id <- def.pipe(ds)  
print(sdf_describe(ds.id))
#descrição do dataset

spark_disconnect(ss)
