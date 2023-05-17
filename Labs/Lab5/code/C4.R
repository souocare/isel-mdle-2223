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
ds<- spark_read_parquet(ss,"data/cancer.data.parquet")

def.pipe <- . %>% filter(F107>0) %>% 
  arrange(IFID) %>% mutate(id = row_number()) %>% 
  select('CLASS','F1','F2','F3','F4','F9','F10','F20','F21','MLO','Xloc','Yloc','Xnloc','Ynloc','LB','id')

#create a production pipeline
prod.pipe <- 
  ml_pipeline(ss) %>%
  ft_r_formula(CLASS ~ .) %>% 
  ml_random_forest_classifier()


#Generate dataset for training and testing  
ds.split <- sdf_random_split(
  def.pipe(ds),
  training = 0.60,
  testing = 0.30,
  rest = 0.10)

#fit the model
ds.pipe.fit <- ml_fit(
  prod.pipe ,
  ds.split$training
)

#Test the model
ds.pipe.fit.test <- ml_transform(
  ds.pipe.fit,
  ds.split$testing
)

ds.pipe.fit.test %>%
  group_by(CLASS, prediction) %>%
  tally()

class(ds.pipe.fit.test) 
#store the resulting Pipeline nad Pipeline model and 

ml_save(
  prod.pipe,
  "Cancer_production_pipeline",
  overwrite = TRUE
)

ml_save(
  ds.pipe.fit,
  "Cancer_production_pipeline_model",
  overwrite = TRUE
)

spark_disconnect(ss)
