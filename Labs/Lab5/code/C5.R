#############################################
#                                           #
# ISEL, ND, 2023                            #
#                                           #
# Didactic material to support the          #
# Big Data Mining course                    #
#                                           #
#############################################


fx.dataprep<-function(ds)
{
  library(dplyr)
  def.pipe <- . %>% filter(F107>0) %>% 
    arrange(IFID) %>% mutate(id = row_number()) %>% 
    select('CLASS','F1','F2','F3','F4','F9','F10','F20','F21','MLO','Xloc','Yloc','Xnloc','Ynloc','LB')
  return(def.pipe(ds))
}

fx.datafsample<-function(ds)
{
  return
  (
    sdf_random_split(
      ds,
      training = 0.60,
      testing = 0.30,
      rest = 0.10)
  )
}

#Initilize 
libs = c('sparklyr', 'dplyr','broom')
lapply(libs, require, character.only = TRUE)

#connect to spark
ss <- spark_connect('local')  

#load_data to spark
ds<- spark_read_parquet(ss,"data/cancer.data.parquet")
ds.prep <- fx.dataprep(ds) %>% fx.datafsample

#Put Path for you case
p<-"/home/Grupo1/AP4"
model <- ml_load(ss, paste(p,"Cancer_production_pipeline",sep="/"))


#See the error
ml_transform(model, ds.prep[[1]])%>%
  group_by(CLASS, prediction) %>%
  tally()
