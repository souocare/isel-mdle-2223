#############################################
#                                           #
# ISEL, ND, 2023                            #
#                                           #
# Didactic material to support the          #
# Big Data Mining course                    #
#                                           #
#############################################

#Initialize --------------------
libs = c('sparklyr', 'dplyr')
lapply(libs, require, character.only = TRUE)

#Connect to spark -------------
ss <- spark_connect('local')  

#Load data to spark -----------
ds<- spark_read_parquet(ss,"../Labs/Lab5/data/cancer.data.parquet")

#See that no features are created or removed
def.pipe <- . %>% filter(F107>0)

#Create a production pipeline -------
prod.pipe <- 
  ml_pipeline(ss) %>%
  ft_r_formula(CLASS ~ F1 + F2 + F3 + F4 + F9 + F10 + F20 + F21 + MLO + Xloc + Yloc + Xnloc + Ynloc + LB ) %>% 
  ml_random_forest_classifier()


#Generate dataset for training and testing  ----- 
ds.split <- sdf_random_split(
  def.pipe(ds),
  training = 0.60,
  testing = 0.30,
  rest = 0.10)

#Fit data to the the model -----------
ds.pipe.fit <- ml_fit(
  prod.pipe ,
  ds.split$training
)

#Test the model ----------
predictions <- ml_transform(
  ds.pipe.fit,
  ds.split$testing
)

ds.pipe.AUC <- predictions %>% ml_binary_classification_evaluator(
  label_col = "CLASS",
  raw_prediction_col = "rawPrediction",
  metric_name = "areaUnderROC"
)

ds.pipe.f1 <- predictions %>% ml_multiclass_classification_evaluator(
  label_col = "CLASS",
  raw_prediction_col = "rawPrediction",
  metric_name = "f1"
)

cat(paste("AUC:",ds.pipe.AUC),"\n")
cat(paste("F1 :",ds.pipe.f1),"\n")

predictions %>%
  group_by(CLASS, prediction) %>%
  tally() %>% 
  print

#Save the resulting Pipeline and Pipeline model------
ml_save(
  prod.pipe,
  "../Labs/Lab5/Cancer_production_pipeline",
  overwrite = TRUE
)

ml_save(
  ds.pipe.fit,
  "../Labs/Lab5/Cancer_production_pipeline_model",
  overwrite = TRUE
)

spark_disconnect(ss)
