#############################################
#                                           #
# ISEL, ND, 2023                            #
#                                           #
# Didactic material to support the          #
# Big Data Mining course                    #
#                                           #
#############################################

sc <- spark_connect(master = "local")

# copy data to Spark and partition into train/test
iris_tbls <- sdf_copy_to(sc, iris, overwrite = TRUE) %>%
  sdf_partition(train = 2/3, validation = 1/3, seed = 2023)

#Feature Transformation #1
vector_assembler <- ft_vector_assembler(
  sc,
  input_cols = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
  output_col = "features"
)

vector_assembler %>% ml_transform(iris_tbls$train) %>% glimpse

#Feature Transformation #2
string_indexer_model <- ft_string_indexer(sc, "Species", "label") %>%   ml_fit(iris_tbls$train)

string_indexer_model   %>% ml_transform(iris_tbls$train) %>% glimpse

iris.pipeline <- ml_pipeline(
  vector_assembler,
  string_indexer_model
) %>%
  ml_logistic_regression()
ml_save(iris.pipeline,"iris.pipeline",overwrite = TRUE)

#Train
iris.model <- iris.pipeline %>%
  ml_fit(iris_tbls$train)
ml_save(iris.model,"iris.model",overwrite = TRUE)


#Test
iris.model %>%
  ml_transform(iris_tbls$validation) %>%
  glimpse()

#Use recorded model
iris.model.recovered <- ml_load(sc,"iris.model")

iris.model.recovered %>%
  ml_transform(iris_tbls$validation) %>%
  glimpse()

