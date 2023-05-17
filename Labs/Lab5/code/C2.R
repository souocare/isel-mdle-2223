#############################################
#                                           #
# ISEL, ND, 2023                            #
#                                           #
# Didactic material to support the          #
# Big Data Mining course                    #
#                                           #
#############################################

### Fx Feature selection ----------------

#functions that apply different feature sellection approaches
fx.fsel1<-function(ds)
{
  library(dplyr)
  return(
    ds %>% 
      select('CLASS','F1','F2','F3','F4','F9','F10','F20','F21','F22','F23','MLO','Xloc','Yloc','Xnloc','Ynloc','LB','id')
  )
}

fx.fsel2<-function(ds)
{
  return(
    ds %>% 
      select('CLASS','F1','F2','F3','F4','F9','F10','F20','F21','MLO','Xloc','Yloc','Xnloc','Ynloc','LB')
  )
}

### FX Sampling ----------------

#functions that apply different samples approaches
fx.fsample1<-function(ds)
{
  return
  (
    return(sdf_sample(ds, fraction = 0.95, replacement = TRUE))
  )
}

fx.fsample2<-function(ds)
{
  return(sdf_sample(ds, fraction = 0.5, replacement = FALSE))
}

### FX Composition ----------------
#composite function
composite <- function(f)
{
  function(p){
    return(f(p))
  }
}

### MAIN FX ----------------
fx.run<<-function()
{
  
  #Initialize 
  libs = c('sparklyr', 'dplyr','purrr')
  lapply(libs, require, character.only = TRUE)
  
  #connect to spark
  ss <- spark_connect('local')  
  
  #load_data to spark
  ds<- spark_read_parquet(ss,"data/cancer.data.parquet")
  
  #Some data manipulation
  ds.id<-
    ds %>% 
    filter(F107>0) %>% 
    arrange(IFID) %>%
    mutate(id = row_number())
  
  #simulate different samples strategies
  data <- list (fx.fsample1(ds.id),fx.fsample2(ds.id))
  
  #define different functions to be applied to the data
  fx <- list(fx.fsel1,fx.fsel2)
  
  system.time(
    res<-lapply(data,FUN=fx.fsel2)
  )
  
  #DEBUG
  #collect(res[[1]])
  #collect(res[[2]])
  
  #use composite fx to apply function to data
  composite(fx.fsel1)(data[[1]])
  list.of.composites <- lapply(fx,composite)
  
  #use purrr.map to apply one composite to the data
  data.proc <- map(data,composite(fx.fsel1))
  
  #print the results
  print(map(data.proc,sdf_describe))
  
  #use the generated composite to apply
  data.proc <- map(data,list.of.composites[[2]])
  
  
  #print the results
  print(map(data.proc,sdf_describe))
  
  spark_disconnect(ss)
}

fx.run()
