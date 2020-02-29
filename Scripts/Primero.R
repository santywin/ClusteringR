

library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)
library(cluster)
library(readr)
library(readxl)
library(stringr)


#leer archivo base formato csv
df_wb <-read_delim("data/base_wb.csv", delim = ";")

#se obtiene el tipo de datos y columas
str(df_wb)

#lo mismo que tiene el str pero mas detallado
glimpse(df_wb)


#modificar columnas
df_wb <-read_delim("data/base_wb.csv", delim = ";") %>% 
    gather(key, val, -iso3c) %>% 
    mutate(val=as.numeric(str_replace(str_trim(val),",","."))) %>% 
    spread(key, val)
  

df_wb %>% View

?read_delim






