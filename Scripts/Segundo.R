library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)
library(cluster)
library(readr)
library(readxl)
library(stringr)
library(purrr)
library(ggdendro)
library(factoextra)

library(ggpubr)
library(FactoMineR)



#leer archivo base formato csv
plas_see <-read_csv("data/plas_see.csv") %>% 
  select(-c(col,surf,morph))



#primero se debe hacer matriz numerica
#escalando los numeros
plas_wb <- as.matrix(df_wb[-1])
rownames(plas_wb) <- as.matrix(plas_see[1])
plas_wb <- scale(plas_wb)