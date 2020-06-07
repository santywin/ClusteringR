library(ggplot2)
library(magrittr)
library(tidyr)
library(dplyr)
library(readr)
library(purrr)
library(factoextra)
library(cluster)
library(stringr)
library(fastDummies)
library(parallelDist)

bank <- read_delim("~/SEE/SEE cluster/data/pract/bank-full.csv", 
                   delim = ";") %>% 
  rename(susc=y) %>% mutate(id=as.character(1:nrow(.))) %>% 
  select(id, everything())

bank %>%
  select(-id) %>% 
  select_if(is.character) %>% 
  gather(key, value) %>% 
  count(key, value) %>% 
  ggplot(aes(value, n, fill=key)) + geom_col(show.legend = FALSE) +
  facet_wrap(~key, scales = "free") + coord_flip()

bank %>% 
  select(-id) %>% 
  select_if(is.numeric) %>% 
  gather(key, value) %>% 
  ggplot(aes(value, fill=key)) + geom_histogram(show.legend = FALSE) +
  facet_wrap(~key, scales = "free")

bankC <- bank %>%
  select_if(is.character)

bank_cat <- dummy_cols(bankC, names(bankC)[-c(1, 5, 6, 7, 11)], remove_first_dummy = FALSE, 
                       remove_selected_columns = TRUE) %>% 
  mutate(def=if_else(default=="no", 0, 1), hous=if_else(housing=="no", 0, 1),
         loa=if_else(loan=="no", 0, 1), sus=if_else(susc=="no", 0, 1)) %>% 
  select(-c(default, housing, loan, susc))

bankN <- bank %>% 
  mutate(id=as.numeric(id)) %>% select_if(is.numeric) %>% 
  mutate(id=as.character(id))

bank_num <- bankN %>% 
  gather(key, value, -id) %>% 
  group_by(key) %>% mutate(value=scale(value)) %>% 
  spread(key, value)

bnk <- bank_cat %>% left_join(bank_num, by="id")

bank_mat <- as.matrix(bnk[-1])
rownames(bank_mat) <- as.matrix(bnk[1])

bank_matN <- as.matrix(bank_num[-1])
rownames(bank_matN) <- as.matrix(bank_num[1])


d_bank <- daisy(bank_mat, metric = "gower", type = list(assym=c(1:46)))

d_bankN <- parDist(bank_matN, method = "euclidean")


clara_mods <- map_df(c(2:25), function(x){
  set.seed(2001)
  # mod <-  kmeans(d_bankN, centers = x)
  # sil <- mean(silhouette(mod$cluster, d_wb)[, 3])
  clara <- clara(bank_matN, k=x, samples = 10, sampsize = 1000)$silinfo$avg.width
  
  tibble(k=x, sil=clara)
})
  
clara_mods %>% 
  ggplot(aes(k, sil)) +  geom_line() + 
  geom_point()

