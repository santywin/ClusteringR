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

data(mtcars)
mtcars <- as_tibble(mtcars, rownames = "car") %>% 
  mutate(vs=as.character(vs), am=as.character(am), 
         gear=as.character(gear), carb=as.character(carb),
         cyl=as.character(cyl))

mtcars %>% 
  select_if(is.numeric) %>% 
  gather(key, value) %>% 
  ggplot(aes(value, fill=key)) + geom_histogram(show.legend = FALSE) +
  facet_wrap(~key, scales="free")

mtcars %>% 
  select_if(is.character) %>% 
  select(-car) %>% 
  gather(key, value) %>% 
  count(key, value) %>% 
  ggplot(aes(value, n, fill=key)) + geom_col(show.legend = FALSE) +
  facet_wrap(~key, scales="free")

mtc_cat <- mtcars %>% 
  select_if(is.character) %>% 
# select(-car)  %>% 
  dummy_cols(select_columns = names(.)[-1], remove_first_dummy = FALSE, 
             remove_selected_columns = TRUE)

mtc_num <- mtcars %>% 
  select_if(is.numeric) %>% 
  mutate(cars=mtcars$car) %>% 
  gather(key, val, -cars) %>% 
  group_by(key) %>% 
  mutate(val=scale(val)) %>% 
  ungroup() %>% spread(key, val)

mtc <- mtc_cat %>% left_join(mtc_num, by=c("car"="cars"))

mtc_mat <- as.matrix(mtc[-1])
rownames(mtc_mat) <- as.matrix(mtc[1])

d_mtc <- daisy(mtc_mat, metric = "gower", 
               type = list(assym=c(1:16)))

hc_mtc <- hclust(d_mtc, method = "complete")

clus_mods <- map_df(c(2:26), function(k){
  set.seed(2001)
  
  km <- mean(silhouette(kmeans(d_mtc, centers=k)$cluster, d_mtc)[,3])
  hc_com <- hcut(d_mtc, k=k, hc_method = "complete")$silinfo$avg.width
  hc_sin <- hcut(d_mtc, k=k, hc_method = "single")$silinfo$avg.width
  hc_ave <- hcut(d_mtc, k=k, hc_method = "average")$silinfo$avg.width
  pam <- pam(d_mtc, k=k)$silinfo$avg.width
  
  tibble(k=k, km=km, hc.com=hc_com, hc.sin=hc_sin, hc.ave=hc_ave, pam=pam)
  
})

clus_mods %>% 
  gather(mods, sil, -k) %>% 
  ggplot(aes(k, sil, color=mods)) + geom_line() + 
  geom_point() +
  scale_x_continuous(breaks = c(2:26))

set.seed(2001)
best_mod <- pam(d_mtc, k=13)

fviz_silhouette(best_mod)

tibble::enframe(best_mod$clustering, name = "car", value = "cluster") %>% 
  left_join(mtcars, by="car") %>% 
  select(cluster, mpg, disp, hp, drat, wt, qsec) %>% 
  group_by(cluster) %>% 
  summarise(size=n(), mpg=mean(mpg), disp=mean(disp), hp=mean(hp),
            drat=mean(drat), wt=mean(wt), qsec=mean(qsec)) %>% 
  gather(carac, val, -cluster) %>% 
  ggplot(aes(cluster, val, fill=carac)) + geom_col() +
  facet_wrap(~carac, scales="free") +
  scale_x_continuous(breaks = c(1:13)) 

tibble::enframe(best_mod$clustering, name = "car", value = "cluster") %>% 
  left_join(mtcars, by="car") %>% 
  mutate(cluster=as.character(cluster)) %>% 
  select_if(is.character) %>% 
  select(-car) %>% 
  gather(carac, val, -cluster) %>% 
  count(cluster, carac, val) %>% 
  ggplot(aes(val, n, fill=carac)) + geom_col() +
  facet_grid(cluster~carac, scales = "free")
