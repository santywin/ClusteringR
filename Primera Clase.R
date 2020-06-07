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

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

df_wb <- read_delim("data/base_wb.csv", delim = ";") %>% 
  gather(key, val, -iso3c) %>% 
  mutate(val=as.numeric(str_replace(str_trim(val), ",", "."))) %>% 
  spread(key, val)

ind <- read_delim("data/indicadores.csv", ";")

lab <- setNames(ind$indicator, ind$indicator.id)

df_wb %>% 
  gather(key, val, -iso3c) %>% 
  ggplot(aes(key, val, fill=key)) + 
  geom_boxplot() +
  facet_wrap(~key, scales = "free", nrow=2,
             labeller = as_labeller(lab))+ 
  guides(fill=FALSE) +
  theme(axis.text.x = element_blank())

mat_wb <- as.matrix(df_wb[-1])
rownames(mat_wb) <- as.matrix(df_wb[1])
mat_wb <- scale(mat_wb)

d_wb <- dist(mat_wb)

## Algoritmo jerarquico

prom <- hclust(d_wb, method = "average")
plot(prom)

meth <- c("single", "complete", "average")

plt <- map(meth, function(x) {
  
  mod <- hclust(d_wb, method = x)
  ggdendrogram(mod) + 
    ggtitle(label=paste(x, "mod", sep = " "))
  
})

multiplot(plotlist = plt, cols = 3)

## Algoritmo de particion
set.seed(2001)
km_mod <-  kmeans(d_wb, centers = 5)

km_mods <- map_df(c(2:26), function(x){
  set.seed(2001)
  mod <-  kmeans(d_wb, centers = x)
  sil <- mean(silhouette(mod$cluster, d_wb)[, 3])
  
  tibble(k=x, error=mod$tot.withinss, sil=sil)
})

km_mods %>% 
  ggplot(aes(k, sil, label=round(sil, 2))) + 
  geom_line() + 
  geom_text(size=3) +
  scale_x_continuous(breaks = c(2:26))

clus_mods <- map_df(c(2:26), function(x){
  set.seed(2001)
  
  km <- mean(silhouette(kmeans(d_wb, centers = x)$cluster, d_wb)[, 3])
  hc_ave <- hcut(d_wb, k=x, hc_method = "average")$silinfo$avg.width
  hc_com <- hcut(d_wb, k=x, hc_method = "complete")$silinfo$avg.width
  hc_sin <- hcut(d_wb, k=x, hc_method = "single")$silinfo$avg.width
  
  tibble(k=x, km=km, hc.ave=hc_ave, hc.com=hc_com, hc.sin=hc_sin)
})

clus_mods %>% 
  gather(columnas, valor, -k) %>% 
  ggplot(aes(x=k, y=valor, color=columnas)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks=c(2:26)) +
  labs(color="Modelos") +
  #  guides(color=guide_legend(label = c("HC Promedio", "HC Complete", "HC Single", "K-means"))) +
  theme(axis.line=element_line(), 
        panel.background = element_blank(),
        panel.grid.major = 
          element_line(linetype = "dashed",
                       color="gray80"))

plas_see <- read_csv("data/plas_see.csv") %>% 
  select(-c(col, surf, morph))


plas_mat <- as.matrix(plas_see[-1])
rownames(plas_mat) <- as.matrix(plas_see[1])


sc_plas <- scale(plas_mat)
d_plas <- dist(sc_plas)


clus_mods <- map_df(c(2:26), function(x){
  set.seed(2001)
  
  km <- mean(silhouette(kmeans(d_plas, centers = x)$cluster, d_plas)[, 3])
  hc_ave <- hcut(d_plas, k=x, hc_method = "average")$silinfo$avg.width
  hc_com <- hcut(d_plas, k=x, hc_method = "complete")$silinfo$avg.width
  hc_sin <- hcut(d_plas, k=x, hc_method = "single")$silinfo$avg.width
  pam <- pam(d_plas, k=x)$silinfo$avg.width
  
  # Clara necesita la base de datos (estandarizada)
  clara <- clara(sc_plas, k=x)$silinfo$avg.width
  
  tibble(k=x, km=km, hc.ave=hc_ave, hc.com=hc_com, hc.sin=hc_sin, pam=pam, clara=clara)
})


clus_mods %>% 
  gather(columnas, valor, -k) %>% 
  ggplot(aes(x=k, y=valor, color=columnas)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks=c(2:26))