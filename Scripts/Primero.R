#install.packages("ggdendro")
#install.packages("FactoMineR")

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
df_wb <-read_delim("data/base_wb.csv", delim = ";")

#se obtiene el tipo de datos y columas
str(df_wb)

#lo mismo que tiene el str pero mas detallado
glimpse(df_wb)


#modificar columnas
#limpia los datos de la data
df_wb <-read_delim("data/base_wb.csv", delim = ";") %>% 
    gather(key, val, -iso3c) %>% 
    mutate(val=as.numeric(str_replace(str_trim(val),",","."))) %>% 
    spread(key, val)
  

df_wb %>% View

head(df_wb)


summary(df_wb)


#grafico
df_wb %>% 
  gather(key, val, -iso3c) %>% 
  ggplot(aes(key, val, color=key)) +
  geom_boxplot()

#grafico
df_wb %>% 
  gather(key, val, -iso3c) %>% 
  ggplot(aes(key, val, color=key)) +
  geom_boxplot() + 
  facet_wrap(~key, scales = "free", nrow = 2)


#grafico
df_wb %>% 
  gather(key, val, -iso3c) %>% 
  ggplot(aes(key, val, fill=key)) +
  geom_boxplot() + 
  facet_wrap(~key, scales = "free", nrow = 2) + 
  guides(fill=FALSE)

  

#leer archivo detalle columnas
ind <- read_delim("data/indicadores.csv", ";")

#envia nombrede de columnas
lab <- setNames(ind$indicator, ind$indicator.id)


#grafico full mejor
df_wb %>% 
  gather(key, val, -iso3c) %>% 
  ggplot(aes(key, val, fill=key)) +
  geom_boxplot() + 
  facet_wrap(~key, scales = "free", nrow = 2,
             labeller = as_labeller(lab)) + 
  guides(fill=FALSE) + 
  theme(axis.text = element_blank(), strip.text = element_text(hjust = 0, inherit.blank = FALSE))







?scale


#primero se debe hacer matriz numerica
#escalando los numeros
mat_wb <- as.matrix(df_wb[-1])
rownames(mat_wb) <- as.matrix(df_wb[1])
mat_wb <- scale(mat_wb)

class(mat_wb)

head(as.matrix(d_wb))

#matriz distancia
#parece ser como se calcula la distancia euclidiana
#otra funcion para distancias se llama daisy tiene un metodo gower
#si tienes data continua y categorica funciona bien
d_wb <- dist(mat_wb)

###algoritmo de clusters

#algoritmo jerarquico
    #Linkage:single, complete, average
prom <- hclust(d_wb, method = "average")
plot(prom)

#algoritmo kmeans

#siendo tuco
meth <- c("single","complete","average")

plt <- map(meth, function(x){
  mod <- hclust(d_wb, method = x)
  ggdendrogram(mod)
})


plot(plt[[1]])
plot(plt[[2]])





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


#siendo tuco
meth <- c("single","complete","average")

plt <- map(meth, function(x){
  mod <- hclust(d_wb, method = x)
  ggdendrogram(mod) + ggtitle(label = paste(x, "mod", sep = " "))
})


multiplot(plotlist = plt, cols = 3)


set.seed(2001)
km_mod <- kmeans(d_wb, centers = 5)


km_mod$tot.withinss


km_mods <- map_df(c(2:26),function(x){
  set.seed(2001)
  mod <- kmeans(d_wb, centers = x)
  tibble(k=x, error=mod$tot.withinss)
})


km_mods %>% 
  ggplot(aes(k, error, label=round(error,2))) + geom_line()+
  geom_line() +
  geom_text(size=3)+
  scale_x_continuous(breaks = c(2:26))



?silhouette


# 
# km_mods <- map_df(c(2:26),function(x){
#   set.seed(2001)
#   mod <- kmeans(d_wb, centers = x)
#   sil <- silhouette(mod$cluster)
#   tibble(k=x, error=mod$tot.withinss, sil=sil)
# })


#cluster hmeans silueta por cluster
km_mods <- map_df(c(2:26),function(x){
  set.seed(2001)
  mod <- kmeans(d_wb, centers = x)
  sil <- mean(silhouette(mod$cluster, d_wb)[, 3])
  tibble(k=x, error=mod$tot.withinss, sil=sil)
})

km_mods %>% 
  ggplot(aes(k, sil, label=round(sil,2))) + geom_line()+
  geom_line() +
  geom_text(size=3)+
  scale_x_continuous(breaks = c(2:26))






#buscar el mejor metodo
clus_mods <- map_df(c(2:26), function(x){
  set.seed(2001)
  km <- mean(silhouette(kmeans(d_wb, centers = x)$cluster, d_wb)[, 3])
  hclus_sin<- hcut(d_wb, k = x, hc_func = "hclust", hc_method = "single", stand = TRUE)$silinfo$avg.width
  hclus_avg<- hcut(d_wb, k = x, hc_func = "hclust", hc_method = "average", stand = TRUE)$silinfo$avg.width
  hclus_com<- hcut(d_wb, k = x, hc_func = "hclust", hc_method = "complete", stand = TRUE)$silinfo$avg.width
  tibble(k=x, Kmeans=km,
         Average = hclus_avg,
         Single = hclus_sin,
         Complete = hclus_com,
  )
})



clus_mods %>% 
  gather(Clusters,value, -k) %>% 
  ggplot(aes(x=k, y=value, color=Clusters))+
  geom_line()+
  geom_point() +
  scale_x_continuous(breaks = c(2:26))+
  labs(color="Modelos") +
  guides(color=guide_legend(label = c("HC Promedio", "HC Completo", "HC Kmeans", "HC Simple")))
  theme(axis.line = element_line(),
        panel.background = element_blank(),
        panel.grid.major = element_line(linetype = "dashed", colour = "gray80"))






#practicando mientras llega el profe

res <- hcut(d_wb, k = 4, stand = TRUE)

res$cluster

res$size

fviz_dend(res, rect = TRUE)


fviz_silhouette(res)

fviz_cluster(res)


length(d_wb)
