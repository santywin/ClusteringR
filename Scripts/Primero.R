#install.packages("ggdendro")

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

head(df_wb)


summary(df_wb)



df_wb %>% 
  gather(key, val, -iso3c) %>% 
  ggplot(aes(key, val, color=key)) +
  geom_boxplot()


df_wb %>% 
  gather(key, val, -iso3c) %>% 
  ggplot(aes(key, val, color=key)) +
  geom_boxplot() + 
  facet_wrap(~key, scales = "free", nrow = 2)



df_wb %>% 
  gather(key, val, -iso3c) %>% 
  ggplot(aes(key, val, fill=key)) +
  geom_boxplot() + 
  facet_wrap(~key, scales = "free", nrow = 2) + 
  guides(fill=FALSE)

  


ind <- read_delim("data/indicadores.csv", ";")

lab <- setNames(ind$indicator, ind$indicator.id)


df_wb %>% 
  gather(key, val, -iso3c) %>% 
  ggplot(aes(key, val, fill=key)) +
  geom_boxplot() + 
  facet_wrap(~key, scales = "free", nrow = 2,
             labeller = as_labeller(lab)) + 
  guides(fill=FALSE) + 
  theme(axis.text = element_blank(), strip.text = element_text(hjust = 0, inherit.blank = FALSE))






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

mat_wb <- as.matrix(df_wb[-1])
rownames(mat_wb) <- as.matrix(df_wb[1])
mat_wb <- scale(mat_wb)

class(mat_wb)

head(as.matrix(d_wb))

#matriz distancia
d_wb <- dist(mat_wb)

#algoritmo jerarquico

prom <- hclust(d_wb, method = "average")

plot(prom)


#me apesta la vida
comp <- hclust(d_wb, method = "complete")
plot(comp)

#maximo es el completo 


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




