library(tidyr)
library(dtwclust)
library(dplyr)
library(ggplot2)
library(reshape)
co2 <- read.csv('/Users/artemsmirnov/Desktop/Росэк/df_diesel_diff.xlsx.csv', sep=',')
#http://rstudio-pubs-static.s3.amazonaws.com/398402_abe1a0343a4e4e03977de8f3791e96bb.html
co2_long <- gather(co2[2:375,2:86])
co2_long$time<-rep(1:374, 85)
#co2_long  %>% 
#  ggplot(aes(x= time, y= value, color= key)) +
#  geom_line( size=0.2) +
#  ggtitle("Control chart sequences") + 
#  facet_wrap(~ key , scales = 'free_x', nrow= 10) 
co2_list <- as.list(utils::unstack(co2_long, value ~ key))

co2_list_z <- dtwclust::zscore(co2_list)

#hierarchical clustering with 10% window size for up to k=10 clusters
cluster_dtw_h <-list()
for (i in 2:5)
{
  cluster_dtw_h[[i]] <- tsclust(co2_list_z, type = "partitional", k = i, distance = "dtw", seed = 390, preproc = NULL, args = tsclust_args(dist = list(window.size =5L)))
}
k=3
plot(cluster_dtw_h[[k]])
cluster_dtw_h[[k]]@cluster
names <- names(co2[, 2:86])
results <- as.data.frame(cbind(names(co2[, 2:86]), cluster_dtw_h[[2]]@cluster, cluster_dtw_h[[3]]@cluster, cluster_dtw_h[[4]]@cluster, cluster_dtw_h[[5]]@cluster))

library("writexl")
write_xlsx(results,"/Users/artemsmirnov/Documents/diesel_results_95_diff.xlsx")
#Ищем минимум и максимум волатильности. Перед каждым запуском кода ниже перезапускать загрузку датафрейма
df <- t(co2[2:375,2:86])
vol <- sapply(co2, sd, na.rm = TRUE)
vol <- data.frame(vol)
vol <- vol[2:86,]
df_res_vol <- cbind.data.frame(vol, results)
#При выделении 2 кластеров:
for (i in 1:2){
  print(i)
  print(min(df_res_vol$vol[df_res_vol$V2 == i], na.rm = TRUE))
  print(max(df_res_vol$vol[df_res_vol$V2 == i], na.rm = TRUE))
}

for (i in 1:3){
  print(i)
  print(min(df_res_vol$vol[df_res_vol$V3 == i], na.rm = TRUE))
  print(max(df_res_vol$vol[df_res_vol$V3 == i], na.rm = TRUE))
}

for (i in 1:4){
  print(i)
  print(min(df_res_vol$vol[df_res_vol$V4 == i], na.rm = TRUE))
  print(max(df_res_vol$vol[df_res_vol$V4 == i], na.rm = TRUE))
}

for (i in 1:5){
  print(i)
  print(min(df_res_vol$vol[df_res_vol$V5 == i], na.rm = TRUE))
  print(max(df_res_vol$vol[df_res_vol$V5 == i], na.rm = TRUE))
}
