library(tidyverse)
library(sf)
library(NbClust)
library(tmap)
library(factoextra)
library (cluster)
library (clustertend)

data<-readRDS("acs_places_rural_food.RDS")
data<-select(data, pct_poverty, pct_no_bach, pct_not_in_labor_force, DEPRESSION, SLEEP, PHLTH)

nomd<-na.omit(data)
numdata<-nomd %>%
  st_drop_geometry()
scaled_data <-scale(numdata)
cl1<-NbClust(scaled_data, 
             distance="euclidean",
             min.nc=2,
             max.nc=12,
             method="complete",
             index="all")

dis<-dist(scaled_data, method="euclidean")
hc<-hclust(dis, method="complete")
plot(hc)

clusters<-cutree(hc, k=4)
nomd$cluster<-as.factor(clusters)


clustermeans<-nomd %>%
  st_drop_geometry()%>%
  group_by(cluster) %>%
  summarize(
    pct_poverty = mean(pct_poverty),
    pct_no_bach = mean(pct_no_bach),
    pct_not_in_labor_force = mean(pct_not_in_labor_force),
    DEPRESSION = mean(DEPRESSION),
    SLEEP=mean(SLEEP),
    PHLTH = mean(PHLTH)) %>%
  ungroup()

cmeans_std<-as.data.frame(scale(select(clustermeans, -cluster)))
cmeans_std$cluster = clustermeans$cluster

cmeans_std_long <- cmeans_std %>%
  gather(key = "variable", value = "value", -cluster)

ggplot(cmeans_std_long, aes(x = variable, y = value, fill = cluster)) +
  geom_col(position = "dodge") +
  labs(title = "Standardized Differences Between Clusters",
       x = "Variable",
       y = "Standardized Value") +
  theme_minimal()

ggplot(cmeans_std_long, aes(x = cluster, y = value, fill = variable)) +
  geom_col(position = "dodge") +
  labs(title = "Standardized Differences Between Clusters",
       x = "cluster",
       y = "Standardized Value") +
  theme_minimal()

#pairwise scatterplot
plot(select(numdata)
     
fviz_dist(dis, show_labels=F)

tmap_mode("view")
tm_shape(nomd)+
  tm_polygons(col="cluster")
  
fviz_cluster(list(data = scaled_data, cluster = clusters),
             geom = "point",
             stand = FALSE,
             ellipse.type = "convex",
             show.clust.cent = TRUE,
             main = "Cluster Visualization")
s_vals<-silhouette(clusters, dis)
fviz_silhouette(s_vals)

clus2<-clusters<-cutree(hc, k=2)
s_vals2<-silhouette(clus2, dis)
fviz_silhouette(s_vals2)
fviz_cluster(list(data = scaled_data, cluster = clus2),
             geom = "point",
             stand = FALSE,
             ellipse.type = "convex",
             show.clust.cent = TRUE,
             main = "Cluster Visualization")
fviz_cluster(list(data = scaled_data, cluster = clus2),
             star.plot=T,
             stand = FALSE,
             ellipse.type = "convex",
             show.clust.cent = TRUE,
             main = "Cluster Visualization")

clus8<-cutree(hc, k=8)
s_vals8<-silhouette(clus8, dis)
fviz_silhouette(s_vals8)
fviz_cluster(list(data = scaled_data, cluster = clus8),
             geom = "point",
             stand = FALSE,
             ellipse.type = "convex",
             show.clust.cent = TRUE,
             main = "Cluster Visualization")

fviz_nbclust(scaled_data, kmeans, method="silhouette")
fviz_nbclust(scaled_data, hcut, method="silhouette")
fviz_nbclust(scaled_data, hcut, method="gap_stat")
fviz_pca_ind(prcomp(scaled_data), geom="point", ggtheme=theme_classic())
clust_pca<-prcomp(scaled_data)
fviz_pca_ind(clust_pca, col.ind="cos2")
fviz_pca_ind(clust_pca, geom="point", habillage=nomd$cluster)
fviz_pca_var(clust_pca, col.var="contrib")

#kmeans
set.seed(15)
km2<-kmeans(scaled_data, 2, nstart=20)
fviz_cluster(km2, data = scaled_data,
             ellipse.type = "euclid",
             geom="point",
             #repel = TRUE,
             ggtheme = theme())
k_sil_2<-silhouette(km2$cluster, dis)
fviz_silhouette(k_sil_2)

set.seed(15)
km4<-kmeans(scaled_data, 4, nstart=20)
fviz_cluster(km4, data = scaled_data,
             ellipse.type = "euclid",
             geom="point",
             #repel = TRUE,
             ggtheme = theme())
k_sil_4<-silhouette(km4$cluster, dis)
fviz_silhouette(k_sil_4)
