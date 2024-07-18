library(tidyverse)
library(sf)
library(NbClust)
library(tmap)
library(factoextra)
library (cluster)

#load and clean data
data<-readRDS("acs_places_rural_food.RDS")
data<-select(data, pct_poverty, pct_no_bach, pct_not_in_labor_force, DEPRESSION, SLEEP, PHLTH)

nomd<-na.omit(data)
numdata<-nomd %>%
  st_drop_geometry()

#scale data
scaled_data <-scale(numdata)

#calculate distance matrix
dis<-dist(scaled_data)

#calculate principal components
clust_pca<-prcomp(scaled_data)

###HIERARCHICAL (complete linkage)
#create hierachical cluster object
hc<-hclust(dis, method="complete")

#determine number of clusters
##NbClust (slow, combines multiple metrics)
cl1<-NbClust(scaled_data, 
             distance="euclidean",
             min.nc=2,
             max.nc=12,
             method="complete",
             index="all")
cl1$Best.nc

#silhouette (faster)
fviz_nbclust(scaled_data, hcut, method="silhouette")

#dendrogram
plot(hc)

#assign to clusters
hc_clus_2<-cutree(hc, k=2)

#view fit
##silhouette
hc_s_vals<-silhouette(hc_clus_2, dis)
fviz_silhouette(hc_s_vals)

#view distribution
fviz_cluster(list(data = scaled_data, cluster = hc_clus_2),
             geom = "point",
             stand = FALSE,
             ellipse.type = "convex",
             show.clust.cent = TRUE,
             main = "Cluster Visualization")

fviz_pca_ind(clust_pca, geom="point", habillage=hc_clus_2, addEllipses=TRUE, ellipse.level=0.95)

#assign clusters to dataset
nomd$h_cluster<-as.factor(hc_clus_2)

#view cluster means
h_clustermeans<-nomd %>%
  st_drop_geometry()%>%
  group_by(h_cluster) %>%
  summarize(
    pct_poverty = mean(pct_poverty),
    pct_no_bach = mean(pct_no_bach),
    pct_not_in_labor_force = mean(pct_not_in_labor_force),
    DEPRESSION = mean(DEPRESSION),
    SLEEP=mean(SLEEP),
    PHLTH = mean(PHLTH))

h_clustermeans

#visualize differences between clusters
hcm_std<-as.data.frame(scale(select(h_clustermeans, -h_cluster)))
hcm_std$cluster = h_clustermeans$h_cluster

hcm_long <- hcm_std %>%
  gather(key = "variable", value = "value", -cluster)

ggplot(hcm_long, aes(x = variable, y = value, fill = cluster)) +
  geom_col(position = "dodge") +
  labs(title = "Standardized Differences Between Clusters",
       x = "Variable",
       y = "Standardized Value") +
  theme_minimal()

ggplot(hcm_long, aes(x = cluster, y = value, fill = variable)) +
  geom_col(position = "dodge") +
  labs(title = "Standardized Differences Between Clusters",
       x = "cluster",
       y = "Standardized Value") +
  theme_minimal()

###K-MEANS
#calculate number of clusters 
#silhouette
  fviz_nbclust(scaled_data, kmeans, method="silhouette")

#create kmeans cluster object with cluster assignment
set.seed(15)
km2<-kmeans(scaled_data, 2, nstart=20)

#view fit
##silhouette
k_sil_2<-silhouette(km2$cluster, dis)
fviz_silhouette(k_sil_2)

##view distribution
fviz_cluster(km2, data = scaled_data,
             ellipse.type = "euclid",
             geom="point",
             ggtheme = theme())

#assign clusters to dataset
nomd$k_cluster<-as.factor(km2$cluster)

#view cluster means
k_clustermeans<-nomd %>%
  st_drop_geometry()%>%
  group_by(k_cluster) %>%
  summarize(
    pct_poverty = mean(pct_poverty),
    pct_no_bach = mean(pct_no_bach),
    pct_not_in_labor_force = mean(pct_not_in_labor_force),
    DEPRESSION = mean(DEPRESSION),
    SLEEP=mean(SLEEP),
    PHLTH = mean(PHLTH))

k_clustermeans

#visualize differences between clusters
km_std<-as.data.frame(scale(select(k_clustermeans, -k_cluster)))
km_std$cluster = k_clustermeans$k_cluster

km_long <- km_std %>%
  gather(key = "variable", value = "value", -cluster)

ggplot(km_long, aes(x = variable, y = value, fill = cluster)) +
  geom_col(position = "dodge") +
  labs(title = "Standardized Differences Between Clusters",
       x = "Variable",
       y = "Standardized Value") +
  theme_minimal()

ggplot(km_long, aes(x = cluster, y = value, fill = variable)) +
  geom_col(position = "dodge") +
  labs(title = "Standardized Differences Between Clusters",
       x = "cluster",
       y = "Standardized Value") +
  theme_minimal()

#compare maps
tmap_mode("view")
tm_shape(nomd)+
  tm_polygons("h_cluster")

tm_shape(nomd)+
  tm_polygons("k_cluster")


