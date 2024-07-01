library(tidyverse)
library(readxl)
library(networktools)
library(igraph)

data<-readRDS("collab_test.RDS")
#singles
collab_pairs<-select(data, fac1, fac2)

collab_single<-graph_from_data_frame(collab_pairs, directed=F)

collab_single_coms<-cluster_louvain(collab_single)
V(collab_single)$community<-collab_single_coms$membership

pdf("collab_single.pdf")
plot(collab_single,
     vertex.color=V(collab_single)$community,
     edge.width=degree(collab_single),
     edge.color="blue")
plot(collab_single,
     vertex.color=V(collab_single)$community,
     edge.width=degree(collab_single),
     edge.color="blue",
     layout=layout.circle(collab_single))
dev.off()

#interactions
interaction_counts<-select(final, fac1, fac2) %>%
  group_by(fac1, fac2)%>%
  summarize(int=n())

collab_match<-graph_from_data_frame(interaction_counts, directed=F) 
collab_match_coms<-cluster_louvain(collab_match)
V(collab_match)$community<-collab_match_coms$membership

pdf("collab_match.pdf")
plot(collab_match,
     vertex.color=V(collab_match)$community,
     edge.width=interaction_counts$int,
     edge.color="blue")

plot(collab_match,
     vertex.color=V(collab_match)$community,
     edge.width=interaction_counts$int,
     edge.color="blue",
     layout=layout.circle(collab_match))  
dev.off()
