library(igraph)
library(tidyverse)
library(ggraph)

gpt<-readRDS("collab_test.RDS") %>%
  select(fac1, col1, fac2, col2)

# gpt-Create an edge list
edges_gpt <- gpt %>%
  group_by(fac1, fac2) %>%
  summarise(int = n(), .groups = 'drop')

#create graph object
g_gpt<-graph_from_data_frame(d = edges_gpt, directed = FALSE)

##define vertex attributes
#communities
g_gpt_sum_coms<-cluster_louvain(g_gpt)
V(g_gpt)$community<-g_gpt_sum_coms$membership

#colleges
nodes <- gpt %>%
  pivot_longer(cols = c(fac1, fac2), names_to = "fac_column", values_to = "name") %>%
  mutate(college = if_else(fac_column == "fac1", col1, col2)) %>%
  select(name, college) %>%
  distinct()

V(g_gpt)$college <-nodes$college[match(V(g_gpt)$name, nodes$name)]

#shapes (for igraph only)
# Define a mapping of colleges to shapes
college_to_shape <- c("CoAg" = "circle", "CoB" = "square", "CoEd" = "csquare",
                      "CoET" = "rectangle", "CoNHS" = "vrectangle", "Other" = "none")

# Assign shapes to vertices based on their college
V(g_gpt)$shape <- college_to_shape[V(g_gpt)$college]

#degree
V(g_gpt)$degree<-degree(g_gpt, mode="all")

#igraph plot
plot(g_gpt,
     vertex.color=V(g_gpt)$community,
     edge.width=edges_gpt$int,
     vertex.size=V(g_gpt)$degree,
     vertex.shape=V(g_gpt)$shape,
     edge.color="blue") 

#ggraph plot

ggraph(g_gpt, layout='fr')+
  geom_edge_link(aes(width=int))+
  geom_node_point(aes(size=degree*2, shape=as.factor(college), color=as.factor(community)))+
  geom_node_text(aes(label=name), repel=T)+
  theme_void()+
  labs(size="Degree", shape="College", color="Community")
