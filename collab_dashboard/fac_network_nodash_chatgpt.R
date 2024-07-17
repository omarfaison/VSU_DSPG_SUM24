library(igraph)
library(tidyverse)
library(ggraph)

gpt<-readRDS("collab_test.RDS") %>%
  select(fac1, col1, fac2, col2)

# gpt- Combine fac1 and fac2 into one long dataframe for counting collaborations
combined_gpt1 <- gpt %>%
  pivot_longer(cols = c(fac1, fac2), names_to = "fac", values_to = "name") %>%
  select(name, fac1, col1, fac2, col2) %>%
  distinct()

#Error in `select()`:
#! Can't select columns that don't exist.
#✖ Column `fac1` doesn't exist.

#fixed?
combined_gpt1 <- gpt %>%
  pivot_longer(cols = c(fac1, fac2), names_to = "fac", values_to = "name") %>%
  select(name, fac, col1, col2) %>%
  distinct()

#let's do this with gather
combined_gpt <- gpt %>%
  gather(fac, name, fac1,fac2) %>%
  select(name, fac, col1, col2) %>%
  distinct()

# gpt-Create an edge list
edges_gpt <- data %>%
  group_by(fac1, fac2) %>%
  summarise(int = n(), .groups = 'drop')

# gpt- Create the igraph object
g_gpt<-graph_from_data_frame(d = edges_gpt, directed = FALSE)

#plot
plot(g_gpt,
     vertex.color=cluster_louvain(g_gpt)$membership,
     edge.width=edges_gpt$int,
     edge.color="blue",
     layout=layout.circle(g_gpt))  

# this looks like the interaction level plot from, note 2 different ways of assigning vertex color to membership: direct (above) vs assign to object (below)

g_gpt_sum_coms<-cluster_louvain(g_gpt)
V(g_gpt)$community<-g_gpt_sum_coms$membership

plot(g_gpt,
     vertex.color=V(g_gpt)$community,
     edge.width=edges_gpt$int,
     vertex.size=degree(g_gpt, mode="all"),
     edge.color="blue",
     layout=layout.circle(g_gpt)) 

plot(g_gpt,
     vertex.color=V(g_gpt)$community,
     edge.width=edges_gpt$int,
     vertex.size=degree(g_gpt, mode="all"),
     edge.color="blue") 

#nodes with colleges
nodes <- gpt %>%
  pivot_longer(cols = c(fac1, fac2), names_to = "fac_column", values_to = "name") %>%
  mutate(college = if_else(fac_column == "fac1", col1, col2)) %>%
  select(name, college) %>%
  distinct()

V(g_gpt)$college <-nodes$college[match(V(g_gpt)$name, nodes$name)]

# Define a mapping of colleges to shapes
college_to_shape <- c("CoAg" = "circle", "CoB" = "square", "CoEd" = "csquare",
                      "CoET" = "rectangle", "CoNHS" = "vrectangle", "Other" = "none")

# Assign shapes to vertices based on their college
V(g_gpt)$shape <- college_to_shape[V(g_gpt)$college]

plot(g_gpt,
     vertex.color=V(g_gpt)$community,
     edge.width=edges_gpt$int,
     vertex.size=degree(g_gpt, mode="all"),
     vertex.shape=V(g_gpt)$shape,
     edge.color="blue") 

#trying with ggraph
V(g_gpt)$degree<-degree(g_gpt, mode="all")

ggraph_gpt_plot<-ggraph(g_gpt, layout='fr')+
  geom_edge_link(aes(width=int))+
  geom_node_point(aes(size=degree*2, shape=as.factor(college), color=as.factor(community)))+
  geom_node_text(aes(label=name), repel=T)+
  theme_void()+
  labs(size="Degree", shape="College", color="Community")
