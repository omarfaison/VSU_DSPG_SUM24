library(shiny)
library(shinydashboard)
library(igraph)
library(visNetwork)
library(tidyverse)

# Read in CSV file and prepare the network data
Faculty_1_2 <- read.csv("Faculty 1 and 2.csv")
net1 <- graph_from_data_frame(Faculty_1_2, directed = FALSE)

# Detect communities
comsW <- cluster_walktrap(net1)
V(net1)$community <- comsW$membership

# Calculate centrality and assign to net1
V(net1)$degree <- degree(net1) # Number of connections each nodes has
V(net1)$betweenness <- betweenness(net1) #Measure how often a node is on the shortest path between other nodes
V(net1)$closeness <- closeness(net1) #Measure how close a vertex is to all other vertices
V(net1)$eigenvector <- eigen_centrality(net1)$vector #Measure a vertex's influence based on the influence of its neighbors

# Create a data frame name nodes and add node attributes from net1 to nodes
nodes <- data.frame(id = V(net1)$name,
                    label = V(net1)$name,
                    group = V(net1)$community,
                    degree = V(net1)$degree,
                    betweenness = V(net1)$betweenness,
                    closeness = V(net1)$closeness,
                    eigenvector = V(net1)$eigenvector)

#Create a data frame edges with edge connections from net1 plot
edges <- data.frame(from = ends(net1, E(net1))[, 1],
                    to = ends(net1, E(net1))[, 2])

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Network Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Network Analysis Plot", tabName = "network_analysis", icon = icon("network-wired"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "network_analysis",
              fluidRow(
                visNetworkOutput("network", height = "700px")
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$network <- renderVisNetwork({
    visNetwork(nodes, edges) %>%
      visNodes(color = list(background = "lightblue")) %>%
      visEdges(color = "blue", width = 3) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1),
                 nodesIdSelection = TRUE) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visEvents(selectNode = "function(nodes) {
                var node = nodes.nodes[0];
                Shiny.onInputChange('selected_node', node);
                }")
  })
  
  observeEvent(input$selected_node, {
    node_id <- input$selected_node
    node_data <- nodes[nodes$id == node_id, ]
    
 #displays a modal dialog with detailed information about the selected node(degree, betweenness,etc)
    showModal(modalDialog(
      title = paste("Node Information:", node_data$label),
      paste("Degree:", node_data$degree),
      paste("Betweenness:", node_data$betweenness),
      paste("Closeness:", node_data$closeness),
      paste("Eigenvector:", node_data$eigenvector),
      easyClose = TRUE,
      footer = NULL
    ))
  })
}

# Run the shiny app
shinyApp(ui = ui, server = server)


