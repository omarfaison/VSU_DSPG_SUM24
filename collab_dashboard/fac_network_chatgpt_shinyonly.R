library(shiny)
library(igraph)
library(tidyverse)
library(ggraph)

# Load the dataset
gpt <- readRDS("collab_test.RDS") %>% select(fac1, col1, fac2, col2)

# Create an edge list
edges_gpt <- gpt %>%
  group_by(fac1, fac2) %>%
  summarise(int = n(), .groups = 'drop')

# Create the Shiny UI
ui <- fluidPage(
  titlePanel("Faculty Collaboration Network"),
  sidebarLayout(
    sidebarPanel(
      selectInput("college", "Select College:",
                  choices = unique(gpt$col1),
                  selected = unique(gpt$col1),
                  multiple = TRUE)
    ),
    mainPanel(
      plotOutput("networkPlot")
    )
  )
)

# Create the Shiny server
server <- function(input, output) {
  
  output$networkPlot <- renderPlot({
    # Filter nodes by selected college(s)
    selected_faculty <- gpt %>%
      filter(col1 %in% input$college | col2 %in% input$college)
    
    # Create filtered edge list
    filtered_edges <- edges_gpt %>%
      filter((fac1 %in% selected_faculty$fac1 & fac2 %in% selected_faculty$fac2) |
               (fac1 %in% selected_faculty$fac2 & fac2 %in% selected_faculty$fac1))
    
    # Create graph object
    g_gpt <- graph_from_data_frame(d = filtered_edges, directed = FALSE)
    
    # Define vertex attributes
    # Communities
    g_gpt_sum_coms <- cluster_louvain(g_gpt)
    V(g_gpt)$community <- g_gpt_sum_coms$membership
    
    # Colleges
    nodes <- selected_faculty %>%
      pivot_longer(cols = c(fac1, fac2), names_to = "fac_column", values_to = "name") %>%
      mutate(college = if_else(fac_column == "fac1", col1, col2)) %>%
      select(name, college) %>%
      distinct()
    
    V(g_gpt)$college <- nodes$college[match(V(g_gpt)$name, nodes$name)]
    
    # Degree
    V(g_gpt)$degree <- degree(g_gpt, mode = "all")
    
    # Plot the network
    ggraph(g_gpt, layout = 'fr') +
      geom_edge_link(aes(width = int)) +
      geom_node_point(aes(size = degree * 2, shape = as.factor(college), color = as.factor(community))) +
      geom_node_text(aes(label = name), repel = TRUE) +
      theme_void() +
      labs(size = "Degree", shape = "College", color = "Community")
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

