#setwd("D:/code/git/VSU_DSPG_SUM24/Assignment03")
library(tidyverse)
library(shiny)

a1data<-readRDS("assignment01_finaldata.RDS")
a2data<-readRDS("assignment02data.RDS")

dbdata<-a2data %>%
  select(-State) %>%
  full_join(a1data, by=c("FIPS","County"))

allcancer<-ggplot(drop_na(dbdata,All.Site), aes(x=reorder(County, All.Site), y=All.Site))+
  geom_col()+
  coord_flip()+
  labs(x=NULL, y="Cancer rates", title="Cancer rates by county")+
  theme_minimal()

pcancer<-ggplot(drop_na(dbdata,pct_pancreas), aes(x=reorder(County, pct_pancreas), y=pct_pancreas*100))+
  geom_col()+
  coord_flip()+
  labs(x=NULL, y="% Cancer cases that are pancreatic", title="Pancreatic cancer % by county")+
  theme_minimal()

dxblack<-ggplot(dbdata, aes(x=Diabetes_DX, y=Black*100))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  labs(x="Diabetes Diagnoses",
       y="% Black population",
       title="relationship between diabetes diagnoses and black population")+
  theme_minimal()

ui <- fluidPage(
  titlePanel("Assignment 03 Dashboard"),
  tabsetPanel(
    tabPanel("Cancer Rates",
             plotOutput("allcancer_plot", height="900px")
    ),
    tabPanel("Pancreatic Cancer %",
             plotOutput("pcancer_plot", height="900px")
    ),
    tabPanel("Diabetes & Black Population",
             plotOutput("dxblack_plot")
    )
  )
)

# Define the server
server <- function(input, output) {
  output$allcancer_plot <- renderPlot({ allcancer })
  output$pcancer_plot <- renderPlot({ pcancer })
  output$dxblack_plot <- renderPlot({ dxblack })
}

# Run the app
shinyApp(ui = ui, server = server)
  