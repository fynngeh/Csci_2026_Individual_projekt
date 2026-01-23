library(tidyverse)
library(shiny)
 
source("Data/OpenData.R")
                 
shots_all <- readRDS("Data/shots_all.rds")

ui <- fluidPage(
  titlePanel("Bin Average xG (Bundesliga)"),
  mainPanel(
    plotOutput("pitch", height = "600px")
  )
)

server <- function(input, output, session) {
  output$pitch <- renderPlot({
    ggplot() +
      # Spielfeld Rand
      geom_rect(aes(xmin = 0, xmax = 120, ymin = 0, ymax = 80),
                fill = NA, color = "black", linewidth = 1) +
      # Mittellinie
      geom_segment(aes(x = 60, xend = 60, y = 0, yend = 80),
                   color = "black", linewidth = 0.6) +
      # Strafräume
      geom_rect(aes(xmin = 102, xmax = 120, ymin = 18, ymax = 62),
                fill = NA, color = "black", linewidth = 0.6) +
      geom_rect(aes(xmin = 0, xmax = 18, ymin = 18, ymax = 62),
                fill = NA, color = "black", linewidth = 0.6) +
      coord_fixed() +
      theme_void()
  })
}

shinyApp(ui, server)



