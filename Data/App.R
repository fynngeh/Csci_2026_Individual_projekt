library(tidyverse)
library(shiny)
 

                 
shots_all <- readRDS("Data/shots_all.rds")
#Bins
bin_w <- 10   
bin_h <- 10   

shots_all <- shots_all |>
  mutate(
    bin_x = floor(x / bin_w) * bin_w,
    bin_y = floor(y / bin_h) * bin_h
  )


bin_summary <- shots_all |>
  group_by(bin_x, bin_y) |>
  summarise(
    n_shots = n(),
    n_goals = sum(goal),
    avg_goal = mean(goal),
    .groups = "drop"
  )

ui <- fluidPage(
  titlePanel("Bin Average xG – Bundesliga"),

  sidebarLayout(
    sidebarPanel(
      p("Click on a position on the field"),
      p("The colour shows the expected goals"),
      p("based on the shots of the last 2 Bundesliga season"),
      hr(),
      verbatimTextOutput("click_info")
    ),

    mainPanel(
      plotOutput("pitch", height = "600px", click = "pitch_click")
    )
  )
)



server <- function(input, output, session) {

  output$pitch <- renderPlot({
    ggplot() +
      # Spielfeld Rand
      geom_rect(aes(xmin = 0, xmax = 120, ymin = 0, ymax = 80),
                fill = "#2E8B57", color = "black", linewidth = 1) +
      # Mittellinie
      geom_segment(aes(x = 60, xend = 60, y = 0, yend = 80),
                   color = "black", linewidth = 0.6) +
      # Strafräume
      geom_rect(aes(xmin = 102, xmax = 120, ymin = 18, ymax = 62),
                fill = NA, color = "black", linewidth = 0.6) +
      geom_rect(aes(xmin = 0, xmax = 18, ymin = 18, ymax = 62),
                fill = NA, color = "black", linewidth = 0.6) +
      geom_rect(
        data = bin_summary,
        aes(
          xmin = bin_x,
          xmax = bin_x + bin_w,
          ymin = bin_y,
          ymax = bin_y + bin_h,
          fill = avg_goal
        ),
        alpha = 0.75
      ) +
      scale_fill_gradient(low = "#f7fbff",high = "#cb181d",name = "Avg goal\nprobability")+
      coord_fixed() +
      theme_void() +
      theme(
        panel.background = element_rect(fill = "#2E8B57", color = NA),
        plot.background  = element_rect(fill = "white", color = NA)
      )
  })

  output$click_info <- renderPrint({
  req(input$pitch_click)

  click_x <- input$pitch_click$x
  click_y <- input$pitch_click$y
  bx <- floor(click_x / bin_w) * bin_w
  by <- floor(click_y / bin_h) * bin_h
  row <- bin_summary |> filter(bin_x == bx, bin_y == by)
  if (nrow(row) == 0) {
    return(list(
      click_x = click_x,
      click_y = click_y,
      bin_x = bx,
      bin_y = by,
      message = "NO bin fppound (No data)."
    ))
  }

  list(
    click_x = round(click_x, 2),
    click_y = round(click_y, 2),
    bin_x = bx,
    bin_y = by,
    n_shots = row$n_shots[1],
    avg_goal = round(row$avg_goal[1], 3)
  )
})


}


 shinyApp(ui, server)


