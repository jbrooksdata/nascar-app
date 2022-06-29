library(shiny)
library(dplyr)
library(ggplot2)
library(ggthemes)

df <- readRDS("3yrdata.rds") %>%
  filter(season == 2021)

ui <- fluidPage(
  
  titlePanel("2021 Driver Stats"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput("raceInput", "Race:", choices=NULL, selected="", options = list(maxOptions = 10000)), # figure out why only 3 races at most will show, also try to order them as they appear
      selectizeInput("driverInput1", "Driver 1:", choices=NULL, selected= NULL, options = list(maxOptions = 10000)),
      selectizeInput("driverInput2", "Driver 2:", choices=NULL, selected= NULL, options = list(maxOptions = 10000))
    ),
    
    mainPanel(
      plotOutput("speed", height = 500),
      plotOutput("density", height = 350)
    )
  )
)

server <- function(input, output, session) {
  
  updateSelectizeInput(session, 'raceInput',
                       choices = unique(df$race)
  )
  
  updateSelectizeInput(session, 'driverInput1',
                       choices = sort(unique(df$driver)), # show every driver from season in alphabetical order
                       selected = "Chase Elliott",
                       server = TRUE
  )
  
  updateSelectizeInput(session, 'driverInput2',
                       choices = sort(unique(df$driver)),
                       selected = "Kyle Larson",
                       server = TRUE
  )
  
  output$speed <- renderPlot({ # figure out why the legend automatically orders alphabetically
    df %>%
      filter(race == input$raceInput) %>%
      filter(driver %in% c(input$driverInput1,input$driverInput2)) %>%
      ggplot(df, mapping = aes(x = Lap, y = LapSpeed)) +
      geom_path(mapping = aes(x = Lap, y = LapSpeed, col = driver), size = 1.25, alpha = 0.6) +
      xlim(0,max(df %>% filter(race == input$raceInput) %>% summarise(Lap))) +
      ylim(0,max(df %>% filter(race == input$raceInput) %>% summarise(LapSpeed))) +
      geom_vline(xintercept = max(df %>% filter(race == input$raceInput) %>% summarise(Lap)),
                 alpha = 0.75,
                 size = 0.5,
                 color = "black",
                 linetype = "dashed") +
      theme_fivethirtyeight() +
      labs(title = "Lap Speed Comparison",
           subtitle = "",
           caption = "data: nascar.com",
           x = "Lap",
           y = "Lap Speed (mph)") +
      theme(axis.title.y.left = element_text(face = "bold"),
            axis.title.x.bottom = element_text(face = "bold"),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12),
      )
  })
  
  output$density <- renderPlot({ 
    df %>%
      filter(race == input$raceInput) %>%
      filter(driver %in% c(input$driverInput1,input$driverInput2)) %>%
      ggplot(df, mapping = aes(x = LapSpeed, fill = driver)) +
      geom_density(show.legend = TRUE, alpha = 0.5, size = 0.75) +
      xlim(max(df %>% filter(race == input$raceInput) %>% summarise(LapSpeed) * 0.85, na.rm = TRUE),
           max(df %>% filter(race == input$raceInput) %>% summarise(LapSpeed))) +
      theme_fivethirtyeight() +
      labs(title = "Lap Speed Distribution",
           subtitle = "",
           caption = "data: nascar.com",
           x = "Lap Speed (mph)",
           y = "Frequency") +
      theme(axis.title.y.left = element_text(face = "bold"),
            axis.title.x.bottom = element_text(face = "bold"),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12))
  }
  )
}

shinyApp(ui, server)
