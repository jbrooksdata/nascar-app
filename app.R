library(shiny) # creating and deploying app
library(dplyr) # basic functions; mainly %>%
library(ggplot2) # better looking plots
library(ggthemes) # even better looking plots

df <- readRDS("3yrdata.rds") %>% # load lap data and filter
  filter(season == 2021) %>%
  slice(-c(257461,257573)) # remove corrupt rows

stops <- readRDS("2021stops.rds") %>% # load pit stop data
  filter(pit_in_lap_count != 0) # ignore any pre-race logs

ui <- fluidPage(
  
  titlePanel("2021 Driver Stats"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput("raceInput", "Race:", choices=NULL, selected="", options = list(maxOptions = 10000)),
      selectizeInput("driverInput1", "Driver 1:", choices=NULL, selected= NULL, options = list(maxOptions = 10000)),
      selectizeInput("driverInput2", "Driver 2:", choices=NULL, selected= NULL, options = list(maxOptions = 10000)),
      tableOutput("driver1stops"),
      tableOutput("driver2stops")
    ),
    
    mainPanel(
      plotOutput("speed", height = 500),
      plotOutput("density", height = 350) # manual height for each, ideal for 1920x1080
    )
  )
)

server <- function(input, output, session) {
  
  updateSelectizeInput(session, 'raceInput',
                       choices = unique(df$race)
  )
  
  updateSelectizeInput(session, 'driverInput1',
                       choices = sort(unique(df$driver)), # alphabetize drivers
                       selected = "Chase Elliott",
                       server = TRUE
  )
  
  updateSelectizeInput(session, 'driverInput2',
                       choices = sort(unique(df$driver)),
                       selected = "Kyle Larson",
                       server = TRUE
  )
  
  output$driver1stops <- renderTable({ # return table of pit stop data
    stops %>%
      filter(race == input$raceInput) %>%
      filter(driver == input$driverInput1) %>%
      select(`Driver 1` = driver, Lap = pit_in_lap_count, `Stop Type` = pit_stop_type, Duration = pit_stop_duration)
  })
  
  output$driver2stops <- renderTable({ # separate table by driver
    stops %>%
      filter(race == input$raceInput) %>%
      filter(driver == input$driverInput2) %>%
      select(`Driver 2` = driver, Lap = pit_in_lap_count, `Stop Type` = pit_stop_type, Duration = pit_stop_duration)
  })
  
  output$speed <- renderPlot({ # return line plot for comparison of lap speed throughout race
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
                 linetype = "dashed") + # dashed line for end of race
      theme_fivethirtyeight() +
      labs(title = "Driver Speed Comparison",
           subtitle = "",
           caption = "@jbrooksdata | data: nascar.com",
           x = "Lap",
           y = "Speed (mph)") +
      theme(axis.title.y.left = element_text(face = "bold"),
            axis.title.x.bottom = element_text(face = "bold"),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            legend.title = element_text(size = 14), ### figure out why the legend automatically orders alphabetically
            legend.text = element_text(size = 12)
      )
  })
  
  output$density <- renderPlot({ # return density plot of for lap speed comparison
    df %>%
      filter(race == input$raceInput) %>%
      filter(driver %in% c(input$driverInput1,input$driverInput2)) %>%
      ggplot(df, mapping = aes(x = LapSpeed, fill = driver)) +
      geom_density(show.legend = TRUE, alpha = 0.5, size = 0.75) +
      xlim(max(df %>%
                 filter(race == input$raceInput) %>%
                 filter(FlagState == 1) %>%
                 summarise(LapSpeed) * 0.9,
               na.rm = TRUE),
           max(df %>%
                 filter(race == input$raceInput) %>%
                 filter(FlagState == 1) %>% # return speeds for green flag laps
                 summarise(LapSpeed))) +
      theme_fivethirtyeight() +
      labs(title = "Driver Speed Distribution",
           subtitle = "on green flag laps",
           caption = "@jbrooksdata | data: nascar.com",
           x = "Speed (mph)",
           y = "Frequency") +
      theme(axis.title.y.left = element_text(face = "bold"),
            axis.title.x.bottom = element_text(face = "bold"),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12))
  }
  )
}

shinyApp(ui, server)
