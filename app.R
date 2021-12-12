# AMNC Explorer App

library(shiny)
#library(feather)
library(ggplot2)
library(lubridate)
library(tidyverse)
#library(openair)
library(shinythemes)

# Prepping the data file --------------------------------------------

webmasterk<- read_csv("bigd_h.csv") 

# %>%
#   select( #selects certain variables from dataset
#     timestamp_local, pm25, pm10
#   ) %>%
#   rename( # Renames them so that they display nicely in plots
#     PM2.5 = pm25,
#     PM10 = pm10,
#     YMD = timestamp_local
#   ) 



# Define UI for application ---------------------------------------
ui <- fluidPage(
  # Styling
  theme = shinytheme("darkly"),
  #for shinyapps.io verion:
  tags$head(includeCSS("app.css")),
  #tags$head(includeCSS("/Users/owenoconnor/Documents/College/Spring2021/CSC_132/FinalProject/Code/finalproj/www/app.css")),
  
 # tags$script(src = "app.js"),

  # Application title
 titlePanel( div(column(width = 4, tags$a(href="https://landairwater.bard.edu/projects/kaqi/", tags$img(src = "bcslaw-logo.png", height = 50, width = 400))),
                 column(width = 8, h2("Kingston NY Particulate Matter"))
 ),
 windowTitle="Kingston Particulate Matter"
 ),

  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      helpText("To explore the data from the QuantAQ Particle Sensor 
                maintained by the Bard Center for the Study of Land, Air, and Water,
                select the variables and the date range that you would like to display.",tags$br(),tags$br(),
                "You can learn more information about the monitoring program ",
                tags$a(href="https://landairwater.bard.edu/projects/kaqi/", "here."), tags$br(),tags$br(),
      "You can see current conditions ",
      tags$a(href="https://tributary.shinyapps.io/AMNC/", "here.")),
      
      dateRangeInput("dateRange1", "Date range:",
        start = "2020-01-01",
        end   = "2021-11-28",
        min = "2020-01-01",
        max = "2021-11-28"),
      checkboxGroupInput("variable", "Observations to display:",
        c("PM2.5" = "PM2.5",
          "PM10" = "PM10"
          ),
        selected = "PM2.5"
        )
    ),

    # Main display on right hand side
    mainPanel(
          plotOutput("pmPlot"), verbatimTextOutput("summary")
      ) # End of mainPanel
    ) # End of sideBarLayout
  ) # End of fluidPage


# Define server ---------------------------------------------------
server <- function(input, output) {

  # Subsets dataset based on user daterange and variable selections
  data_1 <-  reactive({subset(webmasterk, webmasterk$YMD >= ymd(input$dateRange1[1]) & webmasterk$YMD <= ymd(input$dateRange1[2]) ) %>%
    select(
      YMD,
      c(input$variable)
      ) })

  data_2 <-  reactive({subset(webfulldf,webfulldf$YMD >= ymd(input$dateRange1[1]) & webfulldf$YMD <= ymd(input$dateRange1[2]) ) })

  output$summary <- renderPrint({
    dataset <- data_1() %>%
      select(
      -YMD
      )
    summary(dataset)
    })
  
  output$pmPlot <- renderPlot({
    # Displays gentle error message if no variables are selected in checkbox
    #  validate( 
    #    need(input$variable != "", "Please select at least one variable to display")
     #   )

      # Time series point chart displaying data that user selects
      data_1() %>%
        pivot_longer(starts_with("PM"), names_to = "Pollutant Class", values_to = "observation") %>%
        ggplot(aes(x = YMD, y = observation, color = `Pollutant Class`) ) +
        geom_point() +
        labs(
          y = expression(Mass - (μg/~m^3)),
          x = NULL,
          title = paste(
            "PM2.5 Daily Averages"
          ) ) +
        theme_classic() +
        theme(plot.title = element_text(hjust = 0.5) ) +
        theme(plot.subtitle = element_text(hjust = 0.5) )
      }) # End of renderPlot
 # output$variableTest <- renderText(paste(input$variable, collapse = ", ") )

    
} # End of server

# Runs the application --------------------------------------------
shinyApp(ui = ui, server = server)
