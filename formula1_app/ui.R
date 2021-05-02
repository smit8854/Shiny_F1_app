
library(shiny)


# data analysis
library(tidyverse)
library(httr)
library(jsonlite)
library(devtools)
library(gt)
library(lubridate)
library(gganimate)
library(gifski)

# downloading data from the web
library(formula1data)
library(XML)
library(httr)
library(RCurl)
library(jsonlite)


navbarPage(title = "Formula 1 Dashboard",
                 tabPanel(title = "Season Driver Results",
                          numericInput(inputId = "season_driver", label = "Enter a year from 1950 - 2021", value = 2021, min = 1950, max = 2021),
                          plotOutput("season_driver"),
                          br(),
                          gt_output("driver_season"), br(),br()
                 ),
                 tabPanel(title = "Season Constructor  Results",
                          numericInput(inputId = "year", label = "Enter a year from 1958 - 2021", value = 2021, min = 1958, max = 2021),
                          plotOutput("constructor_plot"),
                          br(),
                          gt_output("season_table"), br(), br()
                 ),
                 # tabPanel(title = "Constructor History",
                 #         selectInput(inputId = "team", label = "Constructor Name", choices = constructor_results$Team),
                 #         gt_output("team_table")                          
                 # plotOutput("season_input")
                 # ),     
                 # tabPanel(title = "Driver History",
                 #            selectInput(inputId = "driver", label = "Driver Name", choices = drivers$surname),
                 #            tableOutput("driver_table")
                 # ),
                 tabPanel(title = "2021 Schedule",
                          gt_output(outputId = "round_table")
                 ),
                 tabPanel(title = "Race Results 2021",
                          # numericInput(inputId = "year_gif", label = "Enter a year from 1950 - 2021", value = 2020, min = 1950, max = 2021),
                          selectInput(inputId = "race_gif", label = "Select a round from the 2021 season", choices = c("Bahrain" = 1, "Imola" = 2, "Portugal" = 3),
                                      selected = NULL),
                          imageOutput("results_gif"),
                          br(),br(),br(),br(),br(),br(),
                          gt_output("results_table"),
                          br(), br()
                 )
)
