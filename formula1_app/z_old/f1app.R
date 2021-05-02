
# host on shiny
# library(rsconnect)
library(shiny)

runApp()
# # data analysis
# library(tidyverse)
# library(httr)
# library(jsonlite)
# library(devtools)
# library(gt)
# library(lubridate)
# library(gganimate)
# library(gifski)
# 
# # downloading data from the web
# library(formula1data)
# library(XML)
# library(httr)
# library(RCurl)
# library(jsonlite)

# rsconnect::setAccountInfo(name='michael-l-smith', token='D8F3BAD63D286339FEF56DBFADC9AFBD', secret='oTor0p/dUI/TAlDGK/Qg7ohuJFhISgC0qdhDA30T')
# 
# rsconnect::deployApp(appDir = "C:\\Users\\Michael\\Documents\\_PhD\\1_Spring_21\\Data_Viz_7462\\Assignments\\Shiny_App\\Formula1_app\\app\\")
# 
# setwd("C:\\Users\\Michael\\Documents\\_PhD\\1_Spring_21\\Data_Viz_7462\\Assignments\\Shiny_App\\formula1_app\\app")
# 
# rsconnect::configureApp(appName = "formula1_app", size = "large")
# 
# # createF1db()


# getF1Schedule2 <- function(year){
#     url <- paste0("https://ergast.com/api/f1/", year, ".json")
#     sched <- fromJSON(content(GET(url), as = "text"))$MRData$RaceTable$Races
#     sched <- sched %>%
#         mutate_at(c("season", "round"), as.integer) %>%
#         mutate(time = gsub("Z", "", .data$time),
#                datetime = ymd_hms(paste(.data$date, .data$time)))
#     # select(-.data$time)
#     sched
#     
# }
# 
# ui <- navbarPage(title = "Formula 1 Dashboard",
#                  tabPanel(title = "Season Driver Results",
#                           numericInput(inputId = "season_driver", label = "Enter a year from 1950 - 2021", value = 2021, min = 1950, max = 2021),
#                           plotOutput("season_driver"),
#                           br(),
#                           gt_output("driver_season"), br(),br()
#                  ),
#                  tabPanel(title = "Season Constructor  Results",
#                           numericInput(inputId = "year", label = "Enter a year from 1958 - 2021", value = 2021, min = 1958, max = 2021),
#                           plotOutput("constructor_plot"),
#                           br(),
#                           gt_output("season_table"), br(), br()
#                  ),
#                  # tabPanel(title = "Constructor History",
#                  #         selectInput(inputId = "team", label = "Constructor Name", choices = constructor_results$Team),
#                  #         gt_output("team_table")                          
#                  # plotOutput("season_input")
#                  # ),     
#                  # tabPanel(title = "Driver History",
#                  #            selectInput(inputId = "driver", label = "Driver Name", choices = drivers$surname),
#                  #            tableOutput("driver_table")
#                  # ),
#                  tabPanel(title = "2021 Schedule",
#                           gt_output(outputId = "round_table")
#                  ),
#                  tabPanel(title = "Race Results 2021",
#                           # numericInput(inputId = "year_gif", label = "Enter a year from 1950 - 2021", value = 2020, min = 1950, max = 2021),
#                           selectInput(inputId = "race_gif", label = "Select a round from the 2021 season", choices = c("Bahrain" = 1, "Imola" = 2),
#                                       selected = NULL),
#                           imageOutput("results_gif"),
#                           br(),br(),br(),br(),br(),br(),
#                           gt_output("results_table"),
#                           br(), br()
#                  )
# )
# 
# server <- function(input, output) {
#     
#     season_driver <- reactive({
#         getFinalF1Standings(input$season_driver)
#     })
#     
#     season_team <- reactive({
#         getFinalF1Standings(input$year, type = "constructor")
#     })  
#     
#     schedule_21 <- getF1Schedule2(2021)
#     
#     schedule <- reactive({
#         getF1Schedule(input$race_gif)
#     })
#     
#     lapinfo <- reactive({
#         getLapsByRace(2021, input$race_gif) %>% mutate(constructor = case_when(
#             startsWith(driverId, "ham") ~ "Mercedes", startsWith(driverId, "bot") ~ "Mercedes",
#             startsWith(driverId, "max") ~ "Red Bull", startsWith(driverId, "per") ~ "Red Bull",
#             startsWith(driverId, "lec") ~ "Ferrari", startsWith(driverId, "sai") ~ "Ferrari",
#             startsWith(driverId, "ric") ~ "McLaren", startsWith(driverId, "nor") ~ "McLaren",
#             startsWith(driverId, "vet") ~ "Aston Martin", startsWith(driverId, "str") ~ "Aston Martin",
#             startsWith(driverId, "gas") ~ "Alpha Tauri", startsWith(driverId, "tsu") ~ "Alpha Tauri",
#             startsWith(driverId, "gio") ~ "Alfa Romeo", startsWith(driverId, "rai") ~ "Alfa Romeo",
#             startsWith(driverId, "maz") ~ "Haas", startsWith(driverId, "mick") ~ "Haas",
#             startsWith(driverId, "oco") ~ "Alpine", startsWith(driverId, "alo") ~ "Alpine",
#             startsWith(driverId, "rus") ~ "Williams", startsWith(driverId, "lat") ~ "Williams"))
#     })
#     
#     team.color <- c("Mercedes" = "green",
#                     "Red Bull" = "blue",
#                     "Ferrari" = "red",
#                     "McLaren" = "orange",
#                     "Aston Martin" = "dark green",
#                     "Alpha Tauri" = "dark blue",
#                     "Alfa Romeo" = "dark red",
#                     "Haas" = "dark grey",
#                     "Alpine" = "cyan",
#                     "Williams" = "black")
#     
#     race_result <- reactive({
#         getRaceResults(2021, input$race_gif)
#     })
#     
#     # Season Driver Tab
#     observeEvent(input$season_driver, {
#         
#         # Plot
#         output$season_driver <- renderPlot({
#             # Fix overlap labels: some drivers are being dropped
#             ggplot(season_driver()) +
#                 aes(x = position, y = points, label = driverId) +
#                 geom_text(check_overlap = F) +
#                 scale_x_continuous(breaks = c(1:99)) +
#                 labs(x = "Position", y = "Points", title = "Final Season Standings") +
#                 theme_bw() + theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16))
#         })
#         # Table
#         output$driver_season <- render_gt({
#             season_driver() %>% 
#                 select(position, driverId, points, wins) %>% 
#                 gt() %>% 
#                 tab_style(locations = cells_body(columns = "driverId"), style = cell_text(transform = "capitalize")) %>% 
#                 tab_options(table.width = px(500)) %>%
#                 cols_align(align = "center", columns = TRUE) %>% 
#                 cols_label(position = "Position", points = "Points", wins = "Wins", driverId = "Driver")
#         })
#         
#     })
#     
#     # Season COnstructor Tab
#     observeEvent(input$year,{
#         
#         # Plot
#         output$constructor_plot <- renderPlot({
#             ggplot(season_team()) +
#                 aes(x = position, y = points, label = constructorId) +
#                 geom_text(size = 5) +
#                 scale_x_continuous(breaks = c(1:30)) +
#                 labs(x = "Position", y = "Points", title = "Final Season Standings") +
#                 theme_bw() + theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16))        
#         })  
#         
#         # output$constructor_plot <- renderPlot({
#         #   constructor_results %>% 
#         #     filter(year == input$year) %>% 
#         #     ggplot() +
#         #       aes(x = Pos, y = Pts, label = Team) +
#         #       geom_text() +
#         #       scale_x_continuous(breaks = c(1:20)) +
#         #       labs(title = "Final Season Standings")
#         #   })
#         
#         # Table
#         output$season_table <- render_gt({
#             season_team() %>% 
#                 select(position, constructorId, points, wins) %>% 
#                 gt() %>% 
#                 tab_style(locations = cells_body(columns = "constructorId"), style = cell_text(transform = "capitalize")) %>% 
#                 tab_options(table.width = px(500)) %>%
#                 cols_align(align = "center", columns = TRUE) %>% 
#                 cols_label(position = "Position", points = "Points", wins = "Wins", constructorId = "Constructor")        
#         })
#         # Table using the other constructor results dataframe from the for loop
#         # output$season_table <- render_gt({
#         #   constructor_results %>% 
#         #     filter(year == input$year) %>%
#         #     gt() %>% 
#         #     fmt_number(columns = "Pos", decimals = 0) %>% 
#         #     cols_align(align = "center", columns = TRUE) %>%  
#         #     cols_label(Pos = "Position", Pts = "Points", year = "Year")
#         #   })
#         
#     })
#     
#     # Constructor History Tab
#     # observeEvent(input$team,{
#     #  
#     #   output$team_table <- render_gt({
#     #     constructor_results %>% 
#     #       filter(Team == input$team) %>%
#     #       group_by(Pos) %>%
#     #       summarize(num_finishes = n()) %>% 
#     #       gt() %>% 
#     #       fmt_number(columns = "Pos", decimals = 0) %>% 
#     #       cols_align(align = "center", columns = TRUE) %>%         
#     #       cols_label(Pos = "Position", num_finishes = "Number of Finishes")
#     #     })
#     # })
#     # 
#     # Driver History Tab  
#     output$driver_table <- render_gt({
#         drivers
#     })
#     
#     # Calendar Tab
#     output$round_table <- render_gt({
#         schedule_21 %>% 
#             mutate(circuit.name = Circuit$circuitName,
#                    City = Circuit$Location$locality,
#                    Country = Circuit$Location$country) %>% 
#             select(round, raceName, circuit.name, City, Country, date, time) %>%     
#             gt() %>%
#             cols_align(align = "center", columns = TRUE) %>% 
#             fmt_time(columns = vars(time), time_style = 4) %>% 
#             fmt_date(columns = vars(date), date_style = 5) %>% 
#             cols_label(round = "Round", raceName = "Race Name", circuit.name = "Circuit Name", date = "Date", time = "Local Race Time")
#     })
#     
#     # Animation Tab
#     output$results_gif <- renderImage({
#         outfile <- tempfile(fileext = ".gif")
#         
#         gif <- ggplot(lapinfo()) +
#             aes(x = lap, y = position, label = driverId, color = constructor) +
#             geom_text(size = 5) +
#             scale_y_reverse(breaks = c(1:30)) +
#             labs(title = "Lap {frame_time}") +
#             scale_color_manual(values = team.color) +
#             theme_bw() + theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
#             transition_time(lap) +
#             ease_aes("linear")
#         
#         anim_save("outfile.gif", animate(gif, nframes = 300, fps = 15, end_pause = 100))
#         
#         list(src = "outfile.gif",
#              contentType = 'image/gif'
#              # width = 400,
#              # height = 300,
#              # alt = "This is alternate text"
#         )},deleteFile = TRUE) 
#     
#     # result table
#     output$results_table <- render_gt({
#         race_result() %>% 
#             mutate(driver.familyname = Driver$familyName,
#                    constructor.name = Constructor$name,
#                    time = Time$time) %>% 
#             select(positionText, points, driver.familyname, constructor.name, time, grid) %>% 
#             gt() %>% 
#             cols_align(align = "center", columns = TRUE) %>% 
#             cols_label(positionText = "Position", points = "Points", driver.familyname = "Driver", constructor.name = "Constructor",
#                        time = "Total Race Time", grid = "Qualifying Position")
#         
#     })
#     
# }
# 
# 
# 
# shinyApp(server = server, ui = ui)
