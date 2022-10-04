#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(tigris)
library(cfbfastR)
library(tidyverse)
library(png)
library(maptools)
library(sf)
library(DT)
library(lubridate)

library(shinycssloaders)
library(shinyWidgets)
library(shinydashboard)

# DATA --------------------------------------------------
source('./keys.R')

yr <- 2022

# see /scripts/app-sandbox.R
# counties <- readRDS("counties-shifted-simple.RDS")
states <- readRDS("./data/states-shifted-simple.RDS")

# counties <- sf::st_simplify(counties)
# states <- sf::st_simplify(states)
# write_rds(counties, "counties-shifted-simple.RDS")
# write_rds(states, "states-shifted-simple.RDS")

# base_map <- readRDS("base-map-shifted-block-groups.RDS")
base_map_ <- readRDS("./data/base-map-shifted-illinois-contig.RDS")

dark_logo_list <- c("Oregon", "USC", "Nevada", "UCLA", "Kansas State", "Air Force", "Washington State",
                    "Indiana", "Michigan State", "Rice", "Texas", "TCU", "Clemson", "Duke", "Pittsburgh", "Alabama")
alt_color_list <- c("Tennessee", "North Texas", "Temple", "LSU", "San Diego State", "UMass", "Iowa", "California",
                    "Cincinnati", "Northwestern", "Utah State", "UC Davis", "Montana", "USC", "Wisconsin", "NC State",
                    "BYU", "Oklahoma", "Minnesota", "Kent State", "SMU", "Akron", "Tulsa", "Houston")

# # team information dataframe, includes FCS teams
# ds_teams_ <- cfbfastR::cfbd_team_info(only_fbs = FALSE)
# 
# ds_teams <- ds_teams_ |>
#   unnest_wider(col = logos, names_sep = ",") |>
#   rename(
#     logo_light = `logos,1`,
#     logo_dark = `logos,2`
#   ) |>
#   mutate(
#     conference = if_else(is.na(conference), "FCS", conference),
#     # default backup logo
#     logo_light = if_else(is.na(logo_light), "https://b.fssta.com/uploads/application/leagues/logos/NCAAFootball.vresize.350.350.medium.2.png", logo_light),
#     logo_dark = if_else(is.na(logo_dark), "https://b.fssta.com/uploads/application/leagues/logos/NCAAFootball.vresize.350.350.medium.2.png", logo_dark)
#   ) 
# write_csv(ds_teams, "ds_teams_2021.csv")
ds_teams <- read_csv("./data/ds_teams_2021.csv")

# ds_fcs <- ds_teams |>
#   filter(conference != "FCS") |>
#   mutate(
#     latitude = if_else(school == "Hawai'i", 29.3, latitude),
#     longitude = if_else(school == "Hawai'i", -123.23, longitude),
#   )
# write_csv(ds_fcs, "ds_fcs_2021.csv")
ds_fcs <- read_csv("./data/ds_fcs_2021.csv")

# results -- see app_sandbox.R
ds_results <- readRDS("./data/fbs-results-2000-2021.RDS")

# adding current year results
temp <- cfbfastR::cfbd_game_info(year = yr,
                                 season_type = "both") |>
  mutate(week = if_else(season_type == "postseason", max(week) + 1, as.numeric(week))) |>
  arrange(week) |>
  filter(!is.na(home_points | away_points)) |>
  mutate(
    winner = if_else(home_points > away_points, home_team, away_team),
    loser = if_else(home_points < away_points, home_team, away_team),
    start_date = floor_date(ymd_hms(start_date), "days")
  ) |>
  select(season, week, season_type, start_date, winner, loser)

ds_results <- ds_results |> 
  rbind(temp)

# Getting start / end dates
ds_dates <- ds_results |>
  group_by(season, week) |>
  summarize(
    begin_date = min(start_date, na.rm = T),
    final_date = max(start_date, na.rm = T)
  )
# write_csv(ds_dates, "ds_dates.csv")
# ds_dates <- read_csv("./data/ds_dates.csv")

# Define UI -----------------------------------------------------------
ui <- fluidPage(
  
  # Lets me use the fancy infoboxes
  useShinydashboard(),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel = "icon", type = "image/png", href = "cfb-imp-map-logo.png")
  ),

    # Application title
  tags$div(class = "titlebar",
    titlePanel(title = div(img(src="cfb-imp-map-logo.png", width = 60, height = 60, class = "titlelogo"), 
                           "NCAA Football Imperialism Map Viewer", class = "titlebar"),
               windowTitle = "CFB Imperialism Map")
  ),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          width = 3,
            shiny::selectInput(inputId = "chosen_year",
                             label = "Select A Starting Season",
                             choices = c(2016:2022),
                             selected = 2022),
          shiny::selectInput(inputId = "chosen_stop_year",
                             label = "Select an ending season",
                             selected = 2022,
                             choices = c(2022)
          ),
          shiny::selectInput(inputId = "chosen_stop_week",
                             label = "Select an ending week",
                             selected = 16,
                             choices = c(1:16)
          ),
            # uiOutput("choose_stop_year"),
            # uiOutput("choose_week"),
            shiny::actionButton(inputId = "go_button",
                                label = "Generate Map"),
            dataTableOutput(outputId = "results_table")
        ),

        # Leaflet output
        mainPanel(
          leafletOutput(outputId = "map", width = '110%') |> withSpinner(),
          uiOutput("leaderboxes")
        )
    )
)

# Define server logic -----------------------------
server <- function(input, output) {
  
  # update map ownership based on inputs ----------------------------------------------------------------
  # reactive shape file with counties assigned to the correct school based on selected results
  ds_map.react <- eventReactive(input$go_button, valueExpr = {
    
    withProgress(expr = {
    
    setProgress(value = 0, message = "Indexing results in timespan...")
      
    chosen_date <- ds_dates[which(ds_dates$season == input$chosen_year & ds_dates$week == 1),] |> pull(var = begin_date)
    chosen_stop_date <- ds_dates[which(ds_dates$season == input$chosen_stop_year & ds_dates$week == input$chosen_stop_week),] |> pull(var = final_date)
    
    date_interval <- ds_results |>
      filter(start_date >= chosen_date & start_date <= chosen_stop_date)
    
    # This is the loop that iterates through each game result and gives the loser's land to the winner
    # also now logs the past owners in the 'owners' variable
    ds_map <- base_map_ |>
      mutate(owners = school)
    
    for(i in (1:nrow(date_interval))){
      ds_map <- ds_map |>
        mutate(
          owners = if_else(school == date_interval$loser[i], paste(owners, date_interval$winner[i], sep = " > "), owners),
          school = if_else(school == date_interval$loser[i], date_interval$winner[i], school)
        )
      
      setProgress(value = round(i / nrow(date_interval), 2), message = "Simulating map changes...")
      }
    
    setProgress(value = 0.99, message = "Generating final map...")
    
    ds_map <- ds_map |>
      left_join(ds_teams, by = "school") |>
      mutate(
        logo_chosen = if_else(school %in% dark_logo_list, logo_dark, logo_light),
        color_chosen = if_else(school %in% alt_color_list, alt_color, color),
      )
    
    }, message = "Beginning map generation...")
    
    return(ds_map)
    
    })
  
  # reactive list of game results in selected timespan
  ds_results_list.react <- eventReactive(input$go_button, valueExpr = {
    chosen_date <- ds_dates[which(ds_dates$season == input$chosen_year & ds_dates$week == 1),] |> pull(var = begin_date)
    chosen_stop_date <- ds_dates[which(ds_dates$season == input$chosen_stop_year & ds_dates$week == input$chosen_stop_week),] |> pull(var = final_date)
    
    results_list <- ds_results |>
      filter(start_date >= chosen_date & start_date <= chosen_stop_date) |>
      mutate(result = paste0(season, ", week ", week, ": ", winner, " > ", loser)) |>
      select(result)
    
    return(results_list)
  })
  
  # inputs (UI) =======================================================================================
  
  # Default end year selection UI
  # output$choose_stop_year <- shiny::renderUI({
  #   shiny::selectInput(inputId = "chosen_stop_year",
  #                      label = "Select an ending season",
  #                      selected = 16,
  #                      choices = c(1:16)
  #   )
  # })
  # Changes end year selection UI when start year is changed
  observeEvent(input$chosen_year, eventExpr = {
    shiny::updateSelectInput(inputId = "chosen_stop_year",
                             selected = ds_dates |> filter(season >= input$chosen_year) |> pull(var = season) |> min(),
                             choices = ds_dates |> filter(season >= input$chosen_year) |> distinct(season) |> pull(var = season) |> unlist(use.names = F)
    )
  })
  
  # output$choose_week <- shiny::renderUI({
  #   shiny::selectInput(inputId = "chosen_stop_week",
  #                      label = "Select an ending week",
  #                      selected = ds_dates |> filter(season == input$chosen_stop_year) |> pull(var = week) |> max(),
  #                      choices = ds_dates |> filter(season == input$chosen_stop_year) |> distinct(week) |> pull(var = week) |> unlist(use.names = F)
  #   )
  # })
  observeEvent(input$chosen_year, eventExpr = {
    shiny::updateSelectInput(inputId = "chosen_stop_week",
                             selected = ds_dates |> filter(season == input$chosen_stop_year) |> pull(var = week) |> max(),
                             choices = ds_dates |> filter(season == input$chosen_stop_year) |> distinct(week) |> pull(var = week) |> unlist(use.names = F)
    )
  })
  
  # outputs ===========================================================================================
  # base map 
  output$map <- renderLeaflet({
    
    base_map <- base_map_ |>
      left_join(ds_teams, by = "school") |>
      mutate(
        logo_chosen = if_else(school %in% dark_logo_list, logo_dark, logo_light),
        color_chosen = if_else(school %in% alt_color_list, alt_color, color),
        owners = school
      )
    
    # base_icons
    base_logoIcons <- icons(
      iconUrl = base_map$logo_chosen,
      iconWidth = (as.numeric(log(st_area(base_map))) - 21) * 11,
      iconHeight = (as.numeric(log(st_area(base_map))) - 21) * 11
    )
    
    leaflet(options = leafletOptions(zoomControl = TRUE,
                                     zoomSnap = 0.25)) |>
      setView(lng = -97.24580, lat = 36.99909, zoom = 4.5) |>
      addCircleMarkers(data = ds_fcs,
                       group = "Toggle School Locations",
                       color = "black",
                       opacity = 1,
                       weight = 1,
                       fillColor = ~color,
                       stroke = T,
                       radius = 5
      ) |>
      addPolygons(data = base_map,
                  group = "Base Map",
                  stroke = T,
                  weight = 2,
                  fillOpacity = 1,
                  color = "black",
                  fillColor = ~color_chosen,
                  popup = ~paste0("<b>Owner:</b> ", school, "<br>",
                                  "<b>Past Owners:</b> ", owners)
      ) |>
      addMarkers(data = st_centroid(base_map, of_largest_polygon = TRUE),
                 group = "Base Map",
                 icon = base_logoIcons,
                 popup = ~paste0("<b>Owner:</b> ", school, "<br>",
                                 "<b>Past Owners:</b> ", owners)
                 ) |>
      addPolylines(
        data = states,
        group = "Toggle State Borders",
        color = "grey",
        weight = 3,
        opacity = 1
      ) |>
      # addPolylines(
      #   data = counties,
      #   group = "Toggle County Borders",
      #   color = "grey",
      #   weight = 0.5,
      #   opacity = 1
      # ) |>
      addLayersControl(
        overlayGroups = c("Toggle School Locations", "Toggle State Borders"), # "Toggle County Borders", 
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      hideGroup(c("Toggle School Locations", "Toggle State Borders")) # "Toggle County Borders", 
    
  })
  
  observeEvent(input$go_button, {
    
    m <- ds_map.react()
    
    # icons
    logoIcons <- icons(
      iconUrl = m$logo_chosen,
      iconWidth = (as.numeric(log(st_area(m))) - 21) * 11,
      iconHeight = (as.numeric(log(st_area(m))) - 21) * 11
    )
    
    leafletProxy("map") |>
      clearGroup("Base Map") |>
      clearGroup("Display Map") |>
      addPolygons(data = m,
                  group = "Display Map",
                  stroke = T,
                  weight = 2,
                  fillOpacity = 1,
                  color = "black",
                  fillColor = ~color_chosen,
                  popup = ~paste0("<b>Owner:</b> ", school, "<br>",
                                  "<b>Past Owners:</b> ", owners)
      ) |>
      addMarkers(data = st_centroid(m, of_largest_polygon = TRUE),
                 group = "Display Map",
                 icon = logoIcons,
                 popup = ~paste0("<b>Owner:</b> ", school, "<br>",
                                 "<b>Past Owners:</b> ", owners)
      ) 
      # addPolylines(data = counties,
      #              group = "Toggle County Borders",
      #              stroke = T,
      #              weight = 0.5,
      #              color = "black"
      # ) |>
      # addPolylines(data = states,
      #              group = "Toggle State Borders",
      #              stroke = T,
      #              weight = 1,
      #              color = "black"
      # ) 
    
  })
  
  # Results list
  output$results_table <- renderDataTable({
    
    DT::datatable(data = ds_results_list.react(),
                  rownames = FALSE,
                  colnames = "Results in selected timespan:",
                  options = list(scrollY = 470, 
                                 pageLength = 100, 
                                 dom = 'Bfrtip'), 
                  escape = FALSE)
    
  })
  
  # Leaderboxes -------
  ds_stats.react <- reactive({
    ds_map.react() |>
      group_by(school) |>
      summarize(
        total_counties = sum(n_counties),
        total_land = sum(sum_land),
        total_water = sum(sum_water),
        total_pop = sum(total_pop),
        total_domain = sum(total_land, total_water)
      )
  })
  
  output$leaderbox_land <- renderUI({
    infoBox(title = "#1 Land Controlled",
            width = 3,
            color = "green",
            icon = icon("fa-solid fa-flag", lib = "font-awesome"),
            value = ds_stats.react() |> slice_max(total_land, n = 1) |> pull(var = school),
            subtitle = paste0(ds_stats.react() |> slice_max(total_land, n = 1) |> pull(var = total_land) |> 
                                round(2) |> format(big.mark = ","), " sq. km")
    )
  })
  
  output$leaderbox_water <- renderUI({
    infoBox(title = "#1 Water Controlled",
            width = 3,
            color = "blue",
            icon = icon("fa-solid fa-water", lib = "font-awesome"),
            value = ds_stats.react() |> slice_max(total_water, n = 1) |> pull(var = school),
            subtitle = paste0(ds_stats.react() |> slice_max(total_water, n = 1) |> pull(var = total_land) |> 
                                round(2) |> format(big.mark = ","), " sq. km")
    )
  })
  
  output$leaderbox_pop <- renderUI({
    infoBox(title = "#1 Total Population",
            width = 3,
            color = "aqua",
            # icon = icon("fa-solid fa-people", lib = "font-awesome"),
            value = ds_stats.react() |> slice_max(total_pop, n = 1) |> pull(var = school),
            subtitle = paste0(ds_stats.react() |> slice_max(total_pop, n = 1) |> pull(var = total_land) |> 
                                round(2) |> format(big.mark = ","), " people")
    )
  })
  
  output$leaderbox_domain <- renderUI({
    infoBox(title = "#1 Total Domain",
            width = 3,
            color = "black",
            icon = icon("fa-solid fa-crown", lib = "font-awesome"),
            value = ds_stats.react() |> slice_max(total_domain, n = 1) |> pull(var = school),
            subtitle = paste0(ds_stats.react() |> slice_max(total_domain, n = 1) |> pull(var = total_land) |>
                                round(2) |> format(big.mark = ","), " sq. km")
    )
  })
  
  # Background colors
  output$leaderboxes <- renderUI({
    
    land_leader <- ds_stats.react() |> slice_max(total_land, n = 1) |> pull(var = school)
    water_leader <- ds_stats.react() |> slice_max(total_water, n = 1) |> pull(var = school)
    pop_leader <- ds_stats.react() |> slice_max(total_pop, n = 1) |> pull(var = school)
    domain_leader <- ds_stats.react() |> slice_max(total_domain, n = 1) |> pull(var = school)
    
    fluidRow(
      tags$style(type = "text/css",
                 paste0('.bg-blue { background-color: ',
                        ds_fcs |> filter(school == water_leader) |> pull(var = color),
                        "!important; }")),
      tags$style(type = "text/css",
                 paste0('.bg-aqua { background-color: ',
                        ds_fcs |> filter(school == pop_leader) |> pull(var = color),
                        "!important; }")),
      tags$style(type = "text/css",
                 paste0('.bg-black { background-color: ',
                        ds_fcs |> filter(school == domain_leader) |> pull(var = color),
                        "!important; }")),
      tags$style(type = "text/css",
                 paste0('.bg-green { background-color: ',
                        ds_fcs |> filter(school == land_leader) |> pull(var = color),
                        "!important; }")),
      
      uiOutput("leaderbox_domain"),
      uiOutput("leaderbox_land"),
      uiOutput("leaderbox_water"),
      uiOutput("leaderbox_pop")
    )
  })
  
    
  }

# Run the application ----------------------------------------------------------
shinyApp(ui = ui, server = server)









