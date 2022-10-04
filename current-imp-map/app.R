# A real time updated cfb imperialism map shiny app

library(tidyverse)
library(leaflet)
library(lubridate)
library(cfbfastR)
library(sf)
library(data.table)
library(tictoc)
library(fontawesome)
library(extrafont)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

# DATA --------------------------------------------------
source('keys.R')
options(scipen=999)

# see /scripts/app-sandbox.R
counties <- readRDS("./data/counties-shifted.RDS")
states <- readRDS("./data/states-shifted.RDS")
pops <- read_csv("./data/counties_pop.csv") |>
  filter(variable == "POP")

# base_map <- readRDS("base-map-shifted-block-groups.RDS")
base_map_ <- readRDS("./data/base-map-shifted.RDS")

dark_logo_list <- c("Oregon", "USC", "Nevada", "UCLA", "Kansas State", "Air Force", "Washington State", "Oklahoma",
                    "Indiana", "Michigan State", "Rice", "Texas", "TCU", "SMU", "Clemson", "Duke", "Pittsburgh")
alt_color_list <- c("Tennessee", "North Texas", "Temple", "LSU", "San Diego State", "UMass", "Iowa", "California",
                    "Cincinnati", "Northwestern", "Utah State", "UC Davis", "Montana", "USC", "Wisconsin", "NC State")

# team information dataframe, sans FCS teams
ds_teams_ <- cfbfastR::cfbd_team_info(only_fbs = F)

ds_teams <- ds_teams_ |>
  # unnest_wider(col = logos, names_sep = ",") |>
  rename(
    # logo_light = `logos,1`,
    # logo_dark = `logos,2`
    logo_light = logo,
    logo_dark = logo_2
  ) |>
  mutate(
    conference = if_else(is.na(conference), "FCS", conference),
    # default backup logo
    logo_light = if_else(is.na(logo_light), "https://b.fssta.com/uploads/application/leagues/logos/NCAAFootball.vresize.350.350.medium.2.png", logo_light),
    logo_dark = if_else(is.na(logo_dark), "https://b.fssta.com/uploads/application/leagues/logos/NCAAFootball.vresize.350.350.medium.2.png", logo_dark),
    latitude = if_else(school == "Hawai'i", 29.3, latitude),
    longitude = if_else(school == "Hawai'i", -123.23, longitude),
    logo_chosen = if_else(school %in% dark_logo_list, logo_dark, logo_light),
    color_chosen = if_else(school %in% alt_color_list, alt_color, color)
  )

# Results data
ds_results_ <- cfbd_game_info(year = year(today()), season_type = "both") |>
  mutate(week = if_else(season_type == "postseason", max(week) + 1, as.numeric(week))) # |>
# filter(week <= wk)

ds_results <- ds_results_ |>
  filter(!is.na(home_points)) |>
  mutate(
    winner = if_else(home_points > away_points, home_team, away_team),
    # winner = if_else(winner %in% ds_teams$school & winner != "UTSA", winner, paste(winner, "(FCS)")),
    loser = if_else(home_points < away_points, home_team, away_team)
  ) |>
  dplyr::select(winner, loser, week) |>
  left_join(ds_teams |> dplyr::select(school, logo_chosen, color_chosen, conference) |> rename(winner = school), by = "winner") |>
  rename(winner_logos = logo_chosen, winner_color = color_chosen) |>
  mutate(
    # default logo for if the others are missing
    winner_logos = if_else(is.na(winner_logos), "https://www.ncaa.com/modules/custom/casablanca_core/img/sportbanners/football.svg", winner_logos)
  ) |>
  distinct()

# This is the loop that iterates through each game result and gives the loser's land to the winner
# TODO: ideally I should only have to change the name, then have this point to a different df w/ the logo, color, etc. 
counties_grouped <- base_map_ |>
  mutate(home_school = school) |>
  left_join(ds_teams |>
              select(home_mascot = mascot,
                     home_conference = conference,
                     home_city = city,
                     school), 
            by = "school")

for(i in (1:nrow(ds_results))){
  
  counties_grouped <- counties_grouped |>
    mutate(
      # conference = if_else(school == ds_results$loser[i], ds_results$conference[i], conference),
      # logos = if_else(school == ds_results$loser[i], ds_results$winner_logos[i], logos),
      school = if_else(school == ds_results$loser[i], ds_results$winner[i], school)
    )
  
  if(i %% (nrow(ds_results) / 4) == 0){
    print(paste0(
      round(100 * (i / nrow(ds_results)), 2), "% done...")
    )
  }
}

counties_grouped <- counties_grouped |>
  left_join(ds_teams |> select(school, logo_chosen, color_chosen), by = "school")

# Define UI for application that draws a histogram
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
                                  "Current NCAA Football Imperialism Map", class = "titlebar"),
                      windowTitle = "CFB Imperialism Map")
  ),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      width = 3,
      dataTableOutput(outputId = "results_table")
    ),
    
    # Leaflet output
    mainPanel(
      leafletOutput(outputId = "map", width = '110%') |> withSpinner(),
      uiOutput("leaderboxes")
    )
  )

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # reactive list of game results in selected timespan -------------------------------
  ds_results_list.react <- reactive({
    
    results_list <- ds_results |>
      mutate(result = paste0(winner, " > ", loser)) |>
      select(result)
    
    return(results_list)
  })
  
  # Results list
  output$results_table <- renderDataTable({
    
    DT::datatable(data = ds_results_list.react(),
                  rownames = FALSE,
                  colnames = "Results in selected timespan:",
                  options = list(scrollY = 380, 
                                 pageLength = 100, 
                                 dom = 'Bfrtip'), 
                  escape = FALSE)
    
  })
  
  output$map <- renderLeaflet({
    
    # This is the actual map -------------------------------------------------------
    logoIcons.os <- icons(
      iconUrl = counties_grouped$logo_chosen,
      iconWidth = (as.numeric(log(st_area(counties_grouped))) - 21) * 11,
      iconHeight = (as.numeric(log(st_area(counties_grouped))) - 21) * 11
    )
    
    # function to apply the correct colors to each territory 
    map_teams <- counties_grouped |> group_by(school) |> summarize(color_chosen = unique(color_chosen))
    fill_team <- colorFactor(map_teams$color_chosen, map_teams$school, na.color = "grey", ordered = TRUE)
    
    # Reprojection
    epsg2163 <- leafletCRS(
      crsClass = "L.Proj.CRS",
      code = "ESRI:102003",
      proj4def = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
      resolutions = 2^(16:7)
    )
    
    leaflet(options = leafletOptions(zoomControl = TRUE,
                                              zoomSnap = 0.25,
                                              crs = epsg2163)) |>
      setView(lng = -97.24580, lat = 36.99909, zoom = 4.5) |>
      addPolygons(data = counties_grouped, 
                  smoothFactor = 0.2, 
                  color = "white", 
                  fillColor = ~fill_team(school), 
                  fillOpacity = 0.9, 
                  label = ~school, 
                  weight = 0,
                  stroke = F
      ) |>
      addMarkers(data = st_centroid(counties_grouped), 
                 label = ~school, 
                 icon = logoIcons.os,
                 popup = paste0(
                   "<center><b>", counties_grouped$home_city, " Territory, home of the ", counties_grouped$home_mascot, "</b><br></center>",
                   "<center>Currently Controlled by ", counties_grouped$school, "<br></center>",
                   "<hr>",
                   "Territory Area: ", format(round(counties_grouped$sum_land, 1), nsmall = 1, big.mark = ","), " sq. miles<br>",
                   "Territory Water area: ", format(round(counties_grouped$sum_water, 1), nsmall = 1, big.mark = ","), " sq. miles<br>",
                   "No. of Counties in Territory: ", format(counties_grouped$n_counties, nsmall = 1, big.mark = ","), "<br>",
                   "Territory Population: ", format(counties_grouped$total_pop, big.mark = ",")
                 )) |>
      addPolylines(data = counties, color = "black", weight = 0.2, smoothFactor = 0, opacity = 1)  |>
      #  addPolylines(data = states, color = "black", weight = 1, smoothFactor = 0, opacity = 1) |>
      addPolylines(data = counties_grouped, color = "black", weight = 1.5, smoothFactor = 0, opacity = 1) # |>
    # addCircleMarkers(data = filter(ds_teams, classification %in% c("fcs", "fbs")), label = ~school, stroke = T, fillOpacity = 0.8, weight = 0.75, color = "black", fillColor = ~color, radius = 5,
    #                popup = paste0("<center><img src=", ds_teams$logos, " width = '50' height = '50'>",
    #                               "<br><hr><b>", ds_teams$school, "</center>",
    #                               "</b><br>", ds_teams$conference,
    #                               "<br>Mascot: ", ds_teams$mascot
    #                ))
    
  })


}

# Run the application 
shinyApp(ui = ui, server = server)
