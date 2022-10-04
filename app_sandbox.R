library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(tidyverse)
library(RColorBrewer)
library(ggplot2)
library(lubridate)
library(sf)
library(tigris)
library(data.table)

# get ds_teams
ds_teams_ <- cfbfastR::cfbd_team_info(only_fbs = TRUE)

ds_teams <- ds_teams_ |>
  rename(
    logo_light = logo,
    logo_dark = logo_2
  ) |>
  mutate(
    conference = if_else(is.na(conference), "FCS", conference),
    logo_light = if_else(is.na(logo_light), "https://b.fssta.com/uploads/application/leagues/logos/NCAAFootball.vresize.350.350.medium.2.png", logo_light),
    logo_dark = if_else(is.na(logo_dark), "https://b.fssta.com/uploads/application/leagues/logos/NCAAFootball.vresize.350.350.medium.2.png", logo_dark)
  ) 

ds_fcs <- ds_teams |>
  filter(conference != "FCS") |>
  # Have to fix fucking illinois still
  mutate(
    latitude = if_else(school == "Illinois", 40.1020, latitude),
    longitude = if_else(school == "Illinois", -88.2272, longitude)
  )

# map reprojection -------------------------------------------------------------

# counties_shifted <- tigris::counties(cb = TRUE) |>
#   shift_geometry(position = "outside") |>
#   filter(STATEFP != "02",
#          STATEFP != "72") |>
#   st_transform(crs = 4326) |>
#   st_simplify() 
# write_rds(counties_shifted, "counties-shifted.RDS")
# 
# states_shifted <- tigris::states(cb = TRUE) |>
#   shift_geometry(position = "outside") |>
#   filter(STATEFP != "02",
#          STATEFP != "72") |>
#   st_transform(crs = 4326) |>
#   st_simplify()
# write_rds(states_shifted, "states-shifted.RDS")

# generate base map ------------------------------------------------------------

pnts_sf <- st_as_sf(ds_fcs, coords = c("longitude", "latitude"))
st_crs(pnts_sf) <- 4326

counties <- counties(cb = TRUE, resolution = "20m") |>
# counties <- tigris::block_groups(cb = TRUE) |>
  # shift_geometry() |>
  st_transform(crs = 4326) |>
  mutate(n = row_number())

closest_ <- list()
for (i in seq_len(nrow(counties))) {
  closest_[[i]] <- pnts_sf[which.min(
    sf::st_distance(pnts_sf, counties[i, ])^2
  ), ]
  
  if(i %% 10 == 0){print(paste0(round(100 * i / nrow(counties), 2), "% completed..."))}
  
}
closest_ <- rbindlist(closest_)

closest_ <- closest_ |>
  dplyr::select(school) |>
  mutate(n = row_number())

counties_ <- left_join(counties, closest_, by = "n")
counties_pop <- read_csv("./data/counties_pop.csv")
counties_ <- left_join(counties_, counties_pop, by = "GEOID")

counties_ <- counties_ |>
  rename(population = estimate) %>%
  dplyr::select(!variable) |>
  shift_geometry(position = "outside") |>
  st_transform(crs = 4326) 

# Removes Alaska
counties_ <- counties_ %>%
  filter(STATEFP != "02",
         STATEFP != "72")

# Re-assigns some non-contiguous counties
counties_ <- counties_ |>
  mutate(
    school = case_when(
      GEOID == "35005" ~ "Texas Tech",
      GEOID == "32009" ~ "UNLV",
      GEOID == "06079" ~ "San José State",
      GEOID == "06009" ~ "Fresno State",
      TRUE ~ school
    )
  )


counties_grouped <- counties_ |>
  group_by(school) |>
  summarise(
    n_counties = n(),
    sum_land = sum(ALAND) * 0.000000386102, # Convert sq meteres to sq miles
    sum_water = sum(AWATER) * 0.000000386102,
    sum_total = sum_land + sum_water,
    total_pop = sum(population, na.rm = T)
  ) |>
  st_cast("MULTIPOLYGON")

write_rds(counties_grouped, "./data/base-map-shifted-illinois-contig.RDS")

# manually split / edit counties

# game results ------------------------------------------------

ds_results <- tibble()

for(yr in 2016:2021) {

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

ds_results <- ds_results |> rbind(temp)

print(paste(yr, "completed..."))

}

write_rds(ds_results, "fbs-results-2000-2021.RDS")

# Getting start / end dates
ds_dates <- ds_results |>
  group_by(season) |>
  summarize(
    begin_date = min(start_date, na.rm = T),
    final_date = max(start_date, na.rm = T)
  )

# INPUTS
chosen_year <- 2021
chosen_stop_year <- 2021

chosen_date <- ds_dates[which(ds_dates$season == chosen_year),] |> pull(var = begin_date)
chosen_stop_date <- ds_dates[which(ds_dates$season == chosen_stop_year),] |> pull(var = final_date)

date_interval <- which(ds_results$start_date == chosen_date):which(ds_results$start_date == chosen_stop_date)  

base_map <- readRDS("base-map-shifted.RDS")
ds_map <- base_map

# This is the loop that iterates through each game result and gives the loser's land to the winner
for(i in (date_interval)){
  
  ds_map <- ds_map |>
    mutate(
      school = if_else(school == ds_results$loser[i], ds_results$winner[i], school)
    )
  
}







