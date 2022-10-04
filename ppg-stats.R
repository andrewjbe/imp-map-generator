library(cfbfastR)
library(tidyverse)

yr <- 2022

ds_teams <- cfbd_team_info(year = yr)

ds <- tibble()
for(tm in ds_teams$school){
  temp <- cfbd_game_team_stats(yr, team = tm)
  
  ds <- rbind(ds, temp)
  paste(tm, "data gathered...")
}


ds_teamstats <- ds |>
  mutate(across(.cols = c(sacks),
                .fns = as.integer)
  ) |>
  group_by(school) |>
  summarize(
    n_games = n(),
    points_scored_pg = mean(points, na.rm = T),
    points_allowed_pg = mean(points_allowed, na.rm = T),
    total_sacks = sum(sacks, na.rm = T),
    sacks_pg = mean(sacks, na.rm = T)
  )
