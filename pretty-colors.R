library(cfbfastR)
library(ggimage)
library(tidyverse)
library(TSP)
library(doParallel)

registerDoParallel(cores = 8)

teams <- cfbd_team_info(only_fbs = TRUE)

# colors <- teams$color
hsv <- col2rgb(colors)
tsp <- as.TSP(dist(t(hsv)))
sol <- solve_TSP(tsp, control = list(repetitions = 1e3))
srt <- colors[sol]

colors <- teams$color
# hsv <- rgb2hsv(col2rgb(colors))
# rgb <- col2rgb(colors)
# tsp <- as.TSP(dist(t(rgb)))

tictoc::tic()
sol <- solve_TSP(tsp,
                 # method = "nearest_insertion",
                 control = list(repetitions = 1)
                 )
srt <- colors[sol]
ggplot2::qplot(x = 1:131, y = 1, fill = I(srt), geom = 'col', width = 1) + ggplot2::theme_void()
tictoc::toc()

sorted <- tibble(color = srt) |>
  distinct(color, .keep_all = T) |>
  mutate(srt = row_number()) |>
  left_join(select(teams, school, color)) |>
  mutate(order = row_number())

ds <- teams |>
  left_join(sorted, by = "school") |>
  mutate(color = color.y,
         position = if_else(order %% 2 == 0, 1, 0)) |>
  select(school, color, logo, order, position)

p <- ds |>
  ggplot(aes(x = order,
             fill = school)) +
  geom_col(aes(y = 1), 
           width = 0.95,
           color = "white") +
  geom_point(aes(y = position),
             color = "white",
             size = 2800,
             shape = 15) +
  cfbplotR::geom_cfb_logos(aes(y = position,
                               team = school),
                           width = 0.07) +
  guides(fill = "none") +
  scale_fill_manual(values = ds$color,
                    breaks = ds$school) +
  coord_flip() +
  theme_void()
# p

ggsave(plot = p, "pretty-color-plot-2.png",
       device = "png", width = 860, height = 12840, units = "px", limitsize = FALSE, dpi = 1)
system(paste0("convert ", "pretty-color-plot-2.png", " -trim ", "pretty-color-plot-2.png")) # trim ws on edges
# -------
# p2 <- ds |>
#   ggplot(aes(x = order,
#              fill = school)) +
#   geom_col(aes(y = 1),
#            width = 1,
#            color = "white") +
#   geom_label(aes(label = school,
#                  y = position),
#              fill = "white",
#              hjust = "inward",
#              size = 3,
#              label.r = unit(0, "lines"),
#              label.padding = unit(0.08, "lines")) +
#   # geom_point(aes(y = position),
#   #            color = "white",
#   #            size = 10) +
#   # geom_image(aes(image = logo, y = position),
#   #            size = 0.012, asp = 1.2) +
#   # cfbplotR::geom_cfb_logos(aes(y = position,
#   #                              team = school),
#   #                          width = 0.010) +
#   scale_size_identity() +
#   guides(fill = "none") +
#   scale_fill_manual(values = ds$color,
#                     breaks = ds$school) +
#   coord_flip() +
#   theme_void()
# p2


