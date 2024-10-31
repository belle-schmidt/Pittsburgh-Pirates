## Libraries
library(tidyverse)
library(baseballr)
library(ggthemes)
library(hexbin)

## Load in data
pitching_models <- read.csv("mlb_pitching_stats_2021-24.csv")

## Only use full seasons
pitching_models <- pitching_models |> 
  mutate(season = factor(season),
         horizontal_break = -horizontal_break)

## set theme
theme_set(theme_bw())

## create own theme for each graph
movement_theme <- function(){
  theme(
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    strip.background = element_rect(fill = "navy", color = "black", linewidth = 1),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 10),
    axis.title.x = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.key.size = unit(0.35, "inches"),
    legend.text = element_text(size = 12),
    panel.spacing = unit(1, "lines")
  )
}

# AGGREGATED PITCHING MODELS 4-SEAM FASTBALL HEX GRAPHS -----------------------------------
## "Qualified" 4-Seam Fastball pitchers
ff_qualified <- pitching_models |> 
  filter(pitch_name == "4-Seam Fastball")

## 4-Seam Fastballs
# ### Velocity
# pitching_models |>
#   filter(pitch_name == "4-Seam Fastball") |>
#   ggplot() +
#   # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) +
#   stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
#                        z = velocity),
#                    binwidth = c(3, 3), fun = mean,
#                    color = "black") +
#   scale_fill_gradient2(low = "dodgerblue2",
#                        mid = "white",
#                        high = "firebrick2",
#                        midpoint = mean(ff_qualified$velocity)) +
#   geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
#   geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
#   geom_vline(aes(xintercept = mean(horizontal_break)),
#              color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
#   geom_hline(aes(yintercept = mean(induced_vertical_break)),
#              color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
#   scale_y_continuous(breaks = seq(-20, 20, 5),
#                      labels = scales::number_format(suffix = "\"")) +
#   scale_x_continuous(breaks = seq(-20, 20, 5),
#                      labels = scales::number_format(suffix = "\"")) +
#   # geom_zone(linecolor = "black") +
#   # geom_plate() +
#   coord_fixed() +
#   labs(#title = "Avg. Velo of 4-Seam Fastballs by Movement",
#     title = "All Pitchers With 100 or More Fastballs Thrown in a Year",
#     subtitle = "POV: RHP facing home plate",
#     caption = "Data courtesy of Baseball Savant",
#     x = "Horizontal Movement (in)",
#     y = "Vertical Movement (in)",
#     fill = "Avg. Velocity") +
#   movement_theme() +
#   facet_wrap(
#     ~ season,
#     nrow = 1
#   )
# 
# ### spin rate
# pitching_models |>
#   filter(pitch_name == "4-Seam Fastball") |>
#   ggplot() +
#   # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) +
#   stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
#                        z = spin),
#                    binwidth = c(3, 3), fun = mean,
#                    color = "black") +
#   scale_fill_gradient2(low = "dodgerblue2",
#                        mid = "white",
#                        high = "firebrick2",
#                        midpoint = mean(ff_qualified$spin)) +
#   geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
#   geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
#   geom_vline(aes(xintercept = mean(horizontal_break)),
#              color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
#   geom_hline(aes(yintercept = mean(induced_vertical_break)),
#              color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
#   scale_y_continuous(breaks = seq(-20, 20, 5),
#                      labels = scales::number_format(suffix = "\"")) +
#   scale_x_continuous(breaks = seq(-20, 20, 5),
#                      labels = scales::number_format(suffix = "\"")) +
#   # geom_zone(linecolor = "black") +
#   # geom_plate() +
#   coord_fixed() +
#   labs(#title = "Spin Rate of 4-Seam Fastballs by Movement",
#     title = "All Pitchers With 100 or More Fastballs Thrown in a Year",
#     subtitle = "POV: RHP facing home plate",
#     x = "Horizontal Movement (in)",
#     y = "Vertical Movement (in)",
#     fill = "Spin (RPM)") +
#   movement_theme() +
#   facet_wrap(~ season)


### whiff%
pitching_models |> 
  filter(pitch_name == "4-Seam Fastball") |> 
  ggplot() + 
  # hexbins
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = whiff_pct), 
                   binwidth = c(3, 3), fun = mean,
                   color = "black") +
  # color gradient of average whiff rate
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = mean(ff_qualified$whiff_pct)) +
  # 0 inches of vertical movement
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  # 0 inches of horizontal movement
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  # mean of horizontal break
  geom_vline(aes(xintercept = mean(horizontal_break)),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  # mean of vertical break
  geom_hline(aes(yintercept = mean(induced_vertical_break)), 
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  # hand side label
  geom_label(label = "HAND SIDE", x = 17.15, y = -0.3,
             label.padding = unit(0.35, "lines"),
             label.r = unit(0.25, "lines"),
             label.size = 1,
             size.unit = "mm") +
  # glove side label
  geom_label(label = "GLOVE SIDE", x = -1.9, y = -0.3,
             label.padding = unit(0.35, "lines"),
             label.r = unit(0.25, "lines"),
             label.size = 1,
             size.unit = "mm") +
  # create breaks and add inches format to y axis
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # create breaks and add inches format to x axis
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  coord_fixed() +
  # labels
  labs(
    title = "POV: Right-Handed Pitcher Facing Home Plate",
    caption = "All pitchers with 100 or more fastballs thrown in a year",
    x = "Horizontal Movement (in)",
    y = "Vertical Movement (in)", 
    fill = "Whiff%"
  ) +
  # movement theme
  movement_theme() +
  # facet wrap by season
  facet_wrap(
    ~ season,
    nrow = 1
  )


### xwOBA
pitching_models |>
  filter(pitch_name == "4-Seam Fastball") |> 
  ggplot() + 
  # hexbins
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = xwOBA), 
                   binwidth = c(3, 3), fun = mean,
                   color = "black") +
  # color gradient of average whiff rate
  scale_fill_gradient2(low = "firebrick2",
                       mid = "white",
                       high = "dodgerblue2",
                       midpoint = mean(ff_qualified$xwOBA)) +
  # 0 inches of vertical movement
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  # 0 inches of horizontal movement
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  # mean of horizontal break
  geom_vline(aes(xintercept = mean(horizontal_break)),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  # mean of vertical break
  geom_hline(aes(yintercept = mean(induced_vertical_break)), 
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  # hand side label
  geom_label(label = "HAND SIDE", x = 17.15, y = -0.3,
             label.padding = unit(0.35, "lines"),
             label.r = unit(0.25, "lines"),
             label.size = 1,
             size.unit = "mm") +
  # glove side label
  geom_label(label = "GLOVE SIDE", x = -1.9, y = -0.3,
             label.padding = unit(0.35, "lines"),
             label.r = unit(0.25, "lines"),
             label.size = 1,
             size.unit = "mm") +
  # create breaks and add inches format to y axis
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # create breaks and add inches format to x axis
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  coord_fixed() +
  # labels
  labs(
    title = "POV: Right-Handed Pitcher Facing Home Plate",
    caption = "All pitchers with 100 or more fastballs thrown in a year",
    x = "Horizontal Movement (in)",
    y = "Vertical Movement (in)", 
    fill = "xwOBA") +
  # add theme function created above
  movement_theme() +
  # facet by each season
  facet_wrap(
    ~ season,
    nrow = 1
  )



### Stuff+
pitching_models |> 
  filter(pitch_name == "4-Seam Fastball") |> 
  ggplot() + 
  # create the bins by Stuff+
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = stuff_plus), 
                   binwidth = c(3, 3), fun = mean,
                   color = "black") +
  # create color gradient with 100 being the average
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = 100) +
  # 0 inches of horizontal movement
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  # 0 inches of vertical movement
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  # average horizontal break
  geom_vline(aes(xintercept = mean(horizontal_break)),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  # average induced vertical break
  geom_hline(aes(yintercept = mean(induced_vertical_break)), 
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  # hand side label
  geom_label(label = "HAND SIDE", x = 17.15, y = -0.3,
             label.padding = unit(0.35, "lines"),
             label.r = unit(0.25, "lines"),
             label.size = 1,
             size.unit = "mm") +
  # glove side label
  geom_label(label = "GLOVE SIDE", x = -1.9, y = -0.3,
             label.padding = unit(0.35, "lines"),
             label.r = unit(0.25, "lines"),
             label.size = 1,
             size.unit = "mm") +
  # create breaks and add inches format to y axis
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # create breaks and add inches format to x axis
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  coord_fixed() +
  # labels
  labs(
    title = "POV: Right-Handed Pitcher Facing Home Plate",
    caption = "All pitchers with 100 or more fastballs thrown in a year",
    x = "Horizontal Movement (in)",
    y = "Vertical Movement (in)", 
    fill = "Stuff+"
  ) +
  # add theme function created above
  movement_theme() +
  # facet by each season
  facet_wrap(
    ~ season,
    nrow = 1
  )











# SLIDERS -----------------------------------------------------------------

### qualified slider/sweeper pitchers
sl_qualified <- pitching_models |>
  filter(pitch_name == "Slider")


# ### velocity
# pitching_models |>
#   filter(pitch_name == "Slider") |>
#   ggplot() +
#   stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
#                        z = velocity),
#                    binwidth = c(3, 3), fun = mean,
#                    color = "black") +
#   scale_fill_gradient2(low = "dodgerblue2",
#                        mid = "white",
#                        high = "firebrick2",
#                        midpoint = mean(sl_qualified$velocity)) +
#   geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
#   geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
#   geom_vline(aes(xintercept = mean(horizontal_break)),
#              color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
#   geom_hline(aes(yintercept = mean(induced_vertical_break)),
#              color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
#   # create breaks and add inches format to y axis
#   scale_y_continuous(breaks = seq(-20, 20, 5),
#                      labels = scales::number_format(suffix = "\"")) +
#   # create breaks and add inches format to x axis
#   scale_x_continuous(breaks = seq(-20, 20, 5),
#                      labels = scales::number_format(suffix = "\"")) +
#   coord_fixed() +
#   # labels
#   labs(
#     title = "POV: Right-Handed Pitcher Facing Home Plate",
#     caption = "All pitchers with 100 or more sliders thrown in a year",
#     x = "Horizontal Movement (in)",
#     y = "Vertical Movement (in)", 
#     fill = "Velocity"
#   ) +
#   movement_theme() +
#   facet_wrap(
#     ~ season,
#     nrow = 1
#   )
# 
# 
# ### spin rate
# pitching_models |>
#   filter(pitch_name == "Slider") |>
#   ggplot() +
#   # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) +
#   stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
#                        z = spin),
#                    binwidth = c(3, 3), fun = mean,
#                    color = "black") +
#   scale_fill_gradient2(low = "dodgerblue2",
#                        mid = "white",
#                        high = "firebrick2",
#                        midpoint = mean(sl_qualified$spin)) +
#   geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
#   geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
#   geom_vline(aes(xintercept = mean(horizontal_break)),
#              color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
#   geom_hline(aes(yintercept = mean(induced_vertical_break)),
#              color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
#   scale_y_continuous(breaks = seq(-20, 20, 5),
#                      labels = scales::number_format(suffix = "\"")) +
#   scale_x_continuous(breaks = seq(-20, 20, 5),
#                      labels = scales::number_format(suffix = "\"")) +
#   # geom_zone(linecolor = "black") +
#   # geom_plate() +
#   coord_fixed() +
#   labs(#title = "Spin Rate of Sliders by Movement",
#     subtitle = "All Pitchers With 100 or More Sliders Thrown in a Year",
#     caption = "Data courtesy of Baseball Savant",
#     x = "Horizontal Movement (in)",
#     y = "Vertical Movement (in)",
#     fill = "Spin (RPM)") +
#   movement_theme() +
#   theme(
#     legend.key.size = unit(0.45, "inches")
#   ) +
#   facet_wrap(
#     ~ season,
#     nrow = 1
#   )

### velocity
pitching_models |> 
  filter(pitch_name == "Slider") |> 
  ggplot() + 
  # create the bins by xwOBA
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = velocity), 
                   binwidth = c(3, 3), fun = mean,
                   color = "black") +
  # create color gradient with average slider xwOBA
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = mean(sl_qualified$velocity)) +
  # 0 inches of horizontal movement
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  # 0 inches of vertical movement
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  # average horizontal break
  geom_vline(aes(xintercept = mean(horizontal_break)),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  # average induced vertical break
  geom_hline(aes(yintercept = mean(induced_vertical_break)), 
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  # hand side label
  geom_label(label = "HAND SIDE", x = 7.25, y = 16.85,
             label.padding = unit(0.35, "lines"),
             label.r = unit(0.25, "lines"),
             label.size = 1,
             size.unit = "mm") +
  # glove side label
  geom_label(label = "GLOVE SIDE", x = -18.65, y = 16.85,
             label.padding = unit(0.35, "lines"),
             label.r = unit(0.25, "lines"),
             label.size = 1,
             size.unit = "mm") +
  # create breaks and add inches format to y axis
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # create breaks and add inches format to x axis
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # labels
  coord_fixed() +
  labs(
    title = "POV: Right-Handed Pitcher Facing Home Plate",
    caption = "All pitchers with 100 or more sliders thrown in a year",
    x = "Horizontal Movement (in)",
    y = "Vertical Movement (in)", 
    fill = "Velocity (MPH)"
  ) +
  # add theme function created above
  movement_theme() +
  # facet by season
  facet_wrap(
    ~ season,
    nrow = 1
  )


### spin rate
pitching_models |> 
  filter(pitch_name == "Slider") |> 
  ggplot() + 
  # create the bins by xwOBA
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = spin), 
                   binwidth = c(3, 3), fun = mean,
                   color = "black") +
  # create color gradient with average slider xwOBA
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = mean(sl_qualified$spin),
                       breaks = c(2250, 2750, 3250)) +
  # 0 inches of horizontal movement
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  # 0 inches of vertical movement
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  # average horizontal break
  geom_vline(aes(xintercept = mean(horizontal_break)),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  # average induced vertical break
  geom_hline(aes(yintercept = mean(induced_vertical_break)), 
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  # hand side label
  geom_label(label = "HAND SIDE", x = 7.25, y = 16.85,
             label.padding = unit(0.35, "lines"),
             label.r = unit(0.25, "lines"),
             label.size = 1,
             size.unit = "mm") +
  # glove side label
  geom_label(label = "GLOVE SIDE", x = -18.65, y = 16.85,
             label.padding = unit(0.35, "lines"),
             label.r = unit(0.25, "lines"),
             label.size = 1,
             size.unit = "mm") +
  # create breaks and add inches format to y axis
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # create breaks and add inches format to x axis
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # labels
  coord_fixed() +
  labs(
    title = "POV: Right-Handed Pitcher Facing Home Plate",
    caption = "All pitchers with 100 or more sliders thrown in a year",
    x = "Horizontal Movement (in)",
    y = "Vertical Movement (in)", 
    fill = "Spin Rate (RPM)"
  ) +
  # add theme function created above
  movement_theme() +
  # facet by season
  facet_wrap(
    ~ season,
    nrow = 1
  )



### xwOBA
pitching_models |> 
  filter(pitch_name == "Slider") |> 
  ggplot() + 
  # create the bins by xwOBA
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = xwOBA), 
                   binwidth = c(3, 3), fun = mean,
                   color = "black") +
  # create color gradient with average slider xwOBA
  scale_fill_gradient2(low = "firebrick2",
                       mid = "white",
                       high = "dodgerblue2",
                       midpoint = mean(sl_qualified$xwOBA),
                       breaks = c(0.200, 0.275, 0.350)) +
  # 0 inches of horizontal movement
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  # 0 inches of vertical movement
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  # average horizontal break
  geom_vline(aes(xintercept = mean(horizontal_break)),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  # average induced vertical break
  geom_hline(aes(yintercept = mean(induced_vertical_break)), 
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  # hand side label
  geom_label(label = "HAND SIDE", x = 7.25, y = 16.85,
             label.padding = unit(0.35, "lines"),
             label.r = unit(0.25, "lines"),
             label.size = 1,
             size.unit = "mm") +
  # glove side label
  geom_label(label = "GLOVE SIDE", x = -18.65, y = 16.85,
             label.padding = unit(0.35, "lines"),
             label.r = unit(0.25, "lines"),
             label.size = 1,
             size.unit = "mm") +
  # create breaks and add inches format to y axis
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # create breaks and add inches format to x axis
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # labels
  coord_fixed() +
  labs(
    title = "POV: Right-Handed Pitcher Facing Home Plate",
    caption = "All pitchers with 100 or more sliders thrown in a year",
    x = "Horizontal Movement (in)",
    y = "Vertical Movement (in)", 
    fill = "xwOBA"
  ) +
  # add theme function created above
  movement_theme() +
  # facet by season
  facet_wrap(
    ~ season,
    nrow = 1
  )



### whiff%
pitching_models |> 
  filter(pitch_name == "Slider") |> 
  ggplot() + 
  # create the bins by Whiff%
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = whiff_pct), 
                   binwidth = c(3, 3), fun = mean,
                   color = "black") +
  # create color gradient with average slider Whiff%
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = mean(sl_qualified$whiff_pct)) +
  # 0 inches of horizontal movement
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  # 0 inches of vertical movement
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  # average horizontal break
  geom_vline(aes(xintercept = mean(horizontal_break)),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  # average induced vertical break
  geom_hline(aes(yintercept = mean(induced_vertical_break)), 
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  # hand side label
  geom_label(label = "HAND SIDE", x = 7.25, y = 16.85,
             label.padding = unit(0.35, "lines"),
             label.r = unit(0.25, "lines"),
             label.size = 1,
             size.unit = "mm") +
  # glove side label
  geom_label(label = "GLOVE SIDE", x = -18.65, y = 16.85,
             label.padding = unit(0.35, "lines"),
             label.r = unit(0.25, "lines"),
             label.size = 1,
             size.unit = "mm") +
  # create breaks and add inches format to y axis
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # create breaks and add inches format to x axis
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  coord_fixed() +
  # labels
  labs(
    title = "POV: Right-Handed Pitcher Facing Home Plate",
    caption = "All pitchers with 100 or more sliders thrown in a year",
    x = "Horizontal Movement (in)",
    y = "Vertical Movement (in)", 
    fill = "Whiff%") +
  # add theme function created above
  movement_theme() +
  # facet by season
  facet_wrap(
    ~ season,
    nrow = 1
  )


### Stuff+
pitching_models |> 
  filter(pitch_name == "Slider") |> 
  ggplot() + 
  # create the bins by stuff+
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = stuff_plus), 
                   binwidth = c(3, 3), fun = mean,
                   color = "black") +
  # create color gradient with 100 being the average
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = 100) +
  # 0 inches of horizontal movement
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  # 0 inches of vertical movement
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  # average horizontal break
  geom_vline(aes(xintercept = mean(horizontal_break)),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  # average induced vertical break
  geom_hline(aes(yintercept = mean(induced_vertical_break)), 
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  # hand side label
  geom_label(label = "HAND SIDE", x = 7.25, y = 16.85,
             label.padding = unit(0.35, "lines"),
             label.r = unit(0.25, "lines"),
             label.size = 1,
             size.unit = "mm") +
  # glove side label
  geom_label(label = "GLOVE SIDE", x = -18.65, y = 16.85,
             label.padding = unit(0.35, "lines"),
             label.r = unit(0.25, "lines"),
             label.size = 1,
             size.unit = "mm") +
  # create breaks and add inches format to y axis
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # create breaks and add inches format to x axis
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  coord_fixed() +
  # labels
  labs(
    title = "POV: Right-Handed Pitcher Facing Home Plate",
    caption = "All pitchers with 100 or more sliders thrown in a year",
    x = "Horizontal Movement (in)",
    y = "Vertical Movement (in)", 
    fill = "Stuff+") +
  # add theme function created above
  movement_theme() +
  # facet by each season
  facet_wrap(
    ~ season,
    nrow = 1
  )



# Average Slider / Sweeper ------------------------------------------------

## libraries
library(gt)
library(gtExtras)

## Load in data
statcast_filter <- read.csv("statcast_2021-24_copy.csv")

glimpse(statcast_filter)

glimpse(sl_qualified)

statcast_filter |> 
  filter(
    pitch_name %in% c("Slider", "Sweeper"),
    pitcher_id %in% sl_qualified$pitcher_id
  ) |> 
  group_by(pitch_name) |> 
  summarize(
    avg_velocity = mean(release_speed, na.rm = TRUE),
    avg_spin = mean(release_spin_rate, na.rm = TRUE),
    avg_spin_axis = mean(spin_axis, na.rm = TRUE),
    avg_horizontal_break = mean(horizontal_movement_in, na.rm = TRUE),
    avg_vertical_break = mean(vertical_movement_in, na.rm = TRUE)
  ) |> 
  # create gt table of mean and standard deviation
  gt(rowname_col = "Pitches") |> 
  fmt_number(columns = c(avg_velocity, avg_horizontal_break, avg_vertical_break), decimals = 2) |> 
  fmt_number(columns = c(avg_spin, avg_spin_axis), decimals = 0) |>
  data_color(columns = avg_velocity,
             fn = scales::col_numeric(palette = c("dodgerblue4", "white", "firebrick2"), 
                                      domain = c(78.26798, 90.63364))) |>
  data_color(columns = avg_spin,
             fn = scales::col_numeric(palette = c("dodgerblue4", "white", "firebrick2"), 
                                      domain = c(1974.244, 2895.012))) |>
  data_color(columns = avg_horizontal_break,
             fn = scales::col_numeric(palette = c("dodgerblue4", "white", "firebrick2"), 
                                      domain = c(-2.733124, 17.03868))) |>
  cols_align(align = "center", columns = c(avg_velocity, avg_horizontal_break, avg_vertical_break,
                                          avg_spin, avg_spin_axis)) |> 
  tab_stubhead(label = "Slider and Sweeper Differences") |> 
  cols_label(
    pitch_name = "Pitch Name",
    avg_velocity = "Velocity (MPH)",
    avg_spin = "Spin Rate (RPM)",
    avg_spin_axis = "Spin Axis",
    avg_horizontal_break = "Horizontal Break (in)",
    avg_vertical_break = "Vertical Break (in)"
  ) |> 
  tab_header(title = md("**Slider and Sweeper Differences**"),
             subtitle = md("*Comparing Pitch Physical Characteristics*"),
  ) |> 
  tab_source_note(source_note = md("*Number of pitchers who threw 100+ Sliders: 699*
                                   <br>*Number of pitchers who threw 100+ Sweepers: 196*")) |> 
  gtExtras::gt_theme_espn()


tibble(
  pitches = c("Slider", "Sweeper"),
  velo = c()
)














# SINKERS -----------------------------------------------------------------

### qualified slider/sweeper pitchers
si_qualified <- pitching_models |>
  filter(pitch_name == "Sinker")


### velocity
pitching_models |>
  filter(pitch_name == "Sinker") |>
  ggplot() +
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) +
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = velocity),
                   binwidth = c(3, 3), fun = mean,
                   color = "black") +
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = mean(si_qualified$velocity)) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  geom_vline(aes(xintercept = mean(horizontal_break)),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  geom_hline(aes(yintercept = mean(induced_vertical_break)),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  coord_fixed() +
  labs(#title = "Avg. Velo of 4-Seam Fastballs by Movement",
    title = "All Pitchers With 100 or More Sliders Thrown in a Year",
    subtitle = "POV: RHP facing home plate",
    caption = "Data courtesy of Baseball Savant",
    x = "Horizontal Movement (in)",
    y = "Vertical Movement (in)",
    fill = "Avg. Velocity") +
  movement_theme() +
  facet_wrap(
    ~ season,
    nrow = 1
  )


### spin rate
pitching_models |>
  filter(pitch_name == "Sinker") |>
  ggplot() +
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) +
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = spin),
                   binwidth = c(3, 3), fun = mean,
                   color = "black") +
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = mean(si_qualified$spin)) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  geom_vline(aes(xintercept = mean(horizontal_break)),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  geom_hline(aes(yintercept = mean(induced_vertical_break)),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # geom_zone(linecolor = "black") +
  # geom_plate() +
  coord_fixed() +
  labs(#title = "Spin Rate of Sliders by Movement",
    subtitle = "All Pitchers With 100 or More Sliders Thrown in a Year",
    caption = "Data courtesy of Baseball Savant",
    x = "Horizontal Movement (in)",
    y = "Vertical Movement (in)",
    fill = "Spin (RPM)") +
  movement_theme() +
  theme(
    legend.key.size = unit(0.45, "inches")
  ) +
  facet_wrap(
    ~ season,
    nrow = 1
  )



### xwOBA
pitching_models |> 
  filter(pitch_name == "Sinker") |> 
  ggplot() + 
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = xwOBA), 
                   binwidth = c(3, 3), fun = mean,
                   color = "black") +
  scale_fill_gradient2(low = "firebrick2",
                       mid = "white",
                       high = "dodgerblue2",
                       midpoint = mean(si_qualified$xwOBA)) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  geom_vline(aes(xintercept = mean(horizontal_break)),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  geom_hline(aes(yintercept = mean(induced_vertical_break)), 
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  # hand side label
  # geom_label(label = "HAND SIDE", x = 18.15, y = -0.3,
  #            label.padding = unit(0.35, "lines"),
  #            label.r = unit(0.25, "lines"),
  #            label.size = 1,
  #            size.unit = "mm") +
  # # glove side label
  # geom_label(label = "GLOVE SIDE", x = -2.9, y = -0.3,
  #            label.padding = unit(0.35, "lines"),
  #            label.r = unit(0.25, "lines"),
  #            label.size = 1,
  #            size.unit = "mm") +
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # geom_zone(linecolor = "black") +
  # geom_plate() +
  coord_fixed() +
  labs(
    title = "POV: Right-Handed Pitcher Facing Home Plate",
    caption = "All pitchers with 100 or more fastballs thrown in a year",
    x = "Horizontal Movement (in)",
    y = "Vertical Movement (in)", 
    fill = "xwOBA") +
  movement_theme() +
  facet_wrap(
    ~ season,
    nrow = 1
  )



### whiff%
pitching_models |> 
  filter(pitch_name == "Sinker") |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = whiff_pct), 
                   binwidth = c(3, 3), fun = mean,
                   color = "black") +
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = mean(si_qualified$whiff_pct)) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  geom_vline(aes(xintercept = mean(horizontal_break)),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  geom_hline(aes(yintercept = mean(induced_vertical_break)), 
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  # hand side label
  # geom_label(label = "HAND SIDE", x = 18.15, y = -0.3,
  #            label.padding = unit(0.35, "lines"),
  #            label.r = unit(0.25, "lines"),
  #            label.size = 1,
  #            size.unit = "mm") +
  # # glove side label
  # geom_label(label = "GLOVE SIDE", x = -2.9, y = -0.3,
  #            label.padding = unit(0.35, "lines"),
  #            label.r = unit(0.25, "lines"),
  #            label.size = 1,
  #            size.unit = "mm") +
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # geom_zone(linecolor = "black") +
  # geom_plate() +
  coord_fixed() +
  labs(
    title = "POV: Right-Handed Pitcher Facing Home Plate",
    caption = "All pitchers with 100 or more fastballs thrown in a year",
    x = "Horizontal Movement (in)",
    y = "Vertical Movement (in)", 
    fill = "Whiff%") +
  movement_theme() +
  facet_wrap(
    ~ season,
    nrow = 1
  )


### Stuff+
pitching_models |> 
  filter(pitch_name == "Sinker") |> 
  ggplot() + 
  # create the bins by stuff+
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = stuff_plus), 
                   binwidth = c(3, 3), fun = mean,
                   color = "black") +
  # create color gradient with 100 being the average
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = 100) +
  # 0 inches of horizontal movement
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  # 0 inches of vertical movement
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  # average horizontal break
  geom_vline(aes(xintercept = mean(horizontal_break)),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  # average induced vertical break
  geom_hline(aes(yintercept = mean(induced_vertical_break)), 
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  # hand side label
  # geom_label(label = "HAND SIDE", x = 18.15, y = -0.3,
  #            label.padding = unit(0.35, "lines"),
  #            label.r = unit(0.25, "lines"),
  #            label.size = 1,
  #            size.unit = "mm") +
  # # glove side label
  # geom_label(label = "GLOVE SIDE", x = -2.9, y = -0.3,
  #            label.padding = unit(0.35, "lines"),
  #            label.r = unit(0.25, "lines"),
  #            label.size = 1,
  #            size.unit = "mm") +
  # create breaks and add inches format to y axis
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # create breaks and add inches format to x axis
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  coord_fixed() +
  # labels
  labs(
    title = "POV: Right-Handed Pitcher Facing Home Plate",
    caption = "All pitchers with 100 or more fastballs thrown in a year",
    x = "Horizontal Movement (in)",
    y = "Vertical Movement (in)", 
    fill = "Stuff+") +
  # add theme function created above
  movement_theme() +
  # facet by each season
  facet_wrap(
    ~ season,
    nrow = 1
  )


# CURVEBALLS -----------------------------------------------------------------

### qualified slider/sweeper pitchers
cu_qualified <- pitching_models |>
  filter(pitch_name == "Curveball")


### velocity
pitching_models |>
  filter(pitch_name == "Curveball") |>
  ggplot() +
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) +
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = velocity),
                   binwidth = c(3, 3), fun = mean,
                   color = "black") +
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = mean(cu_qualified$velocity)) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  geom_vline(aes(xintercept = mean(horizontal_break)),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  geom_hline(aes(yintercept = mean(induced_vertical_break)),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  coord_fixed() +
  labs(#title = "Avg. Velo of 4-Seam Fastballs by Movement",
    title = "All Pitchers With 100 or More Sliders Thrown in a Year",
    subtitle = "POV: RHP facing home plate",
    caption = "Data courtesy of Baseball Savant",
    x = "Horizontal Movement (in)",
    y = "Vertical Movement (in)",
    fill = "Avg. Velocity") +
  movement_theme() +
  facet_wrap(
    ~ season,
    nrow = 1
  )


### spin rate
pitching_models |>
  filter(pitch_name == "Curveball") |>
  ggplot() +
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) +
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = spin),
                   binwidth = c(3, 3), fun = mean,
                   color = "black") +
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = mean(cu_qualified$spin)) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  geom_vline(aes(xintercept = mean(horizontal_break)),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  geom_hline(aes(yintercept = mean(induced_vertical_break)),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # geom_zone(linecolor = "black") +
  # geom_plate() +
  coord_fixed() +
  labs(#title = "Spin Rate of Sliders by Movement",
    subtitle = "All Pitchers With 100 or More Sliders Thrown in a Year",
    caption = "Data courtesy of Baseball Savant",
    x = "Horizontal Movement (in)",
    y = "Vertical Movement (in)",
    fill = "Spin (RPM)") +
  movement_theme() +
  theme(
    legend.key.size = unit(0.45, "inches")
  ) +
  facet_wrap(
    ~ season,
    nrow = 1
  )



### xwOBA
pitching_models |> 
  filter(pitch_name == "Curveball") |> 
  ggplot() + 
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = xwOBA), 
                   binwidth = c(3, 3), fun = mean,
                   color = "black") +
  scale_fill_gradient2(low = "firebrick2",
                       mid = "white",
                       high = "dodgerblue2",
                       midpoint = mean(cu_qualified$xwOBA)) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  geom_vline(aes(xintercept = mean(horizontal_break)),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  geom_hline(aes(yintercept = mean(induced_vertical_break)), 
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  # hand side label
  # geom_label(label = "HAND SIDE", x = 18.15, y = -0.3,
  #            label.padding = unit(0.35, "lines"),
  #            label.r = unit(0.25, "lines"),
  #            label.size = 1,
  #            size.unit = "mm") +
  # # glove side label
  # geom_label(label = "GLOVE SIDE", x = -2.9, y = -0.3,
  #            label.padding = unit(0.35, "lines"),
  #            label.r = unit(0.25, "lines"),
  #            label.size = 1,
  #            size.unit = "mm") +
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # geom_zone(linecolor = "black") +
  # geom_plate() +
  coord_fixed() +
  labs(
    title = "POV: Right-Handed Pitcher Facing Home Plate",
    caption = "All pitchers with 100 or more fastballs thrown in a year",
    x = "Horizontal Movement (in)",
    y = "Vertical Movement (in)", 
    fill = "xwOBA") +
  movement_theme() +
  facet_wrap(
    ~ season,
    nrow = 1
  )



### whiff%
pitching_models |> 
  filter(pitch_name == "Curveball") |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = whiff_pct), 
                   binwidth = c(3, 3), fun = mean,
                   color = "black") +
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = mean(cu_qualified$whiff_pct)) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  geom_vline(aes(xintercept = mean(horizontal_break)),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  geom_hline(aes(yintercept = mean(induced_vertical_break)), 
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  # hand side label
  # geom_label(label = "HAND SIDE", x = 18.15, y = -0.3,
  #            label.padding = unit(0.35, "lines"),
  #            label.r = unit(0.25, "lines"),
  #            label.size = 1,
  #            size.unit = "mm") +
  # # glove side label
  # geom_label(label = "GLOVE SIDE", x = -2.9, y = -0.3,
  #            label.padding = unit(0.35, "lines"),
  #            label.r = unit(0.25, "lines"),
  #            label.size = 1,
  #            size.unit = "mm") +
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # geom_zone(linecolor = "black") +
  # geom_plate() +
  coord_fixed() +
  labs(
    title = "POV: Right-Handed Pitcher Facing Home Plate",
    caption = "All pitchers with 100 or more fastballs thrown in a year",
    x = "Horizontal Movement (in)",
    y = "Vertical Movement (in)", 
    fill = "Whiff%") +
  movement_theme() +
  facet_wrap(
    ~ season,
    nrow = 1
  )


### Stuff+
pitching_models |> 
  filter(pitch_name == "Curveball") |> 
  ggplot() + 
  # create the bins by stuff+
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = stuff_plus), 
                   binwidth = c(3, 3), fun = mean,
                   color = "black") +
  # create color gradient with 100 being the average
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = 100) +
  # 0 inches of horizontal movement
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  # 0 inches of vertical movement
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  # average horizontal break
  geom_vline(aes(xintercept = mean(horizontal_break)),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  # average induced vertical break
  geom_hline(aes(yintercept = mean(induced_vertical_break)), 
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  # hand side label
  # geom_label(label = "HAND SIDE", x = 18.15, y = -0.3,
  #            label.padding = unit(0.35, "lines"),
  #            label.r = unit(0.25, "lines"),
  #            label.size = 1,
  #            size.unit = "mm") +
  # # glove side label
  # geom_label(label = "GLOVE SIDE", x = -2.9, y = -0.3,
  #            label.padding = unit(0.35, "lines"),
  #            label.r = unit(0.25, "lines"),
  #            label.size = 1,
  #            size.unit = "mm") +
  # create breaks and add inches format to y axis
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # create breaks and add inches format to x axis
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  coord_fixed() +
  # labels
  labs(
    title = "POV: Right-Handed Pitcher Facing Home Plate",
    caption = "All pitchers with 100 or more fastballs thrown in a year",
    x = "Horizontal Movement (in)",
    y = "Vertical Movement (in)", 
    fill = "Stuff+") +
  # add theme function created above
  movement_theme() +
  # facet by each season
  facet_wrap(
    ~ season,
    nrow = 1
  )



### 
