## Libraries
library(tidyverse)
library(baseballr)
library(ggthemes)
library(hexbin)

## Load in data
pitching_models <- read.csv("mlb_pitching_stats_2021-24.csv")

## Only use 2021-2023 (full seasons)
pitching_models <- pitching_models |> 
  filter(season %in% c(2021, 2022, 2023)) |> 
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

# AGGREGATED PITCHING MODELS HEX GRAPHS -----------------------------------
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
#                    binwidth = c(2.5, 2.5), fun = mean,
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
#   facet_wrap(~ season)
# 
# ### spin rate
# pitching_models |> 
#   filter(pitch_name == "4-Seam Fastball") |> 
#   ggplot() + 
#   # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
#   stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
#                        z = spin), 
#                    binwidth = c(2.5, 2.5), fun = mean,
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
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = whiff_pct), 
                   binwidth = c(3, 3), fun = mean,
                   color = "black") +
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = mean(ff_qualified$whiff_pct)) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  geom_vline(aes(xintercept = mean(horizontal_break)),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  geom_hline(aes(yintercept = mean(induced_vertical_break)), 
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  # hand side label
  geom_label(label = "HAND SIDE", x = 18.15, y = -0.3,
             label.padding = unit(0.35, "lines"),
             label.r = unit(0.25, "lines"),
             label.size = 1,
             size.unit = "mm") +
  # glove side label
  geom_label(label = "GLOVE SIDE", x = -2.9, y = -0.3,
             label.padding = unit(0.35, "lines"),
             label.r = unit(0.25, "lines"),
             label.size = 1,
             size.unit = "mm") +
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
  facet_wrap(~ season)


### xwOBA
pitching_models |> 
  filter(pitch_name == "4-Seam Fastball") |> 
  ggplot() + 
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = xwOBA), 
                   binwidth = c(3, 3), fun = mean,
                   color = "black") +
  scale_fill_gradient2(low = "firebrick2",
                       mid = "white",
                       high = "dodgerblue2",
                       midpoint = mean(ff_qualified$xwOBA)) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  geom_vline(aes(xintercept = mean(horizontal_break)),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  geom_hline(aes(yintercept = mean(induced_vertical_break)), 
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  # hand side label
  geom_label(label = "HAND SIDE", x = 18.15, y = -0.3,
             label.padding = unit(0.35, "lines"),
             label.r = unit(0.25, "lines"),
             label.size = 1,
             size.unit = "mm") +
  # glove side label
  geom_label(label = "GLOVE SIDE", x = -2.9, y = -0.3,
             label.padding = unit(0.35, "lines"),
             label.r = unit(0.25, "lines"),
             label.size = 1,
             size.unit = "mm") +
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
  facet_wrap(~ season)



### Stuff+
pitching_models |> 
  filter(pitch_name == "4-Seam Fastball") |> 
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
  geom_label(label = "HAND SIDE", x = 18.15, y = -0.3,
             label.padding = unit(0.35, "lines"),
             label.r = unit(0.25, "lines"),
             label.size = 1,
             size.unit = "mm") +
  # glove side label
  geom_label(label = "GLOVE SIDE", x = -2.9, y = -0.3,
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
    fill = "Stuff+") +
  # add theme function created above
  movement_theme() +
  # facet by each season
  facet_wrap(~ season)












## SLIDERS
### qualified slider/sweeper pitchers
# sl_qualified <- pitching_models |> 
#   filter(pitch_name == "Slider")
# 
# 
# ### velocity
# pitching_models |> 
#   filter(pitch_name == "Slider") |> 
#   ggplot() + 
#   # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
#   stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
#                        z = velocity), 
#                    binwidth = c(2, 2), fun = mean,
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
#   scale_y_continuous(breaks = seq(-20, 20, 5),
#                      labels = scales::number_format(suffix = "\"")) +
#   scale_x_continuous(breaks = seq(-20, 20, 5),
#                      labels = scales::number_format(suffix = "\"")) +
#   # geom_zone(linecolor = "black") +
#   # geom_plate() +
#   coord_fixed() +
#   labs(#title = "Avg. Velo of Sliders by Movement",
#     subtitle = "All Pitchers With 100 or More Sliders Thrown in a Year",
#     caption = "Data courtesy of Baseball Savant",
#     x = "Horizontal Movement (in)",
#     y = "Vertical Movement (in)", 
#     fill = "Avg. Velocity") +
#   movement_theme() +
#   facet_wrap(~ season)
# 
# 
# ### spin rate
# pitching_models |> 
#   filter(pitch_name == "Slider") |> 
#   ggplot() + 
#   # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
#   stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
#                        z = spin), 
#                    binwidth = c(2, 2), fun = mean,
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
#   facet_wrap(~ season)
# 
# 
# ### xwOBA
# pitching_models |> 
#   filter(pitch_name == "Slider") |> 
#   ggplot() + 
#   # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
#   stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
#                        z = xwOBA), 
#                    binwidth = c(2, 2), fun = mean,
#                    color = "black") +
#   scale_fill_gradient2(low = "dodgerblue2",
#                        mid = "white",
#                        high = "firebrick2",
#                        midpoint = mean(sl_qualified$xwOBA)) +
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
#   labs(#title = "xwOBA of Sliders by Movement",
#     subtitle = "All Pitchers With 100 or More Sliders Thrown in a Year",
#     caption = "Data courtesy of Baseball Savant",
#     x = "Horizontal Movement (in)",
#     y = "Vertical Movement (in)", 
#     fill = "xwOBA") +
#   movement_theme() +
#   facet_wrap(~ season)
# 
# 
# ### whiff%
# pitching_models |> 
#   filter(pitch_name == "Slider") |> 
#   ggplot() + 
#   # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
#   stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
#                        z = whiff_pct), 
#                    binwidth = c(2, 2), fun = mean,
#                    color = "black") +
#   scale_fill_gradient2(low = "dodgerblue2",
#                        mid = "white",
#                        high = "firebrick2",
#                        midpoint = mean(sl_qualified$whiff_pct)) +
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
#   labs(#title = "Whiff% of Sliders by Movement",
#     subtitle = "All Pitchers With 100 or More Sliders Thrown in a Year",
#     caption = "Data courtesy of Baseball Savant",
#     x = "Horizontal Movement (in)",
#     y = "Vertical Movement (in)", 
#     fill = "Whiff%") +
#   movement_theme() +
#   facet_wrap(~ season)
# 
# 
# ### Stuff+
# pitching_models |> 
#   filter(pitch_name == "Slider") |> 
#   ggplot() + 
#   stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
#                        z = stuff_plus), 
#                    binwidth = c(3, 3), fun = mean,
#                    color = "black") +
#   scale_fill_gradient2(low = "dodgerblue2",
#                        mid = "white",
#                        high = "firebrick2",
#                        midpoint = 100) +
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
#   coord_fixed() +
#   labs(title = "Stuff+ of Sliders by Movement",
#        subtitle = "All Pitchers With 100 or More Sliders Thrown in a Year",
#        caption = "Data courtesy of Baseball Savant",
#        x = "Horizontal Movement (in)",
#        y = "Vertical Movement (in)", 
#        fill = "Stuff+") +
#   movement_theme() +
#   facet_wrap(~ season)
