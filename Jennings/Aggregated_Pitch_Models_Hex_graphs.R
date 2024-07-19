## Load in data
pitching_models <- read.csv("mlb_pitching_stats_2020-24.csv")

## Libraries
library(tidyverse)
library(baseballr)
library(ggthemes)
library(hexbin)

## set theme
theme_set(theme_bw())

## create own theme for each graph
movement_theme <- function(){
  theme(
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    strip.background = element_rect(fill = "navy", color = "black", linewidth = 1),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 10),
    axis.title.x = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.key.size = unit(0.35, "inches"),
    legend.text = element_text(size = 12)
  )
}

# AGGREGATED PITCHING MODELS HEX GRAPHS -----------------------------------
## "Qualified" 4-Seam Fastball pitchers
ff_qualified <- pitching_models |> 
  filter(pitch_name == "4-Seam Fastball")

## 4-Seam Fastballs
### Velocity
pitching_models |> 
  filter(pitch_name == "4-Seam Fastball") |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = velocity), 
                   binwidth = c(2, 2), fun = mean,
                   color = "black") +
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = mean(ff_qualified$velocity)) +
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
  labs(title = "Avg. Velo of 4-Seam Fastballs by Movement",
       subtitle = "All Pitchers With 100 or More Fastballs Thrown in a Year",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Movement (in)",
       y = "Vertical Movement (in)", 
       fill = "Avg. Velocity") +
  movement_theme() +
  facet_wrap(~ season)

### xwOBA
pitching_models |> 
  filter(pitch_name == "4-Seam Fastball") |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = xwOBA), 
                   binwidth = c(2, 2), fun = mean,
                   color = "black") +
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = mean(ff_qualified$xwOBA)) +
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
  labs(title = "xwOBA of 4-Seam Fastballs by Movement",
       subtitle = "All Pitchers With 100 or More Fastballs Thrown in a Year",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Movement (in)",
       y = "Vertical Movement (in)", 
       fill = "xwOBA") +
  movement_theme() +
  facet_wrap(~ season)

### whiff%
pitching_models |> 
  filter(pitch_name == "4-Seam Fastball") |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = whiff_pct), 
                   binwidth = c(2, 2), fun = mean,
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
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # geom_zone(linecolor = "black") +
  # geom_plate() +
  coord_fixed() +
  labs(title = "Whiff% of 4-Seam Fastballs by Movement",
       subtitle = "All Pitchers With 100 or More Fastballs Thrown in a Year",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Movement (in)",
       y = "Vertical Movement (in)", 
       fill = "Whiff%") +
  movement_theme() +
  facet_wrap(~ season)


### spin rate
pitching_models |> 
  filter(pitch_name == "4-Seam Fastball") |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = spin), 
                   binwidth = c(2, 2), fun = mean,
                   color = "black") +
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = mean(ff_qualified$spin)) +
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
  labs(title = "Spin Rate of 4-Seam Fastballs by Movement",
       subtitle = "All Pitchers With 100 or More Fastballs Thrown in a Year",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Movement (in)",
       y = "Vertical Movement (in)", 
       fill = "Spin (RPM)") +
  movement_theme() +
  facet_wrap(~ season)


### Stuff+
pitching_models |> 
  filter(pitch_name == "4-Seam Fastball") |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = stuff_plus), 
                   binwidth = c(2, 2), fun = mean,
                   color = "black") +
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = 100) +
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
  labs(title = "Stuff+ of 4-Seam Fastballs by Movement",
       subtitle = "All Pitchers With 100 or More Fastballs Thrown in a Year",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Movement (in)",
       y = "Vertical Movement (in)", 
       fill = "Stuff+") +
  movement_theme() +
  facet_wrap(~ season)


## SLIDERS
### qualified slider/sweeper pitchers
sl_qualified <- pitching_models |> 
  filter(pitch_name == "Slider")


### velocity
pitching_models |> 
  filter(pitch_name == "Slider") |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = velocity), 
                   binwidth = c(2, 2), fun = mean,
                   color = "black") +
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = mean(sl_qualified$velocity)) +
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
  labs(title = "Avg. Velo of Sliders by Movement",
       subtitle = "All Pitchers With 100 or More Sliders Thrown in a Year",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Movement (in)",
       y = "Vertical Movement (in)", 
       fill = "Avg. Velocity") +
  movement_theme() +
  facet_wrap(~ season)


### xwOBA
pitching_models |> 
  filter(pitch_name == "Slider") |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = xwOBA), 
                   binwidth = c(2, 2), fun = mean,
                   color = "black") +
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = mean(sl_qualified$xwOBA)) +
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
  labs(title = "xwOBA of Sliders by Movement",
       subtitle = "All Pitchers With 100 or More Sliders Thrown in a Year",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Movement (in)",
       y = "Vertical Movement (in)", 
       fill = "xwOBA") +
  movement_theme() +
  facet_wrap(~ season)


### whiff%
pitching_models |> 
  filter(pitch_name == "Slider") |> 
  mutate(horizontal_break = -horizontal_break) |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = whiff_pct), 
                   binwidth = c(3, 3), fun = mean,
                   color = "black") +
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = mean(sl_qualified$whiff_pct)) +
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
  labs(title = "Whiff% of Sliders by Movement",
       subtitle = "All Pitchers With 100 or More Sliders Thrown in a Year",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Movement (in)",
       y = "Vertical Movement (in)", 
       fill = "Whiff%") +
  movement_theme() +
  facet_wrap(~ season)


### spin rate
pitching_models |> 
  filter(pitch_name == "Slider") |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = spin), 
                   binwidth = c(2, 2), fun = mean,
                   color = "black") +
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = mean(sl_qualified$spin)) +
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
  labs(title = "Spin Rate of Sliders by Movement",
       subtitle = "All Pitchers With 100 or More Sliders Thrown in a Year",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Movement (in)",
       y = "Vertical Movement (in)", 
       fill = "Spin (RPM)") +
  movement_theme() +
  facet_wrap(~ season)


### Stuff+
pitching_models |> 
  filter(pitch_name == "Slider") |> 
  mutate(horizontal_break = -horizontal_break) |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = stuff_plus), 
                   binwidth = c(2, 2), fun = mean,
                   color = "black") +
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = 100) +
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
  labs(title = "Stuff+ of Sliders by Movement",
       subtitle = "All Pitchers With 100 or More Sliders Thrown in a Year",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Movement (in)",
       y = "Vertical Movement (in)", 
       fill = "Stuff+") +
  movement_theme() +
  facet_wrap(~ season)




## CURVEBALLS
### qualified curveball/slow curve/slurve pitchers
cu_qualified <- pitching_models |> 
  filter(pitch_name == "Curveball")


### velocity
pitching_models |> 
  filter(pitch_name == "Curveball") |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = velocity), 
                   binwidth = c(2, 2), fun = mean,
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
  # geom_zone(linecolor = "black") +
  # geom_plate() +
  coord_fixed() +
  labs(title = "Avg. Velo of Curveballs by Movement",
       subtitle = "All Pitchers With 100 or More Curveballs Thrown in a Year",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Movement (in)",
       y = "Vertical Movement (in)", 
       fill = "Avg. Velocity") +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    strip.background = element_rect(fill = "navy", color = "black", linewidth = 1),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 10),
    axis.title.x = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.key.size = unit(0.35, "inches"),
    legend.text = element_text(size = 12)
  ) +
  facet_wrap(~ season)


### xwOBA
pitching_models |> 
  filter(pitch_name == "Curveball") |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = xwOBA), 
                   binwidth = c(2, 2), fun = mean,
                   color = "black") +
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = mean(cu_qualified$xwOBA)) +
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
  labs(title = "xwOBA of Curveballs by Movement",
       subtitle = "All Pitchers With 100 or More Curveballs Thrown in a Year",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Movement (in)",
       y = "Vertical Movement (in)", 
       fill = "xwOBA") +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    strip.background = element_rect(fill = "navy", color = "black", linewidth = 1),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 10),
    axis.title.x = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.key.size = unit(0.35, "inches"),
    legend.text = element_text(size = 12)
  ) +
  facet_wrap(~ season)


### whiff%
pitching_models |> 
  filter(pitch_name == "Curveball") |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = whiff_pct), 
                   binwidth = c(2, 2), fun = mean,
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
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # geom_zone(linecolor = "black") +
  # geom_plate() +
  coord_fixed() +
  labs(title = "Whiff% of Curveballs by Movement",
       subtitle = "All Pitchers With 100 or More Curveballs Thrown in a Year",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Movement (in)",
       y = "Vertical Movement (in)", 
       fill = "Whiff%") +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    strip.background = element_rect(fill = "navy", color = "black", linewidth = 1),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 10),
    axis.title.x = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.key.size = unit(0.35, "inches"),
    legend.text = element_text(size = 12)
  ) +
  facet_wrap(~ season)


### spin rate
pitching_models |> 
  filter(pitch_name == "Curveball") |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = spin), 
                   binwidth = c(2, 2), fun = mean,
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
  labs(title = "Spin Rate of Curveballs by Movement",
       subtitle = "All Pitchers With 100 or More Curveballs Thrown in a Year",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Movement (in)",
       y = "Vertical Movement (in)", 
       fill = "Spin (RPM)") +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    strip.background = element_rect(fill = "navy", color = "black", linewidth = 1),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 10),
    axis.title.x = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.key.size = unit(0.35, "inches"),
    legend.text = element_text(size = 10)
  ) +
  facet_wrap(~ season)


### Stuff+
pitching_models |> 
  filter(pitch_name == "Curveball") |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = stuff_plus), 
                   binwidth = c(2, 2), fun = mean,
                   color = "black") +
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = 100) +
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
  labs(title = "Stuff+ of Curveballs by Movement",
       subtitle = "All Pitchers With 100 or More Curveballs Thrown in a Year",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Movement (in)",
       y = "Vertical Movement (in)", 
       fill = "Stuff+") +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    strip.background = element_rect(fill = "navy", color = "black", linewidth = 1),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 10),
    axis.title.x = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.key.size = unit(0.35, "inches"),
    legend.text = element_text(size = 12)
  ) +
  facet_wrap(~ season)

