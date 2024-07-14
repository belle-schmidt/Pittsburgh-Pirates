## Statcast EDA

## libraries
library(tidyverse)
library(ggthemes)

##### color vector
pitch_colors <- c(
  "4-Seam Fastball" = "firebrick1",
  "Cutter" = "maroon",
  "Sinker" = "darkgreen",
  "Changeup" = "darkorange",
  "Forkball" = "coral",
  "Split-Finger" = "brown4",
  "Slider" = "purple3",
  "Sweeper" = "darkmagenta",
  "Slurve" = "violet",
  "Curveball" = "dodgerblue",
  "Knuckle Curve" = "darkblue",
  "Slow Curve" = "royalblue3"
)


### vertical by horizontal movement plot
statcast_pitch_summary_stats |> 
  filter(n_pitches >= 100) |> 
  # for pitcher's POV
  mutate(horizontal_break = -horizontal_break) |> 
  # plot
  ggplot(aes(horizontal_break, induced_vertical_break, color = pitch_name)) +
  geom_point(size = 2, alpha = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  scale_color_manual(values = pitch_colors) +
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     limits = c(-25, 25),
                     labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     limits = c(-25, 25),
                     labels = scales::number_format(suffix = "\"")) +
  labs(
    x = "Horizontal Break",
    y = "Induced Vertical Break",
    color = "Pitch Name",
    title = "Avg. Pitch Movement",
    subtitle = "2020-24 | Pitcher's POV",
    caption = "Data: Baseball Savant via baseballr"
  ) + 
  facet_wrap(~ season,
             nrow = 2,
             ncol = 3) +
  theme(
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    strip.background = element_rect(fill = "navy", color = "black", size = 1),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 14, hjust = 0.5),
    axis.text.y = element_text(size = 14, hjust = 0.5),
    legend.position = "bottom",
    plot.caption = element_text(size = 10)
  )


### vertical by horizontal movement plot for sliders and sweepers
statcast_pitch_summary_stats_q |> 
  filter(n_pitches >= 100 & pitch_name %in% c("Sweeper", "Slider")) |> 
  # for pitcher's POV
  mutate(horizontal_break = -horizontal_break,
         season = factor(season, levels = c(2020, 2021, 2022, 2023, 2024))) |> 
  # plot
  ggplot(aes(horizontal_break, induced_vertical_break, color = pitch_name)) +
  geom_point(size = 2, alpha = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  scale_color_manual(values = c("firebrick", "darkorange")) +
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     limits = c(-25, 25),
                     labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     limits = c(-25, 25),
                     labels = scales::number_format(suffix = "\"")) +
  labs(
    x = "Horizontal Break",
    y = "Induced Vertical Break",
    color = "Pitch Name",
    title = "Avg. Pitch Movement",
    subtitle = "2020-24 | Pitcher's POV",
    caption = "Data: Baseball Savant via baseballr"
  ) + 
  facet_wrap(~ season,
             nrow = 2,
             ncol = 3) +
  theme(
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    strip.background = element_rect(fill = "navy", color = "black", size = 1),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 14, hjust = 0.5),
    axis.text.y = element_text(size = 14, hjust = 0.5),
    legend.position = "bottom",
    plot.caption = element_text(size = 10)
  )

### vertical by horizontal movement plot for slurves, sweepers, curveballs, and sliders
statcast_pitch_summary_stats_q |> 
  filter(pitch_name %in% c("Sweeper", "Slider", "Curveball", "Slurve")) |> 
  # for pitcher's POV
  mutate(horizontal_break = -horizontal_break,
         season = factor(season, levels = c(2020, 2021, 2022, 2023, 2024))) |> 
  # plot
  ggplot(aes(horizontal_break, induced_vertical_break, color = pitch_name)) +
  geom_point(size = 2, alpha = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  scale_color_manual(values = c("firebrick", "dodgerblue", "goldenrod1", "forestgreen")) +
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     limits = c(-25, 25),
                     labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     limits = c(-25, 25),
                     labels = scales::number_format(suffix = "\"")) +
  labs(
    x = "Horizontal Break",
    y = "Induced Vertical Break",
    color = "Pitch Name",
    title = "Avg. Pitch Movement",
    subtitle = "2020-24 | Pitcher's POV",
    caption = "Data: Baseball Savant via baseballr"
  ) + 
  facet_wrap(~ season,
             nrow = 2,
             ncol = 3) +
  theme(
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    strip.background = element_rect(fill = "navy", color = "black", size = 1),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 14, hjust = 0.5),
    axis.text.y = element_text(size = 14, hjust = 0.5),
    legend.position = "bottom",
    plot.caption = element_text(size = 10)
  )

### vertical by horizontal movement plot for 4-seam fastball
statcast_pitch_summary_stats |> 
  # for pitcher's POV
  mutate(horizontal_break = -horizontal_break) |> 
  filter(pitch_name == "4-Seam Fastball" & n_pitches >= 100) |> 
  # plot
  ggplot(aes(horizontal_break, induced_vertical_break, color = season)) +
  geom_point(size = 2, alpha = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  scale_y_continuous(labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(labels = scales::number_format(suffix = "\"")) +
  ggthemes::scale_color_pander() +
  labs(
    x = "Horizontal Break",
    y = "Induced Vertical Break",
    color = "Pitch Name",
    title = "Avg. Four-Seam Fastball Movement",
    subtitle = "2020-24 | Pitcher's POV",
    caption = "Data: Baseball Savant via baseballr"
  ) + 
  theme(
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    strip.background = element_rect(fill = "navy", color = "black", size = 1),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 14, hjust = 0.5),
    axis.text.y = element_text(size = 14, hjust = 0.5),
    legend.position = "bottom",
    plot.caption = element_text(size = 10)
  ) #+
#geom_smooth(se = FALSE)
