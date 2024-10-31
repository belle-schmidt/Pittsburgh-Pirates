## XGBoost Variable Importance Plots for Stuff+, xwOBA, and Whiff%

## libraries
library(tidyverse)

## theme set
theme_set(theme_bw())

## load in the data
xgboost_var_importance <- read.csv("xgboost_var_importance.csv") |> 
  # remove row number colum
  select(-1)


### color scheme
var_imp_color <- c(
  "Velocity Difference" = "#2774AE",
  "Induced Vertical Break" = "#990000",
  "Velocity" = "#FD9651",
  "Spin" = "#FFD520",
  "Extension" = "#E41C38",
  "Vertical Release Point" = "#D6B4FC",
  "Horizontal Break" = "#CFB991",
  "Horizontal Break Difference" = "#96BEE6",
  "Spin Axis" = "#DA5902",
  "Horizontal Release Point" = "#4D1979",
  "Induced Vertical Break Difference" = "#001E44"
)


### Stuff+
xgboost_var_importance |> 
  group_by(season) |> 
  top_n(
    5, 
    scaled_sp_importance
  ) |> 
  ggplot(aes(season, scaled_sp_importance, color = variable)) +
  geom_point(alpha = 0.5) +
  geom_line(
    linewidth = 1.25,
    alpha = 0.75
  ) +
  scale_color_manual(
    values = var_imp_color,
    limits = xgboost_var_importance |> 
      group_by(season) |> 
      top_n(
        5, 
        scaled_sp_importance
      ) |> 
      pull(variable) |> unique()
    ) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.25),
    limits = c(0, 1)
  ) +
  labs(
    x = "Season",
    y = "Importance",
    color = "Characteristic",
    title = "Stuff+ Variable Importance"
  ) +
  theme(
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
  )


### xwOBA
xgboost_var_importance |> 
  group_by(season) |> 
  top_n(
    5, 
    scaled_xwoba_importance
  ) |> 
  ggplot(aes(season, scaled_xwoba_importance, group = variable, color = variable)) +
  geom_point(alpha = 0.75) +
  geom_line(
    linewidth = 1.25,
    alpha = 0.75
  ) +
  scale_color_manual(
    values = var_imp_color,
    limits = xgboost_var_importance |> 
      group_by(season) |> 
      top_n(
        5, 
        scaled_xwoba_importance
      ) |> 
      pull(variable) |> unique()
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.25),
    limits = c(0, 1)
  ) +
  labs(
    x = "Season",
    y = "Importance",
    color = "Characteristic",
    title = "xwOBA Variable Importance"
  ) +
  theme(
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
  )


### Whiff%
xgboost_var_importance |> 
  group_by(season) |> 
  top_n(
    5, 
    scaled_whiff_pct_importance
  ) |> 
  ggplot(aes(season, scaled_whiff_pct_importance, group = variable, color = variable)) +
  geom_point(alpha = 0.75) +
  geom_line(
    linewidth = 1.25,
    alpha = 0.75
  ) +
  scale_color_manual(
    values = var_imp_color,
    limits = xgboost_var_importance |> 
      group_by(season) |> 
      top_n(
        5, 
        scaled_whiff_pct_importance
      ) |> 
      pull(variable) |> unique()
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.25),
    limits = c(0, 1)
  ) +
  labs(
    x = "Season",
    y = "Importance",
    color = "Characteristic",
    title = "Whiff% Variable Importance"
  ) +
  theme(
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
  )
