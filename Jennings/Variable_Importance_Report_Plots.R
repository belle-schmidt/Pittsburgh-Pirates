## Random Forests for Stuff+, xwOBA, and Whiff%

## libraries
library(tidyverse)
library(ranger) # faster implementation of randomforest library
library(vip) # variable importance

## theme set
theme_set(theme_bw())

## data
pitching_models <- read.csv("mlb_pitching_stats_2020-24.csv")

# Stuff+ Random Forests

## 2021 Random Forests Stuff+

## filter
ff_models_21 <- pitching_models |> 
  filter(
    pitch_name == "4-Seam Fastball" &
      !is.na(stuff_plus) & 
      season == 2021
  ) |> 
  mutate(horizontal_break = -horizontal_break) |> 
  select(spin, velocity, extension:horizontal_break, stuff_plus, xwOBA, whiff_pct)

## final model
### set seed
set.seed(0829)

### fit model
sp_rf_final_21 <- ranger(
  stuff_plus ~  . - xwOBA - whiff_pct,
  num.trees = 1000,
  importance = "impurity",
  mtry = 2,
  splitrule = "variance",
  min.node.size = 5,
  data = ff_models_21
)

## 2022 Random Forests Stuff+

## filter
ff_models_22 <- pitching_models |> 
  filter(
    pitch_name == "4-Seam Fastball" &
      !is.na(stuff_plus) & 
      season == 2022
  ) |> 
  mutate(horizontal_break = -horizontal_break) |> 
  select(spin, velocity, extension:horizontal_break, stuff_plus, xwOBA, whiff_pct)

## final model
### set seed
set.seed(0829)

### fit model
sp_rf_final_22 <- ranger(
  stuff_plus ~  . - xwOBA - whiff_pct,
  num.trees = 1000,
  importance = "impurity",
  mtry = 2,
  splitrule = "variance",
  min.node.size = 5,
  data = ff_models_22
)

## 2023 Random Forests Stuff+

## filter
ff_models_23 <- pitching_models |> 
  filter(
    pitch_name == "4-Seam Fastball" &
      !is.na(stuff_plus) & 
      season == 2023
  ) |> 
  mutate(horizontal_break = -horizontal_break) |> 
  select(spin, velocity, extension:horizontal_break, stuff_plus, xwOBA, whiff_pct)

## final model
### set seed
set.seed(0829)

### fit model
sp_rf_final_23 <- ranger(
  stuff_plus ~  . - xwOBA - whiff_pct,
  num.trees = 1000,
  importance = "impurity",
  mtry = 2,
  splitrule = "variance",
  min.node.size = 5,
  data = ff_models_23
)


# xwOBA Random Forests
## 2021 Random Forests xwOBA

## final model
### set seed
set.seed(0829)

### fit model
xwOBA_rf_final_21 <- ranger(
  xwOBA ~ . - stuff_plus - whiff_pct,,
  num.trees = 1000,
  importance = "impurity",
  mtry = 2,
  splitrule = "variance",
  min.node.size = 5,
  data = ff_models_21
)

## 2022 Random Forests xwOBA

## final model
### set seed
set.seed(0829)

### fit model
xwOBA_rf_final_22 <- ranger(
  xwOBA ~ . - stuff_plus - whiff_pct,,
  num.trees = 1000,
  importance = "impurity",
  mtry = 2,
  splitrule = "variance",
  min.node.size = 5,
  data = ff_models_22
)

## 2023 Random Forests xwOBA

## final model
### set seed
set.seed(0829)

### fit model
xwOBA_rf_final_23 <- ranger(
  xwOBA ~ . - stuff_plus - whiff_pct,,
  num.trees = 1000,
  importance = "impurity",
  mtry = 2,
  splitrule = "variance",
  min.node.size = 5,
  data = ff_models_23
)


# whiff% Random Forests
## 2021 Random Forests whiff%

## final model
### set seed
set.seed(0829)

### fit model
whiff_pct_rf_final_21 <- ranger(
  whiff_pct ~ . - stuff_plus - xwOBA,
  num.trees = 1000,
  importance = "impurity",
  mtry = 2,
  splitrule = "variance",
  min.node.size = 5,
  data = ff_models_21
)


## 2022 Random Forests Whiff%

## final model
### set seed
set.seed(0829)

### fit model
whiff_pct_rf_final_22 <- ranger(
  whiff_pct ~ . - stuff_plus - xwOBA,
  num.trees = 1000,
  importance = "impurity",
  mtry = 2,
  splitrule = "variance",
  min.node.size = 5,
  data = ff_models_22
)


## 2023 Random Forests Whiff%

## final model
### set seed
set.seed(0829)

### fit model
whiff_pct_rf_final_23 <- ranger(
  whiff_pct ~ . - stuff_plus - xwOBA,
  num.trees = 1000,
  importance = "impurity",
  mtry = 2,
  splitrule = "variance",
  min.node.size = 5,
  data = ff_models_23
)


## Importance plots
### Combine into one dataset
rf_importance <- tibble(
  season = c(rep(2021, 5), rep(2022, 5), rep(2023, 5)),
  variable = rep(names(sp_rf_final_21$variable.importance), 3),
  sp_importance = c(
    (
      (as.vector(unlist(sp_rf_final_21$variable.importance)) - min(as.vector(unlist(sp_rf_final_21$variable.importance)))) /
        (max(as.vector(unlist(sp_rf_final_21$variable.importance))) - min(as.vector(unlist(sp_rf_final_21$variable.importance))))
    ),
    (
      (as.vector(unlist(sp_rf_final_22$variable.importance)) - min(as.vector(unlist(sp_rf_final_22$variable.importance)))) /
        (max(as.vector(unlist(sp_rf_final_22$variable.importance))) - min(as.vector(unlist(sp_rf_final_22$variable.importance))))
    ),
    (
      (as.vector(unlist(sp_rf_final_23$variable.importance)) - min(as.vector(unlist(sp_rf_final_23$variable.importance)))) /
        (max(as.vector(unlist(sp_rf_final_23$variable.importance))) - min(as.vector(unlist(sp_rf_final_23$variable.importance))))
    )
  ),
  xwOBA_importance = c(
    (
      (as.vector(unlist(xwOBA_rf_final_21$variable.importance)) - min(as.vector(unlist(xwOBA_rf_final_21$variable.importance)))) /
        (max(as.vector(unlist(xwOBA_rf_final_21$variable.importance))) - min(as.vector(unlist(xwOBA_rf_final_21$variable.importance))))
    ),
    (
      (as.vector(unlist(xwOBA_rf_final_22$variable.importance)) - min(as.vector(unlist(xwOBA_rf_final_22$variable.importance)))) /
        (max(as.vector(unlist(xwOBA_rf_final_22$variable.importance))) - min(as.vector(unlist(xwOBA_rf_final_22$variable.importance))))
    ),
    (
      (as.vector(unlist(xwOBA_rf_final_23$variable.importance)) - min(as.vector(unlist(xwOBA_rf_final_23$variable.importance)))) /
        (max(as.vector(unlist(xwOBA_rf_final_23$variable.importance))) - min(as.vector(unlist(xwOBA_rf_final_23$variable.importance))))
    )
  ),
  whiff_importance = c(
    (
      (as.vector(unlist(whiff_pct_rf_final_21$variable.importance)) - min(as.vector(unlist(whiff_pct_rf_final_21$variable.importance)))) /
        (max(as.vector(unlist(whiff_pct_rf_final_21$variable.importance))) - min(as.vector(unlist(whiff_pct_rf_final_21$variable.importance))))
    ),
    (
      (as.vector(unlist(whiff_pct_rf_final_22$variable.importance)) - min(as.vector(unlist(whiff_pct_rf_final_22$variable.importance)))) /
        (max(as.vector(unlist(whiff_pct_rf_final_22$variable.importance))) - min(as.vector(unlist(whiff_pct_rf_final_22$variable.importance))))
    ),
    (
      (as.vector(unlist(whiff_pct_rf_final_23$variable.importance)) - min(as.vector(unlist(whiff_pct_rf_final_23$variable.importance)))) /
        (max(as.vector(unlist(whiff_pct_rf_final_23$variable.importance))) - min(as.vector(unlist(whiff_pct_rf_final_23$variable.importance))))
    )
  )
  ) |> 
  mutate(
    season = factor(season),
    variable = recode(
      variable,
      "extension" = "Extension",
      "horizontal_break" = "Horizontal Break",
      "induced_vertical_break" = "Induced Vertical Break",
      "spin" = "Spin",
      "velocity" = "Velocity"
    )
  )

## Plots
### Stuff+
rf_importance |> 
  ggplot(aes(season, sp_importance, group = variable, color = variable)) +
  geom_point(alpha = 0.75) +
  geom_line(
    linewidth = 1.25
  ) +
  ggthemes::scale_color_pander() +
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
rf_importance |> 
  ggplot(aes(season, xwOBA_importance, group = variable, color = variable)) +
  geom_point(alpha = 0.75) +
  geom_line(
    linewidth = 1.25
  ) +
  ggthemes::scale_color_pander() +
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
    legend.text = element_text(size = 12)
  )

### whiff%
rf_importance |> 
  ggplot(aes(season, whiff_importance, group = variable, color = variable)) +
  geom_point(alpha = 0.75) +
  geom_line(
    linewidth = 1.25
  ) +
  ggthemes::scale_color_pander() +
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
    legend.text = element_text(size = 12)
  )

## correlations
## xwOBA
### 2021
cor(ff_models_21$stuff_plus, ff_models_21$xwOBA)

### 2022
cor(ff_models_22$stuff_plus, ff_models_22$xwOBA)

### 2023
cor(ff_models_23$stuff_plus, ff_models_23$xwOBA)

## whiff rate
### 2021
cor(ff_models_21$stuff_plus, ff_models_21$whiff_pct)

### 2022
cor(ff_models_22$stuff_plus, ff_models_22$whiff_pct)

### 2023
cor(ff_models_23$stuff_plus, ff_models_23$whiff_pct)

