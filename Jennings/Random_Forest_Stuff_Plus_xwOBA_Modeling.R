## Random Forests for Stuff+ and xwOBA

## libraries
library(tidyverse)
library(ranger) # faster implementation of randomforest library
library(vip) # variable importance
library(pdp) # partial dependence plot
library(caret) # train random forest model with cross validation
library(broom) # tidy extract linear summary

## theme set
theme_set(theme_bw())

## data
pitching_models <- read.csv("mlb_pitching_stats_2020-24.csv")

# Stuff+ Random Forests
## 2020 Random Forests Stuff+
## final model
### set seed
set.seed(0829)

## filter
# ff_models_20 <- pitching_models |> 
#   filter(
#     pitch_name == "4-Seam Fastball" &
#       !is.na(stuff_plus) & 
#       season == 2020
#   ) |> 
#   mutate(horizontal_break = -horizontal_break) |> 
#   select(spin, velocity, extension:horizontal_break, stuff_plus, xwOBA, whiff_pct)
# 
# ### fit model
# sp_rf_final_20 <- ranger(
#   stuff_plus ~  . - xwOBA - whiff_pct,
#   num.trees = 1000,
#   importance = "impurity",
#   mtry = 2,
#   splitrule = "variance",
#   min.node.size = 5,
#   data = ff_models_20
# )
# 
# ## 2024 Random Forests Stuff+
# ## final model
# ### set seed
# set.seed(0829)
# 
# ## filter
# ff_models_24 <- pitching_models |> 
#   filter(
#     pitch_name == "4-Seam Fastball" &
#       !is.na(stuff_plus) & 
#       season == 2024
#   ) |> 
#   mutate(horizontal_break = -horizontal_break) |> 
#   select(spin, velocity, extension:horizontal_break, stuff_plus, xwOBA, whiff_pct)
# 
# ### fit model
# sp_rf_final_24 <- ranger(
#   stuff_plus ~  . - xwOBA - whiff_pct,
#   num.trees = 1000,
#   importance = "impurity",
#   mtry = 2,
#   splitrule = "variance",
#   min.node.size = 5,
#   data = ff_models_24
# )

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

## build random forest to predict stuff+ for 2021 season
### set seed
set.seed(0829)

### fit model
sp_rf <- ranger(
  stuff_plus ~  . - xwOBA - whiff_pct,
  num.trees = 1000,
  importance = "impurity",
  mtry = 5 / 3,
  data = ff_models_21
)


### model summary
sp_rf

### variable importance
sp_rf |> 
  vip(
    aesthetics = list(color = "black", 
                      fill = "dodgerblue4")
  )

## partial dependence plot
### horizontal break
# sp_rf |> 
#   partial(pred.var = "horizontal_break") |> 
#   autoplot()
# 
# ### vertical break
# sp_rf |> 
#   partial(pred.var = "induced_vertical_break") |> 
#   autoplot()


## random forest with 5-fold CV
### fit model
sp_rf_cv <- train(
  stuff_plus ~ . - xwOBA - whiff_pct,
  tuneLength = 3,
  data = ff_models_21,
  method = "ranger",
  trControl = trainControl(
    method = "cv",
    number = 5,
    verboseIter = TRUE
  )
)

### plot model
plot(sp_rf_cv)


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

### model summary
sp_rf_final_21

### RMSE
sqrt(sp_rf_final_21$prediction.error)

### variable importance
sp_rf_final_21 |> 
  vip(aesthetics = list(
    color = "black",
    fill = "dodgerblue4"
  ))


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

## build random forest to predict stuff+ for 2022 season
### set seed
set.seed(0829)

### fit model
sp_rf <- ranger(
  stuff_plus ~  . - xwOBA - whiff_pct,
  num.trees = 1000,
  importance = "impurity",
  mtry = 5 / 3,
  data = ff_models_22
)


### model summary
sp_rf

### variable importance
sp_rf |> 
  vip(
    aesthetics = list(color = "black", 
                      fill = "dodgerblue3")
  )

## partial dependence plot
### horizontal break
# sp_rf |> 
#   partial(pred.var = "horizontal_break") |> 
#   autoplot()
# 
# ### vertical break
# sp_rf |> 
#   partial(pred.var = "induced_vertical_break") |> 
#   autoplot()


## random forest with 5-fold CV
### fit model
sp_rf_cv <- train(
  stuff_plus ~ . - xwOBA - whiff_pct,
  tuneLength = 3,
  data = ff_models_22,
  method = "ranger",
  trControl = trainControl(
    method = "cv",
    number = 5,
    verboseIter = TRUE
  )
)

### plot model
plot(sp_rf_cv)


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

### model summary
sp_rf_final_22

### RMSE
sqrt(sp_rf_final_22$prediction.error)

### variable importance
sp_rf_final_22 |> 
  vip(aesthetics = list(
    color = "black",
    fill = "dodgerblue4"
  ))


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

## build random forest to predict stuff+ for 2022 season
### set seed
set.seed(0829)

### fit model
sp_rf <- ranger(
  stuff_plus ~  . - xwOBA - whiff_pct,
  num.trees = 1000,
  importance = "impurity",
  mtry = 5 / 3,
  data = ff_models_23
)


### model summary
sp_rf

### variable importance
sp_rf |> 
  vip(aesthetics = list(
    color = "black",
    fill = "dodgerblue4"
  ))

## partial dependence plot
### horizontal break
# sp_rf |> 
#   partial(pred.var = "horizontal_break") |> 
#   autoplot()
# 
# ### vertical break
# sp_rf |> 
#   partial(pred.var = "induced_vertical_break") |> 
#   autoplot()


## random forest with 5-fold CV
### fit model
sp_rf_cv <- train(
  stuff_plus ~ . - xwOBA - whiff_pct,
  tuneLength = 3,
  data = ff_models_23,
  method = "ranger",
  trControl = trainControl(
    method = "cv",
    number = 5,
    verboseIter = TRUE
  )
)

### plot model
plot(sp_rf_cv)


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

### model summary
sp_rf_final_23

### RMSE
sqrt(sp_rf_final_23$prediction.error)

### variable importance
sp_rf_final_23 |> 
  vip(aesthetics = list(
    color = "black",
    fill = "dodgerblue4"
  ))





# xwOBA Random Forests
## 2021 Random Forests xwOBA

## build random forest to predict xwOBA for 2021 season
### set seed
set.seed(0829)

### fit model
xwOBA_rf <- ranger(
  xwOBA ~ . - stuff_plus - whiff_pct,
  num.trees = 1000,
  importance = "impurity",
  mtry = 5 / 3,
  data = ff_models_21
)


### model summary
xwOBA_rf

### variable importance
xwOBA_rf |> 
  vip(
    aesthetics = list(color = "black", 
                      fill = "dodgerblue4")
  )

## partial dependence plot
### horizontal break
# xwOBA_rf |>
#   partial(pred.var = "horizontal_break") |>
#   autoplot()
# 
# ### vertical break
# xwOBA_rf |> 
#   partial(pred.var = "induced_vertical_break") |> 
#   autoplot()


## random forest with 5-fold CV
### fit model
xwOBA_rf_cv <- train(
  xwOBA ~ . - stuff_plus - whiff_pct,
  tuneLength = 3,
  data = ff_models_21,
  method = "ranger",
  trControl = trainControl(
    method = "cv",
    number = 5,
    verboseIter = TRUE
  )
)

### plot model
plot(xwOBA_rf_cv)


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

### model summary
xwOBA_rf_final_21

### RMSE
sqrt(xwOBA_rf_final_21$prediction.error)

### variable importance
xwOBA_rf_final_21 |> 
  vip(aesthetics = list(
    color = "black",
    fill = "dodgerblue4"
  ))


## 2022 Random Forests xwOBA

## build random forest to predict stuff+ for 2022 season
### set seed
set.seed(0829)

### fit model
xwOBA_rf <- ranger(
  xwOBA ~ . - stuff_plus - whiff_pct,,
  num.trees = 1000,
  importance = "impurity",
  mtry = 5 / 3,
  data = ff_models_22
)


### model summary
xwOBA_rf

### variable importance
xwOBA_rf |> 
  vip(
    aesthetics = list(color = "black", 
                      fill = "dodgerblue4")
  )

## partial dependence plot
### horizontal break
# xwOBA_rf |> 
#   partial(pred.var = "horizontal_break") |> 
#   autoplot()
# 
# ### vertical break
# xwOBA_rf |> 
#   partial(pred.var = "induced_vertical_break") |> 
#   autoplot()


## random forest with 5-fold CV
### fit model
xwOBA_rf_cv <- train(
  stuff_plus ~ . - xwOBA - whiff_pct,
  tuneLength = 3,
  data = ff_models_22,
  method = "ranger",
  trControl = trainControl(
    method = "cv",
    number = 5,
    verboseIter = TRUE
  )
)

### plot model
plot(xwOBA_rf_cv)


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

### model summary
xwOBA_rf_final_22

### RMSE
sqrt(xwOBA_rf_final_22$prediction.error)

### variable importance
xwOBA_rf_final_22 |> 
  vip(aesthetics = list(
    color = "black",
    fill = "dodgerblue4"
  ))


## 2023 Random Forests Stuff+

## build random forest to predict stuff+ for 2022 season
### set seed
set.seed(0829)

### fit model
xwOBA_rf <- ranger(
  xwOBA ~ . - stuff_plus - whiff_pct,,
  num.trees = 1000,
  importance = "impurity",
  mtry = 5 / 3,
  data = ff_models_23
)


### model summary
xwOBA_rf

### variable importance
xwOBA_rf |> 
  vip(aesthetics = list(
    color = "black",
    fill = "dodgerblue4"
  ))

## partial dependence plot
### horizontal break
# xwOBA_rf |> 
#   partial(pred.var = "horizontal_break") |> 
#   autoplot()
# 
# ### vertical break
# xwOBA_rf |> 
#   partial(pred.var = "induced_vertical_break") |> 
#   autoplot()


## random forest with 5-fold CV
### fit model
xwOBA_rf_cv <- train(
  stuff_plus ~ . - xwOBA - whiff_pct,
  tuneLength = 3,
  data = ff_models_23,
  method = "ranger",
  trControl = trainControl(
    method = "cv",
    number = 5,
    verboseIter = TRUE
  )
)

### plot model
plot(xwOBA_rf_cv)


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

### model summary
xwOBA_rf_final_23

### RMSE
sqrt(xwOBA_rf_final_23$prediction.error)

### variable importance
xwOBA_rf_final_23 |> 
  vip(aesthetics = list(
    color = "black",
    fill = "dodgerblue4"
  ))


# whiff% Random Forests
## 2021 Random Forests whiff%

## build random forest to predict whiff% for 2021 season
### set seed
set.seed(0829)

### fit model
whiff_pct_rf <- ranger(
  whiff_pct ~ . - stuff_plus - xwOBA,
  num.trees = 1000,
  importance = "impurity",
  mtry = 5 / 3,
  data = ff_models_21
)


### model summary
whiff_pct_rf

### variable importance
whiff_pct_rf |> 
  vip(
    aesthetics = list(color = "black", 
                      fill = "dodgerblue4")
  )

## partial dependence plot
### horizontal break
# whiff_pct_rf |>
#   partial(pred.var = "horizontal_break") |>
#   autoplot()
# 
# ### vertical break
# whiff_pct_rf |> 
#   partial(pred.var = "induced_vertical_break") |> 
#   autoplot()


## random forest with 5-fold CV
### fit model
whiff_pct_rf_cv <- train(
  whiff_pct ~ . - stuff_plus - whiff_pct,
  tuneLength = 3,
  data = ff_models_21,
  method = "ranger",
  trControl = trainControl(
    method = "cv",
    number = 5,
    verboseIter = TRUE
  )
)

### plot model
plot(whiff_pct_rf_cv)


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

### model summary
whiff_pct_rf_final_21

### RMSE
sqrt(whiff_pct_rf_final_21$prediction.error)

### variable importance
whiff_pct_rf_final_21 |> 
  vip(aesthetics = list(
    color = "black",
    fill = "dodgerblue4"
  ))


## 2022 Random Forests whiff_pct

## build random forest to predict stuff+ for 2022 season
### set seed
set.seed(0829)

### fit model
whiff_pct_rf <- ranger(
  whiff_pct ~ . - stuff_plus - xwOBA,
  num.trees = 1000,
  importance = "impurity",
  mtry = 5 / 3,
  data = ff_models_22
)


### model summary
whiff_pct_rf

### variable importance
whiff_pct_rf |> 
  vip(
    aesthetics = list(color = "black", 
                      fill = "dodgerblue4")
  )

## partial dependence plot
### horizontal break
# whiff_pct_rf |> 
#   partial(pred.var = "horizontal_break") |> 
#   autoplot()
# 
# ### vertical break
# whiff_pct_rf |> 
#   partial(pred.var = "induced_vertical_break") |> 
#   autoplot()


## random forest with 5-fold CV
### fit model
whiff_pct_rf_cv <- train(
  stuff_plus ~ . - whiff_pct - whiff_pct,
  tuneLength = 3,
  data = ff_models_22,
  method = "ranger",
  trControl = trainControl(
    method = "cv",
    number = 5,
    verboseIter = TRUE
  )
)

### plot model
plot(whiff_pct_rf_cv)


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

### model summary
whiff_pct_rf_final_22

### RMSE
sqrt(whiff_pct_rf_final_22$prediction.error)

### variable importance
whiff_pct_rf_final_22 |> 
  vip(aesthetics = list(
    color = "black",
    fill = "dodgerblue4"
  ))


## 2023 Random Forests Stuff+

## build random forest to predict stuff+ for 2022 season
### set seed
set.seed(0829)

### fit model
whiff_pct_rf <- ranger(
  whiff_pct ~ . - stuff_plus - xwOBA,
  num.trees = 1000,
  importance = "impurity",
  mtry = 5 / 3,
  data = ff_models_23
)


### model summary
whiff_pct_rf

### variable importance
whiff_pct_rf |> 
  vip(aesthetics = list(
    color = "black",
    fill = "dodgerblue4"
  ))

## partial dependence plot
### horizontal break
# whiff_pct_rf |> 
#   partial(pred.var = "horizontal_break") |> 
#   autoplot()
# 
# ### vertical break
# whiff_pct_rf |> 
#   partial(pred.var = "induced_vertical_break") |> 
#   autoplot()


## random forest with 5-fold CV
### fit model
whiff_pct_rf_cv <- train(
  stuff_plus ~ . - whiff_pct - whiff_pct,
  tuneLength = 3,
  data = ff_models_23,
  method = "ranger",
  trControl = trainControl(
    method = "cv",
    number = 5,
    verboseIter = TRUE
  )
)

### plot model
plot(whiff_pct_rf_cv)


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

### model summary
whiff_pct_rf_final_23

### RMSE
sqrt(whiff_pct_rf_final_23$prediction.error)

### variable importance
whiff_pct_rf_final_23 |> 
  vip(aesthetics = list(
    color = "black",
    fill = "dodgerblue4"
  ))


## Importance plots
### Stuff+
### Combine into one dataset
rf_importance <- tibble(
  season = c(rep(2021, 5), rep(2022, 5), rep(2023, 5)),
  variable = rep(names(sp_rf_final_21$variable.importance), 3),
  sp_importance = c(
    as.vector(unlist(sp_rf_final_21$variable.importance)),
    as.vector(unlist(sp_rf_final_22$variable.importance)),
    as.vector(unlist(sp_rf_final_23$variable.importance))
    ),
  xwOBA_importance = c(
    as.vector(unlist(xwOBA_rf_final_21$variable.importance)),
    as.vector(unlist(xwOBA_rf_final_22$variable.importance)),
    as.vector(unlist(xwOBA_rf_final_23$variable.importance))),
  whiff_importance = c(
    as.vector(unlist(whiff_pct_rf_final_21$variable.importance)),
    as.vector(unlist(whiff_pct_rf_final_22$variable.importance)),
    as.vector(unlist(whiff_pct_rf_final_23$variable.importance))
  ),) |> 
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


# plot
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
    color = "Characteristic"
  ) +
  theme(
    legend.position = "bottom"
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
    color = "Characteristic"
  ) +
  theme(
    legend.position = "bottom"
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
    color = "Characteristic"
  ) +
  theme(
    legend.position = "bottom"
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
