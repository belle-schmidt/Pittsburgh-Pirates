## XGBoost Stuff+, xwOBA, and whiff rate models

## libraries
library(tidyverse)
library(ranger) # faster implementation of randomforest library
library(vip) # variable importance
library(pdp) # partial dependence plot
library(caret) # train random forest model with cross validation
library(broom) # tidy extract linear summary
library(cowplot) # join graphs together
library(xgboost) # xgboost models
library(caret)

## theme set
theme_set(theme_bw())

## data
pitching_models <- read.csv("mlb_pitching_stats_2021-24.csv")


## XGBoost Models

### create datasets for each year
stuff_plus_21 <- pitching_models |> 
  filter(season == 2021)


stuff_plus_22 <- pitching_models |> 
  filter(season == 2022)


stuff_plus_23 <- pitching_models |> 
  filter(season == 2023)


stuff_plus_24 <- pitching_models |> 
  filter(season == 2024)


### data splitting
features_21 <- stuff_plus_21 |> 
  select(
    velocity, spin, extension, induced_vertical_break, horizontal_break, 
    horizontal_release_point, vertical_release_point, spin_axis,
    diff_velocity, diff_horizontal_break, diff_induced_vertical_break
  ) |> 
  as.matrix()


features_22 <- stuff_plus_22 |> 
  select(
    velocity, spin, extension, induced_vertical_break, horizontal_break, 
    horizontal_release_point, vertical_release_point, spin_axis,
    diff_velocity, diff_horizontal_break, diff_induced_vertical_break
  ) |> 
  as.matrix()


features_23 <- stuff_plus_23 |> 
  select(
    velocity, spin, extension, induced_vertical_break, horizontal_break, 
    horizontal_release_point, vertical_release_point, spin_axis,
    diff_velocity, diff_horizontal_break, diff_induced_vertical_break
  ) |> 
  as.matrix()


features_24 <- stuff_plus_24 |> 
  select(
    velocity, spin, extension, induced_vertical_break, horizontal_break, 
    horizontal_release_point, vertical_release_point, spin_axis,
    diff_velocity, diff_horizontal_break, diff_induced_vertical_break
  ) |> 
  as.matrix()


### 2021 Stuff+

## set seed
set.seed(0829)

## tuning XGBoost
### hyperparameter grid
xg_grid <- crossing(
  nrounds = seq(100, 400, 10),
  eta = c(0.01, 0.05, 0.10, 0.20, 0.25), # learning rate
  gamma = 0,
  max_depth = 2:5, 
  colsample_bytree = 1, 
  min_child_weight = 1, 
  subsample = 1
)

### tuning
xg_tune <- train(
  x = features_21,
  y = stuff_plus_21$run_value, 
  tuneGrid = xg_grid,
  trControl = trainControl(method = "cv", number = 5),
  objective = "reg:squarederror", 
  method = "xgbTree",
  verbosity = 0 # prevents ntree_limit warning
)


## model evaluation
### fit model to training data
xg_fit <- xgboost(
  data = features_21, 
  label = stuff_plus_21$run_value,
  objective = "reg:squarederror",
  nrounds = xg_tune$bestTune$nrounds,
  params = as.list(select(xg_tune$bestTune, -nrounds)),
  verbose = 0
)

### training error
stuff_plus_21 |> 
  mutate(pred = round(predict(xg_fit, newdata = features_21), 4)) |> 
  summarize(MSE = mean((run_value - pred) ^ 2))

### testing error
stuff_plus_22 |> 
  mutate(pred = round(predict(xg_fit, newdata = features_22), 4)) |> 
  summarize(MSE = mean((run_value - pred) ^ 2))


### variable importance
xg_fit |> 
  vip(
    aesthetics = list(
      color = "black",
      fill = "dodgerblue4"
    )
  )



### 2022 Stuff+

## set seed
set.seed(0829)

## tuning XGBoost
### hyperparameter grid
xg_grid <- crossing(
  nrounds = seq(100, 400, 10),
  eta = c(0.01, 0.05, 0.10, 0.20, 0.25), # learning rate
  gamma = 0,
  max_depth = 2:5, 
  colsample_bytree = 1, 
  min_child_weight = 1, 
  subsample = 1
)

### tuning
xg_tune <- train(
  x = features_22,
  y = stuff_plus_22$run_value, 
  tuneGrid = xg_grid,
  trControl = trainControl(method = "cv", number = 5),
  objective = "reg:squarederror", 
  method = "xgbTree",
  verbosity = 0 # prevents ntree_limit warning
)


## model evaluation
### fit model to training data
xg_fit <- xgboost(
  data = features_22, 
  label = stuff_plus_22$run_value,
  objective = "reg:squarederror",
  nrounds = xg_tune$bestTune$nrounds,
  params = as.list(select(xg_tune$bestTune, -nrounds)),
  verbose = 0
)

### training error
stuff_plus_22 |> 
  mutate(pred = round(predict(xg_fit, newdata = features_22), 4)) |> 
  summarize(MSE = mean((run_value - pred) ^ 2))

### testing error
stuff_plus_23 |> 
  mutate(pred = round(predict(xg_fit, newdata = features_23), 4)) |> 
  summarize(MSE = mean((run_value - pred) ^ 2))


### variable importance
xg_fit |> 
  vip(
    aesthetics = list(
      color = "black",
      fill = "dodgerblue4"
    )
  )






### 2021 xwOBA

## set seed
set.seed(0829)

## tuning XGBoost
### hyperparameter grid
xg_grid <- crossing(
  nrounds = seq(50, 300, 10),
  eta = c(0.01, 0.05, 0.10, 0.20, 0.25), # learning rate
  gamma = 0,
  max_depth = 2:5, 
  colsample_bytree = 1, 
  min_child_weight = 1, 
  subsample = 1
)

### tuning
xg_tune <- train(
  x = features_21,
  y = stuff_plus_21$xwOBA, 
  tuneGrid = xg_grid,
  trControl = trainControl(method = "cv", number = 5),
  objective = "reg:squarederror", 
  method = "xgbTree",
  verbosity = 0 # prevents ntree_limit warning
)


## model evaluation
### fit model to training data
xg_fit <- xgboost(
  data = features_21, 
  label = stuff_plus_21$xwOBA,
  objective = "reg:squarederror",
  nrounds = xg_tune$bestTune$nrounds,
  params = as.list(select(xg_tune$bestTune, -nrounds)),
  verbose = 0
)

### training error
stuff_plus_21 |> 
  mutate(pred = round(predict(xg_fit, newdata = features_21), 4)) |> 
  summarize(MSE = mean((xwOBA - pred) ^ 2))

### testing error
stuff_plus_22 |> 
  mutate(pred = round(predict(xg_fit, newdata = features_22), 4)) |> 
  summarize(MSE = mean((xwOBA - pred) ^ 2))


### variable importance
xg_fit |> 
  vip(
    aesthetics = list(
      color = "black",
      fill = "dodgerblue4"
    )
  )








### 2021 whiff rate

## set seed
set.seed(0829)

## tuning XGBoost
### hyperparameter grid
xg_grid <- crossing(
  nrounds = seq(100, 400, 10),
  eta = c(0.01, 0.05, 0.10, 0.20, 0.25), # learning rate
  gamma = 0,
  max_depth = 2:5, 
  colsample_bytree = 1, 
  min_child_weight = 1, 
  subsample = 1
)

### tuning
xg_tune <- train(
  x = features_21,
  y = stuff_plus_21$whiff_pct, 
  tuneGrid = xg_grid,
  trControl = trainControl(method = "cv", number = 5),
  objective = "reg:squarederror", 
  method = "xgbTree",
  verbosity = 0 # prevents ntree_limit warning
)


## model evaluation
### fit model to training data
xg_fit <- xgboost(
  data = features_21, 
  label = stuff_plus_21$whiff_pct,
  objective = "reg:squarederror",
  nrounds = xg_tune$bestTune$nrounds,
  params = as.list(select(xg_tune$bestTune, -nrounds)),
  verbose = 0
)

### training error
stuff_plus_21 |> 
  mutate(pred = round(predict(xg_fit, newdata = features_21), 4)) |> 
  summarize(MSE = mean((whiff_pct - pred) ^ 2))

### testing error
stuff_plus_22 |> 
  mutate(pred = round(predict(xg_fit, newdata = features_22), 4)) |> 
  summarize(MSE = mean((whiff_pct - pred) ^ 2))


### variable importance
xg_fit |> 
  vip(
    aesthetics = list(
      color = "black",
      fill = "dodgerblue4"
    )
  )






## random forest

### fit model
sp_rf <- ranger(
  run_value ~ velocity + spin + extension + induced_vertical_break + horizontal_break + 
  horizontal_release_point + vertical_release_point + spin_axis +
  diff_velocity + diff_horizontal_break + diff_induced_vertical_break,
  num.trees = 1000,
  importance = "impurity",
  mtry = 11 / 3,
  data = stuff_plus_21 |> drop_na(diff_velocity)
)


### model summary
sp_rf

### variable importance
sp_rf |> 
  vip(
    aesthetics = list(color = "black", 
                      fill = "dodgerblue4")
  )


stuff_plus_22 |> 
  drop_na(diff_velocity) |> 
  mutate(pred = round(predict(sp_rf, data = features_22_na)$predictions, 4)) |> 
  summarize(MSE = mean((run_value - pred) ^ 2))
