## XGBoost Stuff+, xwOBA, and whiff rate models

## libraries
library(tidyverse)
library(ranger) # faster implementation of randomforest library
library(vip) # variable importance
library(pdp) # partial dependence plot
library(caret) # train random forest model with cross validation
library(broom) # tidy extract linear summary
#library(cowplot) # join graphs together
library(xgboost) # xgboost models
library(caret)

## theme set
theme_set(theme_bw())

## data
pitching_models <- read.csv("mlb_pitching_stats_2021-24.csv")


## XGBoost Models


# Data Splitting ----------------------------------------------------------

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




# Stuff+ XGBoost Models ---------------------------------------------------

### 2021 Stuff+

## set seed
set.seed(0829)

## tuning XGBoost
### hyperparameter grid
xg_grid_sp_21 <- crossing(
  nrounds = seq(100, 400, 10),
  eta = c(0.01, 0.05, 0.10, 0.20, 0.25), # learning rate
  gamma = 0,
  max_depth = 2:5, 
  colsample_bytree = 1, 
  min_child_weight = 1, 
  subsample = 1
)

### tuning
xg_tune_sp_21 <- train(
  x = features_21,
  y = stuff_plus_21$run_value, 
  tuneGrid = xg_grid_sp_21,
  trControl = trainControl(method = "cv", number = 5),
  objective = "reg:squarederror", 
  method = "xgbTree",
  verbosity = 0 # prevents ntree_limit warning
)


## model evaluation
### fit model to training data
xg_fit_sp_21 <- xgboost(
  data = features_21, 
  label = stuff_plus_21$run_value,
  objective = "reg:squarederror",
  nrounds = xg_tune_sp_21$bestTune$nrounds,
  params = as.list(select(xg_tune_sp_21$bestTune, -nrounds)),
  verbose = 0
)

### training error
stuff_plus_21 |> 
  mutate(pred = round(predict(xg_fit_sp_21, newdata = features_21), 4)) |> 
  summarize(MSE = mean((run_value - pred) ^ 2))

### testing error
stuff_plus_22 |> 
  mutate(pred = round(predict(xg_fit_sp_21, newdata = features_22), 4)) |> 
  summarize(MSE = mean((run_value - pred) ^ 2))


### variable importance
xg_fit_sp_21 |> 
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
xg_grid_sp_22 <- crossing(
  nrounds = seq(100, 400, 10),
  eta = c(0.01, 0.05, 0.10, 0.20, 0.25), # learning rate
  gamma = 0,
  max_depth = 2:5, 
  colsample_bytree = 1, 
  min_child_weight = 1, 
  subsample = 1
)

### tuning
xg_tune_sp_22 <- train(
  x = features_22,
  y = stuff_plus_22$run_value, 
  tuneGrid = xg_grid_sp_22,
  trControl = trainControl(method = "cv", number = 5),
  objective = "reg:squarederror", 
  method = "xgbTree",
  verbosity = 0 # prevents ntree_limit warning
)


## model evaluation
### fit model to training data
xg_fit_sp_22 <- xgboost(
  data = features_22, 
  label = stuff_plus_22$run_value,
  objective = "reg:squarederror",
  nrounds = xg_tune_sp_22$bestTune$nrounds,
  params = as.list(select(xg_tune_sp_22$bestTune, -nrounds)),
  verbose = 0
)

### training error
stuff_plus_22 |> 
  mutate(pred = round(predict(xg_fit_sp_22, newdata = features_22), 4)) |> 
  summarize(MSE = mean((run_value - pred) ^ 2))

### testing error
stuff_plus_23 |> 
  mutate(pred = round(predict(xg_fit_sp_22, newdata = features_23), 4)) |> 
  summarize(MSE = mean((run_value - pred) ^ 2))


### variable importance
xg_fit_sp_22 |> 
  vip(
    aesthetics = list(
      color = "black",
      fill = "dodgerblue4"
    )
  )


### 2023 Stuff+

## set seed
set.seed(0829)

## tuning XGBoost
### hyperparameter grid
xg_grid_sp_23 <- crossing(
  nrounds = seq(100, 400, 10),
  eta = c(0.01, 0.05, 0.10, 0.20, 0.25), # learning rate
  gamma = 0,
  max_depth = 2:5, 
  colsample_bytree = 1, 
  min_child_weight = 1, 
  subsample = 1
)

### tuning
xg_tune_sp_23 <- train(
  x = features_23,
  y = stuff_plus_23$run_value, 
  tuneGrid = xg_grid_sp_23,
  trControl = trainControl(method = "cv", number = 5),
  objective = "reg:squarederror", 
  method = "xgbTree",
  verbosity = 0 # prevents ntree_limit warning
)


## model evaluation
### fit model to training data
xg_fit_sp_23 <- xgboost(
  data = features_23, 
  label = stuff_plus_23$run_value,
  objective = "reg:squarederror",
  nrounds = xg_tune_sp_23$bestTune$nrounds,
  params = as.list(select(xg_tune_sp_23$bestTune, -nrounds)),
  verbose = 0
)

### training error
stuff_plus_23 |> 
  mutate(pred = round(predict(xg_fit_sp_23, newdata = features_23), 4)) |> 
  summarize(MSE = mean((run_value - pred) ^ 2))

### testing error
stuff_plus_24 |> 
  mutate(pred = round(predict(xg_fit_sp_23, newdata = features_24), 4)) |> 
  summarize(MSE = mean((run_value - pred) ^ 2))


### variable importance
xg_fit_sp_23 |> 
  vip(
    aesthetics = list(
      color = "black",
      fill = "dodgerblue4"
    )
  )


### 2024 Stuff+

## set seed
set.seed(0829)

## tuning XGBoost
### hyperparameter grid
xg_grid_sp_24 <- crossing(
  nrounds = seq(100, 400, 10),
  eta = c(0.01, 0.05, 0.10, 0.20, 0.25), # learning rate
  gamma = 0,
  max_depth = 2:5, 
  colsample_bytree = 1, 
  min_child_weight = 1, 
  subsample = 1
)

### tuning
xg_tune_sp_24 <- train(
  x = features_24,
  y = stuff_plus_24$run_value, 
  tuneGrid = xg_grid_sp_24,
  trControl = trainControl(method = "cv", number = 5),
  objective = "reg:squarederror", 
  method = "xgbTree",
  verbosity = 0 # prevents ntree_limit warning
)


## model evaluation
### fit model to training data
xg_fit_sp_24 <- xgboost(
  data = features_24, 
  label = stuff_plus_24$run_value,
  objective = "reg:squarederror",
  nrounds = xg_tune_sp_24$bestTune$nrounds,
  params = as.list(select(xg_tune_sp_24$bestTune, -nrounds)),
  verbose = 0
)

### training error
stuff_plus_24 |> 
  mutate(pred = round(predict(xg_fit_sp_24, newdata = features_24), 4)) |> 
  summarize(MSE = mean((run_value - pred) ^ 2))

### testing error
# stuff_plus_24 |> 
#   mutate(pred = round(predict(xg_fit_sp_23, newdata = features_24), 4)) |> 
#   summarize(MSE = mean((run_value - pred) ^ 2))


### variable importance
xg_fit_sp_24 |> 
  vip(
    aesthetics = list(
      color = "black",
      fill = "dodgerblue4"
    )
  )




# xwOBA XGBoost Models ----------------------------------------------------

### 2021 xwOBA

## set seed
set.seed(0829)

## tuning XGBoost
### hyperparameter grid
xg_grid_xwoba_21 <- crossing(
  nrounds = seq(100, 400, 10),
  eta = c(0.01, 0.05, 0.10, 0.20, 0.25), # learning rate
  gamma = 0,
  max_depth = 2:5, 
  colsample_bytree = 1, 
  min_child_weight = 1, 
  subsample = 1
)

### tuning
xg_tune_xwoba_21 <- train(
  x = features_21,
  y = stuff_plus_21$xwOBA, 
  tuneGrid = xg_grid_xwoba_21,
  trControl = trainControl(method = "cv", number = 5),
  objective = "reg:squarederror", 
  method = "xgbTree",
  verbosity = 0 # prevents ntree_limit warning
)


## model evaluation
### fit model to training data
xg_fit_xwoba_21 <- xgboost(
  data = features_21, 
  label = stuff_plus_21$xwOBA,
  objective = "reg:squarederror",
  nrounds = xg_tune_xwoba_21$bestTune$nrounds,
  params = as.list(select(xg_tune_xwoba_21$bestTune, -nrounds)),
  verbose = 0
)

### training error
stuff_plus_21 |> 
  mutate(pred = round(predict(xg_fit_xwoba_21, newdata = features_21), 4)) |> 
  summarize(MSE = mean((xwOBA - pred) ^ 2))

### testing error
stuff_plus_22 |> 
  mutate(pred = round(predict(xg_fit_xwoba_21, newdata = features_22), 4)) |> 
  summarize(MSE = mean((xwOBA - pred) ^ 2))


### variable importance
xg_fit_xwoba_21 |> 
  vip(
    aesthetics = list(
      color = "black",
      fill = "goldenrod"
    )
  )


### 2022 xwOBA

## set seed
set.seed(0829)

## tuning XGBoost
### hyperparameter grid
xg_grid_xwoba_22 <- crossing(
  nrounds = seq(50, 400, 10),
  eta = c(0.01, 0.05, 0.10, 0.20, 0.25), # learning rate
  gamma = 0,
  max_depth = 2:5, 
  colsample_bytree = 1, 
  min_child_weight = 1, 
  subsample = 1
)

### tuning
xg_tune_xwoba_22 <- train(
  x = features_22,
  y = stuff_plus_22$xwOBA, 
  tuneGrid = xg_grid_xwoba_22,
  trControl = trainControl(method = "cv", number = 5),
  objective = "reg:squarederror", 
  method = "xgbTree",
  verbosity = 0 # prevents ntree_limit warning
)


## model evaluation
### fit model to training data
xg_fit_xwoba_22 <- xgboost(
  data = features_22, 
  label = stuff_plus_22$xwOBA,
  objective = "reg:squarederror",
  nrounds = xg_tune_xwoba_22$bestTune$nrounds,
  params = as.list(select(xg_tune_xwoba_22$bestTune, -nrounds)),
  verbose = 0
)

### training error
stuff_plus_22 |> 
  mutate(pred = round(predict(xg_fit_xwoba_22, newdata = features_22), 4)) |> 
  summarize(MSE = mean((xwOBA - pred) ^ 2))

### testing error
stuff_plus_23 |> 
  mutate(pred = round(predict(xg_fit_xwoba_22, newdata = features_23), 4)) |> 
  summarize(MSE = mean((xwOBA - pred) ^ 2))


### variable importance
xg_fit_xwoba_22 |> 
  vip(
    aesthetics = list(
      color = "black",
      fill = "goldenrod"
    )
  )


### 2023 xwOBA

## set seed
set.seed(0829)

## tuning XGBoost
### hyperparameter grid
xg_grid_xwoba_23 <- crossing(
  nrounds = seq(100, 400, 10),
  eta = c(0.01, 0.05, 0.10, 0.20, 0.25), # learning rate
  gamma = 0,
  max_depth = 2:5, 
  colsample_bytree = 1, 
  min_child_weight = 1, 
  subsample = 1
)

### tuning
xg_tune_xwoba_23 <- train(
  x = features_23,
  y = stuff_plus_23$xwOBA, 
  tuneGrid = xg_grid_xwoba_23,
  trControl = trainControl(method = "cv", number = 5),
  objective = "reg:squarederror", 
  method = "xgbTree",
  verbosity = 0 # prevents ntree_limit warning
)


## model evaluation
### fit model to training data
xg_fit_xwoba_23 <- xgboost(
  data = features_23, 
  label = stuff_plus_23$xwOBA,
  objective = "reg:squarederror",
  nrounds = xg_tune_xwoba_23$bestTune$nrounds,
  params = as.list(select(xg_tune_xwoba_23$bestTune, -nrounds)),
  verbose = 0
)

### training error
stuff_plus_23 |> 
  mutate(pred = round(predict(xg_fit_xwoba_23, newdata = features_23), 4)) |> 
  summarize(MSE = mean((xwOBA - pred) ^ 2))

### testing error
stuff_plus_24 |> 
  mutate(pred = round(predict(xg_fit_xwoba_23, newdata = features_24), 4)) |> 
  summarize(MSE = mean((xwOBA - pred) ^ 2))


### variable importance
xg_fit_xwoba_23 |> 
  vip(
    aesthetics = list(
      color = "black",
      fill = "goldenrod"
    )
  )


### 2024 xwOBA

## set seed
set.seed(0829)

## tuning XGBoost
### hyperparameter grid
xg_grid_xwoba_24 <- crossing(
  nrounds = seq(100, 400, 10),
  eta = c(0.01, 0.05, 0.10, 0.20, 0.25), # learning rate
  gamma = 0,
  max_depth = 2:5, 
  colsample_bytree = 1, 
  min_child_weight = 1, 
  subsample = 1
)

### tuning
xg_tune_xwoba_24 <- train(
  x = features_24,
  y = stuff_plus_24$xwOBA, 
  tuneGrid = xg_grid_xwoba_24,
  trControl = trainControl(method = "cv", number = 5),
  objective = "reg:squarederror", 
  method = "xgbTree",
  verbosity = 0 # prevents ntree_limit warning
)


## model evaluation
### fit model to training data
xg_fit_xwoba_24 <- xgboost(
  data = features_24, 
  label = stuff_plus_24$xwOBA,
  objective = "reg:squarederror",
  nrounds = xg_tune_xwoba_24$bestTune$nrounds,
  params = as.list(select(xg_tune_xwoba_24$bestTune, -nrounds)),
  verbose = 0
)

### training error
stuff_plus_24 |> 
  mutate(pred = round(predict(xg_fit_xwoba_24, newdata = features_24), 4)) |> 
  summarize(MSE = mean((xwOBA - pred) ^ 2))

### testing error
# stuff_plus_24 |> 
#   mutate(pred = round(predict(xg_fit_xwoba_23, newdata = features_24), 4)) |> 
#   summarize(MSE = mean((xwOBA - pred) ^ 2))


### variable importance
xg_fit_xwoba_24 |> 
  vip(
    aesthetics = list(
      color = "black",
      fill = "goldenrod"
    )
  )


# Whiff Rate --------------------------------------------------------------

### 2021 whiff rate

## set seed
set.seed(0829)

## tuning XGBoost
### hyperparameter grid
xg_grid_whiff_pct_21 <- crossing(
  nrounds = seq(100, 600, 25),
  eta = seq(0.01, 0.10, 0.01), # learning rate
  gamma = 0,
  max_depth = 2:5, 
  colsample_bytree = 1, 
  min_child_weight = 1, 
  subsample = 0.8
)

### tuning
xg_tune_whiff_pct_21 <- train(
  x = features_21,
  y = stuff_plus_21$whiff_pct, 
  tuneGrid = xg_grid_whiff_pct_21,
  trControl = trainControl(method = "cv", number = 5),
  objective = "reg:squarederror", 
  method = "xgbTree",
  verbosity = 0 # prevents ntree_limit warning
)


## model evaluation
### fit model to training data
xg_fit_whiff_pct_21 <- xgboost(
  data = features_21, 
  label = stuff_plus_21$whiff_pct,
  objective = "reg:squarederror",
  nrounds = 100,
  params = as.list(select(xg_tune_whiff_pct_21$bestTune, -nrounds)),
  lambda = 50,
  verbose = 0
)

### training error
stuff_plus_21 |> 
  mutate(pred = round(predict(xg_fit_whiff_pct_21, newdata = features_21), 4)) |> 
  summarize(MSE = mean((whiff_pct - pred) ^ 2))

### testing error
stuff_plus_22 |> 
  mutate(pred = round(predict(xg_fit_whiff_pct_21, newdata = features_22), 4)) |> 
  summarize(MSE = mean((whiff_pct - pred) ^ 2))


### variable importance
xg_fit_whiff_pct_21 |> 
  vip(
    aesthetics = list(
      color = "black",
      fill = "goldenrod"
    )
  )


### 2022 whiff rate

## set seed
set.seed(0829)

## tuning XGBoost
### hyperparameter grid
xg_grid_whiff_pct_22 <- crossing(
  nrounds = seq(100, 600, 25),
  eta = seq(0.01, 0.10, 0.01), # learning rate
  gamma = 0,
  max_depth = 2:5, 
  colsample_bytree = 1, 
  min_child_weight = 1, 
  subsample = 0.8
)

### tuning
xg_tune_whiff_pct_22 <- train(
  x = features_22,
  y = stuff_plus_22$whiff_pct, 
  tuneGrid = xg_grid_whiff_pct_22,
  trControl = trainControl(method = "cv", number = 5),
  objective = "reg:squarederror", 
  method = "xgbTree",
  verbosity = 0 # prevents ntree_limit warning
)


## model evaluation
### fit model to training data
xg_fit_whiff_pct_22 <- xgboost(
  data = features_22, 
  label = stuff_plus_22$whiff_pct,
  objective = "reg:squarederror",
  nrounds = xg_tune_whiff_pct_22$bestTune$nrounds,
  params = as.list(select(xg_tune_whiff_pct_22$bestTune, -nrounds)),
  lambda = 500, 
  verbose = 0
)

### training error
stuff_plus_22 |> 
  mutate(pred = round(predict(xg_fit_whiff_pct_22, newdata = features_22), 4)) |> 
  summarize(MSE = mean((whiff_pct - pred) ^ 2))

### testing error
stuff_plus_23 |> 
  mutate(pred = round(predict(xg_fit_whiff_pct_22, newdata = features_23), 4)) |> 
  summarize(MSE = mean((whiff_pct - pred) ^ 2))


### variable importance
xg_fit_whiff_pct_22 |> 
  vip(
    aesthetics = list(
      color = "black",
      fill = "goldenrod"
    )
  )


### 2023 whiff rate

## set seed
set.seed(0829)

## tuning XGBoost
### hyperparameter grid
xg_grid_whiff_pct_23 <- crossing(
  nrounds = seq(100, 600, 25),
  eta = seq(0.01, 0.10, 0.01), # learning rate
  gamma = 0,
  max_depth = 2:5, 
  colsample_bytree = 1, 
  min_child_weight = 1, 
  subsample = 0.8
)

### tuning
xg_tune_whiff_pct_23 <- train(
  x = features_23,
  y = stuff_plus_23$whiff_pct, 
  tuneGrid = xg_grid_whiff_pct_23,
  trControl = trainControl(method = "cv", number = 5),
  objective = "reg:squarederror", 
  method = "xgbTree",
  verbosity = 0 # prevents ntree_limit warning
)


## model evaluation
### fit model to training data
xg_fit_whiff_pct_23 <- xgboost(
  data = features_23, 
  label = stuff_plus_23$whiff_pct,
  objective = "reg:squarederror",
  nrounds = xg_tune_whiff_pct_23$bestTune$nrounds,
  params = as.list(select(xg_tune_whiff_pct_23$bestTune, -nrounds)),
  lambda = 200, 
  verbose = 0
)

### training error
stuff_plus_23 |> 
  mutate(pred = round(predict(xg_fit_whiff_pct_23, newdata = features_23), 4)) |> 
  summarize(MSE = mean((whiff_pct - pred) ^ 2))

### testing error
stuff_plus_24 |> 
  mutate(pred = round(predict(xg_fit_whiff_pct_23, newdata = features_24), 4)) |> 
  summarize(MSE = mean((whiff_pct - pred) ^ 2))


### variable importance
xg_fit_whiff_pct_23 |> 
  vip(
    aesthetics = list(
      color = "black",
      fill = "goldenrod"
    )
  )


### 2024 whiff rate

## set seed
set.seed(0829)

## tuning XGBoost
### hyperparameter grid
xg_grid_whiff_pct_24 <- crossing(
  nrounds = seq(100, 600, 25),
  eta = seq(0.01, 0.10, 0.01), # learning rate
  gamma = 0,
  max_depth = 2:5, 
  colsample_bytree = 1, 
  min_child_weight = 1, 
  subsample = 0.8
)

### tuning
xg_tune_whiff_pct_24 <- train(
  x = features_24,
  y = stuff_plus_24$whiff_pct, 
  tuneGrid = xg_grid_whiff_pct_24,
  trControl = trainControl(method = "cv", number = 5),
  objective = "reg:squarederror", 
  method = "xgbTree",
  verbosity = 0 # prevents ntree_limit warning
)


## model evaluation
### fit model to training data
xg_fit_whiff_pct_24 <- xgboost(
  data = features_24, 
  label = stuff_plus_24$whiff_pct,
  objective = "reg:squarederror",
  nrounds = xg_tune_whiff_pct_24$bestTune$nrounds,
  params = as.list(select(xg_tune_whiff_pct_24$bestTune, -nrounds)),
  lambda = 250,
  verbose = 0
)

### training error
stuff_plus_24 |> 
  mutate(pred = round(predict(xg_fit_whiff_pct_24, newdata = features_24), 4)) |> 
  summarize(MSE = mean((whiff_pct - pred) ^ 2))

### testing error
# stuff_plus_24 |> 
#   mutate(pred = round(predict(xg_fit_whiff_pct_23, newdata = features_24), 4)) |> 
#   summarize(MSE = mean((whiff_pct - pred) ^ 2))


### variable importance
xg_fit_whiff_pct_24 |> 
  vip(
    aesthetics = list(
      color = "black",
      fill = "goldenrod"
    )
  )





# Full XGBoost Models -----------------------------------------------------

### Full Stuff+

### data splitting
features_21_23 <- pitching_models |> 
  filter(season != 2024) |> 
  select(
    velocity, spin, extension, induced_vertical_break, horizontal_break, 
    horizontal_release_point, vertical_release_point, spin_axis,
    diff_velocity, diff_horizontal_break, diff_induced_vertical_break
  ) |> 
  as.matrix()

## data splitting
train <- pitching_models |> 
  slice_sample(prop = 0.8)

test <- pitching_models |> 
  anti_join(train)

x_train <- train |> 
  select(
    velocity, spin, extension, induced_vertical_break, horizontal_break, 
    horizontal_release_point, vertical_release_point, spin_axis,
    diff_velocity, diff_horizontal_break, diff_induced_vertical_break
  ) |> 
  as.matrix()

x_test <- test |> 
  select(
    velocity, spin, extension, induced_vertical_break, horizontal_break, 
    horizontal_release_point, vertical_release_point, spin_axis,
    diff_velocity, diff_horizontal_break, diff_induced_vertical_break
  ) |> 
  as.matrix()

## set seed
set.seed(0829)

## tuning XGBoost
### hyperparameter grid
xg_grid_sp <- crossing(
  nrounds = seq(150, 550, 10),
  eta = c(0.01, 0.03, 0.05, 0.10, 0.20), # learning rate
  gamma = 0,
  max_depth = 2:5, 
  colsample_bytree = 0.8, 
  min_child_weight = 1, 
  subsample = 0.8
)

### tuning
xg_tune_sp <- train(
  x = x_train,
  y = train$run_value, 
  tuneGrid = xg_grid_sp,
  trControl = trainControl(method = "cv", number = 5),
  objective = "reg:squarederror", 
  method = "xgbTree",
  verbosity = 0 # prevents ntree_limit warning
)


## model evaluation
### fit model to training data
xg_fit_sp <- xgboost(
  data = x_train, 
  label = train$run_value,
  objective = "reg:squarederror",
  nrounds = xg_tune_sp$bestTune$nrounds,
  params = as.list(select(xg_tune_sp$bestTune, -nrounds)),
  verbose = 0
)

### training error
train |> 
  mutate(pred = round(predict(xg_fit_sp, newdata = x_train), 4)) |> 
  summarize(MSE = mean((run_value - pred) ^ 2))

### testing error
test |> 
  mutate(pred = round(predict(xg_fit_sp, newdata = x_test), 4)) |> 
  summarize(MSE = mean((run_value - pred) ^ 2))


### variable importance
xg_fit_sp |> 
  vip(
    aesthetics = list(
      color = "black",
      fill = "dodgerblue4"
    )
  )



### Full xwOBA

## set seed
set.seed(0829)

## tuning XGBoost
### hyperparameter grid
xg_grid_xwOBA <- crossing(
  nrounds = seq(150, 550, 10),
  eta = c(0.01, 0.03, 0.05, 0.10, 0.20), # learning rate
  gamma = 0,
  max_depth = 2:5, 
  colsample_bytree = 0.8, 
  min_child_weight = 1, 
  subsample = 0.8
)

### tuning
xg_tune_xwOBA <- train(
  x = x_train,
  y = train$xwOBA, 
  tuneGrid = xg_grid_xwOBA,
  trControl = trainControl(method = "cv", number = 5),
  objective = "reg:squarederror", 
  method = "xgbTree",
  verbosity = 0 # prevents ntree_limit warning
)


## model evaluation
### fit model to training data
xg_fit_xwOBA <- xgboost(
  data = x_train, 
  label = train$xwOBA,
  objective = "reg:squarederror",
  nrounds = xg_tune_xwOBA$bestTune$nrounds,
  params = as.list(select(xg_tune_xwOBA$bestTune, -nrounds)),
  verbose = 0
)

### training error
train |> 
  mutate(pred = round(predict(xg_fit_xwOBA, newdata = x_train), 4)) |> 
  summarize(MSE = mean((xwOBA - pred) ^ 2))

### testing error
test |> 
  mutate(pred = round(predict(xg_fit_xwOBA, newdata = x_test), 4)) |> 
  summarize(MSE = mean((xwOBA - pred) ^ 2))


### variable importance
xg_fit_xwOBA  |> 
  vip(
    aesthetics = list(
      color = "black",
      fill = "goldenrod"
    )
  )



### Full Whiff Rate

## set seed
set.seed(0829)

## tuning XGBoost
### hyperparameter grid
xg_grid_whiff_rate <- crossing(
  nrounds = seq(150, 550, 10),
  eta = seq(0.01, 0.09, 0.02), # learning rate
  gamma = 0,
  max_depth = 2:5, 
  colsample_bytree = 0.8, 
  min_child_weight = 1, 
  subsample = 0.8
)

### tuning
xg_tune_whiff_rate <- train(
  x = x_train,
  y = train$whiff_pct, 
  tuneGrid = xg_grid_whiff_rate,
  trControl = trainControl(method = "cv", number = 5),
  objective = "reg:squarederror", 
  method = "xgbTree",
  verbosity = 0 # prevents ntree_limit warning
)


## model evaluation
### fit model to training data
xg_fit_whiff_rate <- xgboost(
  data = x_train, 
  label = train$whiff_pct,
  objective = "reg:squarederror",
  nrounds = xg_tune_whiff_rate$bestTune$nrounds,
  params = as.list(select(xg_tune_whiff_rate$bestTune, -nrounds)),
  verbose = 0, 
  lambda = 500
)

### training error
train |> 
  mutate(pred = round(predict(xg_fit_whiff_rate, newdata = x_train), 4)) |> 
  summarize(MSE = mean((whiff_pct - pred) ^ 2))

### testing error
test |> 
  mutate(pred = round(predict(xg_fit_whiff_rate, newdata = x_test), 4)) |> 
  summarize(MSE = mean((whiff_pct - pred) ^ 2))


### variable importance
xg_fit_whiff_rate |> 
  vip(
    aesthetics = list(
      color = "black",
      fill = "goldenrod"
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
