## General Additive Model to Compare to Random Forest for Stuff+

## libraries
library(tidyverse)
library(mgcv)
library(broom) # tidy extract linear summary
library(gratia)
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


## Fitting GAM with mgcv
### Training data
set.seed(0829)
train <- ff_models |> 
  slice_sample(prop = 0.8)
test <- ff_models |> 
  anti_join(train)

### Fit model
stuff_plus_gam <- gam(stuff_plus ~ extension + s(horizontal_break) + 
                        s(induced_vertical_break) + s(velocity) + s(spin),
                      family = "gaussian",
                      method = "REML", # more stable solution than default
                      data = train)

### View model summary
#### Splines
stuff_plus_gam |> 
  tidy()

#### Parametric
stuff_plus_gam |> 
  tidy(parametric = TRUE)

#### Model G-o-F (emphasize sampling)
stuff_plus_gam |> 
  glance()

## Visualize partial response functions
#install.packages('gratia')
draw(stuff_plus_gam)

### for model diagnostics
stuff_plus_gam |> 
  appraise()

### Model checking
stuff_plus_gam |> 
  gam.check()

## Evaluate prediction accuracy
### In-sample performance (training set)
stuff_plus_gam |> 
  augment(newdata = train) |> 
  mutate(pred = round(.fitted)) |> 
  summarize(rmse = sqrt(mean((stuff_plus - .fitted)^2)))

### Out-of-sample performance (test set)
stuff_plus_gam |> 
  augment(newdata = test) |> 
  mutate(pred = round(.fitted)) |> 
  summarize(rmse = sqrt(mean((stuff_plus - .fitted)^2)))


## Final model
### Fit model
stuff_plus_gam <- gam(stuff_plus ~ extension + s(horizontal_break) + 
                        s(induced_vertical_break) + s(velocity) + s(spin),
                      family = "gaussian",
                      method = "REML", 
                      data = pitching_models)

### RMSE
stuff_plus_gam |> 
  augment() |> 
  mutate(pred = round(.fitted)) |> 
  summarize(rmse = sqrt(mean((stuff_plus - .fitted)^2)))




## Fitting GAM with mgcv for xwOBA
### 2021
### Training data
set.seed(0829)
train <- ff_models_21 |> 
  slice_sample(prop = 0.8)
test <- ff_models_21 |> 
  anti_join(train)

### Fit model
stuff_plus_gam_21 <- gam(stuff_plus ~ extension + s(horizontal_break) + 
                        s(induced_vertical_break) + s(velocity) + s(spin),
                      family = "gaussian",
                      method = "REML", # more stable solution than default
                      data = train)


## Visualize partial response functions
draw(stuff_plus_gam_21)

## Evaluate prediction accuracy
### In-sample performance (training set)
stuff_plus_gam_21 |> 
  augment(newdata = train) |> 
  mutate(pred = round(.fitted)) |> 
  summarize(rmse = sqrt(mean((stuff_plus - .fitted)^2)))

### Out-of-sample performance (test set)
stuff_plus_gam_21 |> 
  augment(newdata = test) |> 
  mutate(pred = round(.fitted)) |> 
  summarize(rmse = sqrt(mean((stuff_plus - .fitted)^2)))


## Final model
### Fit model
stuff_plus_gam_21 <- gam(stuff_plus ~ extension + s(horizontal_break) + 
                        s(induced_vertical_break) + s(velocity) + s(spin),
                      family = "gaussian",
                      method = "REML", 
                      data = ff_models_21)

### RMSE
stuff_plus_gam_21 |> 
  augment() |> 
  mutate(pred = round(.fitted)) |> 
  summarize(rmse = sqrt(mean((stuff_plus - .fitted)^2)))


### 2022
### Training data
set.seed(0829)
train <- ff_models_22 |> 
  slice_sample(prop = 0.8)
test <- ff_models_22 |> 
  anti_join(train)

### Fit model
stuff_plus_gam_22 <- gam(stuff_plus ~ extension + s(horizontal_break) + 
                           s(induced_vertical_break) + s(velocity) + s(spin),
                         family = "gaussian",
                         method = "REML", # more stable solution than default
                         data = train)


## Visualize partial response functions
draw(stuff_plus_gam_22)

## Evaluate prediction accuracy
### In-sample performance (training set)
stuff_plus_gam_22 |> 
  augment(newdata = train) |> 
  mutate(pred = round(.fitted)) |> 
  summarize(rmse = sqrt(mean((stuff_plus - .fitted)^2)))

### Out-of-sample performance (test set)
stuff_plus_gam_22 |> 
  augment(newdata = test) |> 
  mutate(pred = round(.fitted)) |> 
  summarize(rmse = sqrt(mean((stuff_plus - .fitted)^2)))


## Final model
### Fit model
stuff_plus_gam_22 <- gam(stuff_plus ~ extension + s(horizontal_break) + 
                           s(induced_vertical_break) + s(velocity) + s(spin),
                         family = "gaussian",
                         method = "REML", 
                         data = ff_models_22)

### RMSE
stuff_plus_gam_22 |> 
  augment() |> 
  mutate(pred = round(.fitted)) |> 
  summarize(rmse = sqrt(mean((stuff_plus - .fitted)^2)))


### 2023
### Training data
set.seed(0829)
train <- ff_models_23 |> 
  slice_sample(prop = 0.8)
test <- ff_models_23 |> 
  anti_join(train)

### Fit model
stuff_plus_gam_23 <- gam(stuff_plus ~ extension + s(horizontal_break) + 
                           s(induced_vertical_break) + s(velocity) + s(spin),
                         family = "gaussian",
                         method = "REML", # more stable solution than default
                         data = train)


## Visualize partial response functions
draw(stuff_plus_gam_23)

## Evaluate prediction accuracy
### In-sample performance (training set)
stuff_plus_gam_23 |> 
  augment(newdata = train) |> 
  mutate(pred = round(.fitted)) |> 
  summarize(rmse = sqrt(mean((stuff_plus - .fitted)^2)))

### Out-of-sample performance (test set)
stuff_plus_gam_23 |> 
  augment(newdata = test) |> 
  mutate(pred = round(.fitted)) |> 
  summarize(rmse = sqrt(mean((stuff_plus - .fitted)^2)))


## Final model
### Fit model
stuff_plus_gam_23 <- gam(stuff_plus ~ extension + s(horizontal_break) + 
                           s(induced_vertical_break) + s(velocity) + s(spin),
                         family = "gaussian",
                         method = "REML", 
                         data = ff_models_23)

### RMSE
stuff_plus_gam_23 |> 
  augment() |> 
  mutate(pred = round(.fitted)) |> 
  summarize(rmse = sqrt(mean((stuff_plus - .fitted)^2)))





## Fitting GAM with mgcv
### Training data
set.seed(0829)
train <- ff_models |> 
  slice_sample(prop = 0.8)
test <- ff_models |> 
  anti_join(train)

### Fit model
stuff_plus_gam <- gam(stuff_plus ~ extension + s(horizontal_break) + 
                s(induced_vertical_break) + s(velocity) + s(spin),
              family = "gaussian",
              method = "REML", # more stable solution than default
              data = train)

### View model summary
#### Splines
stuff_plus_gam |> 
  tidy()

#### Parametric
stuff_plus_gam |> 
  tidy(parametric = TRUE)

#### Model G-o-F (emphasize sampling)
stuff_plus_gam |> 
  glance()

## Visualize partial response functions
#install.packages('gratia')
draw(stuff_plus_gam)

### for model diagnostics
stuff_plus_gam |> 
  appraise()

### Model checking
stuff_plus_gam |> 
  gam.check()

## Evaluate prediction accuracy
### In-sample performance (training set)
stuff_plus_gam |> 
  augment(newdata = train) |> 
  mutate(pred = round(.fitted)) |> 
  summarize(rmse = sqrt(mean((stuff_plus - .fitted)^2)))

### Out-of-sample performance (test set)
stuff_plus_gam |> 
  augment(newdata = test) |> 
  mutate(pred = round(.fitted)) |> 
  summarize(rmse = sqrt(mean((stuff_plus - .fitted)^2)))


## Final model
### Fit model
stuff_plus_gam <- gam(stuff_plus ~ extension + s(horizontal_break) + 
                        s(induced_vertical_break) + s(velocity) + s(spin),
                      family = "gaussian",
                      method = "REML", 
                      data = pitching_models)

### RMSE
stuff_plus_gam |> 
  augment() |> 
  mutate(pred = round(.fitted)) |> 
  summarize(rmse = sqrt(mean((stuff_plus - .fitted)^2)))



## PREDICT 2022 ON 2021
## GAM
xwoba_lm <- lm(xwOBA ~ stuff_plus, data = ff_models_21)

ff_models_21 |> 
  ggplot(aes(stuff_plus, xwOBA)) +
  geom_point(color = "dodgerblue4")

resid_analysis <- xwoba_lm |> 
  augment() |> 
  # remove outliers
  filter(abs(.std.resid) <= 2.6) 
# remove high leverage points
# filter(.hat <= 3*(1 + 1) / nrow(ff_models))

## fit model again
new_xwoba_lm <- lm(xwOBA ~ stuff_plus, data = resid_analysis)


# Set seed for reproducibility if you're doing any random operation in prediction
set.seed(0829)
predictions_2021 <- predict(stuff_plus_gam_21, newdata = ff_models_21, type = "response")
predictions_2022 <- predict(stuff_plus_gam_21, newdata = ff_models_22, type = "response")

# Add predictions to the 2022 data frame
ff_models_22$predicted_gam_stuff_plus <- predictions_2022


# Add predictions to the 2022 data frame
ff_models_21$predicted_gam_stuff_plus <- predictions_2021
ggplot(ff_models_21, aes(x = stuff_plus, y = predicted_gam_stuff_plus)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = TRUE) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Actual vs Predicted Stuff+ for 2021",
       x = "Actual Stuff+",
       y = "Predicted Stuff+") 


ggplot(ff_models_22, aes(x = stuff_plus, y = predicted_gam_stuff_plus)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = TRUE) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Actual vs Predicted Stuff+ for 2022",
       x = "Actual Stuff+",
       y = "Predicted Stuff+") +
  theme_minimal()


# Set seed for reproducibility if you're doing any random operation in prediction
set.seed(0829)
predictions_2021 <- predict(sp_rf_final_21, data = ff_models_21, type = "response")
predictions_2022 <- predict(sp_rf_final_22, data = ff_models_22, type = "response")

# Add predictions to the 2022 data frame
ff_models_22$predicted_rf_stuff_plus <- predictions_2022$predictions


# Add predictions to the 2022 data frame
ff_models_21$predicted_rf_stuff_plus <- predictions_2021$predictions
ggplot(ff_models_21, aes(x = stuff_plus, y = predicted_rf_stuff_plus)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = TRUE) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Actual vs Predicted Stuff+ for 2021",
       x = "Actual Stuff+",
       y = "Predicted Stuff+") 


ggplot(ff_models_22, aes(x = stuff_plus, y = predicted_rf_stuff_plus)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = TRUE) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Actual vs Predicted Stuff+ for 2022",
       x = "Actual Stuff+",
       y = "Predicted Stuff+") +
  theme_minimal()

