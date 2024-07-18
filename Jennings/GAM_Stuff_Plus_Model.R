## General Additive Model to Compare to Random Forest for Stuff+

## libraries
library(tidyverse)
library(mgcv)
library(broom) # tidy extract linear summary
library(gratia)

## theme set
theme_set(theme_bw())

## data
pitching_models <- read.csv("mlb_pitching_stats_2020-24.csv")

## filter
ff_models <- pitching_models |> 
  filter(
    pitch_name == "4-Seam Fastball" &
      !is.na(stuff_plus) & 
      season == 2021
  ) |> 
  mutate(horizontal_break = -horizontal_break) |> 
  select(spin, velocity, extension:horizontal_break, stuff_plus, xwOBA, whiff_pct)

## Fitting GAM with mgcv
### Training data
set.seed(0829)
train <- ff_models |> 
  slice_sample(prop = 0.7)
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
