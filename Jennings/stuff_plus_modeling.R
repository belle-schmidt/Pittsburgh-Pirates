## Stuff+ Modeling

## libraries
library(tidyverse)
library(ranger) # faster implementation of randomforest library
library(vip) # variable importance
library(pdp) # partial dependence plot
library(caret) # train random forest model with cross validation
library(broom) # tidy extract linear summary
library(ggfortify) # check linear assumptions

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

## build random forest to predict stuff+ for 2021 season
### set seed
set.seed(0829)

### fit model
sp_rf <- ranger(
  stuff_plus ~  . - xwOBA - whiff_pct,
  num.trees = 1000,
  importance = "impurity",
  mtry = 5 / 3,
  data = ff_models
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
sp_rf |> 
  partial(pred.var = "horizontal_break") |> 
  autoplot()

### vertical break
sp_rf |> 
  partial(pred.var = "induced_vertical_break") |> 
  autoplot()


## random forest with 5-fold CV
### fit model
sp_rf_cv <- train(
  stuff_plus ~ . - xwOBA - whiff_pct,
  tuneLength = 3,
  data = ff_models,
  method = "ranger",
  trControl = trainControl(
    method = "cv",
    number = 5,
    verboseIter = TRUE
  )
)

### plot model
plot(sp_rf_cv)


### try tuneGrid
tuneGrid <- data.frame(
  .mtry = c(2, 3, 4, 5),
  .splitrule = "variance",
  .min.node.size = 5
)

### fit model again
sp_rf_cv <- train(
  stuff_plus ~  . - xwOBA - whiff_pct,
  tuneGrid = tuneGrid,
  data = ff_models,
  method = "ranger",
  trControl = trainControl(
    method = "cv",
    number = 5,
    verboseIter = TRUE
  )
)

### model summary
sp_rf_cv


### plot model
plot(sp_rf_cv)


## final model
### set seed
set.seed(0829)

### fit model
sp_rf_final <- ranger(
  stuff_plus ~  . - xwOBA - whiff_pct,
  num.trees = 1000,
  importance = "impurity",
  mtry = 2,
  splitrule = "variance",
  min.node.size = 5,
  data = ff_models
)

### model summary
sp_rf_final

### RMSE
sqrt(139.6339)

### apply predictions to the dataset
ff_models <- ff_models |> 
  mutate(pred = sp_rf_final$predictions)

### plot predictions vs. original observed values
ff_models |> 
  ggplot(aes(stuff_plus, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "firebrick1")



## linear regression to predict xwOBA
### plot xwOBA by predicted stuff+
ff_models |> 
  ggplot(aes(pred, xwOBA)) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 100, linetype = "dashed", color = "firebrick1")

### plot xwOBA by stuff+
ff_models |> 
  ggplot(aes(stuff_plus, xwOBA)) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 100, linetype = "dashed", color = "firebrick1")

### fit model
xwoba_lm <- lm(xwOBA ~ stuff_plus, data = ff_models)

### model summary
xwoba_lm |> 
  tidy()

xwoba_lm |> 
  glance()


### normality check
#### bin width
bw <-  2 * IQR(ff_models$stuff_plus) / length(ff_models$stuff_plus) ^ (1/3)

#### plot
ff_models |> 
  ggplot(aes(stuff_plus)) +
  geom_histogram(fill = "dodgerblue2", color = "grey50", binwidth = bw)

# #### sqrt bin width
# sqrt_bw <-  2 * IQR(sqrt(ff_models$stuff_plus)) / length(sqrt(ff_models$stuff_plus)) ^ (1/3)
# 
# #### plot
# ff_models |> 
#   ggplot(aes(sqrt(stuff_plus))) +
#   geom_histogram(fill = "dodgerblue2", color = "grey50", binwidth = sqrt_bw)



### xwOBA normality
#### bin width
bw <-  2 * IQR(ff_models$xwOBA) / length(ff_models$xwOBA) ^ (1/3)

#### plot
ff_models |> 
  ggplot(aes(xwOBA)) +
  geom_histogram(fill = "dodgerblue2", color = "grey50", binwidth = bw)


### sqrt transformation
# ### plot xwOBA by sqrt(stuff+)
# ff_models |> 
#   ggplot(aes(sqrt(stuff_plus), sqrt(xwOBA))) +
#   geom_point(alpha = 0.5) +
#   geom_vline(xintercept = 10, linetype = "dashed", color = "firebrick1")
# 
# 
# 
# ## add sqrt(stuff+) to dataset
# ff_models <- ff_models |> 
#   mutate(sqrt_stuff_plus = sqrt(stuff_plus),
#          sqrt_xwOBA = sqrt(xwOBA))

# 
# ## fit model
# xwoba_lm <- lm(xwOBA ~ stuff_plus, data = ff_models)
# 
# 
# ## plot residuals against predicted values
# xwoba_lm |> 
#   augment() |> 
#   ggplot(aes(.fitted, .resid)) +
#   geom_point(color = "gray", alpha = 0.75) +
#   geom_hline(yintercept = 0, color = "red",
#              linetype = "dashed", linewidth = 1.5) +
#   geom_smooth(se = FALSE)


## check assumptions
autoplot(xwoba_lm, ncol = 4)


## standardized residuals to remove outliers
outliers <- xwoba_lm |> 
  augment() |> 
  filter(abs(.std.resid) >= 2.6)

## high leverage points 
# hl <- xwoba_lm_sqrt |> 
#   augment() |> 
#   filter(.hat >= 3*(1 + 1) / nrow(ff_models))

## there were no high influential points by cook's distance


## remove points that are outliers, high leverage points, and influential points
resid_analysis <- xwoba_lm |> 
  augment() |> 
  # remove outliers
  filter(abs(.std.resid) <= 2.6) 
  # remove high leverage points
  # filter(.hat <= 3*(1 + 1) / nrow(ff_models))

## fit model again
new_xwoba_lm <- lm(xwOBA ~ stuff_plus, data = resid_analysis)

## model summary
new_xwoba_lm |> 
  glance()

## plot
resid_analysis |> 
  ggplot(aes(stuff_plus, xwOBA)) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 100, linetype = "dashed", color = "firebrick1") +
  #geom_smooth(method = "loess") +
  geom_smooth(method = "lm")

