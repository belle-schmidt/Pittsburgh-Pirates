## XGBoost Stuff+, xwOBA, and Whiff Rate Variable Importance Plots

## libraries
library(tidyverse)

# Stuff+ Importance
xgboost_sp_importance <- tibble(
  # seasons
  season = c(
    rep(2021, 11), rep(2022, 11), rep(2023, 11), rep(2024, 11)
  ),
  # variable names
  variable = c(
    # 2021 Stuff+ Feature Names
    xgb.importance(feature_names = xg_fit_sp_21$feature_names, model = xg_fit_sp_21)$Feature,
    # 2022 Stuff+ Feature Names
    c(
      xgb.importance(feature_names = xg_fit_sp_22$feature_names, model = xg_fit_sp_22)$Feature,
      # difference in horizontal break is not accounted for in the modle
      "diff_horizontal_break"
    ),
    # 2023 Stuff+ Feature Names
    xgb.importance(feature_names = xg_fit_sp_23$feature_names, model = xg_fit_sp_23)$Feature,
    # 2024 Stuff+ Feature Names
    c(
      xgb.importance(feature_names = xg_fit_sp_24$feature_names, model = xg_fit_sp_24)$Feature,
      # difference in horizontal break is not accounted for in the modle
      "diff_horizontal_break"
    )
  ),
  # Stuff+ Variable Importance
  sp_importance = c(
    # 2021 Stuff+ Variable Importance
    xgb.importance(feature_names = xg_fit_sp_21$feature_names, model = xg_fit_sp_21)$Gain,
    # 2022 Stuff+ Variable Importance
    c(
      xgb.importance(feature_names = xg_fit_sp_22$feature_names, model = xg_fit_sp_22)$Gain,
      # difference in horizontal break is not accounted for in the model
      0
    ),
    # 2023 Stuff+ Variable Importance
    xgb.importance(feature_names = xg_fit_sp_23$feature_names, model = xg_fit_sp_23)$Gain,
    # 2024 Stuff+ Variable Importance
    c(
      xgb.importance(feature_names = xg_fit_sp_24$feature_names, model = xg_fit_sp_24)$Gain,
      # difference in horizontal break is not accounted for in the model
      0
    )
  )
) |> 
  mutate(
    scaled_sp_importance = (sp_importance - min(sp_importance, na.rm = TRUE)) / 
      (max(sp_importance, na.rm = TRUE) - min(sp_importance, na.rm = TRUE))
  )


# xwOBA Importance
xgboost_xwoba_importance <- tibble(
  # seasons
  season = c(
    rep(2021, 11), rep(2022, 11), rep(2023, 11), rep(2024, 11)
  ),
  # variable names
  variable = c(
    # 2021 xwOBA Feature Names
    xgb.importance(feature_names = xg_fit_xwoba_21$feature_names, model = xg_fit_xwoba_21)$Feature,
    
    # 2022 xwOBA Feature Names
    xgb.importance(feature_names = xg_fit_xwoba_22$feature_names, model = xg_fit_xwoba_22)$Feature,
    
    # 2023 xwOBA Feature Names
    xgb.importance(feature_names = xg_fit_xwoba_23$feature_names, model = xg_fit_xwoba_23)$Feature,
    
    # 2024 xwOBA Feature Names
    xgb.importance(feature_names = xg_fit_xwoba_24$feature_names, model = xg_fit_xwoba_24)$Feature
  ),
  # xwOBA Variable Importance
  xwoba_importance = c(
    # 2021 xwOBA Variable Importance
    xgb.importance(feature_names = xg_fit_xwoba_21$feature_names, model = xg_fit_xwoba_21)$Gain,
    
    # 2022 xwOBA Variable Importance
    xgb.importance(feature_names = xg_fit_xwoba_22$feature_names, model = xg_fit_xwoba_22)$Gain,
    
    # 2023 xwOBA Variable Importance
    xgb.importance(feature_names = xg_fit_xwoba_23$feature_names, model = xg_fit_xwoba_23)$Gain,
    
    # 2024 xwOBA Variable Importance
    xgb.importance(feature_names = xg_fit_xwoba_24$feature_names, model = xg_fit_xwoba_24)$Gain
  )
) |> 
  # scaled variable importance
  mutate(
    scaled_xwoba_importance = (xwoba_importance - min(xwoba_importance, na.rm = TRUE)) / 
      (max(xwoba_importance, na.rm = TRUE) - min(xwoba_importance, na.rm = TRUE))
  )


# Whiff Rate Importance
xgboost_whiff_pct_importance <- tibble(
  # seasons
  season = c(
    rep(2021, 11), rep(2022, 11), rep(2023, 11), rep(2024, 11)
  ),
  # variable names
  variable = c(
    # 2021 Whiff Rate Feature Names
    xgb.importance(feature_names = xg_fit_whiff_pct_21$feature_names, model = xg_fit_whiff_pct_21)$Feature,
    
    # 2022 Whiff Rate Feature Names
    xgb.importance(feature_names = xg_fit_whiff_pct_22$feature_names, model = xg_fit_whiff_pct_22)$Feature,
    
    # 2023 Whiff Rate Feature Names
    xgb.importance(feature_names = xg_fit_whiff_pct_23$feature_names, model = xg_fit_whiff_pct_23)$Feature,
    
    # 2024 Whiff Rate Feature Names
    xgb.importance(feature_names = xg_fit_whiff_pct_24$feature_names, model = xg_fit_whiff_pct_24)$Feature
  ),
  # Whiff Rate Variable Importance
  whiff_pct_importance = c(
    # 2021 Whiff Rate Variable Importance
    xgb.importance(feature_names = xg_fit_whiff_pct_21$feature_names, model = xg_fit_whiff_pct_21)$Gain,
    
    # 2022 Whiff Rate Variable Importance
    xgb.importance(feature_names = xg_fit_whiff_pct_22$feature_names, model = xg_fit_whiff_pct_22)$Gain,
    
    # 2023 Whiff Rate Variable Importance
    xgb.importance(feature_names = xg_fit_whiff_pct_23$feature_names, model = xg_fit_whiff_pct_23)$Gain,
    
    # 2024 Whiff Rate Variable Importance
    xgb.importance(feature_names = xg_fit_whiff_pct_24$feature_names, model = xg_fit_whiff_pct_24)$Gain
  )
) |> 
  # scaled variable importance
  mutate(
    scaled_whiff_pct_importance = (whiff_pct_importance - min(whiff_pct_importance, na.rm = TRUE)) / 
      (max(whiff_pct_importance, na.rm = TRUE) - min(whiff_pct_importance, na.rm = TRUE))
  )


# join the variable importances
xgboost_var_importance <- xgboost_sp_importance |> 
  # join with xwOBA importance
  inner_join(
    xgboost_xwoba_importance
  ) |> 
  # join with whiff rateimportance
  inner_join(
    xgboost_whiff_pct_importance
  ) |> 
  select(
    season,
    variable,
    scaled_sp_importance,
    scaled_xwoba_importance,
    scaled_whiff_pct_importance
  ) |> 
  mutate(
    variable = case_match(
      variable,
      "diff_velocity" ~ "Velocity Difference",
      "induced_vertical_break" ~ "Induced Vertical Break",
      "velocity" ~ "Velocity",
      "spin" ~ "Spin",
      "extension" ~ "Extension",
      "vertical_release_point" ~ "Vertical Release Point",
      "horizontal_break" ~ "Horizontal Break",
      "diff_horizontal_break" ~ "Horizontal Break Difference",
      "spin_axis" ~ "Spin Axis",
      "horizontal_release_point" ~ "Horizontal Release Point",
      "diff_induced_vertical_break" ~ "Induced Vertical Break Difference"
    )
  )


# write to csv
write.csv(xgboost_var_importance, "xgboost_var_importance.csv")
