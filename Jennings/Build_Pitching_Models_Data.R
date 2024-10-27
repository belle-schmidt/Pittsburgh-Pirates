## Load Statcast Data

### Libraries
library(tidyverse)
library(baseballr)
library(ggthemes)
# library(plotly)
library(hexbin)

## set theme
theme_set(theme_bw())

## Data 
statcast <- read.csv("Statcast.csv")
updated_statcast <- read.csv("statcast_2024.csv")
fangraphs <- read.csv("Fangraphs_Pitching_Models_2021-24.csv")

## glimpse
glimpse(statcast)
glimpse(updated_statcast)
glimpse(fangraphs)

## remove unnecessary columns from statcast
statcast_filter <- statcast |> 
  select(-1) |> 
  rbind(updated_statcast) |> 
  filter(game_year != 2020) |> 
  select(-c(X, spin_dir:break_length_deprecated, tfs_deprecated, tfs_zulu_deprecated,
         umpire, sv_id, fielder_2, pitcher.1:fielder_9, home_score:of_fielding_alignment, bat_speed, swing_length)) |> 
  rename(pitcher_id = pitcher)

## write filtered csv
#write.csv(statcast_filter, "statcast_2021-24.csv")

## remove unnecessary columns from FanGraphs
fangraphs_filter <- fangraphs |> 
  select(Season:xMLBAMID, Role, stuff_plus_stuff_CH:stuff_plus_stuff_FS, stuff_plus_stuff_FO)

## Filter the Data
### Statcast pitch summary stats
statcast_filter <- statcast_filter %>%
  # only include pitches from FanGraphs (will combine and adjust later b/c of Stuff+ model)
  filter(pitch_name %in% c("Sinker", "4-Seam Fastball",
                           "Cutter", "Curveball",
                           "Changeup", "Split-Finger",
                           "Slider", "Knuckle Curve",
                           "Sweeper", "Forkball",
                           "Slow Curve", "Slurve")) |> 
  
  mutate(
    # change game_year to Season
    season = factor(game_year, levels = c(2021, 2022, 2023, 2024)),
    
    # Create a swing indicator
    swing = ifelse(description %in%
                     c("bunt_foul_tip",
                       "foul", "foul_bunt",
                       "foul_pitchout",
                       "foul_tip", "hit_into_play",
                       "missed_bunt", "swinging_strike",
                       "swinging_strike_blocked"), 
                   1, 0),
    
    
    # Create an indicator for a missed swing attempt
    miss = ifelse(description %in%
                    c("missed_bunt", "swinging_strike",
                      "swinging_strike_blocked", "foul_tip",
                      "bunt_foul_tip"), 
                  1, 0),
    # put lefties on the same scale as righties and convert to inches
    horizontal_movement_in = ifelse(p_throws == "L", -pfx_x * 12, pfx_x * 12),
    # convert to inches
    vertical_movement_in = pfx_z * 12,
    
    # put lefties on the same scale as righties and convert to inches
    release_point_x_in = ifelse(p_throws == "L", -release_pos_x * 12, release_pos_x * 12),
    # convert to inches
    release_point_z_in = release_pos_z * 12,
    
    # combine some pitchers together because of Stuff+ (can comment out when necessary)
    pitch_name = recode(pitch_name,
                        "Sweeper" = "Slider",
                        "Slow Curve" = "Curveball",
                        "Slurve" = "Curveball"))
 

statcast_pitch_summary_stats <- statcast_filter |>  
  # calculate various stats at the pitch type level
  group_by(pitcher_id, p_throws, pitch_name, season) %>%
  
  # calculate frequencies of indicators
  summarize(
    # counting stats
    n_pitches = n(),
    n_swings = sum(swing, na.rm = TRUE),
    n_miss = sum(miss, na.rm = TRUE),
    swing_strikes = sum(swing == 1 & miss == 1, na.rm = TRUE),
    
    # physical characteristics
    velocity = mean(release_speed, na.rm = TRUE),
    spin = mean(release_spin_rate, na.rm = TRUE),
    extension = mean(release_extension, na.rm = TRUE),
    induced_vertical_break = mean(vertical_movement_in, na.rm = TRUE),
    horizontal_break = mean(horizontal_movement_in, na.rm = TRUE),
    horizontal_release_point = mean(release_point_x_in, na.rm = TRUE),
    vertical_release_point = mean(release_point_z_in, na.rm = TRUE),
    spin_axis = mean(spin_axis, na.rm = TRUE),
    
    # target variables
    xwOBA = mean(estimated_woba_using_speedangle, na.rm = TRUE),
    run_value = sum(delta_run_exp, na.rm = TRUE)
  ) |>   
  
  # calculate common baseball stats
  mutate(
    swing_pct = round(n_swings / n_pitches, 6) * 100,
    whiff_pct = round(n_miss / n_swings, 6) * 100,
    swing_and_miss_pct = round(swing_strikes / n_pitches, 6) * 100,
    spin = round(spin, 0),
    run_value_per100 = (run_value / n_pitches) * 100,
  ) |> 
  ungroup()


### dataset with at least 100 pitches thrown for a pitch
statcast_pitch_summary_stats <- statcast_pitch_summary_stats |> 
  filter(n_pitches >= 100) |> 
  # assign pitch groups
  mutate(
    pitch_group = case_when(
      pitch_name %in% c("4-Seam Fastball", "Sinker", "Cutter") ~ "Fastball",
      pitch_name %in% c("Changeup", "Forkball", "Split-Finger") ~ "Offspeed",
      pitch_name %in% c("Curveball", "Knuckle Curve", "Slider") ~ "Breaking"
    )
  ) |> 
  # move pitch group after pitch name
  relocate(
    pitch_group,
    .after = pitch_name
  )


### FanGraphs
#### pivot longer
fangraphs_long <- fangraphs_filter |> 
  pivot_longer(
    cols = starts_with("stuff_plus"),
    names_to = "pitch_type",
    names_prefix = "stuff_plus_stuff_",
    values_to = "stuff_plus"
  ) |> 
  mutate(pitch_name = case_match(pitch_type,
                                 "CH" ~ "Changeup", "FF" ~ "4-Seam Fastball",
                                 "SI" ~ "Sinker", "SL" ~ "Slider",
                                 "CU" ~ "Curveball", "FC" ~ "Cutter",
                                 "FS" ~ "Split-Finger", "KC" ~ "Knuckle Curve",
                                 "FO" ~ "Forkball"),
         season = factor(Season, levels = c(2020, 2021, 2022, 2023, 2024))) |>
  select(season, p_throws = Throws, player_name = PlayerName, pitcher_id = xMLBAMID, 
         role = Role, pitch_name, stuff_plus)



### join the datasets
pitching_models <- statcast_pitch_summary_stats |> 
  left_join(fangraphs_long, by = c("season", "pitcher_id", "p_throws", "pitch_name")) |> 
  # move player name first
  relocate(player_name, .before = pitcher_id) |>
  # move role after p_throws
  relocate(role, .after = p_throws) |> 
  # move run value variables next to each other
  relocate(run_value, .before = run_value_per100) |> 
  # remove pitcher_id since we have names
  select(-pitcher_id) |> 
  mutate(
    role = factor(role, levels = c("SP", "RP"))
  ) |> 
  # arrange in alphebetical order
  arrange(
    player_name, pitch_name, season
  )


### primary fastball
primary_fastball <- pitching_models |> 
  group_by(player_name, pitch_group, season) |> 
  filter(
    n_pitches == max(n_pitches),
    pitch_group == "Fastball"
  ) |> 
  ungroup() |> 
  # manual remove rows that have tied primary pitches
  slice(
    -c(
      # Chris Martin 2023 4-Seam Fastball
      59,
      # Alek Manoah 2024 Sinker
      1844
    )
  ) |> 
  select(
    player_name, 
    p_throws, 
    season, 
    primary_FB_velocity = velocity,
    primary_FB_horizontal_break = horizontal_break,
    primary_FB_induced_vertical_break = induced_vertical_break
  )


### pitchers without a primary fastball
no_fastball <- pitching_models |> 
  filter(
    !(player_name %in% primary_fastball$player_name)
  ) |> 
  # add fastball velocity column for rbind
  mutate(
    primary_FB_velocity = NA,
    primary_FB_horizontal_break = NA,
    primary_FB_induced_vertical_break = NA
  )

### pitchers who didn't have qualified fastballs (100+ pitches) we're 
### automatically removed in the join

### join with pitching models
pitching_models_1 <- pitching_models |> 
  inner_join(
    primary_fastball, 
    relationship = "many-to-many"
  ) |> 
  rbind(no_fastball) |> 
  # arrange in alphebetical order
  arrange(
    player_name, pitch_name, season
  ) |> 
  # calculate velo and movement difference between primary fastball and secondary pitches
  mutate(
    diff_velocity = velocity - primary_FB_velocity,
    diff_horizontal_break = horizontal_break - primary_FB_horizontal_break,
    diff_induced_vertical_break = induced_vertical_break - primary_FB_induced_vertical_break
  ) |> 
  # selecy everything but the primary Fastball statistics
  select(
    -c(primary_FB_velocity, primary_FB_horizontal_break, primary_FB_induced_vertical_break)
  )
  



### write to .csv
write.csv(pitching_models_1, "mlb_pitching_stats_2021-24.csv")

