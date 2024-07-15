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
fangraphs <- read.csv("Fangraphs_Pitching_Models_2020-24.csv")

## glimpse
glimpse(statcast)
glimpse(fangraphs)

## remove unnecessary columns from statcast
statcast_filter <- statcast |> 
  select(-c(X.1, X, spin_dir:break_length_deprecated, tfs_deprecated, tfs_zulu_deprecated,
         umpire, sv_id, fielder_2, pitcher.1:fielder_9, home_score:delta_home_win_exp, bat_speed, swing_length)) |> 
  rename(pitcher_id = pitcher)

## remove unnecessary columns from FanGraphs
fangraphs_filter <- fangraphs |> 
  select(Season:xMLBAMID, Role, stuff_plus_stuff_CH:stuff_plus_stuff_FS, stuff_plus_stuff_FO)

### First look at fastballs, then sliders
## lm(SaM ~ Velocity * factor(Season)) # Interaction term
### see different slopes between years
### weight by number of pitches
## Minimum of 100 pitches
### can do lm for other variables as well

### Next steps:
## if we find relationship, expand into it more
### view things spatially in the strike zone (heatmap or hexmap)

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
    season = factor(game_year, levels = c(2020, 2021, 2022, 2023, 2024)),
    
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
    vertical_movement_in = pfx_z * 12, #|>  ,
    
    # combine some pitchers together because of Stuff+ (can comment out when necessary)
    pitch_name = recode(pitch_name,
                        "Sweeper" = "Slider",
                        "Slow Curve" = "Curveball", #)) |> ,
                        "Slurve" = "Curveball"))
 

statcast_pitch_summary_stats <- statcast_filter |>  
  # Now can calculate various stats at the pitch type level:
  group_by(pitcher_id, p_throws, pitch_name, season) %>%
  
  # Use the summarise function to calculate the frequencies for each of
  # these indicators:
  summarise(n_pitches = n(),
            n_swings = sum(swing, na.rm = TRUE),
            n_miss = sum(miss, na.rm = TRUE),
            swing_strikes = sum(swing == 1 & miss == 1, na.rm = TRUE),
            spin = mean(release_spin_rate, na.rm = TRUE),
            velocity = mean(release_speed, na.rm = TRUE),
            xwOBA = mean(estimated_woba_using_speedangle, na.rm = TRUE),
            #wOBA = mean(woba_value, na.rm = TRUE), factors in defense, looking for pitcher's contributions
            extension = mean(release_extension, na.rm = TRUE),
            induced_vertical_break = mean(vertical_movement_in, na.rm = TRUE),
            horizontal_break = mean(horizontal_movement_in, na.rm = TRUE),
            run_value = sum(delta_run_exp, na.rm = TRUE)) |>   
  
  # Using these sums can calculate commonly seen baseball stats:
  mutate(swing_pct = round(n_swings / n_pitches, 6) * 100,
         whiff_pct = round(n_miss / n_swings, 6) * 100,
         swing_and_miss_pct = round(swing_strikes / n_pitches, 6) * 100,
         spin = round(spin, 0),
         run_value_per100 = (run_value / n_pitches) * 100) |> 
  ungroup()


### dataset with at least 100 pitches thrown for a pitch
statcast_pitch_summary_stats <- statcast_pitch_summary_stats |> 
  filter(n_pitches >= 100)

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



### Join the datasets
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
  mutate(role = factor(role, levels = c("SP", "RP")))

### NOTE: For now, pitchers' names, role, and stuff+ are NA for Sweeper and Slurve because those pitch names
### do not register in the FanGraphs dataset. This is not as important for now because we are looking at pitches
### thrown more than 5% of the time total. 

### UPDATED NOTE: Joined Sweepers with sliders and slurves with curveballs in previous dataset so NAs are gone

### Write to .csv
#write.csv(pitching_models, "mlb_pitching_stats_2020-24.csv")

