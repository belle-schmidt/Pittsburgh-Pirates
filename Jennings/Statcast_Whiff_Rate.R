## Load Statcast Data

### Libraries
library(tidyverse)
library(baseballr)

## Data 
statcast <- read.csv("Statcast.csv")

glimpse(statcast)

statcast |> 
  select(description) |> 
  unique() |> 
  arrange(description)





## Code
mutate(# Indicator for swing attempt
  swing = ifelse(description %in%
                   c("bunt_foul_tip",
                     "foul", "foul_bunt",
                     "foul_pitchout",
                     "foul_tip", "hit_into_play",
                     "missed_bunt", "swinging_strike",
                     "swinging_strike_blocked"), 
                 1, 0),
  
  
  # Indicator for miss 
  miss = ifelse(description %in%
                  c("missed_bunt", "swinging_strike",
                    "swinging_strike_blocked", "foul_tip"), 
                1, 0)) %>%
  
  # Now can calculate various stats at the pitch type level:
  group_by(pitch_type) %>%
  
  # Use the summarise function to calculate the frequencies for each of
  # these indicators:
  summarize(n_pitches = n(),
            n_swings = sum(swing, na.rm = TRUE),
            n_miss = sum(miss, na.rm = TRUE),
            spin = mean(release_spin_rate, na.rm = TRUE)) %>%  
  
  # Using these sums can calculate stats
  mutate(swing_rate = round(n_swings / n_pitches, 3),
         miss_rate = round(n_miss / n_swings, 3),
         spin = round(spin, 0))  



baseballr::playerid_lookup(last_name = "Bieber",
                           first_name = "Shane")

slider_bieber <- statcast |> 
  filter(pitcher == 669456 & str_like(game_date, "2024%") &
           pitch_type == "SL") |> 
  select(description) |> 
  unique() |> 
  arrange(description)

slider_bieber <- statcast |> 
  filter(pitcher == 669456 & str_like(game_date, "2024%") &
           pitch_type == "SL") |> 
  mutate(# Create an indicator for whether or not Votto made an attempt at the pitch:
    swing = ifelse(description %in%
                     c("bunt_foul_tip",
                       "foul", "foul_bunt",
                       "foul_pitchout",
                       "foul_tip", "hit_into_play",
                       "missed_bunt", "swinging_strike",
                       "swinging_strike_blocked"), 
                   1, 0),
    
    
    # Create an indicator for whether or not Votto missed in his attempt:  
    miss = ifelse(description %in%
                    c("missed_bunt", "swinging_strike",
                      "swinging_strike_blocked"), 
                  1, 0)
    ) |> 
  select(description, des, swing, miss)

ch_bieber <- statcast |> 
  filter(pitcher == 669456 & str_like(game_date, "2024%") &
           pitch_type == "CH") |> 
  mutate(# Create an indicator for whether or not Votto made an attempt at the pitch:
    swing = ifelse(description %in%
                     c("bunt_foul_tip",
                       "foul", "foul_bunt",
                       "foul_pitchout",
                       "foul_tip", "hit_into_play",
                       "missed_bunt", "swinging_strike",
                       "swinging_strike_blocked"), 
                   1, 0),
    
    
    # Create an indicator for whether or not Votto missed in his attempt:  
    miss = ifelse(description %in%
                    c("missed_bunt", "swinging_strike",
                      "swinging_strike_blocked"), 
                  1, 0)
  ) |> 
  select(description, swing, miss)

ch_bieber <- statcast |> 
  filter(pitcher == 669456 & str_like(game_date, "2024%") &
           pitch_type == "CH") |> 
  select(description) |> 
  unique() |> 
  arrange(description)

ff_bieber <- statcast |> 
  filter(pitcher == 669456 & str_like(game_date, "2023%") &
           pitch_type == "FF") |> 
  mutate(# Create an indicator for whether or not Votto made an attempt at the pitch:
    swing = ifelse(description %in%
                     c("bunt_foul_tip",
                       "foul", "foul_bunt",
                       "foul_pitchout",
                       "foul_tip", "hit_into_play",
                       "missed_bunt", "swinging_strike",
                       "swinging_strike_blocked") & events != "catcher_interf", 
                   1, 0),
    
    
    # Create an indicator for whether or not Votto missed in his attempt:  
    miss = ifelse(description %in%
                    c("missed_bunt", "swinging_strike",
                      "swinging_strike_blocked") | 
                    description == "foul_tip" & events == "strikeout", 
                  1, 0)
  ) |> 
  select(pitch_type, description, events, des, swing, miss, strikes, balls)


fc_bieber <- statcast |> 
  filter(pitcher == 669456 & str_like(game_date, "2023%") &
           pitch_type == "FC") |> 
  mutate(# Create an indicator for whether or not Votto made an attempt at the pitch:
    swing = ifelse(description %in%
                     c("bunt_foul_tip",
                       "foul", "foul_bunt",
                       "foul_pitchout",
                       "foul_tip", "hit_into_play",
                       "missed_bunt", "swinging_strike",
                       "swinging_strike_blocked"), 
                   1, 0),
    
    
    # Create an indicator for whether or not Votto missed in his attempt:  
    miss = ifelse(description %in%
                    c("missed_bunt", "swinging_strike",
                      "swinging_strike_blocked", "foul_tip"), 
                  1, 0)
  ) |> 
  select(pitch_type, description, events, des, swing, miss, strikes, balls)



tip_bieber <- statcast |> 
  filter(pitcher == 669456 & str_like(game_date, "2024%") &
           description == "foul_bunt") |> 
  mutate(# Create an indicator for whether or not Votto made an attempt at the pitch:
    swing = ifelse(description %in%
                     c("bunt_foul_tip",
                       "foul", "foul_bunt",
                       "foul_pitchout",
                       "foul_tip", "hit_into_play",
                       "missed_bunt", "swinging_strike",
                       "swinging_strike_blocked"), 
                   1, 0),
    
    
    # Create an indicator for whether or not Votto missed in his attempt:  
    miss = ifelse(description %in%
                    c("missed_bunt", "swinging_strike",
                      "swinging_strike_blocked") | 
                    description == "foul_tip" & events == "strikeout", 
                  1, 0)
  ) |> 
  select(pitch_type, description, des, swing, miss, strikes, balls)


tip_bieber <- statcast |> 
  filter(pitcher == 669456 & str_like(game_date, "2023%") &
           description == "foul_tip") |> 
  mutate(# Create an indicator for whether or not Votto made an attempt at the pitch:
    swing = ifelse(description %in%
                     c("bunt_foul_tip",
                       "foul", "foul_bunt",
                       "foul_pitchout",
                       "foul_tip", "hit_into_play",
                       "missed_bunt", "swinging_strike",
                       "swinging_strike_blocked"), 
                   1, 0),
    
    
    # Create an indicator for whether or not Votto missed in his attempt:  
    miss = ifelse(description %in%
                    c("missed_bunt", "swinging_strike",
                      "swinging_strike_blocked") | 
                    description == "foul_tip" & events == "strikeout", 
                  1, 0)
  ) |> 
  select(pitch_type, description, des, swing, miss, strikes)

statcast |> 
  filter(des == "Javier Baez strikes out on a foul tip." & 
           pitcher == 669456 & str_like(game_date, "2023%")) |> 
  arrange(pitch_number)


## Miss rate example
bieber_pitch_summary_stats_1 <- statcast %>%
  # First filter the votto dataset to be the most common pitches:
  filter(pitcher == 669456 & str_like(game_date, "2020%")) %>%
  
  # Next, using the description, type, and events columns - let's define some useful  
  # columns that will help us calculate stats for each of the different pitch types: 
  
  mutate(# Create an indicator for whether or not Votto made an attempt at the pitch:
    swing = ifelse(description %in%
                     c("bunt_foul_tip",
                       "foul", "foul_bunt",
                       "foul_pitchout",
                       "foul_tip", "hit_into_play",
                       "missed_bunt", "swinging_strike",
                       "swinging_strike_blocked"), 
                   1, 0),
    
    
    # Create an indicator for whether or not Votto missed in his attempt:  
    miss = ifelse(description %in%
                    c("missed_bunt", "swinging_strike",
                      "swinging_strike_blocked", "foul_tip",
                      "bunt_foul_tip"), 
                  1, 0)) |> 
  
  # Now can calculate various stats at the pitch type level:
  group_by(pitch_type) %>%
  
  # Use the summarise function to calculate the frequencies for each of
  # these indicators:
  summarise(n_pitches = n(),
            n_swings = sum(swing, na.rm = TRUE),
            n_miss = sum(miss, na.rm = TRUE),
            swing_strikes = sum(swing == 1 & miss == 1, na.rm = TRUE),
            spin = mean(release_spin_rate, na.rm = TRUE)) %>%  
  
  # Using these sums can calculate commonly seen baseball stats:
  mutate(swing_rate = round(n_swings / n_pitches, 3),
         whiff_rate = round(n_miss / n_swings, 3),
         swing_and_miss_pct = round(swing_strikes / n_pitches, 3),
         spin = round(spin, 0))  

mean(bieber_pitch_summary_stats_1$swing_strike_rate)



bieber_pitch_summary_stats <- statcast %>%
  # First filter the votto dataset to be the most common pitches:
  filter(pitcher == 669456 & str_like(game_date, "2024%")) %>%
  
  # Next, using the description, type, and events columns - let's define some useful  
  # columns that will help us calculate stats for each of the different pitch types: 
  
  mutate(# Create an indicator for whether or not Votto made an attempt at the pitch:
    swing = ifelse(description %in%
                     c("bunt_foul_tip",
                       "foul", "foul_bunt",
                       "foul_pitchout",
                       "foul_tip", "hit_into_play",
                       "missed_bunt", "swinging_strike",
                       "swinging_strike_blocked"), 
                   1, 0),
    
    
    # Create an indicator for whether or not Votto missed in his attempt:  
    miss = ifelse(description %in%
                    c("missed_bunt", "swinging_strike",
                      "swinging_strike_blocked"), 
                  1, 0),
    
    # Using the type and events column, create indicators for each hit type:
    single = ifelse(type == "X" & events == "single", 1, 0),
    double = ifelse(type == "X" & events == "double", 1, 0),
    triple = ifelse(type == "X" & events == "triple", 1, 0),
    home_run = ifelse(type == "X" & events == "home_run", 1, 0)) %>%
  
  # Now can calculate various stats at the pitch type level:
  group_by(pitch_type) %>%
  
  # Use the summarise function to calculate the frequencies for each of
  # these indicators:
  summarise(n_pitches = n(),
            n_swings = sum(swing, na.rm = TRUE),
            n_miss = sum(miss, na.rm = TRUE),
            n_singles = sum(single, na.rm = TRUE),
            n_doubles = sum(double, na.rm = TRUE),
            n_triples = sum(triple, na.rm = TRUE),
            n_home_runs = sum(home_run, na.rm = TRUE),
            spin = mean(release_spin_rate, na.rm = TRUE)) %>%  
  
  # Using these sums can calculate commonly seen baseball stats:
  mutate(swing_rate = round(n_swings / n_pitches, 3),
         miss_rate = round(n_miss / n_swings, 3),
         batting_average = round((n_singles + n_doubles + n_triples + n_home_runs) / n_swings, 3),
         slugging_percentage = round((n_singles + 2 * n_doubles + 3 * n_triples + 4 * n_home_runs) / n_swings, 3),
         ops = round(batting_average + slugging_percentage, 3), 
         spin = round(spin, 0))  


## 2024
statcast_24 <- statcast |> 
  filter(str_like(game_date, "2024%")) |> 
  slice_head(n = 100)


### Function to return players name from pitcher ID
player_name <- function(pitcherID){
  name_list <- playername_lookup(pitcherID)[1:2]
  
  name_vector <- as.vector(unlist(name_list))
  
  name <- paste(name_vector[1], name_vector[2])
  
  return(name)
}




pitchers_24 <- map(statcast_24$pitcher, player_name) |> 
  unlist() |> 
  as.data.frame()


chadwich <- baseballr::chadwick_player_lu()


chadwick <- read.csv("SFBB Player ID Map - PLAYERIDMAP.csv")

chadwick_mlb <- chadwick |> 
  select(MLBID, MLBNAME)

statcast_pitches <- statcast |> 
  group_by()

playername_lookup(643327)
