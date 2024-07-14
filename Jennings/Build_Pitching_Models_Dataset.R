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
         umpire, sv_id, fielder_2, pitcher.1:fielder_9, home_score:swing_length)) |> 
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
    Season = game_year,
    
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
  group_by(pitcher_id, p_throws, pitch_name, Season) %>%
  
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
            horizontal_break = mean(horizontal_movement_in, na.rm = TRUE)) |>   
  
  # Using these sums can calculate commonly seen baseball stats:
  mutate(swing_pct = round(n_swings / n_pitches, 6) * 100,
         whiff_pct = round(n_miss / n_swings, 6) * 100,
         swing_and_miss_pct = round(swing_strikes / n_pitches, 6) * 100,
         spin = round(spin, 0),
         Season = factor(Season)) |> 
  ungroup()


### dataset with at least 75 pitches thrown for a pitch
statcast_pitch_summary_stats_q <- statcast_pitch_summary_stats |> 
  filter(n_pitches >= 100)

##### color vector
pitch_colors <- c(
  "4-Seam Fastball" = "firebrick1",
  "Cutter" = "maroon",
  "Sinker" = "darkgreen",
  "Changeup" = "darkorange",
  "Forkball" = "coral",
  "Split-Finger" = "brown4",
  "Slider" = "purple3",
  "Sweeper" = "darkmagenta",
  "Slurve" = "violet",
  "Curveball" = "dodgerblue",
  "Knuckle Curve" = "darkblue",
  "Slow Curve" = "royalblue3"
)


### vertical by horizontal movement plot
statcast_pitch_summary_stats |> 
  filter(n_pitches >= 100) |> 
  # for pitcher's POV
  mutate(horizontal_break = -horizontal_break) |> 
  # plot
  ggplot(aes(horizontal_break, induced_vertical_break, color = pitch_name)) +
  geom_point(size = 2, alpha = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  scale_color_manual(values = pitch_colors) +
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     limits = c(-25, 25),
                     labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     limits = c(-25, 25),
                     labels = scales::number_format(suffix = "\"")) +
  labs(
    x = "Horizontal Break",
    y = "Induced Vertical Break",
    color = "Pitch Name",
    title = "Avg. Pitch Movement",
    subtitle = "2020-24 | Pitcher's POV",
    caption = "Data: Baseball Savant via baseballr"
  ) + 
  facet_wrap(~ Season,
             nrow = 2,
             ncol = 3) +
  theme(
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    strip.background = element_rect(fill = "navy", color = "black", size = 1),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 14, hjust = 0.5),
    axis.text.y = element_text(size = 14, hjust = 0.5),
    legend.position = "bottom",
    plot.caption = element_text(size = 10)
  )


### vertical by horizontal movement plot for sliders and sweepers
statcast_pitch_summary_stats_q |> 
  filter(n_pitches >= 100 & pitch_name %in% c("Sweeper", "Slider")) |> 
  # for pitcher's POV
  mutate(horizontal_break = -horizontal_break,
         Season = factor(Season, levels = c(2020, 2021, 2022, 2023, 2024))) |> 
  # plot
  ggplot(aes(horizontal_break, induced_vertical_break, color = pitch_name)) +
  geom_point(size = 2, alpha = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  scale_color_manual(values = c("firebrick", "darkorange")) +
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     limits = c(-25, 25),
                     labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     limits = c(-25, 25),
                     labels = scales::number_format(suffix = "\"")) +
  labs(
    x = "Horizontal Break",
    y = "Induced Vertical Break",
    color = "Pitch Name",
    title = "Avg. Pitch Movement",
    subtitle = "2020-24 | Pitcher's POV",
    caption = "Data: Baseball Savant via baseballr"
  ) + 
  facet_wrap(~ Season,
             nrow = 2,
             ncol = 3) +
  theme(
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    strip.background = element_rect(fill = "navy", color = "black", size = 1),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 14, hjust = 0.5),
    axis.text.y = element_text(size = 14, hjust = 0.5),
    legend.position = "bottom",
    plot.caption = element_text(size = 10)
  )

### vertical by horizontal movement plot for slurves, sweepers, curveballs, and sliders
statcast_pitch_summary_stats_q |> 
  filter(pitch_name %in% c("Sweeper", "Slider", "Curveball", "Slurve")) |> 
  # for pitcher's POV
  mutate(horizontal_break = -horizontal_break,
         Season = factor(Season, levels = c(2020, 2021, 2022, 2023, 2024))) |> 
  # plot
  ggplot(aes(horizontal_break, induced_vertical_break, color = pitch_name)) +
  geom_point(size = 2, alpha = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  scale_color_manual(values = c("firebrick", "dodgerblue", "goldenrod1", "forestgreen")) +
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     limits = c(-25, 25),
                     labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     limits = c(-25, 25),
                     labels = scales::number_format(suffix = "\"")) +
  labs(
    x = "Horizontal Break",
    y = "Induced Vertical Break",
    color = "Pitch Name",
    title = "Avg. Pitch Movement",
    subtitle = "2020-24 | Pitcher's POV",
    caption = "Data: Baseball Savant via baseballr"
  ) + 
  facet_wrap(~ Season,
             nrow = 2,
             ncol = 3) +
  theme(
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    strip.background = element_rect(fill = "navy", color = "black", size = 1),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 14, hjust = 0.5),
    axis.text.y = element_text(size = 14, hjust = 0.5),
    legend.position = "bottom",
    plot.caption = element_text(size = 10)
  )

### vertical by horizontal movement plot for 4-seam fastball
statcast_pitch_summary_stats |> 
  # for pitcher's POV
  mutate(horizontal_break = -horizontal_break) |> 
  filter(pitch_name == "4-Seam Fastball" & n_pitches >= 100) |> 
  # plot
  ggplot(aes(horizontal_break, induced_vertical_break, color = Season)) +
  geom_point(size = 2, alpha = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  scale_y_continuous(labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(labels = scales::number_format(suffix = "\"")) +
  ggthemes::scale_color_pander() +
  labs(
    x = "Horizontal Break",
    y = "Induced Vertical Break",
    color = "Pitch Name",
    title = "Avg. Four-Seam Fastball Movement",
    subtitle = "2020-24 | Pitcher's POV",
    caption = "Data: Baseball Savant via baseballr"
  ) + 
  theme(
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    strip.background = element_rect(fill = "navy", color = "black", size = 1),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 14, hjust = 0.5),
    axis.text.y = element_text(size = 14, hjust = 0.5),
    legend.position = "bottom",
    plot.caption = element_text(size = 10)
  ) #+
  #geom_smooth(se = FALSE)


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
         Season = factor(Season)) |>
  select(Season, p_throws = Throws, PlayerName, pitcher_id = xMLBAMID, 
         Role, pitch_name, stuff_plus)



### Join the datasets
pitching_models <- statcast_pitch_summary_stats_q |> 
  left_join(fangraphs_long, by = c("Season", "pitcher_id", "p_throws", "pitch_name")) |> 
  # move player name first
  relocate(PlayerName, .before = pitcher_id) |> 
  select(player_name = PlayerName, pitcher_id:swing_and_miss_pct, role = Role, stuff_plus) |> 
  mutate(season = factor(Season, levels = c(2020, 2021, 2022, 2023, 2024))) |> 
  select(-Season)

### NOTE: For now, pitchers' names, role, and stuff+ are NA for Sweeper and Slurve because those pitch names
### do not register in the FanGraphs dataset. This is not as important for now because we are looking at pitches
### thrown more than 5% of the time total. 

### UPDATED NOTE: Joined Sweepers with sliders and slurves with curveballs in previous dataset so NAs are gone

### Write to .csv
#write.csv(pitching_models, "mlb_pitching_stats_2020-24.csv")






# HEX GRAPHS --------------------------------------------------------------
### ALL PITCHERS AND ALL YEARS
#### qualified fastball pitchers for 2020-24
ff_qualified <- pitching_models |>
  filter(pitch_name == "4-Seam Fastball")

#### mean of movement for each year
mean_movement <- statcast_filter |> 
  filter(pitcher_id %in% ff_qualified$pitcher_id &
           plate_z >= 0.5 & plate_z <= 4.75 &
           plate_x >= (-.7083 - 1) & plate_x <= (.7083 + 1) & 
           pitch_name == "4-Seam Fastball") |> 
  group_by(Season) |> 
  summarize(
    mean_horizontal = mean(horizontal_movement_in, na.rm = TRUE),
    mean_vertical = mean(vertical_movement_in, na.rm = TRUE)
  )

#### whiff rate plot on movement
statcast_filter |> 
  filter(pitcher_id %in% ff_qualified$pitcher_id &
           plate_z >= 0.5 & plate_z <= 4.75 &
           plate_x >= (-.7083 - 1) & plate_x <= (.7083 + 1) & 
           pitch_name == "4-Seam Fastball") |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = horizontal_movement_in, y = vertical_movement_in, z = miss), 
                   binwidth = c(2, 2), fun = mean,
                   color = "black") +
  scale_fill_gradient(low = "royalblue4",
                      high = "goldenrod") +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  geom_vline(xintercept = mean(mean_movement$mean_horizontal, na.rm = TRUE),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  geom_hline(yintercept = mean(ff_qualified$induced_vertical_break, na.rm = TRUE), 
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # geom_zone(linecolor = "black") +
  # geom_plate() +
  coord_fixed() +
  labs(title = "Whiff% of 4-Seam Fastballs Thrown by All Pitchers in 2024",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Movement (in)",
       y = "Vertical Movement (in)", 
       fill = "Whiff%") +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    strip.background = element_rect(fill = "navy", color = "black", size = 1),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    plot.caption = element_text(size = 10)
  ) +
  facet_wrap(~ season)


#### xwOBA plot on movement
statcast_filter |> 
  filter(pitcher_id %in% ff_qualified$pitcher_id &
           plate_z >= 0.5 & plate_z <= 4.75 &
           plate_x >= (-.7083 - 1) & plate_x <= (.7083 + 1) &
           pitch_name == "4-Seam Fastball" & !is.na(estimated_woba_using_speedangle)) |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = horizontal_movement_in, y = vertical_movement_in, z = estimated_woba_using_speedangle), 
                   binwidth = c(2, 2), fun = mean,
                   color = "black") +
  scale_fill_gradient(low = "royalblue4",
                      high = "goldenrod") +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  geom_vline(xintercept = mean(mean_movement$mean_horizontal, na.rm = TRUE),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  geom_hline(yintercept = mean(ff_qualified$induced_vertical_break, na.rm = TRUE), 
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # geom_zone(linecolor = "black") +
  # geom_plate() +
  coord_fixed() +
  labs(title = "Location of 4-Seam Fastball Thrown by All Pitchers in 2024",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Movement (in)",
       y = "Vertical Movement (in)", 
       fill = "xwOBA") +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    strip.background = element_rect(fill = "navy", color = "black", size = 1),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    plot.caption = element_text(size = 10)
  ) +
  facet_wrap(~ season)


#### xwoba frequency
statcast_filter |> 
  filter(pitcher_id %in% ff_qualified$pitcher_id &
           plate_z >= 0.5 & plate_z <= 4.75 &
           plate_x >= (-.7083 - 1) & plate_x <= (.7083 + 1) &
           pitch_name == "4-Seam Fastball" & !is.na(estimated_woba_using_speedangle)) |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  geom_hex(aes(x = horizontal_movement_in, y = vertical_movement_in), 
                   binwidth = c(2, 2),
                   color = "black") +
  scale_fill_gradient(low = "royalblue4",
                      high = "goldenrod") +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  geom_vline(xintercept = mean(mean_movement$mean_horizontal, na.rm = TRUE),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  geom_hline(yintercept = mean(ff_qualified$induced_vertical_break, na.rm = TRUE), 
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # geom_zone(linecolor = "black") +
  # geom_plate() +
  coord_fixed() +
  labs(title = "Location of 4-Seam Fastball Thrown by All Pitchers in 2024",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Movement (in)",
       y = "Vertical Movement (in)", 
       fill = "Frequency") +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    strip.background = element_rect(fill = "navy", color = "black", size = 1),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    plot.caption = element_text(size = 10)
  ) +
  facet_wrap(~ season)

# AGGREGATED PITCHING MODELS HEX GRAPHS -----------------------------------
## 4-Seam Fastballs
### Velocity
pitching_models |> 
  filter(pitch_name == "4-Seam Fastball") |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = velocity), 
                   binwidth = c(2, 2), fun = mean,
                   color = "black") +
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = mean(ff_qualified$velocity)) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  geom_vline(aes(xintercept = mean(horizontal_break)),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  geom_hline(aes(yintercept = mean(induced_vertical_break)), 
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # geom_zone(linecolor = "black") +
  # geom_plate() +
  coord_fixed() +
  labs(title = "Avg. Velo of 4-Seam Fastballs by Movement",
       subtitle = "All Pitchers With 100 or More Fastballs Thrown in a Year",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Movement (in)",
       y = "Vertical Movement (in)", 
       fill = "Avg. Velocity") +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    strip.background = element_rect(fill = "navy", color = "black", size = 1),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 10),
    axis.title.x = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.key.size = unit(0.35, "inches"),
    legend.text = element_text(size = 12)
  ) +
  facet_wrap(~ season)

### xwOBA
pitching_models |> 
  filter(pitch_name == "4-Seam Fastball") |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = xwOBA), 
                   binwidth = c(2, 2), fun = mean,
                   color = "black") +
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = mean(ff_qualified$xwOBA)) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  geom_vline(aes(xintercept = mean(horizontal_break)),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  geom_hline(aes(yintercept = mean(induced_vertical_break)), 
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # geom_zone(linecolor = "black") +
  # geom_plate() +
  coord_fixed() +
  labs(title = "xwOBA of 4-Seam Fastballs by Movement",
       subtitle = "All Pitchers With 100 or More Fastballs Thrown in a Year",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Movement (in)",
       y = "Vertical Movement (in)", 
       fill = "xwOBA") +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    strip.background = element_rect(fill = "navy", color = "black", size = 1),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 10),
    axis.title.x = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.key.size = unit(0.35, "inches"),
    legend.text = element_text(size = 12)
  ) +
  facet_wrap(~ season)

### whiff%
pitching_models |> 
  filter(pitch_name == "4-Seam Fastball") |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = whiff_pct), 
                   binwidth = c(2, 2), fun = mean,
                   color = "black") +
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = mean(ff_qualified$whiff_pct)) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  geom_vline(aes(xintercept = mean(horizontal_break)),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  geom_hline(aes(yintercept = mean(induced_vertical_break)), 
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # geom_zone(linecolor = "black") +
  # geom_plate() +
  coord_fixed() +
  labs(title = "Whiff% of 4-Seam Fastballs by Movement",
       subtitle = "All Pitchers With 100 or More Fastballs Thrown in a Year",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Movement (in)",
       y = "Vertical Movement (in)", 
       fill = "Whiff%") +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    strip.background = element_rect(fill = "navy", color = "black", size = 1),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 10),
    axis.title.x = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.key.size = unit(0.35, "inches"),
    legend.text = element_text(size = 12)
  ) +
  facet_wrap(~ season)


### spin rate
pitching_models |> 
  filter(pitch_name == "4-Seam Fastball") |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = spin), 
                   binwidth = c(2, 2), fun = mean,
                   color = "black") +
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = mean(ff_qualified$spin)) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  geom_vline(aes(xintercept = mean(horizontal_break)),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  geom_hline(aes(yintercept = mean(induced_vertical_break)), 
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # geom_zone(linecolor = "black") +
  # geom_plate() +
  coord_fixed() +
  labs(title = "Spin Rate of 4-Seam Fastballs by Movement",
       subtitle = "All Pitchers With 100 or More Fastballs Thrown in a Year",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Movement (in)",
       y = "Vertical Movement (in)", 
       fill = "Spin (RPM)") +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    strip.background = element_rect(fill = "navy", color = "black", size = 1),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 10),
    axis.title.x = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.key.size = unit(0.35, "inches"),
    legend.text = element_text(size = 12)
  ) +
  facet_wrap(~ season)


### Stuff+
pitching_models |> 
  filter(pitch_name == "4-Seam Fastball") |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = stuff_plus), 
                   binwidth = c(2, 2), fun = mean,
                   color = "black") +
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = 100) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  geom_vline(aes(xintercept = mean(horizontal_break)),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  geom_hline(aes(yintercept = mean(induced_vertical_break)), 
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # geom_zone(linecolor = "black") +
  # geom_plate() +
  coord_fixed() +
  labs(title = "Stuff+ of 4-Seam Fastballs by Movement",
       subtitle = "All Pitchers With 100 or More Fastballs Thrown in a Year",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Movement (in)",
       y = "Vertical Movement (in)", 
       fill = "Stuff+") +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    strip.background = element_rect(fill = "navy", color = "black", size = 1),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 10),
    axis.title.x = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.key.size = unit(0.35, "inches"),
    legend.text = element_text(size = 12)
  ) +
  facet_wrap(~ season)


## SLIDERS
### qualified slider/sweeper pitchers
sl_qualified <- pitching_models |> 
  filter(pitch_name == "Slider")


### velocity
pitching_models |> 
  filter(pitch_name == "Slider") |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = velocity), 
                   binwidth = c(2, 2), fun = mean,
                   color = "black") +
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = mean(sl_qualified$velocity)) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  geom_vline(aes(xintercept = mean(horizontal_break)),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  geom_hline(aes(yintercept = mean(induced_vertical_break)), 
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # geom_zone(linecolor = "black") +
  # geom_plate() +
  coord_fixed() +
  labs(title = "Avg. Velo of Sliders by Movement",
       subtitle = "All Pitchers With 100 or More Sliders Thrown in a Year",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Movement (in)",
       y = "Vertical Movement (in)", 
       fill = "Avg. Velocity") +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    strip.background = element_rect(fill = "navy", color = "black", size = 1),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 10),
    axis.title.x = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.key.size = unit(0.35, "inches"),
    legend.text = element_text(size = 12)
  ) +
  facet_wrap(~ season)


### xwOBA
pitching_models |> 
  filter(pitch_name == "Slider") |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = xwOBA), 
                   binwidth = c(2, 2), fun = mean,
                   color = "black") +
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = mean(sl_qualified$xwOBA)) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  geom_vline(aes(xintercept = mean(horizontal_break)),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  geom_hline(aes(yintercept = mean(induced_vertical_break)), 
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # geom_zone(linecolor = "black") +
  # geom_plate() +
  coord_fixed() +
  labs(title = "xwOBA of Sliders by Movement",
       subtitle = "All Pitchers With 100 or More Sliders Thrown in a Year",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Movement (in)",
       y = "Vertical Movement (in)", 
       fill = "xwOBA") +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    strip.background = element_rect(fill = "navy", color = "black", size = 1),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 10),
    axis.title.x = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.key.size = unit(0.35, "inches"),
    legend.text = element_text(size = 12)
  ) +
  facet_wrap(~ season)


### whiff%
pitching_models |> 
  filter(pitch_name == "Slider") |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = whiff_pct), 
                   binwidth = c(2, 2), fun = mean,
                   color = "black") +
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = mean(sl_qualified$whiff_pct)) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  geom_vline(aes(xintercept = mean(horizontal_break)),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  geom_hline(aes(yintercept = mean(induced_vertical_break)), 
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # geom_zone(linecolor = "black") +
  # geom_plate() +
  coord_fixed() +
  labs(title = "Whiff% of Sliders by Movement",
       subtitle = "All Pitchers With 100 or More Sliders Thrown in a Year",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Movement (in)",
       y = "Vertical Movement (in)", 
       fill = "Whiff%") +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    strip.background = element_rect(fill = "navy", color = "black", size = 1),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 10),
    axis.title.x = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.key.size = unit(0.35, "inches"),
    legend.text = element_text(size = 12)
  ) +
  facet_wrap(~ season)


### spin rate
pitching_models |> 
  filter(pitch_name == "Slider") |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = spin), 
                   binwidth = c(2, 2), fun = mean,
                   color = "black") +
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = mean(sl_qualified$spin)) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  geom_vline(aes(xintercept = mean(horizontal_break)),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  geom_hline(aes(yintercept = mean(induced_vertical_break)), 
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # geom_zone(linecolor = "black") +
  # geom_plate() +
  coord_fixed() +
  labs(title = "Spin Rate of Sliders by Movement",
       subtitle = "All Pitchers With 100 or More Sliders Thrown in a Year",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Movement (in)",
       y = "Vertical Movement (in)", 
       fill = "Spin (RPM)") +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    strip.background = element_rect(fill = "navy", color = "black", size = 1),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 10),
    axis.title.x = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.key.size = unit(0.35, "inches"),
    legend.text = element_text(size = 10)
  ) +
  facet_wrap(~ season)


### Stuff+
pitching_models |> 
  filter(pitch_name == "Slider") |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = horizontal_break, y = induced_vertical_break,
                       z = stuff_plus), 
                   binwidth = c(2, 2), fun = mean,
                   color = "black") +
  scale_fill_gradient2(low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick2",
                       midpoint = 100) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  geom_vline(aes(xintercept = mean(horizontal_break)),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  geom_hline(aes(yintercept = mean(induced_vertical_break)), 
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # geom_zone(linecolor = "black") +
  # geom_plate() +
  coord_fixed() +
  labs(title = "Stuff+ of Sliders by Movement",
       subtitle = "All Pitchers With 100 or More Sliders Thrown in a Year",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Movement (in)",
       y = "Vertical Movement (in)", 
       fill = "Stuff+") +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    strip.background = element_rect(fill = "navy", color = "black", size = 1),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 10),
    axis.title.x = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.key.size = unit(0.35, "inches"),
    legend.text = element_text(size = 12)
  ) +
  facet_wrap(~ season)






#### whiff rate plot on movement
statcast_filter |> 
  filter(pitcher_id %in% sl_qualified$pitcher_id & Season == 2024 &
           plate_z >= 0.5 & plate_z <= 4.75 &
           plate_x >= (-.7083 - 1) & plate_x <= (.7083 + 1) & 
           pitch_name == "Slider") |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = horizontal_movement_in, y = vertical_movement_in, z = miss), 
                   binwidth = c(2, 2), fun = mean,
                   color = "black") +
  scale_fill_gradient(low = "royalblue4",
                      high = "goldenrod") +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     limits = c(-25, 25),
                     labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     limits = c(-25, 25),
                     labels = scales::number_format(suffix = "\"")) +
  # geom_zone(linecolor = "black") +
  # geom_plate() +
  coord_fixed() +
  labs(title = "Whiff% of Sliders Thrown by All Pitchers in 2024",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Movement (in)",
       y = "Vertical Movement (in)", 
       fill = "Whiff%") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.50),
        legend.position = "bottom")


#### xwOBA plot on movement
statcast_filter |> 
  filter(pitcher_id %in% sl_qualified$pitcher_id & Season == 2024 &
           plate_z >= 0.5 & plate_z <= 4.75 &
           plate_x >= (-.7083 - 1) & plate_x <= (.7083 + 1) &
           pitch_name == "Slider" & !is.na(estimated_woba_using_speedangle)) |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = horizontal_movement_in, y = vertical_movement_in, z = estimated_woba_using_speedangle), 
                   binwidth = c(2, 2), fun = mean,
                   color = "black") +
  scale_fill_gradient(low = "royalblue4",
                      high = "goldenrod") +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     limits = c(-25, 25),
                     labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     limits = c(-25, 25),
                     labels = scales::number_format(suffix = "\"")) +
  # geom_zone(linecolor = "black") +
  # geom_plate() +
  coord_fixed() +
  labs(title = "Location of Sliders Thrown by All Pitchers in 2024",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Movement (in)",
       y = "Vertical Movement (in)", 
       fill = "xwOBA") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.50),
        legend.position = "bottom")


#### NEXT:
##### CHECK FREQUENCY OF LOCATIONS
##### USE OTHER VARIABLES AS Z VALUE



# TEST CODE ---------------------------------------------------------------

########## SLIDERS DO NOT BREAK ANYWHERE AS MUCH AS SWEEPERS
########## MAYBE THAT'S WHY SLIDERS ARE SO EFFECTIVE IN STUFF+
statcast_pitch_summary_stats_q |> 
  filter(pitch_name %in% c("Sweeper", "Slider", "Curveball", "Slurve")) |>  # & horizontal_break >= 0) |> removing negative sliders doesn't impact avg.
  group_by(pitch_name) |> 
  summarize(
    spin = mean(spin, na.rm = TRUE),
    induced_vertical_break = mean(induced_vertical_break, na.rm = TRUE),
    horizontal_break = mean(horizontal_break, na.rm = TRUE),
    whiff_pct = mean(whiff_pct, na.rm = TRUE)
  )

### velocity
statcast_filter |> 
  filter(pitcher_id %in% ff_qualified$pitcher_id &
           plate_z >= 0.5 & plate_z <= 4.75 &
           plate_x >= (-.7083 - 1) & plate_x <= (.7083 + 1) &
           pitch_name == "4-Seam Fastball" & !is.na(estimated_woba_using_speedangle)) |> 
  mutate(
    Season = factor(Season),
    Season = fct_relevel(Season, 2022, after = 2021)
  ) |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = horizontal_movement_in, y = vertical_movement_in, 
                       z = release_speed), 
                   binwidth = c(2, 2), fun = mean,
                   color = "black") +
  scale_fill_gradient(low = "royalblue4",
                      high = "goldenrod") +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  geom_vline(xintercept = mean(mean_movement$mean_horizontal, na.rm = TRUE),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  geom_hline(yintercept = mean(ff_qualified$induced_vertical_break, na.rm = TRUE), 
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # geom_zone(linecolor = "black") +
  # geom_plate() +
  coord_fixed() +
  labs(title = "Location of 4-Seam Fastball Thrown by All Pitchers in 2024",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Movement (in)",
       y = "Vertical Movement (in)", 
       fill = "Avg. Velocity") +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    strip.background = element_rect(fill = "navy", color = "black", size = 1),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    plot.caption = element_text(size = 10)
  ) +
  facet_wrap(~ season)

statcast_filter |> 
  filter(pitcher_id %in% ff_qualified$pitcher_id &
           plate_z >= 0.5 & plate_z <= 4.75 &
           plate_x >= (-.7083 - 1) & plate_x <= (.7083 + 1) &
           pitch_name == "4-Seam Fastball" & !is.na(estimated_woba_using_speedangle)) |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = horizontal_movement_in, y = vertical_movement_in, 
                       z = release_speed), 
                   binwidth = c(2, 2), fun = mean,
                   color = "black") +
  scale_fill_gradient(low = "royalblue4",
                      high = "goldenrod") +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  geom_vline(xintercept = mean(mean_movement$mean_horizontal, na.rm = TRUE),
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  geom_hline(yintercept = mean(ff_qualified$induced_vertical_break, na.rm = TRUE), 
             color = "firebrick1", linewidth = 1.25, linetype = "dashed") +
  scale_y_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  scale_x_continuous(breaks = seq(-20, 20, 5),
                     labels = scales::number_format(suffix = "\"")) +
  # geom_zone(linecolor = "black") +
  # geom_plate() +
  coord_fixed() +
  labs(title = "Location of 4-Seam Fastball Thrown by All Pitchers in 2024",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Movement (in)",
       y = "Vertical Movement (in)", 
       fill = "Avg. Velocity") +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    strip.background = element_rect(fill = "navy", color = "black", size = 1),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    plot.caption = element_text(size = 10)
  ) +
  facet_wrap(~ Season)


### Whiff% Hexbin of sliders thrown by all qualified pitchers in 2024
#### qualified sliders pitchers in 2024
sl_qualified <- pitching_models |> 
  filter(pitch_name == "Slider" & Season == 2024)

#### whiff rate plot
statcast_filter |> 
  filter(pitcher_id %in% sl_qualified$pitcher_id & Season == 2024 &
           plate_z >= 0.5 & plate_z <= 4.75 &
           plate_x >= (-.7083 - 1) & plate_x <= (.7083 + 1) &
           pitch_name == "Slider") |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = plate_x, y = plate_z, z = miss), 
                   binwidth = c(1 / 12, 1 / 12), fun = mean,
                   color = "black") +
  scale_fill_gradient(low = "royalblue4",
                      high = "goldenrod") +
  geom_zone(linecolor = "black") +
  geom_plate() +
  coord_fixed() +
  labs(title = "Location of Sliders Thrown by All Pitchers in 2024",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Location (ft)",
       y = "Vertical Location (ft)", 
       fill = "Whiff%") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.50),
        legend.position = "bottom")

#### xwOBA plot
statcast_filter |> 
  filter(pitcher_id %in% sl_qualified$pitcher_id & Season == 2024 &
           plate_z >= 0.5 & plate_z <= 4.75 &
           plate_x >= (-.7083 - 1) & plate_x <= (.7083 + 1) &
           pitch_name == "Slider" & !is.na(estimated_woba_using_speedangle)) |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = plate_x, y = plate_z, z = estimated_woba_using_speedangle), 
                   binwidth = c(1 / 12, 1 / 12), fun = mean,
                   color = "black") +
  scale_fill_gradient(low = "royalblue4",
                      high = "goldenrod") +
  geom_zone(linecolor = "black") +
  geom_plate() +
  coord_fixed() +
  labs(title = "Location of Sliders Thrown by All Pitchers in 2024",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Location (ft)",
       y = "Vertical Location (ft)", 
       fill = "xwOBA") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.50),
        legend.position = "bottom")



#### Building a strike zone and home plate
##### Strike Zone
geom_zone <- function(top = 3.75, bottom = 1.5, linecolor = "gray60"){
  geom_rect(xmin = -.7083, xmax = .7083, ymin = bottom, ymax = top,
            alpha = 0, color = linecolor, linewidth = 1.5)
}

#### Home plate
geom_plate <- function(){
  df <- data.frame(x = c(-.7083, .7083, .7083 ,0, -.7083), y = c(0, 0, -.25, -.5, -.25))
  plate <- geom_polygon(data = df, aes(x = x, y = y), fill = "white", color = "gray60", linewidth = 1.25)
  plate
}

## 
### plot
statcast_filter |> 
  filter(pitcher_id == 694973 & pitch_name == "4-Seam Fastball") |> 
  ggplot() + 
  geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  geom_zone(linecolor = "black") +
  geom_plate() +
  coord_fixed() +
  labs(title = "Location of Four-Seam Fastballs Thrown by Paul Skenes",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Location (ft)",
       y = "Vertical Location (ft)", 
       color = "Miss") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.50))



### Whiff% Hexbin of 4-Seam Fastballs thrown by Paul Skenes
statcast_filter |> 
  filter(pitcher_id == 694973 & pitch_name == "4-Seam Fastball") |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = plate_x, y = plate_z, z = miss), 
                   binwidth = c(3 / 12, 3 / 12), fun = mean,
                   color = "black") +
  scale_fill_gradient(low = "royalblue4",
                      high = "goldenrod") +
  geom_zone(linecolor = "black") +
  geom_plate() +
  coord_fixed() +
  labs(title = "Location of Four-Seam Fastballs Thrown by Paul Skenes",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Location (ft)",
       y = "Vertical Location (ft)", 
       fill = "Whiff%") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.50),
        legend.position = "bottom")


### Whiff% Hexbin of 4-Seam Fastballs thrown by all qualified pitchers in 2024
#### qualified fastball pitchers in 2024
ff_qualified_24 <- pitching_models |>
  filter(pitch_name == "4-Seam Fastball" & Season == 2024)

#### whiff% plot
statcast_filter |>
  filter(pitcher_id %in% ff_qualified_24$pitcher_id & Season == 2024 &
           plate_z >= 0.5 & plate_z <= 4.75 &
           plate_x >= (-.7083 - 1) & plate_x <= (.7083 + 1) &
           pitch_name == "4-Seam Fastball") |>
  ggplot() +
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) +
  stat_summary_hex(aes(x = plate_x, y = plate_z, z = miss),
                   binwidth = c(1 / 12, 1 / 12), fun = mean,
                   color = "black") +
  scale_fill_gradient(low = "royalblue4",
                      high = "goldenrod") +
  geom_zone(linecolor = "black") +
  geom_plate() +
  coord_fixed() +
  labs(title = "Location of 4-Seam Fastballs Thrown by All Pitchers in 2024",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Location (ft)",
       y = "Vertical Location (ft)",
       fill = "Whiff%") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.50),
        legend.position = "bottom")


#### xwOBA plot
statcast_filter |> 
  filter(pitcher_id %in% ff_qualified$pitcher_id & Season == 2024 &
           plate_z >= 0.5 & plate_z <= 4.75 &
           plate_x >= (-.7083 - 1) & plate_x <= (.7083 + 1) &
           pitch_name == "4-Seam Fastball" & !is.na(estimated_woba_using_speedangle)) |> 
  ggplot() + 
  # geom_point(aes(x = plate_x, y = plate_z, color = factor(miss)), alpha = 0.75) + 
  stat_summary_hex(aes(x = plate_x, y = plate_z, z = estimated_woba_using_speedangle), 
                   binwidth = c(1 / 12, 1 / 12), fun = mean,
                   color = "black") +
  scale_fill_gradient(low = "royalblue4",
                      high = "goldenrod") +
  geom_zone(linecolor = "black") +
  geom_plate() +
  coord_fixed() +
  labs(title = "Location of Sliders Thrown by All Pitchers in 2024",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Location (ft)",
       y = "Vertical Location (ft)", 
       fill = "xwOBA") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.50),
        legend.position = "bottom")



#### Build strike zone
# Outside layer
top_zone <- unit(mean(statcast_filter$sz_top* 12, na.rm = TRUE), "inch")
bot_zone <- unit(mean(statcast_filter$sz_bot * 12, na.rm = TRUE), "inch")
left_zone <- unit(-17/2, "inch")
right_zone <- unit(17/2, "inch")
strike_zone_df <- data.frame(
  x = c(left_zone, left_zone, right_zone, right_zone, left_zone),
  y = c(bot_zone, top_zone, top_zone, bot_zone, bot_zone)
)

# Horizontal lines to break the strike zone into ninths
top_zone <- unit(mean(statcast_filter$sz_top * 12, na.rm = TRUE), "inch")
bot_zone <- unit(mean(statcast_filter$sz_bot * 12, na.rm = TRUE), "inch")
left_third <- unit(-17/2 + 17/3, "inch")
right_third <- unit(17/2 - 17/3, "inch")
inside_strike_zone_df <- data.frame(
  x = c(left_third, left_third, right_third, right_third, left_third),
  y = c(bot_zone, top_zone, top_zone, bot_zone, bot_zone)
)

# Vertical lines to break the strike zone into ninths
top_third <- top_zone - ((top_zone - bot_zone)/3)
bot_third <- ((top_zone - bot_zone)/3) + bot_zone
vertical_strike_zone_df <- data.frame(
  x = c(left_zone, left_zone, right_zone, right_zone, left_zone),
  y = c(bot_third, top_third, top_third, bot_third, bot_third)
)

# plate


### plot
statcast_filter |> 
  filter(pitcher_id == 694973 & pitch_name == "4-Seam Fastball") |> 
  mutate(
    plate_x_in = plate_x * 12,
    plate_z_in = plate_z * 12
  ) |> 
  ggplot() + 
  geom_point(aes(x = plate_x_in, y = plate_z_in, color = pitch_name), alpha = 0.75) + 
  geom_path(data = strike_zone_df, aes(x, y), linewidth = 1.5, color = "black") + 
  geom_path(data = inside_strike_zone_df, aes(x, y), color = "black") +
  geom_path(data = vertical_strike_zone_df, aes(x, y), color = "black") +
  geom_segment(aes(x = -0.708 * 12, y = 0.15 * 12, xend = 0.708 * 12, yend = 0.15 * 12), 
               size = 1, color = "black") +
  geom_segment(aes(x = -0.708 * 12, y = 0.3 * 12, xend = -0.708 * 12, yend = 0.15 * 12), 
               size = 1, color = "black") + 
  geom_segment(aes(x = -0.708 * 12, y = 0.3 * 12, xend = 0 * 12, yend = 0.5 * 12), 
               size = 1, 
               color = "black") + 
  geom_segment(aes(x = 0 * 12, y = 0.5 * 12, xend = 0.708 * 12, yend = 0.3 * 12),
               size = 1, 
               color = "black") + 
  geom_segment(aes(x = 0.708 * 12, y = 0.3 * 12, xend = 0.708 * 12, yend = 0.15 * 12), 
               size = 1, 
               color = "black") + 
  #geom_text_repel(aes(label = count)) +
  coord_fixed() +
  labs(title = "Location of Pitches Thrown Against Oneil Cruz on August 5th by Pitch Type",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Location (in)",
       y = "Vertical Location (in)", 
       color = "Pitch Type") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.50))


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


#### convert to plotly object
# statcast_pitch_summary_stats |> 
#   
#   # for pitcher's POV
#   mutate(horizontal_break = -horizontal_break,
#          game_year = factor(game_year)) |> 
#   
#   # filter for FF only
#   filter(pitch_name == "4-Seam Fastball" & n_pitches >= 100) |>
#   
#   # plot
#   plot_ly(x = ~horizontal_break, 
#           y = ~induced_vertical_break) |>  #,
#           #colors = team_colors_off,
#           # hoverinfo = "text",
#           # text = ~paste0("Pitcher: ", playerName,
#           #                "<br>Carries: ", carries,
#           #                "<br>Rush YAC: ", rush_yac, " yds")) %>% 
#   
#   # add frame for each week 
#   add_markers(frame = ~game_year) |> 
#   
#   # labels
#   layout(xaxis = list(title = "Horizontal Movement (in)"),
#          yaxis = list(title = "Induced Vertical Movement (in)"),
#          title = "Avg. 4-Seam Fastball Movement") |> 
#   
#   # adjust frame time to 4 seconds and transition time to 3 seconds with elastic easing
#   animation_opts(frame = 4000, transition = 3000,
#                  easing = "elastic") |> 
#   
#   # change frame to "Week" and adjust color and size
#   animation_slider(currentvalue = list(prefix = "Year: ", 
#                                        font = list(color = "black", size = 20)))
