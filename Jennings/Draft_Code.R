## Test code


### Libraries
library(tidyverse)
library(baseballr)
library(ggthemes)
# library(plotly)
library(hexbin)


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
## remove unnecessary columns from statcast
statcast_filter <- statcast |> 
  select(-c(X.1, X, spin_dir:break_length_deprecated, tfs_deprecated, tfs_zulu_deprecated,
            umpire, sv_id, fielder_2, pitcher.1:fielder_9, home_score:delta_home_win_exp, bat_speed, swing_length)) |> 
  rename(pitcher_id = pitcher)

### Statcast pitch summary stats
statcast_filter_test_code <- statcast_filter %>%
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
    vertical_movement_in = pfx_z * 12) # ,
    
    # combine some pitchers together because of Stuff+ (can comment out when necessary)
    # pitch_name = recode(pitch_name,
    #                     "Sweeper" = "Slider",
    #                     "Slow Curve" = "Curveball", #)) |> ,
    #                     "Slurve" = "Curveball"))


statcast_pitch_summary_stats_test <- statcast_filter_test_code |>  
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
statcast_pitch_summary_stats_test <- statcast_pitch_summary_stats_test |> 
  filter(n_pitches >= 100)


########## SLIDERS DO NOT BREAK ANYWHERE AS MUCH AS SWEEPERS
########## MAYBE THAT'S WHY SLIDERS ARE SO EFFECTIVE IN STUFF+
statcast_pitch_summary_stats_test |> 
  filter(pitch_name %in% c("Sweeper", "Slider", "Curveball", "Slurve", "Knuckle Curve")) |>  # & horizontal_break >= 0) |> removing negative sliders doesn't impact avg.
  group_by(pitch_name) |> 
  summarize(
    spin = mean(spin, na.rm = TRUE),
    induced_vertical_break = mean(induced_vertical_break, na.rm = TRUE),
    horizontal_break = mean(horizontal_break, na.rm = TRUE),
    whiff_pct = mean(whiff_pct, na.rm = TRUE),
    velocity = mean(velocity, na.rm = TRUE)
  )

#### qualified breaking ball pitchers
break_qualified <- pitching_models|> 
  filter(pitch_name %in% c("Sweeper", "Slider", "Curveball", "Slurve", "Knuckle Curve"))

### vertical by horizontal movement plot for sliders and sweepers
statcast_pitch_summary_stats_test |> 
  filter(n_pitches >= 100 & pitch_name %in% c("Curveball", "Slurve", "Knuckle Curve")) |> 
  # for pitcher's POV
  mutate(horizontal_break = -horizontal_break) |> 
  # plot
  ggplot(aes(horizontal_break, induced_vertical_break, color = pitch_name)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.25) +
  scale_color_manual(values = c("firebrick",
                                "purple",
                                "dodgerblue")) +
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
  facet_wrap(~ season,
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
