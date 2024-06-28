## Statcast

## libraries
library(tidyverse)
library(ggthemes)

## set theme
theme_set(theme_bw())

## Data
statcast_stuff_plus <- read.csv("Joined_FanGraphs_Statcast.csv")

## Filter
statcast_stuff_plus <- statcast_stuff_plus |> 
  select(Season:stuff_plus_stuff_FS, run_value_per100_CU:avg_velocity_FO)

## Pivot longer
### Stuff
statcast_long_stuff <- statcast_stuff_plus |> 
  select(Season:stuff_plus_stuff_FS) |> 
  pivot_longer(
    cols = starts_with("stuff_plus"),
    names_to = "pitch_type",
    names_prefix = "stuff_plus_stuff_",
    values_to = "stuff_plus"
  )

### Run Value
statcast_long_RV <- statcast_stuff_plus |> 
  select(Season:Relief_IP, run_value_per100_CU:run_value_per100_FO) |> 
  pivot_longer(
    cols = starts_with("run_value"),
    names_to = "pitch_type",
    names_prefix = "run_value_per100_",
    values_to = "run_value_per100"
  ) |> 
  filter(!(pitch_type %in% c("FO", "KN")))

### Avg. Velo
statcast_long_avg_velo <- statcast_stuff_plus |> 
  select(Season:Relief_IP, avg_velocity_CU:avg_velocity_FO) |> 
  pivot_longer(
    cols = starts_with("avg_velocity"),
    names_to = "pitch_type",
    names_prefix = "avg_velocity_",
    values_to = "avg_velocity"
  ) |> 
  filter(!(pitch_type %in% c("FO", "KN")))

## Join
total_statcast_stuff_plus <- statcast_long_stuff |> 
  cbind(statcast_long_RV[8], 
        statcast_long_avg_velo[8])

## Summary statistics / EDA
total_statcast_stuff_plus |> 
  summary()

### Histograms
#### number of bins (struges' method)
bins <- log(nrow(total_statcast_stuff_plus), base = 2) + 1

#### Stuff Plus
total_statcast_stuff_plus |> 
  # remove outliers
  filter(stuff_plus >= 0 & stuff_plus <= 200) |> 
  ggplot(aes(stuff_plus)) +
  geom_histogram(color = "white", fill = "dodgerblue2",
                 bins = round(bins, 0))

#### RV/100
total_statcast_stuff_plus |> 
  filter(run_value_per100 >= -10 & run_value_per100 <= 10) |> 
  ggplot(aes(run_value_per100)) +
  geom_histogram(color = "white", fill = "firebrick",
                 bins = round(bins, 0))

#### Avg Velo
total_statcast_stuff_plus |> 
  ggplot(aes(avg_velocity)) +
  geom_histogram(color = "white", fill = "purple2",
                 bins = round(bins, 0))

## Plot
### Starters
stuff_plus_starters <- total_statcast_stuff_plus |>
  filter(
      stuff_plus >= 0 & stuff_plus <= 200 &
      run_value_per100 >= -10 & run_value_per100 <= 10 &
      Season %in% c(2021, 2022, 2023) &
      IP >= 162
           ) |> 
  group_by(Season, pitch_type) |> 
  summarize(
    stuff_plus = mean(stuff_plus, na.rm = TRUE),
    RV_100 = mean(run_value_per100, na.rm = TRUE),
    velo = mean(avg_velocity, na.rm = TRUE)
  ) 

stuff_plus_starters |> 
  ggplot(aes(stuff_plus, RV_100, 
             color = pitch_type)) + 
  geom_point(alpha = 0.75, size = 1.5) +
  labs(
    x = "Stuff+",
    y = "RV / 100 Pitches",
    color = "Pitch"
    ) +
  geom_smooth(method = "loess") +
  geom_label(aes(label = Season)) +
  geom_hline(yintercept = mean(stuff_plus_starters$RV_100, na.rm = TRUE),
             linetype = "dashed", color = "firebrick", linewidth = 1.25) +
  geom_vline(xintercept = 100, 
             linetype = "dashed", color = "royalblue4", linewidth = 1.25)
  
  
### Relievers
stuff_plus_relievers <- total_statcast_stuff_plus |>
  filter(
    stuff_plus >= 0 & stuff_plus <= 200 &
      run_value_per100 >= -10 & run_value_per100 <= 10 &
      Season %in% c(2021, 2022, 2023) &
      IP >= 40 & Relief_IP >= IP * 0.5
  ) |> 
  group_by(Season, pitch_type) |> 
  summarize(
    stuff_plus = mean(stuff_plus, na.rm = TRUE),
    RV_100 = mean(run_value_per100, na.rm = TRUE),
    velo = mean(avg_velocity, na.rm = TRUE)
  ) 

stuff_plus_relievers |> 
  ggplot(aes(stuff_plus, RV_100, 
             color = pitch_type)) + 
  geom_point(alpha = 0.75, size = 1.5) +
  labs(
    x = "Stuff+",
    y = "RV / 100 Pitches",
    color = "Pitch"
  ) +
  geom_smooth(method = "loess") +
  geom_label(aes(label = Season)) +
  geom_hline(yintercept = mean(stuff_plus_relievers$RV_100, na.rm = TRUE),
             linetype = "dashed", color = "firebrick", linewidth = 1.25) +
  geom_vline(xintercept = 100, 
             linetype = "dashed", color = "royalblue4", linewidth = 1.25)


### LOOK AT ALL STARTERS  
