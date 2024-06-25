## Load Frangaphs data
## Identify trends in Stuff+

### Libraries
library(tidyverse)
library(baseballr)

### Data
#### 2020
fg_stuff_plus_20 <- baseballr::fg_pitcher_leaders(startseason = "2020", endseason = "2020")

##### select columns
fg_stuff_plus_20 <- fg_stuff_plus_20 |> 
  select(Season, Throws, PlayerName, IP, Relief_IP,
         # stuff plus stuff variables
         stuff_plus_CH = sp_s_CH, stuff_plus_FF = sp_s_FF, 
         stuff_plus_SL = sp_s_SL, stuff_plus_KC = sp_s_KC,
         stuff_plus_FC = sp_s_FC, stuff_plus_CU = sp_s_CU, 
         stuff_plus_SI = sp_s_SI, stuff_plus_FS = sp_s_FS,
         stuff_plus = sp_stuff,
         # stuff plus location variables
         stuff_plus_loc_CH = sp_l_CH, stuff_plus_loc_FF = sp_l_FF, 
         stuff_plus_loc_SL = sp_l_SL, stuff_plus_loc_KC = sp_l_KC,
         stuff_plus_loc_FC = sp_l_FC, stuff_plus_loc_CU = sp_l_CU, 
         stuff_plus_loc_SI = sp_l_SI, stuff_plus_loc_FS = sp_l_FS,
         stuff_plus_loc = sp_location,
         # PitchingBot stuff
         pb_s_CH, pb_s_FF, 
         pb_s_SL, pb_s_KC,
         pb_s_FC, pb_s_CU, 
         pb_s_SI, pb_s_FS,
         pb_stuff
         ) |> 
  filter(IP >= 6) |> 
  mutate(Role = ifelse(Relief_IP <= IP * 0.25 | is.na(Relief_IP) == TRUE, "SP", "RP"))

#### 2021
fg_stuff_plus_21 <- baseballr::fg_pitcher_leaders(startseason = "2021", endseason = "2021")

##### select columns
fg_stuff_plus_21 <- fg_stuff_plus_21 |> 
  select(Season, Throws, PlayerName, IP, Relief_IP,
         stuff_plus_CH = sp_s_CH, stuff_plus_FF = sp_s_FF, 
         stuff_plus_SL = sp_s_SL, stuff_plus_KC = sp_s_KC,
         stuff_plus_FC = sp_s_FC, stuff_plus_CU = sp_s_CU, 
         stuff_plus_SI = sp_s_SI, stuff_plus_FS = sp_s_FS,
         stuff_plus = sp_stuff,
         # stuff plus location variables
         stuff_plus_loc_CH = sp_l_CH, stuff_plus_loc_FF = sp_l_FF, 
         stuff_plus_loc_SL = sp_l_SL, stuff_plus_loc_KC = sp_l_KC,
         stuff_plus_loc_FC = sp_l_FC, stuff_plus_loc_CU = sp_l_CU, 
         stuff_plus_loc_SI = sp_l_SI, stuff_plus_loc_FS = sp_l_FS,
         stuff_plus_loc = sp_location,
         # PitchingBot stuff
         pb_s_CH, pb_s_FF, 
         pb_s_SL, pb_s_KC,
         pb_s_FC, pb_s_CU, 
         pb_s_SI, pb_s_FS,
         pb_stuff) |> 
  filter(IP >= 9) |> 
  mutate(Role = ifelse(Relief_IP <= IP * 0.25 | is.na(Relief_IP) == TRUE, "SP", "RP"))

#### 2022
fg_stuff_plus_22 <- baseballr::fg_pitcher_leaders(startseason = "2022", endseason = "2022")

##### select columns
fg_stuff_plus_22 <- fg_stuff_plus_22 |> 
  select(Season, Throws, PlayerName, IP, Relief_IP,
         stuff_plus_CH = sp_s_CH, stuff_plus_FF = sp_s_FF, 
         stuff_plus_SL = sp_s_SL, stuff_plus_KC = sp_s_KC,
         stuff_plus_FC = sp_s_FC, stuff_plus_CU = sp_s_CU, 
         stuff_plus_SI = sp_s_SI, stuff_plus_FS = sp_s_FS,
         stuff_plus = sp_stuff,
         # stuff plus location variables
         stuff_plus_loc_CH = sp_l_CH, stuff_plus_loc_FF = sp_l_FF, 
         stuff_plus_loc_SL = sp_l_SL, stuff_plus_loc_KC = sp_l_KC,
         stuff_plus_loc_FC = sp_l_FC, stuff_plus_loc_CU = sp_l_CU, 
         stuff_plus_loc_SI = sp_l_SI, stuff_plus_loc_FS = sp_l_FS,
         stuff_plus_loc = sp_location,
         # PitchingBot stuff
         pb_s_CH, pb_s_FF, 
         pb_s_SL, pb_s_KC,
         pb_s_FC, pb_s_CU, 
         pb_s_SI, pb_s_FS,
         pb_stuff) |> 
  filter(IP >= 9) |> 
  mutate(Role = ifelse(Relief_IP <= IP * 0.25 | is.na(Relief_IP) == TRUE, "SP", "RP"))

#### 2023
fg_stuff_plus_23 <- baseballr::fg_pitcher_leaders(startseason = "2023", endseason = "2023")

##### select columns
fg_stuff_plus_23 <- fg_stuff_plus_23 |> 
  select(Season, Throws, PlayerName, IP, Relief_IP,
         stuff_plus_CH = sp_s_CH, stuff_plus_FF = sp_s_FF, 
         stuff_plus_SL = sp_s_SL, stuff_plus_KC = sp_s_KC,
         stuff_plus_FC = sp_s_FC, stuff_plus_CU = sp_s_CU, 
         stuff_plus_SI = sp_s_SI, stuff_plus_FS = sp_s_FS,
         stuff_plus = sp_stuff,
         # stuff plus location variables
         stuff_plus_loc_CH = sp_l_CH, stuff_plus_loc_FF = sp_l_FF, 
         stuff_plus_loc_SL = sp_l_SL, stuff_plus_loc_KC = sp_l_KC,
         stuff_plus_loc_FC = sp_l_FC, stuff_plus_loc_CU = sp_l_CU, 
         stuff_plus_loc_SI = sp_l_SI, stuff_plus_loc_FS = sp_l_FS,
         stuff_plus_loc = sp_location,
         # PitchingBot stuff
         pb_s_CH, pb_s_FF, 
         pb_s_SL, pb_s_KC,
         pb_s_FC, pb_s_CU, 
         pb_s_SI, pb_s_FS,
         pb_stuff) |> 
  filter(IP >= 10.5) |> 
  mutate(Role = ifelse(Relief_IP <= IP * 0.25 | is.na(Relief_IP) == TRUE, "SP", "RP")) 


#### 2024
fg_stuff_plus_24 <- baseballr::fg_pitcher_leaders(startseason = "2024", endseason = "2024")

##### select columns
fg_stuff_plus_24 <- fg_stuff_plus_24 |> 
  select(Season, Throws, PlayerName, IP, Relief_IP,
         stuff_plus_CH = sp_s_CH, stuff_plus_FF = sp_s_FF, 
         stuff_plus_SL = sp_s_SL, stuff_plus_KC = sp_s_KC,
         stuff_plus_FC = sp_s_FC, stuff_plus_CU = sp_s_CU, 
         stuff_plus_SI = sp_s_SI, stuff_plus_FS = sp_s_FS,
         stuff_plus = sp_stuff,
         # stuff plus location variables
         stuff_plus_loc_CH = sp_l_CH, stuff_plus_loc_FF = sp_l_FF, 
         stuff_plus_loc_SL = sp_l_SL, stuff_plus_loc_KC = sp_l_KC,
         stuff_plus_loc_FC = sp_l_FC, stuff_plus_loc_CU = sp_l_CU, 
         stuff_plus_loc_SI = sp_l_SI, stuff_plus_loc_FS = sp_l_FS,
         stuff_plus_loc = sp_location,
         # PitchingBot stuff
         pb_s_CH, pb_s_FF, 
         pb_s_SL, pb_s_KC,
         pb_s_FC, pb_s_CU, 
         pb_s_SI, pb_s_FS,
         pb_stuff) |> 
  filter(IP >= 9) |> 
  mutate(Role = ifelse(Relief_IP <= IP * 0.25 | is.na(Relief_IP) == TRUE, "SP", "RP"))


#### row bind
fg_stuff_plus <- rbind(
  fg_stuff_plus_20,
  fg_stuff_plus_21,
  fg_stuff_plus_22,
  fg_stuff_plus_23,
  fg_stuff_plus_24
  )


### plot
library(ggridges)
library(ggthemes)
fg_stuff_plus |> 
  mutate(Season_fct = factor(Season)) |> 
  ggplot(aes(x = stuff_plus_CH, y = Season_fct, fill = Season_fct)) +
  geom_density_ridges() +
  labs(
    x = "Stuff+",
    y = "Season",
    title = "Stuff+ Changeup from 2020-24"
  ) +
  scale_fill_colorblind() +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20)
  )

fg_stuff_plus |> 
  mutate(Season_fct = factor(Season)) |> 
  ggplot(aes(x = stuff_plus_FF, y = Season_fct, fill = Season_fct)) +
  geom_density_ridges() +
  labs(
    x = "Stuff+",
    y = "Season",
    title = "Stuff+ Four-seam Fastball from 2020-24"
  ) +
  scale_fill_colorblind() +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20)
  )

fg_stuff_plus |> 
  mutate(Season_fct = factor(Season)) |> 
  filter(stuff_plus_CU >= -100) |> 
  ggplot(aes(x = stuff_plus_CU, y = Season_fct, fill = Season_fct)) +
  geom_density_ridges() +
  labs(
    x = "Stuff+",
    y = "Season",
    title = "Stuff+ Curveball from 2020-24"
  ) +
  scale_fill_colorblind() +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20)
  )

fg_stuff_plus |> 
  mutate(Season_fct = factor(Season)) |> 
  ggplot(aes(x = stuff_plus_SI, y = Season_fct, fill = Season_fct)) +
  geom_density_ridges() +
  labs(
    x = "Stuff+",
    y = "Season",
    title = "Stuff+ Sinker from 2020-24"
  ) +
  geom_vline(xintercept = mean(fg_stuff_plus$stuff_plus_SI, na.rm = TRUE),
             color = "firebrick1", linetype = "dashed", linewidth = 1.5) +
  scale_fill_colorblind() +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20)
  )


fg_stuff_plus |> 
  mutate(Season_fct = factor(Season)) |> 
  ggplot(aes(x = stuff_plus_FS, y = Season_fct, fill = Season_fct)) +
  geom_density_ridges() +
  labs(
    x = "Stuff+",
    y = "Season",
    title = "Stuff+ Splitter from 2020-24"
  ) +
  geom_vline(xintercept = mean(fg_stuff_plus$stuff_plus_FS, na.rm = TRUE),
             color = "firebrick1", linetype = "dashed", linewidth = 1.5) +
  scale_fill_colorblind() +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20)
  )


fg_stuff_plus |> 
  mutate(Season_fct = factor(Season)) |> 
  ggplot(aes(x = stuff_plus_SL, y = Season_fct, fill = Season_fct)) +
  geom_density_ridges() +
  labs(
    x = "Stuff+",
    y = "Season",
    title = "Stuff+ Slider from 2020-24"
  ) +
  geom_vline(xintercept = mean(fg_stuff_plus$stuff_plus_SL, na.rm = TRUE),
             color = "firebrick1", linetype = "dashed", linewidth = 1.5) +
  scale_fill_colorblind() +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20)
  )

## mean Stuff+ for each pitch
fg_stuff_plus |> 
  select(stuff_plus_CH:stuff_plus,
         pb_s_CH:pb_stuff) |> 
  map(mean, na.rm = TRUE) |> 
  unlist()


###### INTERESTING THOUGHT ######
##### PAUL SKENES SLIDER STUFF IS WELL ABOVE AVERAGE FOR BOTH MODELS
##### HIS SPLITTER IS ONLY SLIGHTLY ABOVE AVERAGE
##### SLIDERS HAVE THE HIGHEST AVERAGE STUFF+ STUFF OF THE PITCH TYPES
#### LOOK AT PAUL SKENES RUN VALUE FOR SPLITTER AND SLIDER 
##### RUN VALUE SLIDER: -2
##### RUN VALUE SPLITTER: 10

fg_stuff_plus |> 
  mutate(Season_fct = factor(Season)) |> 
  ggplot(aes(x = pb_s_FS, y = Season_fct, fill = Season_fct)) +
  geom_density_ridges() +
  labs(
    x = "PitchingBot Stuff",
    y = "Season",
    title = "PitchingBot Stuff Splitter from 2020-24"
  ) +
  geom_vline(xintercept = mean(fg_stuff_plus$pb_s_FS, na.rm = TRUE),
             color = "firebrick1", linetype = "dashed", linewidth = 1.5) +
  scale_fill_colorblind() +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20)
  )

##### NEXT: USE STATCAST TO CALCULATE RUN VALUE AND COMPARE TO STUFF+
##### ALSO, COMPARE STUFF+ TO PITCH+, LOCATION+, AND PITCHINGBOT
##### LOOK INTO DIFFERENCES BETWEEN ROLES

fg_stuff_plus |> 
  mutate(Season_fct = factor(Season)) |> 
  ggplot(aes(x = stuff_plus_SL, y = Season_fct, fill = Season_fct)) +
  geom_density_ridges() +
  labs(
    x = "Stuff+",
    y = "Season",
    title = "Stuff+ Slider from 2020-24"
  ) +
  geom_vline(xintercept = mean(fg_stuff_plus$stuff_plus_SL, na.rm = TRUE),
             color = "firebrick1", linetype = "dashed", linewidth = 1.5) +
  scale_fill_colorblind() +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20)
  ) +
  facet_wrap(~ Role, ncol = 1)

fg_stuff_plus |> 
  filter(stuff_plus_FF <= 200) |> 
  mutate(Season_fct = factor(Season)) |> 
  ggplot(aes(x = stuff_plus_FF, y = Season_fct, fill = Season_fct)) +
  geom_density_ridges() +
  labs(
    x = "Stuff+",
    y = "Season",
    title = "Stuff+ Fastballs from 2020-24"
  ) +
  geom_vline(xintercept = mean(fg_stuff_plus$stuff_plus_FF, na.rm = TRUE),
             color = "firebrick1", linetype = "dashed", linewidth = 1.5) +
  scale_fill_colorblind() +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20)
  ) +
  facet_wrap(~ Role, ncol = 1)


### glimpse
glimpse(fg_stuff_plus_22)


### select columns
fg_pitching <- fg_stuff_plus |> 
  # keep player name to add a MLBAM ID column to join to savant
  select(PlayerName, pb_o_CH:sp_pitching, pb_o_FS:sp_p_KC, sp_s_FO:sp_p_FO)

summary(fg_pitching$sp_s_CH)