## Load Frangaphs data
## Identify trends in Stuff+

### Libraries
library(tidyverse)
library(baseballr)
library(ggridges)
library(ggthemes)
library(scales)
#install.packages('gtExtras')
library(gtExtras)



# NOTES -------------------------------------------------------------------
##### ADD NA COLUMNS FOR FORKBALL & KNUCKLEBALL

###### INTERESTING THOUGHT ######
##### PAUL SKENES SLIDER STUFF IS WELL ABOVE AVERAGE FOR BOTH MODELS
##### HIS SPLITTER IS ONLY SLIGHTLY ABOVE AVERAGE
##### SLIDERS HAVE THE HIGHEST AVERAGE STUFF+ STUFF OF THE PITCH TYPES
#### LOOK AT PAUL SKENES RUN VALUE FOR SPLITTER AND SLIDER 
##### RUN VALUE SLIDER: -2
##### RUN VALUE SPLITTER: 10


##### NEXT: USE STATCAST TO CALCULATE RUN VALUE AND COMPARE TO STUFF+
##### ALSO, COMPARE STUFF+ TO PITCH+, LOCATION+, AND PITCHINGBOT
##### LOOK INTO DIFFERENCES BETWEEN ROLES

# PROJECT -----------------------------------------------------------------



### Data
#### 2020
fg_stuff_plus_20 <- baseballr::fg_pitcher_leaders(startseason = "2020", endseason = "2020")

##### select columns
fg_stuff_plus_20 <- fg_stuff_plus_20 |> 
  select(Season, Throws, PlayerName, xMLBAMID, IP, Relief_IP,
         # stuff plus stuff variables
         stuff_plus_stuff_CH = sp_s_CH, stuff_plus_stuff_FF = sp_s_FF, 
         stuff_plus_stuff_SL = sp_s_SL, stuff_plus_stuff_KC = sp_s_KC,
         stuff_plus_stuff_FC = sp_s_FC, stuff_plus_stuff_CU = sp_s_CU, 
         stuff_plus_stuff_SI = sp_s_SI, stuff_plus_stuff_FS = sp_s_FS,
         stuff_plus_stuff = sp_stuff,
         # stuff plus location variables
         stuff_plus_loc_CH = sp_l_CH, stuff_plus_loc_FF = sp_l_FF, 
         stuff_plus_loc_SL = sp_l_SL, stuff_plus_loc_KC = sp_l_KC,
         stuff_plus_loc_FC = sp_l_FC, stuff_plus_loc_CU = sp_l_CU, 
         stuff_plus_loc_SI = sp_l_SI, stuff_plus_loc_FS = sp_l_FS,
         stuff_plus_loc = sp_location,
         # stuff plus pitch variables
         stuff_plus_pit_CH = sp_l_CH, stuff_plus_pit_FF = sp_l_FF, 
         stuff_plus_pit_SL = sp_l_SL, stuff_plus_pit_KC = sp_l_KC,
         stuff_plus_pit_FC = sp_l_FC, stuff_plus_pit_CU = sp_l_CU, 
         stuff_plus_pit_SI = sp_l_SI, stuff_plus_pit_FS = sp_l_FS,
         stuff_plus_pit = sp_pitching,
         # PitchingBot stuff
         pitch_bot_stuff_CH = pb_s_CH, pitch_bot_stuff_FF = pb_s_FF, 
         pitch_bot_stuff_SL = pb_s_SL, pitch_bot_stuff_KC = pb_s_KC,
         pitch_bot_stuff_FC = pb_s_FC, pitch_bot_stuff_CU = pb_s_CU, 
         pitch_bot_stuff_SI = pb_s_SI, pitch_bot_stuff_FS = pb_s_FS,
         pitch_bot_stuff = pb_stuff,
         # PitchingBot command
         pitch_bot_com_CH = pb_c_CH, pitch_bot_com_FF = pb_c_FF, 
         pitch_bot_com_SL = pb_c_SL, pitch_bot_com_KC = pb_c_KC,
         pitch_bot_com_FC = pb_c_FC, pitch_bot_com_CU = pb_c_CU, 
         pitch_bot_com_SI = pb_c_SI, pitch_bot_com_FS = pb_c_FS,
         pitch_bot_com = pb_command,
         # PitchingBot overall
         pitch_bot_ovr_CH = pb_o_CH, pitch_bot_ovr_FF = pb_o_FF, 
         pitch_bot_ovr_SL = pb_o_SL, pitch_bot_ovr_KC = pb_o_KC,
         pitch_bot_ovr_FC = pb_o_FC, pitch_bot_ovr_CU = pb_o_CU, 
         pitch_bot_ovr_SI = pb_o_SI, pitch_bot_ovr_FS = pb_o_FS,
         pitch_bot_ovr = pb_overall,
         # pitch F/X pitch frequencies
         pfx_CH_pct, pfx_FA_pct,
         pfx_SL_pct, pfx_KC_pct,
         pfx_FC_pct, pfx_CU_pct,
         pfx_SI_pct, pfx_FS_pct
         ) |> 
  filter(IP >= 6) |> 
  mutate(Role = ifelse(Relief_IP <= IP * 0.25 | is.na(Relief_IP) == TRUE, "SP", "RP"))

#### 2021
fg_stuff_plus_21 <- baseballr::fg_pitcher_leaders(startseason = "2021", endseason = "2021")

##### select columns
fg_stuff_plus_21 <- fg_stuff_plus_21 |> 
  select(Season, Throws, PlayerName, xMLBAMID, IP, Relief_IP,
         # stuff plus stuff variables
         stuff_plus_stuff_CH = sp_s_CH, stuff_plus_stuff_FF = sp_s_FF, 
         stuff_plus_stuff_SL = sp_s_SL, stuff_plus_stuff_KC = sp_s_KC,
         stuff_plus_stuff_FC = sp_s_FC, stuff_plus_stuff_CU = sp_s_CU, 
         stuff_plus_stuff_SI = sp_s_SI, stuff_plus_stuff_FS = sp_s_FS,
         stuff_plus_stuff = sp_stuff,
         # stuff plus location variables
         stuff_plus_loc_CH = sp_l_CH, stuff_plus_loc_FF = sp_l_FF, 
         stuff_plus_loc_SL = sp_l_SL, stuff_plus_loc_KC = sp_l_KC,
         stuff_plus_loc_FC = sp_l_FC, stuff_plus_loc_CU = sp_l_CU, 
         stuff_plus_loc_SI = sp_l_SI, stuff_plus_loc_FS = sp_l_FS,
         stuff_plus_loc = sp_location,
         # stuff plus pitch variables
         stuff_plus_pit_CH = sp_l_CH, stuff_plus_pit_FF = sp_l_FF, 
         stuff_plus_pit_SL = sp_l_SL, stuff_plus_pit_KC = sp_l_KC,
         stuff_plus_pit_FC = sp_l_FC, stuff_plus_pit_CU = sp_l_CU, 
         stuff_plus_pit_SI = sp_l_SI, stuff_plus_pit_FS = sp_l_FS,
         stuff_plus_pit = sp_pitching,
         # PitchingBot stuff
         pitch_bot_stuff_CH = pb_s_CH, pitch_bot_stuff_FF = pb_s_FF, 
         pitch_bot_stuff_SL = pb_s_SL, pitch_bot_stuff_KC = pb_s_KC,
         pitch_bot_stuff_FC = pb_s_FC, pitch_bot_stuff_CU = pb_s_CU, 
         pitch_bot_stuff_SI = pb_s_SI, pitch_bot_stuff_FS = pb_s_FS,
         pitch_bot_stuff = pb_stuff,
         # PitchingBot command
         pitch_bot_com_CH = pb_c_CH, pitch_bot_com_FF = pb_c_FF, 
         pitch_bot_com_SL = pb_c_SL, pitch_bot_com_KC = pb_c_KC,
         pitch_bot_com_FC = pb_c_FC, pitch_bot_com_CU = pb_c_CU, 
         pitch_bot_com_SI = pb_c_SI, pitch_bot_com_FS = pb_c_FS,
         pitch_bot_com = pb_command,
         # PitchingBot overall
         pitch_bot_ovr_CH = pb_o_CH, pitch_bot_ovr_FF = pb_o_FF, 
         pitch_bot_ovr_SL = pb_o_SL, pitch_bot_ovr_KC = pb_o_KC,
         pitch_bot_ovr_FC = pb_o_FC, pitch_bot_ovr_CU = pb_o_CU, 
         pitch_bot_ovr_SI = pb_o_SI, pitch_bot_ovr_FS = pb_o_FS,
         pitch_bot_ovr = pb_overall,
         # pitch F/X pitch frequencies
         pfx_CH_pct, pfx_FA_pct,
         pfx_SL_pct, pfx_KC_pct,
         pfx_FC_pct, pfx_CU_pct,
         pfx_SI_pct, pfx_FS_pct) |> 
  filter(IP >= 6) |> 
  mutate(Role = ifelse(Relief_IP <= IP * 0.25 | is.na(Relief_IP) == TRUE, "SP", "RP"))

#### 2022
fg_stuff_plus_22 <- baseballr::fg_pitcher_leaders(startseason = "2022", endseason = "2022")

##### select columns
fg_stuff_plus_22 <- fg_stuff_plus_22 |> 
  select(Season, Throws, PlayerName, xMLBAMID, IP, Relief_IP,
         # stuff plus stuff variables
         stuff_plus_stuff_CH = sp_s_CH, stuff_plus_stuff_FF = sp_s_FF, 
         stuff_plus_stuff_SL = sp_s_SL, stuff_plus_stuff_KC = sp_s_KC,
         stuff_plus_stuff_FC = sp_s_FC, stuff_plus_stuff_CU = sp_s_CU, 
         stuff_plus_stuff_SI = sp_s_SI, stuff_plus_stuff_FS = sp_s_FS,
         stuff_plus_stuff = sp_stuff,
         # stuff plus location variables
         stuff_plus_loc_CH = sp_l_CH, stuff_plus_loc_FF = sp_l_FF, 
         stuff_plus_loc_SL = sp_l_SL, stuff_plus_loc_KC = sp_l_KC,
         stuff_plus_loc_FC = sp_l_FC, stuff_plus_loc_CU = sp_l_CU, 
         stuff_plus_loc_SI = sp_l_SI, stuff_plus_loc_FS = sp_l_FS,
         stuff_plus_loc = sp_location,
         # stuff plus pitch variables
         stuff_plus_pit_CH = sp_l_CH, stuff_plus_pit_FF = sp_l_FF, 
         stuff_plus_pit_SL = sp_l_SL, stuff_plus_pit_KC = sp_l_KC,
         stuff_plus_pit_FC = sp_l_FC, stuff_plus_pit_CU = sp_l_CU, 
         stuff_plus_pit_SI = sp_l_SI, stuff_plus_pit_FS = sp_l_FS,
         stuff_plus_pit = sp_pitching,
         # PitchingBot stuff
         pitch_bot_stuff_CH = pb_s_CH, pitch_bot_stuff_FF = pb_s_FF, 
         pitch_bot_stuff_SL = pb_s_SL, pitch_bot_stuff_KC = pb_s_KC,
         pitch_bot_stuff_FC = pb_s_FC, pitch_bot_stuff_CU = pb_s_CU, 
         pitch_bot_stuff_SI = pb_s_SI, pitch_bot_stuff_FS = pb_s_FS,
         pitch_bot_stuff = pb_stuff,
         # PitchingBot command
         pitch_bot_com_CH = pb_c_CH, pitch_bot_com_FF = pb_c_FF, 
         pitch_bot_com_SL = pb_c_SL, pitch_bot_com_KC = pb_c_KC,
         pitch_bot_com_FC = pb_c_FC, pitch_bot_com_CU = pb_c_CU, 
         pitch_bot_com_SI = pb_c_SI, pitch_bot_com_FS = pb_c_FS,
         pitch_bot_com = pb_command,
         # PitchingBot overall
         pitch_bot_ovr_CH = pb_o_CH, pitch_bot_ovr_FF = pb_o_FF, 
         pitch_bot_ovr_SL = pb_o_SL, pitch_bot_ovr_KC = pb_o_KC,
         pitch_bot_ovr_FC = pb_o_FC, pitch_bot_ovr_CU = pb_o_CU, 
         pitch_bot_ovr_SI = pb_o_SI, pitch_bot_ovr_FS = pb_o_FS,
         pitch_bot_ovr = pb_overall,
         # pitch F/X pitch frequencies
         pfx_CH_pct, pfx_FA_pct,
         pfx_SL_pct, pfx_KC_pct,
         pfx_FC_pct, pfx_CU_pct,
         pfx_SI_pct, pfx_FS_pct) |> 
  filter(IP >= 6) |> 
  mutate(Role = ifelse(Relief_IP <= IP * 0.25 | is.na(Relief_IP) == TRUE, "SP", "RP"))

#### 2023
fg_stuff_plus_23 <- baseballr::fg_pitcher_leaders(startseason = "2023", endseason = "2023")

##### take out forkball values for later
sp_s_FO_23 <- fg_stuff_plus_23 |> 
  filter(IP >= 6) |> 
  pull(sp_s_FO)

sp_l_FO_23 <- fg_stuff_plus_23 |> 
  filter(IP >= 6) |> 
  pull(sp_l_FO)

sp_p_FO_23 <- fg_stuff_plus_23 |> 
  filter(IP >= 6) |> 
  pull(sp_p_FO)

##### select columns
fg_stuff_plus_23 <- fg_stuff_plus_23 |> 
  select(Season, Throws, PlayerName, xMLBAMID, IP, Relief_IP,
         # stuff plus stuff variables
         stuff_plus_stuff_CH = sp_s_CH, stuff_plus_stuff_FF = sp_s_FF, 
         stuff_plus_stuff_SL = sp_s_SL, stuff_plus_stuff_KC = sp_s_KC,
         stuff_plus_stuff_FC = sp_s_FC, stuff_plus_stuff_CU = sp_s_CU, 
         stuff_plus_stuff_SI = sp_s_SI, stuff_plus_stuff_FS = sp_s_FS,
         stuff_plus_stuff = sp_stuff,
         # stuff plus location variables
         stuff_plus_loc_CH = sp_l_CH, stuff_plus_loc_FF = sp_l_FF, 
         stuff_plus_loc_SL = sp_l_SL, stuff_plus_loc_KC = sp_l_KC,
         stuff_plus_loc_FC = sp_l_FC, stuff_plus_loc_CU = sp_l_CU, 
         stuff_plus_loc_SI = sp_l_SI, stuff_plus_loc_FS = sp_l_FS,
         stuff_plus_loc = sp_location,
         # stuff plus pitch variables
         stuff_plus_pit_CH = sp_l_CH, stuff_plus_pit_FF = sp_l_FF, 
         stuff_plus_pit_SL = sp_l_SL, stuff_plus_pit_KC = sp_l_KC,
         stuff_plus_pit_FC = sp_l_FC, stuff_plus_pit_CU = sp_l_CU, 
         stuff_plus_pit_SI = sp_l_SI, stuff_plus_pit_FS = sp_l_FS,
         stuff_plus_pit = sp_pitching,
         # PitchingBot stuff
         pitch_bot_stuff_CH = pb_s_CH, pitch_bot_stuff_FF = pb_s_FF, 
         pitch_bot_stuff_SL = pb_s_SL, pitch_bot_stuff_KC = pb_s_KC,
         pitch_bot_stuff_FC = pb_s_FC, pitch_bot_stuff_CU = pb_s_CU, 
         pitch_bot_stuff_SI = pb_s_SI, pitch_bot_stuff_FS = pb_s_FS,
         pitch_bot_stuff = pb_stuff,
         # PitchingBot command
         pitch_bot_com_CH = pb_c_CH, pitch_bot_com_FF = pb_c_FF, 
         pitch_bot_com_SL = pb_c_SL, pitch_bot_com_KC = pb_c_KC,
         pitch_bot_com_FC = pb_c_FC, pitch_bot_com_CU = pb_c_CU, 
         pitch_bot_com_SI = pb_c_SI, pitch_bot_com_FS = pb_c_FS,
         pitch_bot_com = pb_command,
         # PitchingBot overall
         pitch_bot_ovr_CH = pb_o_CH, pitch_bot_ovr_FF = pb_o_FF, 
         pitch_bot_ovr_SL = pb_o_SL, pitch_bot_ovr_KC = pb_o_KC,
         pitch_bot_ovr_FC = pb_o_FC, pitch_bot_ovr_CU = pb_o_CU, 
         pitch_bot_ovr_SI = pb_o_SI, pitch_bot_ovr_FS = pb_o_FS,
         pitch_bot_ovr = pb_overall,
         # pitch F/X pitch frequencies
         pfx_CH_pct, pfx_FA_pct,
         pfx_SL_pct, pfx_KC_pct,
         pfx_FC_pct, pfx_CU_pct,
         pfx_SI_pct, pfx_FS_pct) |> 
  filter(IP >= 6) |> 
  mutate(Role = ifelse(Relief_IP <= IP * 0.25 | is.na(Relief_IP) == TRUE, "SP", "RP")) 


#### 2024
fg_stuff_plus_24 <- baseballr::fg_pitcher_leaders(startseason = "2024", endseason = "2024")

##### take out forkball values for later
sp_s_FO_24 <- fg_stuff_plus_24 |> 
  filter(IP >= 6) |> 
  pull(sp_s_FO)

sp_l_FO_24 <- fg_stuff_plus_24 |> 
  filter(IP >= 6) |> 
  pull(sp_l_FO)

sp_p_FO_24 <- fg_stuff_plus_24 |> 
  filter(IP >= 6) |> 
  pull(sp_p_FO)

##### select columns
fg_stuff_plus_24 <- fg_stuff_plus_24 |> 
   select(Season, Throws, PlayerName, xMLBAMID, IP, Relief_IP,
         # stuff plus stuff variables
         stuff_plus_stuff_CH = sp_s_CH, stuff_plus_stuff_FF = sp_s_FF, 
         stuff_plus_stuff_SL = sp_s_SL, stuff_plus_stuff_KC = sp_s_KC,
         stuff_plus_stuff_FC = sp_s_FC, stuff_plus_stuff_CU = sp_s_CU, 
         stuff_plus_stuff_SI = sp_s_SI, stuff_plus_stuff_FS = sp_s_FS,
         stuff_plus_stuff = sp_stuff,
         # stuff plus location variables
         stuff_plus_loc_CH = sp_l_CH, stuff_plus_loc_FF = sp_l_FF, 
         stuff_plus_loc_SL = sp_l_SL, stuff_plus_loc_KC = sp_l_KC,
         stuff_plus_loc_FC = sp_l_FC, stuff_plus_loc_CU = sp_l_CU, 
         stuff_plus_loc_SI = sp_l_SI, stuff_plus_loc_FS = sp_l_FS,
         stuff_plus_loc = sp_location,
         # stuff plus pitch variables
         stuff_plus_pit_CH = sp_l_CH, stuff_plus_pit_FF = sp_l_FF, 
         stuff_plus_pit_SL = sp_l_SL, stuff_plus_pit_KC = sp_l_KC,
         stuff_plus_pit_FC = sp_l_FC, stuff_plus_pit_CU = sp_l_CU, 
         stuff_plus_pit_SI = sp_l_SI, stuff_plus_pit_FS = sp_l_FS,
         stuff_plus_pit = sp_pitching,
         # PitchingBot stuff
         pitch_bot_stuff_CH = pb_s_CH, pitch_bot_stuff_FF = pb_s_FF, 
         pitch_bot_stuff_SL = pb_s_SL, pitch_bot_stuff_KC = pb_s_KC,
         pitch_bot_stuff_FC = pb_s_FC, pitch_bot_stuff_CU = pb_s_CU, 
         pitch_bot_stuff_SI = pb_s_SI, pitch_bot_stuff_FS = pb_s_FS,
         pitch_bot_stuff = pb_stuff,
         # PitchingBot command
         pitch_bot_com_CH = pb_c_CH, pitch_bot_com_FF = pb_c_FF, 
         pitch_bot_com_SL = pb_c_SL, pitch_bot_com_KC = pb_c_KC,
         pitch_bot_com_FC = pb_c_FC, pitch_bot_com_CU = pb_c_CU, 
         pitch_bot_com_SI = pb_c_SI, pitch_bot_com_FS = pb_c_FS,
         pitch_bot_com = pb_command,
         # PitchingBot overall
         pitch_bot_ovr_CH = pb_o_CH, pitch_bot_ovr_FF = pb_o_FF, 
         pitch_bot_ovr_SL = pb_o_SL, pitch_bot_ovr_KC = pb_o_KC,
         pitch_bot_ovr_FC = pb_o_FC, pitch_bot_ovr_CU = pb_o_CU, 
         pitch_bot_ovr_SI = pb_o_SI, pitch_bot_ovr_FS = pb_o_FS,
         pitch_bot_ovr = pb_overall,
         # pitch F/X pitch frequencies
         pfx_CH_pct, pfx_FA_pct,
         pfx_SL_pct, pfx_KC_pct,
         pfx_FC_pct, pfx_CU_pct,
         pfx_SI_pct, pfx_FS_pct) |> 
  filter(IP >= 6) |> 
  mutate(Role = ifelse(Relief_IP <= IP * 0.25 | is.na(Relief_IP) == TRUE, "SP", "RP"))

##### add NA's in forkball vectors because forkballs were not thrown until 2023
sp_s_FO <- c(rep(NA, 
      sum(nrow(fg_stuff_plus_20), nrow(fg_stuff_plus_21), nrow(fg_stuff_plus_22))), 
  sp_s_FO_23, sp_s_FO_24)

sp_l_FO <- c(rep(NA, 
                 sum(nrow(fg_stuff_plus_20), nrow(fg_stuff_plus_21), nrow(fg_stuff_plus_22))), 
             sp_l_FO_23, sp_l_FO_24)

sp_p_FO <- c(rep(NA, 
                 sum(nrow(fg_stuff_plus_20), nrow(fg_stuff_plus_21), nrow(fg_stuff_plus_22))), 
             sp_p_FO_23, sp_p_FO_24)

#### row bind
fg_stuff_plus <- rbind(
  fg_stuff_plus_20,
  fg_stuff_plus_21,
  fg_stuff_plus_22,
  fg_stuff_plus_23,
  fg_stuff_plus_24
  )

#### add forkball variables to dataset
fg_stuff_plus$stuff_plus_stuff_FO <- sp_s_FO

fg_stuff_plus$stuff_plus_loc_FO <- sp_l_FO

fg_stuff_plus$stuff_plus_pit_FO <- sp_p_FO

#### write to dataset
write.csv(fg_stuff_plus, "Fangraphs_Pitching_Models_2020-24.csv")

### plots
#### Stuff+ Changeup over years
fg_stuff_plus |> 
  mutate(Season_fct = factor(Season)) |> 
  ggplot(aes(x = stuff_plus_stuff_CH, y = Season_fct, fill = Season_fct)) +
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


#### Stuff+ Four-seam fastball over years
fg_stuff_plus |> 
  mutate(Season_fct = factor(Season)) |> 
  ggplot(aes(x = stuff_plus_stuff_FF, y = Season_fct, fill = Season_fct)) +
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


#### Stuff+ Curveball over years
fg_stuff_plus |> 
  mutate(Season_fct = factor(Season)) |> 
  filter(stuff_plus_stuff_CU >= -100) |> 
  ggplot(aes(x = stuff_plus_stuff_CU, y = Season_fct, fill = Season_fct)) +
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


#### Stuff+ Sinker with avg. Stuff+ over years
fg_stuff_plus |> 
  mutate(Season_fct = factor(Season)) |> 
  ggplot(aes(x = stuff_plus_stuff_SI, y = Season_fct, fill = Season_fct)) +
  geom_density_ridges() +
  labs(
    x = "Stuff+",
    y = "Season",
    title = "Stuff+ Sinker from 2020-24"
  ) +
  geom_vline(xintercept = mean(fg_stuff_plus$stuff_plus_stuff_SI, na.rm = TRUE),
             color = "firebrick1", linetype = "dashed", linewidth = 1.5) +
  scale_fill_colorblind() +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20)
  )


#### Stuff+ Splitter with avg. Stuff+ over years
fg_stuff_plus |> 
  mutate(Season_fct = factor(Season)) |> 
  ggplot(aes(x = stuff_plus_stuff_FS, y = Season_fct, fill = Season_fct)) +
  geom_density_ridges() +
  labs(
    x = "Stuff+",
    y = "Season",
    title = "Stuff+ Splitter from 2020-24"
  ) +
  geom_vline(xintercept = mean(fg_stuff_plus$stuff_plus_stuff_FS, na.rm = TRUE),
             color = "firebrick1", linetype = "dashed", linewidth = 1.5) +
  scale_fill_colorblind() +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20)
  )


#### Stuff+ Slider with avg. Stuff+ over years
fg_stuff_plus |> 
  filter(stuff_plus_stuff_SL >= 0 & stuff_plus_stuff_SL <= 200) |> 
  mutate(Season_fct = factor(Season)) |> 
  ggplot(aes(x = stuff_plus_stuff_SL, y = Season_fct, fill = Season_fct)) +
  geom_density_ridges() +
  labs(
    x = "Stuff+",
    y = "Season",
    title = "Stuff+ Slider from 2020-24"
  ) +
  geom_vline(xintercept = mean(fg_stuff_plus$stuff_plus_stuff_SL, na.rm = TRUE),
             color = "firebrick1", linetype = "dashed", linewidth = 1.5) +
  scale_fill_colorblind() +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20)
  )

#### Stuff+ Four-seam Fastball with avg. Stuff+ over years
fg_stuff_plus |> 
  filter(stuff_plus_stuff_FF >= 0 & stuff_plus_stuff_FF <= 200) |> 
  mutate(Season_fct = factor(Season)) |> 
  ggplot(aes(x = stuff_plus_stuff_FF, y = Season_fct, fill = Season_fct)) +
  geom_density_ridges() +
  labs(
    x = "Stuff+",
    y = "Season",
    title = "Stuff+ Four-Seam Fastball from 2020-24"
  ) +
  geom_vline(xintercept = mean(fg_stuff_plus$stuff_plus_stuff_FF, na.rm = TRUE),
             color = "firebrick1", linetype = "dashed", linewidth = 1.5) +
  scale_fill_colorblind() +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20)
  )

#### Stuff+ Curveball with avg. Stuff+ over years
fg_stuff_plus |> 
  filter(stuff_plus_stuff_CU >= 0 & stuff_plus_stuff_CU <= 200) |> 
  mutate(Season_fct = factor(Season)) |> 
  ggplot(aes(x = stuff_plus_stuff_CU, y = Season_fct, fill = Season_fct)) +
  geom_density_ridges() +
  labs(
    x = "Stuff+",
    y = "Season",
    title = "Stuff+ Curveball from 2020-24"
  ) +
  geom_vline(xintercept = mean(fg_stuff_plus$stuff_plus_stuff_CU, na.rm = TRUE),
             color = "firebrick1", linetype = "dashed", linewidth = 1.5) +
  scale_fill_colorblind() +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20)
  )

#### Stuff+ Changeup with avg. Stuff+ over years
fg_stuff_plus |> 
  filter(stuff_plus_stuff_CH >= 0 & stuff_plus_stuff_CH <= 200) |> 
  mutate(Season_fct = factor(Season)) |> 
  ggplot(aes(x = stuff_plus_stuff_CH, y = Season_fct, fill = Season_fct)) +
  geom_density_ridges() +
  labs(
    x = "Stuff+",
    y = "Season",
    title = "Stuff+ Changeup from 2020-24"
  ) +
  geom_vline(xintercept = mean(fg_stuff_plus$stuff_plus_stuff_CH, na.rm = TRUE),
             color = "firebrick1", linetype = "dashed", linewidth = 1.5) +
  scale_fill_colorblind() +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20)
  )


#### Stuff+ Sinker with avg. Stuff+ over years
fg_stuff_plus |> 
  filter(stuff_plus_stuff_SI >= 0 & stuff_plus_stuff_SI <= 200) |> 
  mutate(Season_fct = factor(Season)) |> 
  ggplot(aes(x = stuff_plus_stuff_SI, y = Season_fct, fill = Season_fct)) +
  geom_density_ridges() +
  labs(
    x = "Stuff+",
    y = "Season",
    title = "Stuff+ Sinker from 2020-24"
  ) +
  geom_vline(xintercept = mean(fg_stuff_plus$stuff_plus_stuff_SI, na.rm = TRUE),
             color = "firebrick1", linetype = "dashed", linewidth = 1.5) +
  scale_fill_colorblind() +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20)
  )


#### pitch usage over time
##### Sinkers
fg_stuff_plus |> 
  mutate(Season_fct = factor(Season)) |> 
  ggplot(aes(x = pfx_SI_pct, y = Season_fct, fill = Season_fct)) +
  geom_density_ridges(alpha = 0.70) +
  labs(
    x = "Percentage",
    y = "Season",
    title = "Sinker Usage % from 2020-24"
  ) +
  geom_vline(xintercept = mean(fg_stuff_plus$pfx_SI_pct, na.rm = TRUE),
             color = "firebrick1", linetype = "dashed", linewidth = 1.5) +
  scale_fill_colorblind() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20)
  )

##### Splitters
fg_stuff_plus |> 
  mutate(Season_fct = factor(Season)) |> 
  ggplot(aes(x = pfx_FS_pct, y = Season_fct, fill = Season_fct)) +
  geom_density_ridges(alpha = 0.70) +
  labs(
    x = "Percentage",
    y = "Season",
    title = "Splitter Usage % from 2020-24"
  ) +
  geom_vline(xintercept = mean(fg_stuff_plus$pfx_FS_pct, na.rm = TRUE),
             color = "firebrick1", linetype = "dashed", linewidth = 1.5) +
  scale_fill_colorblind() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20)
  )

##### Slider
fg_stuff_plus |> 
  mutate(Season_fct = factor(Season)) |> 
  ggplot(aes(x = pfx_SL_pct, y = Season_fct, fill = Season_fct)) +
  geom_density_ridges(alpha = 0.70) +
  labs(
    x = "Percentage",
    y = "Season",
    title = "Slider Usage % from 2020-24"
  ) +
  geom_vline(xintercept = mean(fg_stuff_plus$pfx_SL_pct, na.rm = TRUE),
             color = "firebrick1", linetype = "dashed", linewidth = 1.5) +
  scale_fill_colorblind() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20)
  )

## mean Stuff+ for each pitch
fg_stuff_plus |> 
  select(stuff_plus_stuff_CH:stuff_plus_stuff,
         pb_s_CH:pb_stuff) |> 
  map(mean, na.rm = TRUE) |> 
  unlist()

### Pitching Models Table by Pitch
fg_stuff_plus |> 
  mutate(Season = factor(Season)) |>
  group_by(Season) |> 
  summarize(
    stuff_plus_stuff_SL = mean(stuff_plus_stuff_SL, na.rm = TRUE),  
    stuff_plus_loc_SL = mean(stuff_plus_loc_SL, na.rm = TRUE), , 
    stuff_plus_pit_SL = mean(stuff_plus_pit_SL, na.rm = TRUE), ,
    pitch_bot_stuff_SL = mean(pitch_bot_stuff_SL, na.rm = TRUE), , 
    pitch_bot_com_SL = mean(pitch_bot_com_SL, na.rm = TRUE), , 
    pitch_bot_ovr_SL = mean(pitch_bot_ovr_SL, na.rm = TRUE), , 
    pfx_SL_pct = mean(pfx_SL_pct, na.rm = TRUE) * 100) |> 
  gt(rowname_col = "Season") |> 
  fmt_number(columns = c(stuff_plus_stuff_SL, stuff_plus_loc_SL, stuff_plus_pit_SL,
                         pitch_bot_stuff_SL, pitch_bot_com_SL, pitch_bot_ovr_SL, 
                         pfx_SL_pct), decimals = 2) |>
  data_color(columns = c(stuff_plus_stuff_SL, stuff_plus_loc_SL, stuff_plus_pit_SL,
                         pitch_bot_stuff_SL, pitch_bot_com_SL, pitch_bot_ovr_SL),
             fn = scales::col_numeric(palette = c("lightblue1", "royalblue4"), domain = NULL)) |> 
  cols_align(align = "center", columns = c("Season")) |> 
  tab_stubhead(label = "Season") |> 
  cols_label(
    Season = "Season",
    stuff_plus_stuff_SL = "Stuff+", 
    stuff_plus_loc_SL = "Location+", 
    stuff_plus_pit_SL = "Pitch+",
    pitch_bot_stuff_SL = "PitchingBot Stuff", 
    pitch_bot_com_SL = "PitchingBot Command", 
    pitch_bot_ovr_SL = "PitchingBot Overall", 
    pfx_SL_pct = "Usage %"
             ) |>
  tab_header(title = md("**Average Slider Statistics**"),
             subtitle = md("*For seasons between 2020 and 2024*")) |> 
  gtExtras::gt_theme_espn()


##### USAGE PLOT *doesn't work*
fg_stuff_plus |>
  mutate(Season = factor(Season)) |> 
  group_by(Season) |> 
  summarize(
    CH = mean(pfx_CH_pct, na.rm = TRUE) * 100,
    FF = mean(pfx_FA_pct, na.rm = TRUE) * 100,
    SL = mean(pfx_SL_pct, na.rm = TRUE) * 100,
    KC = mean(pfx_KC_pct, na.rm = TRUE) * 100,
    FC = mean(pfx_FC_pct, na.rm = TRUE) * 100,
    CU = mean(pfx_CU_pct, na.rm = TRUE) * 100,
    SI = mean(pfx_SI_pct, na.rm = TRUE) * 100,
    FS = mean(pfx_FS_pct, na.rm = TRUE) * 100
  ) |> 
  ungroup() |> 
  ggplot(aes(Season)) +
  geom_line(aes(y = CH), color = "red") +
  geom_line(aes(y = FF), color = "orange") +
  geom_line(aes(y = SL), color = "gold") +
  geom_line(aes(y = KC), color = "green3") +
  geom_line(aes(y = FC), color = "blue") +
  geom_line(aes(y = CU), color = "magenta") +
  geom_line(aes(y = SI), color = "violet") +
  geom_line(aes(y = FS), color = "purple3")



##### PITCHING BOT

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



fg_stuff_plus |> 
  mutate(Season_fct = factor(Season)) |> 
  ggplot(aes(x = stuff_plus_stuff_SL, y = Season_fct, fill = Season_fct)) +
  geom_density_ridges() +
  labs(
    x = "Stuff+",
    y = "Season",
    title = "Stuff+ Slider from 2020-24"
  ) +
  geom_vline(xintercept = mean(fg_stuff_plus$stuff_plus_stuff_SL, na.rm = TRUE),
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
  ggplot(aes(x = stuff_plus_stuff_FF, y = Season_fct, fill = Season_fct)) +
  geom_density_ridges() +
  labs(
    x = "Stuff+",
    y = "Season",
    title = "Stuff+ Fastballs from 2020-24"
  ) +
  geom_vline(xintercept = mean(fg_stuff_plus$stuff_plus_stuff_FF, na.rm = TRUE),
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