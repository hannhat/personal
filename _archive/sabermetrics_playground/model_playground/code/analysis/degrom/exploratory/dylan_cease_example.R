# Reset environment -------------------------------------------------------
rm(list = ls())

source("~/personal/sabermetrics_playground/model_playground/code/header.R")
# Code --------------------------------------------------------------------

## Import
df <- read_csv(f.import("statcast/pitch_level_pitching.csv"))
dylan_cease <- df %>% filter(pitcher == 656302)

woba_values <- data.frame(c(3,3,2,3,1,2,0,1,2,0,1,0),
                          c(0,1,0,2,0,1,0,1,2,1,2,2),
                          c(0.622, 0.470, 0.436, 0.384, 0.355, 0.352,
                            0.310, 0.293, 0.273, 0.262, 0.223, 0.196))
colnames(woba_values) <- c("balls", "strikes", "woba")

## Build
cease_stats <- dylan_cease %>% 
  left_join(woba_values, by = c("balls", "strikes")) %>%
  mutate(new_balls = ifelse(description %in% c("ball", "walk"), balls + 1, ifelse(description == "hit_by_pitch", 4, balls)),
         new_strikes = ifelse(description %in% c("foul", "foul_tip"), ifelse(strikes == 2, 3, 2),
                              ifelse(description %in% c("called_strike", "foul_bunt", "swinging_strike", "missed_bunt"),
                                     strikes + 1, strikes)),
         new_count_woba = case_when(new_balls == 4 ~ 0.690,
                                    new_strikes == 3 ~ 0,
                                    description == "hit_into_play" ~ NA,
                                    T ~ NA)) %>%
  left_join(woba_values %>% rename(new_woba = woba), by = c("balls", "strikes")) %>%
  mutate(new_woba = ifelse(!is.na(new_count_woba), new_count_woba, new_woba)) %>%
  mutate(woba_effect = ifelse(description == "hit_into_play", woba - estimated_woba_using_speedangle, woba - new_woba)) %>%
  select(batter, pitcher, zone, pitch_type, balls, strikes, new_balls, new_strikes,
         description, woba, new_woba, estimated_woba_using_speedangle, woba_effect,
         plate_x, plate_z)


## Analysis
cease_breakdown <- cease_stats %>%
  group_by(zone, pitch_type, balls, strikes) %>%
  summarize(count = n(), woba = mean(woba_effect, na.rm = T), sd = sd(woba_effect, na.rm = T))

cease_base_heatmap <- cease_stats %>%
  filter(balls == 0, strikes == 0, pitch_type == "FF") %>%
  select(woba_effect, plate_x, plate_z)


# Build heatmap for selected pitcher or hitter, count, and pitch type

build_heatmap <- function(statcast_df, pitcher = T, batter = T, balls = T, strikes = T, pitch_type = T, gran_x = 0.2, gran_z = 0.2) {
  print("zo")
  if (!(balls == T & strikes == T)) {
    statcast_df <- statcast_df %>% filter(balls == balls, strikes == strikes)
  }
  if (!(pitcher == T)) {
    statcast_df <- statcast_df %>% filter(pitcher == pitcher)
  }
  if (!(batter == T)) {
    statcast_df <- statcast_df %>% filter(batter == batter)
  }
  if (!(pitch_type == T)) {
    statcast_df <- statcast_df %>% filter(pitch_type == pitch_type)
  }
  
  statcast_df$plate_z <- findInterval(statcast_df$plate_z, seq(0, 6, gran_z)) * gran_z - gran_x / 2
  statcast_df$plate_x <- findInterval(statcast_df$plate_x, seq(-3, 3, gran_x)) * gran_x - 3 - gran_x / 2
  
  out <- statcast_df %>%
    group_by(plate_x, plate_z) %>%
    summarize(mean = mean(woba_effect, na.rm = T),
              num_pitches = n(),
              t_score = mean*sqrt(num_pitches) / sd(statcast_df$woba_effect, na.rm = T)) %>%
    arrange(plate_x, plate_z)
  return(out)
}


ggplot(out, aes(x = plate_x, y = plate_z, fill = t_score, label = num_pitches)) + 
  +     geom_tile(color = "white",
                  +               lwd = 0.1,
                  +               linetype = 1) + xlim(-1.5, 1.5) + ylim(1, 4) +
  +     scale_fill_gradientn(
    +         colors=c("red","white","green")) + geom_vline(xintercept=0.83, linetype="dashed", 
                                                            +                                                                   color = "black", size=0.5) + geom_vline(xintercept=-0.83, linetype="dashed", 
                                                                                                                                                                        +                                                                                                       color = "black", size=0.5) + geom_hline(yintercept=1.5, linetype="dashed", 
                                                                                                                                                                                                                                                                                                                        +                                                                                                                                           color = "black", size=0.5) + geom_hline(yintercept=3.5, linetype="dashed", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            +                                                                                                                                                                               color = "black", size=0.5)


