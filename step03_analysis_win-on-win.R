
# SETUP ----

if (!require("pacman")) install.packages("pacman")
library(pacman);p_load(tidyverse, dplyr, tidyr, ggplot2, readr, fixest, broom, fst, purrr, splines);

#install.packages("sportyR")
library(sportyR)

# hockeyR 
## See https://github.com/danmorse314/hockeyR?tab=readme-ov-file

# themes / settings ----

mytheme <- theme_minimal() + theme(
  legend.position = "none",
  panel.grid.minor.y = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank()
)

# READ DATA ----

#df                   <- read_fst(paste0("Data/step02_df.fst"), as.data.table = FALSE)
#df.game.results      <- read_fst(paste0("Data/step02_game_results.fst"), as.data.table = FALSE)
df.game.results.long <- read_fst(paste0("Data/step02_game_results_long.fst"), as.data.table = FALSE)
#df.season            <- read_fst(paste0("Data/step02_season.fst"), as.data.table = FALSE)


# Win-on-win Models... ----

## sample selection... 
table(df.game.results.long$season, df.game.results.long$season_type)
table(df.game.results.long$season, df.game.results.long$result_lag)
table(df.game.results.long$result, df.game.results.long$result_lag)

## what does the time of the last goal look like?
summary(df.game.results.long$last_goal_frac_reg_lag)

## selected sample ----

table(df.game.results.long$result)
table(df.game.results.long$ended_in_shootout)

df.sample <- df.game.results.long %>%
  filter(season_type == "R") %>%  # regular season
  #filter(regular_length_game == T) %>%  # no overtime games
  #filter(regular_length_game_lag == T) %>%  # no overtime games in the game prior
  mutate(
    ## flag late goals as those within the last-two percent 
    ## of 3 periods * 20 minutes * 60 seconds = 3600 seconds
    last_goal_frac_lag_reg_gt98 = ifelse(last_goal_frac_reg_lag>.98, 1, 0),
    win = ifelse(result=="Win", 1, 0)
  ) %>%
  group_by(team) %>% arrange(team, game_id) %>%
  mutate(
    win_lag = lag(win),
    won_on_shots = case_when(
      ended_in_shootout==T & result=="Win" ~ 1,
      T ~ 0),
    won_on_shots_lag = lag(won_on_shots)
    )


table(df.sample$won_on_shots)
table(df.sample$won_on_shots_lag)


## LPM ----


m1.1 <- feols(win ~ win_lag | team,
            data = df.sample,
            ~ team + game_id)

m1.2 <- feols(win ~ won_on_shots_lag | team,
            data = df.sample,
            ~ team + game_id)

etable(m1.1, m1.2)




## Logit models ----

m2.1 <- feglm(win ~ win_lag | team, 
            data = df.sample,
            family = binomial(link = "logit"), 
            ~ team + game_id)

m2.2 <- feglm(win ~ won_on_shots_lag | team, 
            data = df.sample,
            family = binomial(link = "logit"), 
            ~ team + game_id)

etable(m2.1, m2.2)



