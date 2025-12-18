# SETUP ----

if (!require("pacman")) install.packages("pacman")
library(pacman);p_load(tidyverse, dplyr, tidyr, ggplot2, readr, fixest, broom, fst, purrr, splines, kableExtra);

#install.packages("sportyR")
library(sportyR)

# hockeyR 
## See https://github.com/danmorse314/hockeyR?tab=readme-ov-file

# themes / settings ----

# ggsave...
width.=12; height.=9; dpi.=300;

# splines:bs
knots. = c(1200, 2400, 3600)

mytheme <- theme_minimal() + theme(
  legend.position = "none",
  panel.grid.minor.y = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank()
)

# READ DATA ----

df                   <- read_fst(paste0("Data/step02_df.fst"), as.data.table = FALSE)
#df.game.results      <- read_fst(paste0("Data/step02_game_results.fst"), as.data.table = FALSE)
df.game.results.long <- read_fst(paste0("Data/step02_game_results_long.fst"), as.data.table = FALSE)
df.season            <- read_fst(paste0("Data/step02_season.fst"), as.data.table = FALSE)

table(df.game.results.long$season_type, df.game.results.long$season)

df.look <- df %>% filter(season=="20222023")


# Sample ----

df.sample <- df.game.results.long %>%
  left_join(df.season, by=c("team","season")) %>% # joining team stats from earlier seasons
  filter(season_type == "R") %>%  # regular season
  #filter(regular_length_game == T) %>%  # no overtime games
  #filter(regular_length_game_lag == T) %>%  # no overtime games in the game prior
  mutate(
    ## flag late goals as those within the last-two percent 
    ## of 3 periods * 20 minutes * 60 seconds = 3600 seconds
    last_goal_frac_reg_lag_gt98 = ifelse(last_goal_frac_reg_lag>.98, 1, 0),
    win     = ifelse(result=="Win", 1, 0),
    win_lag = ifelse(result_lag=="Win", 1, 0),
  )

table(df.sample$season)
table(df.sample$result)

summary(df.sample$last_goal_seconds_lag)
summary(df.sample$last_goal_frac_reg_lag)

## Descriptive analysis ----

# Models ----

games_unique <- df.game.results.long %>%
  distinct(game_id, .keep_all = TRUE)

total_games <- nrow(games_unique)

ot_games <- games_unique %>% filter(ended_in_overtime == TRUE) %>% nrow()

so_games <- games_unique %>% filter(ended_in_shootout == TRUE) %>% nrow()

df.one_goal_games <- df.game.results.long %>%
  filter(ended_in_overtime == FALSE) %>%
  group_by(game_id) %>%
  summarise(goal_diff = abs(diff(score)), last_goal = max(last_goal_frac_reg)) %>%
  filter(goal_diff == 1)

one_goal_games <- df.one_goal_games %>% nrow()

close_finish_games <- df.one_goal_games %>%
  filter(last_goal >= 0.97) %>%
  nrow()

results <- data.frame(
  Category = c("Total games",
               "Overtime games",
               "Shootout games",
               "One-goal games",
               "Games with last goal in final 3%"),
  Count = c(total_games,
            ot_games,
            so_games,
            one_goal_games,
            close_finish_games),
  Percentage = sprintf("%.1f%%", 100 * c(
    total_games,
    ot_games,
    so_games,
    one_goal_games,
    close_finish_games) / total_games)
)

etable_input <- as.matrix(results[, c("Count", "Percentage")])
rownames(etable_input) <- results$Category

results %>%
  kable(format = "latex", booktabs = TRUE, caption = "Summary of Hockey Games") %>%
  kable_styling(latex_options = c("hold_position", "striped"))


### team FEs and controls ----

# feols( win ~  win + stuff | FE , 
#    data=df.sample, ~ clusters)

m1.0 <- feols( win ~ win_lag,
   data=df.sample, ~ team + game_id)

m1.1 <- feols( win ~ win_lag | team,
   data=df.sample, ~ team + game_id)

m1.2 <- feols( win ~ win_lag | team + season, # season shouldn't matter... winning is zero-sum
   data=df.sample, ~ team + game_id)

m1.3 <- feols( win ~ win_lag | team^season, # season-specific team FEs
   data=df.sample, ~ team + game_id)


m1.4 <- feols( win ~ win_lag + fenwick_events | team^season, 
   data=df.sample, ~ team + game_id)

m1.5 <- feols( win ~ win_lag + blocked + shots | team^season, 
   data=df.sample, ~ team + game_id)

m1.6 <- feols( win ~ win_lag + fenwick_events + fenwick_events_lag + hits_lag | team^season, 
   data=df.sample, ~ team + game_id)


etable(m1.0, m1.1, #m1.2, 
       m1.3, m1.4)

write(etable(m1.0, m1.1, m1.3, m1.4, tex=T), "Tables/table-name.tex")



### shootout wins? ----

feols( win ~ win_lag*ended_in_shootout_lag + fenwick_events | team^season, 
               data=df.sample,
               ~ team + game_id)


m2.1 <- feols( win ~ win_lag + fenwick_events | team^season, 
               data=df.sample %>% filter(ended_in_shootout_lag==F),
               ~ team + game_id)
m2.2 <- feols( win ~ win_lag + fenwick_events | team^season, 
               data=df.sample %>% filter(ended_in_shootout_lag==T),
               ~ team + game_id)

etable(m2.1, m2.2,
       headers = list("Lag not shootout" = 1,
                      "Lag shootout"     = 1)
)

etable(m1.4, m2.1)


### other outcome? (i.e., not winning) ----

m3.1 <- feols( win ~ win_lag + fenwick_events_lag | team + season, 
               data=df.sample,
               ~ team + game_id)

m3.1b <- feols( win ~ win_lag + fenwick_events_lag | team^season, 
               data=df.sample,
               ~ team + game_id)

m3.2 <- feols( fenwick_events ~ win_lag + fenwick_events_lag | team + season, 
               data=df.sample,
               ~ team + game_id)

m3.2b <- feols( fenwick_events ~ win_lag + fenwick_events_lag | team^season, 
               data=df.sample,
               ~ team + game_id)

etable(m3.1, m3.1b, m3.2, m3.2b)





### Logit versions? ----

# LPM are faster to work with, but... eventually think about fitting win probabilities to a sigmoid 

feglm((result=="Win") ~ (result_lag=="Win") |
               team + season, 
            data = df.sample,
            family = binomial(link = "logit"), 
            ~ season)


#Table 1: Summary statistics

#Table 1.5: Team Summary statistics

#Table 2: Wald-Wolfowitz Runs Test

#Table 3: Chi-Squared

#Table 4: Winning on Winning

m1.0 <- feols( win ~ win_lag,
               data=df.sample, ~ team + game_id)

m1.1 <- feols( win ~ win_lag | team,
               data=df.sample, ~ team + game_id)

m1.2 <- feols( win ~ win_lag | team + season,
               data=df.sample, ~ team + game_id)

m1.3 <- feols( win ~ win_lag | team^season,
               data=df.sample, ~ team + game_id)

m1.4 <- feols( win ~ win_lag + fenwick_events_lag | team^season, 
               data=df.sample, ~ team + game_id)

m1.5 <- feols( win ~ win_lag + played | team^season, 
               data=df.sample, ~ team + game_id)

m1.6 <- feols( win ~ win_lag + played | team^season + opp_team^season, 
               data=df.sample, ~ team + game_id)

m1.7 <- feols( win ~ win_lag + played | team^opp_team^season, 
               data=df.sample, ~ team + game_id)

m1.8 <- feols( win ~ win_lag + played | opp_team^season, # gives significant positive effect of winning
               data=df.sample, ~ team + game_id)


etable(m1.0, m1.1, m1.2, 
       m1.3, m1.4, m1.5, m1.6, m1.7, m1.8)
         
write(etable(m1.0, m1.1, m1.2, m1.3, m1.4, m1.5, m1.6, m1.7, m1.8, tex=T), "Tables/table_4_winning_on_winning.tex")


## etable() settings
depvar.        = F
digits.        = 3
digits.stats.  = 2

dict.a = c(
  win                    = "Win",
  home                   = "Home team",
  points_regular_season  = "Regular-season points",
  fenwick_events         = "Fenwick",
  team                   = "Team",
  season                 = "Season",
  season_type            = "Regular/playoff"
  )

write(
  etable(m1.0, m1.1, m1.2, 
       m1.3, m1.4, m1.5, m1.6, m1.7, m1.8,
       style.tex = style.tex("aer"), 
       fitstat = ~ r2 + n, tex = TRUE, 
       dict = dict.a,
       digits=digits., digits.stats=digits.stats., depvar=depvar.),
  paste0("Tables/table_4_winning_on_winning.tex")
  )





#Table 5: Subsets: Winning on winning

#Table 6: Winning on other things

#Table 7: Power analysis




