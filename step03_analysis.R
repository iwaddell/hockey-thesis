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

df.look <- df %>% filter(season=="20222023") #%>% select(game_id, first_faceoff_winner)



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

df.subset.1 <- df.game.results.long %>%
  left_join(df.season, by=c("team","season")) %>% # joining team stats from earlier seasons
  filter(season_type == "R") %>%  # regular season
  filter(ended_in_overtime_lag == T) %>%  # no overtime games
  #filter(regular_length_game_lag == T) %>%  # no overtime games in the game prior
  mutate(
    ## flag late goals as those within the last-two percent 
    ## of 3 periods * 20 minutes * 60 seconds = 3600 seconds
    last_goal_frac_reg_lag_gt98 = ifelse(last_goal_frac_reg_lag>.98, 1, 0),
    win     = ifelse(result=="Win", 1, 0),
    win_lag = ifelse(result_lag=="Win", 1, 0),
  )

df.subset.2 <- df.game.results.long %>%
  left_join(df.season, by=c("team","season")) %>% # joining team stats from earlier seasons
  filter(season_type == "R") %>%  # regular season
  filter(ended_in_shootout_lag == T) %>%  # no overtime games
  #filter(regular_length_game_lag == T) %>%  # no overtime games in the game prior
  mutate(
    ## flag late goals as those within the last-two percent 
    ## of 3 periods * 20 minutes * 60 seconds = 3600 seconds
    last_goal_frac_reg_lag_gt98 = ifelse(last_goal_frac_reg_lag>.98, 1, 0),
    win     = ifelse(result=="Win", 1, 0),
    win_lag = ifelse(result_lag=="Win", 1, 0),
  )

df.subset.3 <- df.game.results.long %>%
  left_join(df.season, by=c("team","season")) %>% # joining team stats from earlier seasons
  filter(season_type == "R") %>%  # regular season
  filter(regular_length_game_lag == T) %>%  # no overtime games
  filter(abs(score_lag - opp_score_lag) == 1) %>%
  mutate(
    ## flag late goals as those within the last-two percent 
    ## of 3 periods * 20 minutes * 60 seconds = 3600 seconds
    last_goal_frac_reg_lag_gt98 = ifelse(last_goal_frac_reg_lag>.98, 1, 0),
    win     = ifelse(result=="Win", 1, 0),
    win_lag = ifelse(result_lag=="Win", 1, 0),
  )
  
df.subset.4 <- df.game.results.long %>%
    left_join(df.season, by=c("team","season")) %>% # joining team stats from earlier seasons
    filter(season_type == "R") %>%  # regular season
    filter(regular_length_game_lag == T) %>%  # no overtime games
    filter(abs(score_lag - opp_score_lag) == 1) %>%
    filter(max(last_goal_frac_reg) >= .97) %>%
    filter(score_before_last_goal_lag == 0) %>%
  mutate(
    ## flag late goals as those within the last-two percent 
    ## of 3 periods * 20 minutes * 60 seconds = 3600 seconds
    last_goal_frac_reg_lag_gt98 = ifelse(last_goal_frac_reg_lag>.98, 1, 0),
    win     = ifelse(result=="Win", 1, 0),
    win_lag = ifelse(result_lag=="Win", 1, 0),
  )

df.subset.5 <- df.game.results.long %>%
  left_join(df.season, by=c("team","season")) %>% # joining team stats from earlier seasons
  filter(season_type == "R") %>%  # regular season
  filter(abs(fenwick_events_lag - opp_fenwick_events_lag) <= 3) %>%
  mutate(
    ## flag late goals as those within the last-two percent 
    ## of 3 periods * 20 minutes * 60 seconds = 3600 seconds
    last_goal_frac_reg_lag_gt98 = ifelse(last_goal_frac_reg_lag>.98, 1, 0),
    win     = ifelse(result=="Win", 1, 0),
    win_lag = ifelse(result_lag=="Win", 1, 0),
  )

df.subset.6 <- df.game.results.long %>%
  left_join(df.season, by=c("team","season")) %>% # joining team stats from earlier seasons
  filter(season_type == "P") %>%  # regular season
  #filter(regular_length_game_lag == T) %>%  # no overtime games
  #filter(regular_length_game_lag == T) %>%  # no overtime games in the game prior
  mutate(
    ## flag late goals as those within the last-two percent 
    ## of 3 periods * 20 minutes * 60 seconds = 3600 seconds
    last_goal_frac_reg_lag_gt98 = ifelse(last_goal_frac_reg_lag>.98, 1, 0),
    win     = ifelse(result=="Win", 1, 0),
    win_lag = ifelse(result_lag=="Win", 1, 0),
  )

table(df.subset.1$season)
table(df.subset.1$result)
table(df.subset.2$season)
table(df.subset.2$result)

summary(df.sample$last_goal_seconds_lag)
summary(df.sample$last_goal_frac_reg_lag)

## Descriptive analysis ----

# Models (example) ----

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



#Table 1: Summary statistics ----

#Table 1.5: Team Summary statistics ----

#Table 2: Wald-Wolfowitz Runs Test ----

df.ww <- df.sample %>%
  group_by(season, team) %>%
  arrange(game_date, .by_group = TRUE) %>%
  select(win, season, team, game_date)

df.wwrt <- df.ww %>%
  arrange(season, team, game_date) %>%
  group_by(season, team) %>%
  summarise(win_vector = list(as.numeric(win)), .groups = "drop") %>%
  mutate(
    num_games = lengths(win_vector),
    num_wins = sapply(win_vector, function(x) sum(x == 1)),
    num_runs = sapply(win_vector, function(x) length(rle(x)$lengths)),
    win_fraction = sapply(win_vector, function(x) mean(x)),
    exp_runs_50 = 1 + 0.5 * (num_games - 1),
    exp_runs_adj = 1 + 2 * win_fraction * (1 - win_fraction) * (num_games - 1),
    p = win_fraction,
    variance_runs = 2 * p * (1 - p) * (2 * p * (1 - p) * (num_games - 2) + 1),
    z_runs = (num_runs - exp_runs_adj) / sqrt(variance_runs),
    p_value = 2 * pnorm(-abs(z_runs))  # two-sided test
  )

df.wwrt2014 <- df.wwrt %>%
  filter(season == "20142015")

df.tableww1 <- df.wwrt2014 %>%
  select(team, num_games, num_wins, num_runs, exp_runs_adj, z_runs, p_value)

write(
  kable(df.tableww1, format = "latex", booktabs = TRUE, digits = 3,
        caption = "Team Win Streak Run Statistics") %>%
    kable_styling(latex_options = c("hold_position", "striped")),
  paste0("Tables/table_2A_wald_wolfowitz.tex")
)


mean_games <- mean(df.tableww1$num_games, na.rm = TRUE)
mean_wins <- mean(df.tableww1$num_wins, na.rm = TRUE)
mean_runs <- mean(df.tableww1$num_runs, na.rm = TRUE)
mean_expruns <- mean(df.tableww1$exp_runs_adj, na.rm = TRUE)
mean_p <- mean(df.tableww1$p_value, na.rm = TRUE)

#table 2b all seasons


df.wwrtall <- df.wwrt %>%
  group_by(team) %>%
  

df.tableww1 <- df.wwrt2014 %>%
  select(team, num_games, num_wins, num_runs, exp_runs_adj, z_runs, p_value)

write(
  kable(df.tableww1, format = "latex", booktabs = TRUE, digits = 3,
        caption = "Team Win Streak Run Statistics") %>%
    kable_styling(latex_options = c("hold_position", "striped")),
  paste0("Tables/table_2A_wald_wolfowitz.tex")
)


mean_games <- mean(df.tableww1$num_games, na.rm = TRUE)
mean_wins <- mean(df.tableww1$num_wins, na.rm = TRUE)
mean_runs <- mean(df.tableww1$num_runs, na.rm = TRUE)
mean_expruns <- mean(df.tableww1$exp_runs_adj, na.rm = TRUE)
mean_p <- mean(df.tableww1$p_value, na.rm = TRUE)



#Table 3: Chi-Squared ----

#longest_streak <- df.sample %>%
#  group_by(season, team) %>%
#  summarise(
#    max_streak = max(rle(win)$lengths),
#    .groups = "drop"
#  ) %>%
#  summarise(longest_streak_overall = max(max_streak)) %>%
#  pull(longest_streak_overall)

df.chi <- df.wwrt %>%
  rowwise() %>%
  mutate(
    streak_counts = list(
      {
        run_lengths <- rle(win_vector)$lengths
        tab <- tabulate(run_lengths, nbins = 18)
        as.list(tab)
      }
    )
  ) 

df.chi2015 <- df.chi %>%
  filter(season == "20142015")

observed <- df.chi2015$streak_counts %>%
  map(~ if (is.null(.x)) rep(0, 18) else as.numeric(.x)) %>%
  reduce(`+`)

# Name the result
names(observed) <- paste0("streak_", seq_along(observed))


p_win <- mean(unlist(df.chi2015$win_vector))

# Simulate many random sequences and count streaks
set.seed(123)
sim_counts <- replicate(10000, {
  seq <- rbinom(82, 1, p_win)  # Simulate a typical-length win_vector
  run_lengths <- rle(seq)$lengths
  tabulate(run_lengths, nbins = 18)
})

expected <- rowMeans(sim_counts) * length(df.chi2015$win_vector)
names(expected) <- paste0("streak_", 1:18)


obs_combined <- c(observed[1:7], sum(observed[8:18]))
exp_combined <- c(expected[1:7], sum(expected[8:18]))

# Run Chi-squared test again
chisq_test_result <- chisq.test(x = obs_combined, p = exp_combined / sum(exp_combined))

observedAll <- df.chi$streak_counts %>%
  map(~ if (is.null(.x)) rep(0, 18) else as.numeric(.x)) %>%
  reduce(`+`)

names(observedAll) <- paste0("streak_", seq_along(observedAll))


p_winAll <- mean(unlist(df.chi$win_vector))

# Simulate many random sequences and count streaks
set.seed(123)
sim_countsAll <- replicate(10000, {
  seq <- rbinom(82, 1, p_win)  # Simulate a typical-length win_vector
  run_lengths <- rle(seq)$lengths
  tabulate(run_lengths, nbins = 18)
})

expectedAll<- rowMeans(sim_countsAll) * length(df.chi$win_vector)
names(expectedAll) <- paste0("streak_", 1:18)


obsAll_combined <- c(observedAll[1:7], sum(observedAll[8:18]))
expAll_combined <- c(expectedAll[1:7], sum(expectedAll[8:18]))

# Run Chi-squared test again
chisq_test_result_all <- chisq.test(x = obsAll_combined, p = expAll_combined / sum(expAll_combined))


#Table 4: Winning on Winning ----

m1.0 <- feols( win ~ win_lag,
               data=df.sample, ~ team + game_id)

m1.1 <- feols( win ~ win_lag | team,
               data=df.sample, ~ team + game_id)

m1.2 <- feols( win ~ win_lag | team + season,
               data=df.sample, ~ team + game_id)

m1.3 <- feols( win ~ win_lag | team^season,
               data=df.sample, ~ team + game_id)

m1.4 <- feols( win ~ I(corsi_events_lag - score_lag) | team^season, 
               data=df.sample, ~ team + game_id)

m1.5 <- feols( win ~ win_lag + I(corsi_events_lag - score_lag) | team^season, 
               data=df.sample, ~ team + game_id)

m1.6 <- feols( win ~ win_lag + I(corsi_events_lag - score_lag) + played | team^season, 
               data=df.sample, ~ team + game_id)

m1.7 <- feols( win ~ win_lag + I(corsi_events_lag - score_lag) + played | team^season + opp_team^season, 
               data=df.sample, ~ team + game_id)

m1.8 <- feols( win ~ win_lag + I(corsi_events_lag - score_lag) + played | team^opp_team^season, 
               data=df.sample, ~ team + game_id)

#m1.9 <- feols( win ~ win_lag + played | opp_team^season, # gives significant positive effect of winning
#               data=df.sample, ~ team + game_id)

#m1.10 <- feols( win ~ win_lag + played + ended_in_overtime_lag + ended_in_overtime_lag*win_lag | team^season, # gives significant positive effect of winning
#               data=df.sample, ~ team + game_id)


etable(m1.0, m1.1, m1.2,
       m1.3, m1.4, m1.5, m1.6, m1.7, m1.8)
#          
# write(etable(m1.0, m1.1, m1.2, m1.3, m1.4, m1.5, m1.6, m1.7, m1.8, tex=T), "Tables/table_4_winning_on_winning.tex")


m8.0 <- feols( win ~ win_lag + I(corsi_events_lag - score_lag) + played, 
               data=df.sample, ~ team + game_id)

m8.1 <- feols( win ~ win_lag + I(corsi_events_lag - score_lag) + played | team^season, 
               data=df.sample, ~ team + game_id)

mean(df.sample$score - df.sample$opp_score)


## etable() settings
depvar.        = F
digits.        = 3
digits.stats.  = 2

dict. = c(
  win                    = "Win",
  win_lag                = "Won last game",
  playedhome             = "Home team",
  points_regular_season  = "Regular-season points",
  fenwick_events         = "Fenwick",
  fenwick_events_lag     = "Fenwick (last game)",
  team                   = "Team",
  opp_team               = "Opponent",
  season                 = "Season",
  season_type            = "Regular/playoff",
  ended_in_overtime_lagTRUE  = "OT last game",
  I(corsi_events_lag-score_lag) = "Corsi (no goals)"
  )

write(
  etable(m1.0, m1.1, m1.2, 
       m1.3, m1.4, m1.5, m1.6, m1.7, m1.8,
       style.tex = style.tex("aer"), 
       fitstat = ~ r2 + n, tex = TRUE, 
       dict = dict.,
      order = c("!Constant"),
       digits=digits., digits.stats=digits.stats., depvar=depvar.),
  paste0("Tables/table_4_winning_on_winning.tex")
  )


mean_corsi_win0 <- var(df.sample$corsi_events[df.sample$win == 0], na.rm = TRUE)
mean_corsi_win1 <- var(df.sample$corsi_events[df.sample$win == 1], na.rm = TRUE)

# Difference: mean for win == 1 minus mean for win == 0
difference <- mean_corsi_win1 - mean_corsi_win0

# Print result
print(difference)


#Table 5: Subsets: Winning on winning ----

n1.0 <- feols( win ~ win_lag,
               data=df.subset.1, ~ team + game_id)

n1.1 <- feols( win ~ win_lag + played | team + season,
               data=df.subset.1, ~ team + game_id)

n1.2 <- feols( win ~ win_lag + I(corsi_events_lag - score_lag) | team^season,
               data=df.subset.1, ~ team + game_id)

n1.3 <- feols( win ~ win_lag + I(corsi_events_lag - score_lag) + played | team^season,
               data=df.subset.1, ~ team + game_id)

n1.4 <- feols( win ~ win_lag + I(corsi_events_lag - score_lag) + played | team^season + opp_team^season, 
               data=df.subset.1, ~ team + game_id)

n2.3 <- feols( win ~ win_lag + I(corsi_events_lag - score_lag) + played | team^season,
               data=df.subset.2, ~ team + game_id)
n3.3 <- feols( win ~ win_lag + I(corsi_events_lag - score_lag) + played | team^season,
               data=df.subset.3, ~ team + game_id)
n4.3 <- feols( win ~ win_lag + I(corsi_events_lag - score_lag) + played | team^season,
               data=df.subset.4, ~ team + game_id)
n5.3 <- feols( win ~ win_lag + I(corsi_events_lag - score_lag) + played | team^season,
               data=df.subset.5, ~ team + game_id)
n6.3 <- feols( win ~ win_lag + I(corsi_events_lag - score_lag) + played | team^season,
               data=df.subset.6, ~ team + game_id)

depvar.        = F
digits.        = 3
digits.stats.  = 2

write(
  etable(n1.3, n2.3, n3.3, 
         n4.3, n5.3, n6.3,
         style.tex = style.tex("aer"), 
         fitstat = ~ r2 + n, tex = TRUE, 
         dict = dict.,
         order = c("!Constant"),
        headers = list("Overtime games"           = 1,
                        "Shootout games"          = 1,
                        "Won by one goal"         = 1,
                        "Won in last 100 seconds" = 1, 
                        "Fenwick"                 = 1,
                        "Playoff games"           = 1),
         digits=digits., digits.stats=digits.stats., depvar=depvar.),
  paste0("Tables/table_5_subset_winning.tex")
)


#Table 6: Winning on other things ----

m6.0 <- feols(win ~ win_lag + I(corsi_events_lag - score_lag) + played | team^season,
               data=df.sample, ~ team + game_id)

m6.1 <- feols(score ~ win_lag + I(corsi_events_lag - score_lag) + played | team^season,
              data=df.sample, ~ team + game_id)

m6.2 <- feols(fenwick_events ~ win_lag + I(corsi_events_lag - score_lag) + played | team^season,
              data=df.sample, ~ team + game_id)

m6.3 <- feols(score_1st ~ win_lag + I(corsi_events_lag - score_lag) + played | team^season,
              data=df.sample, ~ team + game_id)

m6.4 <- feols(I(score_1st > opp_score_1st) ~ win_lag + I(corsi_events_lag - score_lag) + played | team^season,
              data=df.sample, ~ team + game_id)

m6.5 <- feols(fenwick_1st ~ win_lag + I(corsi_events_lag - score_lag) + played | team^season,
              data=df.sample, ~ team + game_id)

m6.6 <- feols(I(fenwick_events > opp_fenwick_events) ~ win_lag + I(corsi_events_lag - score_lag) + played | team^season,
              data=df.sample, ~ team + game_id)

m6.7 <- feols(I(first_faceoff_winner == team) ~ win_lag + I(corsi_events_lag - score_lag) + played | team^season,
              data=df.sample, ~ team + game_id)

m6.8 <- feols(possession_frac ~ win_lag + I(corsi_events_lag - score_lag) + played | team^season,
              data=df.sample, ~ team + game_id)

write(
  etable(m6.0, m6.1, m6.2, m6.6, m6.8, m6.3, m6.4, m6.5, m6.7,
         style.tex = style.tex("aer"), 
         fitstat = ~ r2 + n, tex = TRUE, 
         dict = dict.,
         order = c("!Constant"),
         headers = list("Win"           = 1,
                        "Score"          = 1,
                        "Fenwick"         = 1,
                        "Higher Fenwick" = 1, 
                        "Possession (%)"                 = 1,
                        "Score (1st pd)"           = 1,
                        "Ahead (1st pd)" = 1,
                        "Fenwick (1st pd)" = 1,
                        "Won First Faceoff"=1),
         digits=digits., digits.stats=digits.stats., depvar=depvar.),
  paste0("Tables/table_6_winning_on_other_things.tex")
)

#Table 7: Power analysis ----




