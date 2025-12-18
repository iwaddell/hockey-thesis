
# ctrl-opt "oh" collapses sections

# check out "collapse" package? apparently very efficient with reshape-type tasks.

# SETUP ----

if (!require("pacman")) install.packages("pacman")
library(pacman);p_load(tidyverse, dplyr, tidyr, ggplot2, readr, fixest, broom, fst, purrr, splines);

#install.packages("sportyR")
library(sportyR)

# hockeyR 
## See https://github.com/danmorse314/hockeyR?tab=readme-ov-file

# themes / settings ----

# ggsave...
width.=12;height.=9;dpi.=300;

# splines:bs
knots. = c(1200, 2400, 3600)

mytheme <- theme_minimal() + theme(
  legend.position = "none",
  panel.grid.minor.y = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank()
)

## The available data goes back to the 2010-2011 season...
seasons <- c(2010:2023)

# Once saved locally (see step01_), can use this...
readpbp <- function(season){
  df <- read_fst(paste0("pbpdata/pbp_",season,".fst"), as.data.table = FALSE)
  # columns changed in 2023...
  if (season==2023) {
    df <- df %>% rename(
      venue_name        = venue,
      home_abbreviation = home_abbr,
      away_abbreviation = away_abbr
      ) %>%
      mutate(      
        season_type = case_when(season_type == "REG" | season_type == "2" ~ "R",
                                season_type == "POST"                     ~ "P")
        )
  }
  return(df)
}
## Read in a specific season...
#df.2023 <- readpbp(2023)

# Just looking at data here... ----

df.look <- readpbp(2021)     # readpbp() reads from local file...


df.look1 <- readpbp(2022)     # readpbp() reads from local file...
df.look2 <- readpbp(2023)
# Exact same names *and* order?
identical(names(df.look1), names(df.look2))
# → TRUE if they match perfectly

# Same set of names, regardless of order?
setequal(names(df.look1), names(df.look2))
# → TRUE if they contain the same names but maybe in a different order

# If you want to see which names differ:
setdiff(names(df.look1), names(df.look2))  # in df1 but not df2
setdiff(names(df.look2), names(df.look1))  # in df2 but not df1

df.look <- readpbp(2021) %>% filter(event_type=="GOAL")
df.look <- readpbp(2021) %>% filter(event_type=="FACEOFF")


df.faceoffs <- readpbp(2021) %>% filter(event_type=="FACEOFF") %>%
  group_by(game_id) %>% #arrange(game_id, game_seconds) %>%
  mutate(
    infaceoffs = list(unique(c(event_player_1_name,event_player_2_name)))
  )

df.look <- readpbp(2022) %>% 
  select(game_id, home_abbreviation, away_abbreviation, game_seconds, event_type, 
         home_score, away_score, home_final, away_final, ordinal_num) %>%
  filter(event_type=="GAME_END") %>%
  mutate(
    flag1 = ifelse(home_score!=home_final, T, F),
    flag2 = ifelse(away_score!=away_final, T, F)
  )

table(df.look$flag1, useNA="ifany")
table(df.look$flag2, useNA="ifany")

unique(subset(df.look,is.na(flag1))$game_id)
#2022030161 2022030173

df.look2 <- readpbp(2022) %>% filter(game_id==2022030161) %>%
    select(game_id, home_abbreviation, away_abbreviation, game_seconds, event_type, 
         home_score, away_score, home_final, away_final, ordinal_num, period, season_type, period_type, game_date)



# what's in the dataset?
# table(df.look$season)
# table(df.look$season_type)
# length(unique(as.numeric(df.look$game_id)))
# 
# unique(df.look$event_type)
# 
# table(df.look$period, df.look$season_type)
# 
# table(df.look$venue_name)


# Main df starts HERE ----

## working example of a single year...

# look at all columns... 
#df.2021 <- readpbp(2021) %>% filter(event_type=="PENALTY")

# play around here. if good, copy into the readcommon_df function below...
df.test <- readpbp(2021) %>% 
  select(season, season_type, game_id, game_date, venue_name, 
         event_type, event_team_abbr, penalty_severity, 
         description, secondary_type, event_player_1_name, event_player_1_id,
         event_player_2_name, event_player_2_id,
         home_name, away_name,
         period, period_seconds, game_seconds,
         home_abbreviation, away_abbreviation, 
         empty_net, #game_winning_goal,
         home_score, away_score, home_final, away_final, 
         x_fixed, y_fixed, x, y,
         shot_distance, shot_angle, home_skaters, away_skaters,
         period_seconds_remaining   # for possession estimation
         ) %>%
  arrange(game_id, period, period_seconds, game_seconds)

#df.2018 <- readpbp(2018)
#df.2022 <- readpbp(2022)
#df.2023 <- readpbp(2023)

#df.look <- df.2021 %>% filter(event_type=="PENALTY")
#unique(df.look$secondary_type)

# a function that does the same, that can be applied to any season if they have these columns of data...
readcommon_df <- function(season){
  readpbp(season) %>% 
  select(season, season_type, game_id, game_date, venue_name, 
         event_type, event_team_abbr, penalty_severity, 
         description, secondary_type, event_player_1_name, event_player_1_id,
         event_player_2_name, event_player_2_id,
         home_name, away_name,
         period, period_seconds, game_seconds,
         home_abbreviation, away_abbreviation, 
         empty_net, #game_winning_goal,
         home_score, away_score, home_final, away_final, 
         x_fixed, y_fixed, x, y,
         shot_distance, shot_angle, home_skaters, away_skaters,
         period_seconds_remaining   # for possession estimation
         ) %>%
  arrange(game_id, period, period_seconds, game_seconds)
}


## work with at-least two seasons, to get code structure set up... 

df <- rbind(
  readcommon_df(2010), 
  readcommon_df(2011), 
  readcommon_df(2012), 
  readcommon_df(2013), 
  readcommon_df(2014), 
  readcommon_df(2015), 
  readcommon_df(2016), 
  readcommon_df(2017), 
  readcommon_df(2018), 
  readcommon_df(2019), 
  readcommon_df(2020), 
  readcommon_df(2021),
  readcommon_df(2022),
  readcommon_df(2023))

table(df$season)
      
## FIXES (as found) ----

df <- df %>% 
  rename(
    home_score_orig = home_score, # re-created below off of event_type info
    away_score_orig = away_score,
    home_final_orig = home_final,
    away_final_orig = away_final) %>%
  mutate(
    # The playoffs commenced on August 1, 2020, and concluded on September 28, but
    # some were recorded as "R"
    season_type = case_when(season == "20192020" & game_date >= "2020-08-01" ~ "P",
                            T ~ season_type)
  )

table(df$season,df$season_type) # WA?
df.look <- df %>% filter(season_type=="WA")
# 2020 NHL All-Star Skills Competition # ARE THERE OTHE ALL-STAR GAMES IN THE DF?
df <- df %>% filter(season_type!="WA")


#df$away_score = ifelse(df$game_id==2022030215 & df$event_type=="GAME_END", 3, df$away_score)
#df$away_final = ifelse(df$game_id==2022030215 & df$event_type=="GAME_END", 3, df$away_final)

## COVID info ----

# 2019–20 Season: Suspension and Bubble Playoffs
# Season Pause: On March 12, 2020, the NHL suspended the regular season in response to the escalating COVID-19 pandemic.
# Season Conclusion: The regular season was officially declared over in May 2020. 
# Return to Play: The NHL and NHLPA agreed on a 24-team playoff format held in two hub cities: Toronto and Edmonton. Games were played without spectators, and strict health protocols were enforced. 
# Outcome: The playoffs commenced on August 1, 2020, and concluded on September 28, with the Tampa Bay Lightning winning the Stanley Cup. 

# 2020–21 Season: Shortened Schedule and Realignment
# Delayed Start: The season began on January 13, 2021, later than usual, and featured a reduced 56-game schedule. 
# Division Realignment: To minimize travel and accommodate cross-border restrictions, the NHL temporarily realigned into four divisions, including an all-Canadian division. 
# Health Protocols: The league implemented comprehensive COVID-19 protocols, including daily testing, mask mandates, and restrictions on player interactions.
# Attendance: Most games were played without fans, though some U.S. arenas allowed limited attendance later in the season, depending on local regulations.
month_to_num <- function(m) {
  sprintf("%02d", match(m, month.abb))
}
pad_zero <- function(x) {
  sprintf("%02d", as.integer(x))
}
df.covid.timing <- read.csv("NHL_2020_21_Fan_Attendance.csv") %>%
  rename(
    home_abbreviation = Abbreviation
  ) %>%
  mutate(
    season         = "20202021",
    game_date_open = paste0("2021-",month_to_num(substr(Date.Fans.Allowed,1,3)),
                       "-",pad_zero(substr(Date.Fans.Allowed,5,6))),
    percent_open   = as.numeric(substr(Capacity.Percent,1,2))
  ) %>%
  select(season, home_abbreviation, game_date_open, percent_open, Notes)
# Season Conclusion: The Tampa Bay Lightning secured their second consecutive Stanley Cup on July 7, 2021. 

## Flag international/neutral venues ----

# df.special.games <- read.csv("NHL_special_games.csv") %>%
#   rename(
#     home_abbreviation = Home,
#     away_abbreviation = Away,
#     special_type      = Type,
#     special_location  = Location
#   ) %>%
#   mutate(
#     game_date = as.Date(Date, format = "%m/%d/%y"),
#     year      = year(game_date),
#     month     = month(game_date),
#     season    = case_when(
#       month > 9   ~ paste0(year,year+1),
#       month <= 9  ~ paste0(year-1,year)
#     ),
#     game_id =  case_when(
#       game_date=="2010-10-07" & home_abbreviation %in% c("MIN","CAR") ~ 2010020003,
#       game_date=="2010-10-08" & home_abbreviation %in% c("MIN","CAR") ~ 2010020008,
#       game_date=="2010-10-09" & home_abbreviation %in% c("BOS","PHX") ~ 2010020012,
#       game_date=="2010-10-10" & home_abbreviation %in% c("BOS","PHX") ~ 2010020024,
#       game_date=="2011-01-01" & home_abbreviation %in% c("PIT","WSH") ~ 2010020566,
#       game_date=="2014-01-25" & home_abbreviation %in% c("LAK","ANA") ~ 2013020782,
#       game_date=="2017-11-10" & home_abbreviation %in% c("COL","OTT") ~ 2017020247,
#       game_date=="2017-11-11" & home_abbreviation %in% c("COL","OTT") ~ 2017020252,
#       game_date=="2018-11-01" & home_abbreviation %in% c("WPG","FLA") ~ 2018020180,
#       game_date=="2018-11-02" & home_abbreviation %in% c("WPG","FLA") ~ 2018020192,
#       game_date=="2019-10-04" & home_abbreviation %in% c("CHI","PHI") ~ 2019020016,
#       game_date=="2019-11-08" & home_abbreviation %in% c("TBL","BUF") ~ 2019020249,
#       game_date=="2019-11-09" & home_abbreviation %in% c("TBL","BUF") ~ 2019020256,
#       game_date=="2021-02-20" & home_abbreviation %in% c("COL","VGK") ~ 2020020287,
#       game_date=="2021-02-21" & home_abbreviation %in% c("BOS","PHI") ~ 2020020290,
#       game_date=="2022-10-07" & home_abbreviation %in% c("NSH","SJS") ~ 2022020001,
#       game_date=="2022-10-08" & home_abbreviation %in% c("NSH","SJS") ~ 2022020002,
#       game_date=="2023-11-16" & home_abbreviation %in% c("DET","OTT") ~ 2023020242,
#       game_date=="2023-11-17" & home_abbreviation %in% c("TOR","DET") ~ 2023020251,
#       game_date=="2023-11-18" & home_abbreviation %in% c("MIN","OTT") ~ 2023020254,
#       game_date=="2023-11-19" & home_abbreviation %in% c("MIN","TOR") ~ 2023020267,
#     )
#   ) %>% 
#   filter(year>=2010) %>%
#   arrange(game_date) %>%
#   select(game_date, game_id, home_abbreviation, special_location, special_type)

# df.look <- df %>% 
#   filter(season == "20232024") %>%
#   filter(
#     (home_abbreviation == "MIN" & away_abbreviation == "OTT") | 
#     (away_abbreviation == "MIN" & home_abbreviation == "OTT"))
# 
# unique(df.look$game_date)
# table(df.look$game_date,df.look$game_id)

special_game_ids = c(2010020003, 2010020008, 2010020012, 2010020024, 2010020566, 2013020782, 2017020247, 2017020252, 2018020180, 2018020192, 2019020016, 2019020249, 2019020256, 2020020287, 2020020290, 2022020001, 2022020002, 2023020242, 2023020251, 2023020254, 2023020267)

special_venues = c("Ericsson Globe", "O2 Arena Berlin", "Scandinavium", "McMahon Stadium", "Hartwall Areena", "Citizens Bank Park", "The Big House","Yankee Stadium", "Soldier Field", "BC Place", "Levi's Stadium", "Gillette Stadium", "TCF Bank Stadium", "Coors Field", "Investors Group Field", "BMO Field", "Busch Stadium", "Heinz Field", "Lansdowne Park", "Citi Field", "Navy-Marine Corps Memorial Stadium", "Notre Dame Stadium", "Lincoln Financial Field", "Mosaic Stadium", "Cotton Bowl Stadium", "Falcon Stadium", "Tim Hortons Field", "Nokia Arena", "Fenway Park", "Carter-Finley Stadium", "Commonwealth Stadium, Edmonton", "T-Mobile Park", "MetLife Stadium")

## moved below... 
# df <- df %>%
#   mutate(
#     special_game = game_id %in% special_game_ids | venue_name %in% special_venues
#   )
# unique(subset(df, special_game==T)$venue_name)
# unique(subset(df, special_game!=T)$venue_name)


## Add game-level Xs here ----

# team/game performance measures 
### Corsi = Goals + shots on goal + missed shots + blocked shots. (Basically, all shot attempts.)
corsi_events <- c("BLOCKED_SHOT","MISSED_SHOT","SHOT","GOAL")
### Fenwick = Goals + shots on goal + missed shots. (All unblocked shot attempts.)
fenwick_events <- c("MISSED_SHOT","SHOT","GOAL")


df <- df %>% #subset(season=="20102011") %>%
  left_join(df.covid.timing, by=c("season","home_abbreviation")) %>%
  mutate(
    ## other game attributes...
    special_game       = (game_id %in% special_game_ids) | (venue_name %in% special_venues),
    faceoff_won_home   = ifelse(event_type =="FACEOFF" & event_team_abbr == home_abbreviation, 1, 0),
    faceoff_won_away   = ifelse(event_type =="FACEOFF" & event_team_abbr == away_abbreviation, 1, 0),
    first_faceoff_won_home = ifelse(event_type =="FACEOFF" & game_seconds == 0 & event_team_abbr == home_abbreviation, 1, 0),
    first_faceoff_won_away = ifelse(event_type =="FACEOFF" & game_seconds == 0 & event_team_abbr == away_abbreviation, 1, 0),
    corsi_event_home   = ifelse(event_type %in% corsi_events & event_team_abbr == home_abbreviation, 1, 0),
    corsi_event_away   = ifelse(event_type %in% corsi_events & event_team_abbr == away_abbreviation, 1, 0),
    fenwick_event_home = ifelse(event_type %in% fenwick_events & event_team_abbr == home_abbreviation, 1, 0),
    fenwick_event_away = ifelse(event_type %in% fenwick_events & event_team_abbr == away_abbreviation, 1, 0),
    blocked_home       = ifelse(event_type =="BLOCKED_SHOT" & event_team_abbr == home_abbreviation, 1, 0),
    blocked_away       = ifelse(event_type =="BLOCKED_SHOT" & event_team_abbr == away_abbreviation, 1, 0),
    blocked_theirs_home = ifelse(event_type =="BLOCKED_SHOT" & event_team_abbr == away_abbreviation, 1, 0), # flip
    blocked_theirs_away = ifelse(event_type =="BLOCKED_SHOT" & event_team_abbr == home_abbreviation, 1, 0), # flip
    shot_home          = ifelse(event_type =="SHOT" & event_team_abbr == home_abbreviation, 1, 0),
    shot_away          = ifelse(event_type =="SHOT" & event_team_abbr == away_abbreviation, 1, 0),
    missed_home        = ifelse(event_type =="MISSED_SHOT" & event_team_abbr == home_abbreviation, 1, 0),
    missed_away        = ifelse(event_type =="MISSED_SHOT" & event_team_abbr == away_abbreviation, 1, 0),
    penalty_home       = ifelse(event_type =="PENALTY" & event_team_abbr == home_abbreviation, 1, 0),
    penalty_away       = ifelse(event_type =="PENALTY" & event_team_abbr == away_abbreviation, 1, 0),
    penalty_min_home   = ifelse(event_type =="PENALTY" & event_team_abbr == home_abbreviation & penalty_severity=="Minor", 1, 0),
    penalty_min_away   = ifelse(event_type =="PENALTY" & event_team_abbr == away_abbreviation & penalty_severity=="Minor", 1, 0),
    penalty_maj_home   = ifelse(event_type =="PENALTY" & event_team_abbr == home_abbreviation & penalty_severity=="Major", 1, 0),
    penalty_maj_away   = ifelse(event_type =="PENALTY" & event_team_abbr == away_abbreviation & penalty_severity=="Major", 1, 0),
    hit_home           = ifelse(event_type == "HIT" & event_team_abbr == home_abbreviation, 1, 0),
    hit_away           = ifelse(event_type == "HIT" & event_team_abbr == away_abbreviation, 1, 0),
    covid = case_when(
      game_date < "2020-03-12"                                                  ~ "Before Covid",
      season=="20192020" & game_date >= "2020-03-12" & game_date < "2020-08-01" ~ "Covid (games suspended)",
      season=="20192020" & game_date >= "2020-08-01"                            ~ "Covid bubble playoff",
      season=="20202021" & !is.na(game_date_open) & game_date >= as.Date(game_date_open) ~ "Covid (some fans)",
      season=="20202021"                                                        ~ "Covid (no fans)",
      T ~ "Regular post-Covid play"),
    covid_nofans       = ifelse(covid=="Covid (no fans)",1,0),
    covid_somefans     = ifelse(covid=="Covid (some fans)",1,0),
    covid_bubble       = ifelse(covid=="Covid bubble playoff",1,0),
    covid_bubble_or_nofans = case_when(
      covid=="Covid bubble playoff" ~ 1,
      covid=="Covid (no fans)"      ~ 1,
      T                             ~ 0)
  ) %>%
  # add attributes that pick up info from within games
  group_by(game_id) %>% arrange(game_id, game_seconds) %>%
  mutate(
    # inclusive of R shootouts... i.e., anything over regulation time
    ended_in_overtime       = ifelse(last(period)>3, T, F), 
    ended_in_shootout       = case_when(season_type=="R" & last(period)<=4 ~ F,
                                        season_type=="R" & last(period)==5 ~ T,
                                        T ~ NA),
    period_max              = max(period),
    # was home_score_orig, fixing...
    home_score              = cumsum(event_type=="GOAL" & event_team_abbr==home_abbreviation), 
    # was away_score_orig, fixing...
    away_score              = cumsum(event_type=="GOAL" & event_team_abbr==away_abbreviation), 
    # was home_final_orig, fixing...
    home_final              = sum(event_type=="GOAL" & event_team_abbr==home_abbreviation),
    # was away_final_orig, fixing...
    away_final              = sum(event_type=="GOAL" & event_team_abbr==away_abbreviation),
    # faceoff_winner        = case_when(
    #   event_type=="FACEOFF" & game_seconds == 0  ~ event_team_abbr,
    #   T ~ NA),
    faceoff_winner          = case_when(
      event_type=="FACEOFF" ~ event_team_abbr,
      T ~ NA),
    first_faceoff_winner    = na.omit(faceoff_winner)[1],
    home_score_endof1st     = sum(event_type=="GOAL" & event_team_abbr==home_abbreviation & period<=1), 
    away_score_endof1st     = sum(event_type=="GOAL" & event_team_abbr==away_abbreviation & period<=1), 
    home_score_endof2nd     = sum(event_type=="GOAL" & event_team_abbr==home_abbreviation & period<=2), 
    away_score_endof2nd     = sum(event_type=="GOAL" & event_team_abbr==away_abbreviation & period<=2), 
    home_score_endof3rd     = sum(event_type=="GOAL" & event_team_abbr==home_abbreviation & period<=3), 
    away_score_endof3rd     = sum(event_type=="GOAL" & event_team_abbr==away_abbreviation & period<=3), 
    home_fenwick_endof1st     = sum(event_type %in% fenwick_events & event_team_abbr==home_abbreviation & period<=1), 
    away_fenwick_endof1st     = sum(event_type %in% fenwick_events & event_team_abbr==away_abbreviation & period<=1), 
    faceoff_won_home_cum    = cumsum(faceoff_won_home),
    faceoff_won_away_cum    = cumsum(faceoff_won_away),
    first_faceoff_won_home_cum    = cumsum(first_faceoff_won_home),
    first_faceoff_won_away_cum    = cumsum(first_faceoff_won_away),
    corsi_event_home_cum    = cumsum(corsi_event_home),
    corsi_event_away_cum    = cumsum(corsi_event_away),
    fenwick_event_home_cum  = cumsum(fenwick_event_home),
    fenwick_event_away_cum  = cumsum(fenwick_event_away),
    blocked_home_cum        = cumsum(blocked_home),
    blocked_away_cum        = cumsum(blocked_away),
    blocked_theirs_home_cum = cumsum(blocked_theirs_home),
    blocked_theirs_away_cum = cumsum(blocked_theirs_away),
    shot_home_cum           = cumsum(shot_home),
    shot_away_cum           = cumsum(shot_away),
    missed_home_cum         = cumsum(missed_home),
    missed_away_cum         = cumsum(missed_away),
    penalty_home_cum        = cumsum(penalty_home),
    penalty_away_cum        = cumsum(penalty_away),
    penalty_min_home_cum    = cumsum(penalty_min_home),
    penalty_min_away_cum    = cumsum(penalty_min_away),
    penalty_maj_home_cum    = cumsum(penalty_maj_home),
    penalty_maj_away_cum    = cumsum(penalty_maj_away),
    hits_home_cum           = cumsum(hit_home),
    hits_away_cum           = cumsum(hit_away),
    ## timing of the last goal in each game...
    last_goal_seconds       = case_when(
      any(event_type=="GOAL") ~ max(game_seconds[event_type=="GOAL"]),
      T ~ NA),
    last_goal_frac_reg      = last_goal_seconds/(3*20*60), # periods * minutes * seconds
    last_goal_frac_actual   = last_goal_seconds/max(game_seconds[event_type=="GAME_END"]),
    score_before_last_goal = case_when(
      event_type=="GOAL" & last_goal_seconds == game_seconds ~ lag(home_score) - lag(away_score),
      T ~ NA),
    score_after_last_goal   = case_when(
      event_type=="GOAL" & last_goal_seconds == game_seconds ~ home_score - away_score,
      T ~ NA),
    score_diff_last_goal  = score_after_last_goal - score_before_last_goal,
    # also get score before... 
    score_before_goal     = case_when(
      event_type=="GOAL" ~ lag(home_score) - lag(away_score),
      T ~ NA),
    score_after_goal      = case_when(
      event_type=="GOAL" ~ home_score - away_score,
      T ~ NA),
    goal_score_diff = score_after_goal - score_before_goal,
  ) %>%
  ungroup()


# df.look <- df %>% filter(season=="20202021")
# table(df.look$covid)
# df.look <- df %>% filter(game_id=="2022030215")

# overtime? 8 periods?
# table(df$period,df$ended_in_shootout)
# table(df$period,df$season)
# df.look <- df %>% filter(period==8)
# 
# table(df$season, df$covid)

## Game-level results ----

df.game.results <- df %>%
  group_by(game_id) %>% arrange(game_id, game_seconds) %>%
  fill(score_before_last_goal, .direction = "down") %>% # propagates last non‐NA forward
  fill(score_after_last_goal,  .direction = "down") %>%
  fill(score_diff_last_goal,   .direction = "down") %>%
  filter(event_type=="GAME_END") %>%
  summarise(
    season                  = season,
    season_type             = season_type,
    special_game            = special_game,
    venue_name              = venue_name,
    game_date               = game_date,
    home_name               = home_name,
    away_name               = away_name,
    home_final_orig         = home_final_orig,
    away_final_orig         = away_final_orig,
    home_final              = home_final,
    away_final              = away_final,
    ended_in_overtime       = ended_in_overtime,
    ended_in_shootout       = ended_in_shootout,
    faceoff_won_home        = faceoff_won_home,
    faceoff_won_away        = faceoff_won_away,
    first_faceoff_won_home  = first_faceoff_won_home,
    first_faceoff_won_away  = first_faceoff_won_away,
    faceoff_winner          = faceoff_winner,
    first_faceoff_winner    = first_faceoff_winner,
    home_fenwick_endof1st   = home_fenwick_endof1st,
    away_fenwick_endof1st   = away_fenwick_endof1st,
    home_score_endof1st     = home_score_endof1st,
    away_score_endof1st     = away_score_endof1st,
    period_max              = period_max,
    covid                   = covid,
    covid_nofans            = covid_nofans,
    covid_somefans          = covid_somefans,
    covid_bubble            = covid_bubble,
    covid_bubble_or_nofans  = covid_bubble_or_nofans,
    game_seconds            = game_seconds,
    last_goal_seconds       = last_goal_seconds,
    last_goal_frac_reg      = last_goal_frac_reg,
    last_goal_frac_actual   = last_goal_frac_actual,
    score_before_last_goal  = score_before_last_goal,
    score_after_last_goal   = score_after_last_goal,
    score_diff_last_goal    = score_diff_last_goal,
    home_abbreviation       = home_abbreviation,
    away_abbreviation       = away_abbreviation,
    home_faceoffs           = faceoff_won_home_cum, 
    away_faceoffs           = faceoff_won_away_cum, 
    home_corsi              = corsi_event_home_cum, 
    away_corsi              = corsi_event_away_cum, 
    home_fenwick            = fenwick_event_home_cum, 
    away_fenwick            = fenwick_event_away_cum, 
    home_blocked            = blocked_home_cum, 
    away_blocked            = blocked_away_cum, 
    home_blocked_theirs     = blocked_theirs_home_cum,
    away_blocked_theirs     = blocked_theirs_away_cum,
    home_shots              = shot_home_cum, 
    away_shots              = shot_away_cum, 
    home_missed             = missed_home_cum, 
    away_missed             = missed_away_cum, 
    home_penalties          = penalty_home_cum, 
    away_penalties          = penalty_away_cum, 
    home_penalties_min      = penalty_min_home_cum,
    away_penalties_min      = penalty_min_home_cum,
    home_penalties_maj      = penalty_maj_home_cum,
    away_penalties_maj      = penalty_maj_home_cum,
    home_hits               = hits_home_cum, 
    away_hits               = hits_away_cum, 
    .groups = "drop"
  ) %>%
  mutate(
    series              = paste0(home_abbreviation," v ",away_abbreviation),
    regular_length_game = (game_seconds == 3600) 
  )

df.look <- df.game.results %>% filter(game_id==2022030161) 


## Team-by-season-by-game... ----

# a df that can hold win/loss outcomes for every game...
df.game.results.long <- bind_rows(
  # Home team...
  df.game.results %>%
    transmute(
      game_id,
      season                = season,
      season_type           = season_type,
      special_game          = special_game,
      venue_name            = venue_name,
      game_date             = game_date,
      series                = series,
      ended_in_overtime     = ended_in_overtime,
      ended_in_shootout     = ended_in_shootout,
      period_max            = period_max,
      regular_length_game   = regular_length_game,
      last_goal_seconds     = last_goal_seconds,
      last_goal_frac_reg    = last_goal_frac_reg,
      last_goal_frac_actual = last_goal_frac_actual,
      covid                 = covid,
      covid_nofans          = covid_nofans,
      covid_somefans        = covid_somefans,
      covid_bubble          = covid_bubble,
      covid_bubble_or_nofans=covid_bubble_or_nofans,
      first_faceoff_winner  = first_faceoff_winner,
      played                = "home",
      team                  = home_abbreviation,
      score                 = home_final,
      faceoffs              = home_faceoffs,
      corsi_events          = home_corsi,
      fenwick_events        = home_fenwick,
      blocked               = home_blocked,
      blocked_theirs        = home_blocked_theirs,
      shots                 = home_shots,
      missed                = home_missed,
      penalties             = home_penalties,
      penalties_min         = home_penalties_min,
      penalties_maj         = home_penalties_maj,
      hits                  = home_hits,
      faceoff_won           = faceoff_won_home,
      fenwick_1st           = home_fenwick_endof1st,
      score_1st             = home_score_endof1st,
      opp_score_1st         = away_score_endof1st,
      opp_team              = away_abbreviation,
      opp_score             = away_final,
      opp_faceoffs          = away_faceoffs,
      opp_corsi_events      = away_corsi,
      opp_fenwick_events    = away_fenwick,
      opp_blocked           = away_blocked,
      opp_shots             = away_shots,
      opp_missed            = away_missed,
      opp_penalties         = away_penalties,
      opp_penalties_min     = away_penalties_min,
      opp_penalties_maj     = away_penalties_maj,
      opp_hits              = away_hits,
      score_before_last_goal= score_before_last_goal
    ),
  # Away team...
  df.game.results %>%
    transmute(
      game_id,
      season                = season,
      season_type           = season_type,
      special_game          = special_game,
      venue_name            = venue_name,
      game_date             = game_date,
      series                = series,
      ended_in_overtime     = ended_in_overtime,
      ended_in_shootout     = ended_in_shootout,
      period_max            = period_max,
      regular_length_game   = regular_length_game,
      last_goal_seconds     = last_goal_seconds,
      last_goal_frac_reg    = last_goal_frac_reg,
      last_goal_frac_actual = last_goal_frac_actual,
      covid                 = covid,
      covid_nofans          = covid_nofans,
      covid_somefans        = covid_somefans,
      covid_bubble          = covid_bubble,
      covid_bubble_or_nofans=covid_bubble_or_nofans,
      first_faceoff_winner  = first_faceoff_winner,
      played                = "away",
      team                  = away_abbreviation,
      score                 = away_final,
      faceoffs              = away_faceoffs,
      corsi_events          = away_corsi,
      fenwick_events        = away_fenwick,
      blocked               = away_blocked,
      blocked_theirs        = away_blocked_theirs,
      shots                 = away_shots,
      missed                = away_missed,
      penalties             = away_penalties,
      penalties_min         = away_penalties_min,
      penalties_maj         = away_penalties_maj,
      hits                  = away_hits,
      faceoff_won           = faceoff_won_away,
      fenwick_1st           = away_fenwick_endof1st,
      score_1st             = away_score_endof1st,
      opp_score_1st         = home_score_endof1st,
      opp_team              = home_abbreviation,
      opp_score             = home_final,
      opp_faceoffs          = home_faceoffs,
      opp_corsi_events      = home_corsi,
      opp_fenwick_events    = home_fenwick,
      opp_blocked           = home_blocked,
      opp_shots             = home_shots,
      opp_missed            = home_missed,
      opp_penalties         = home_penalties,
      opp_penalties_min     = home_penalties_min,
      opp_penalties_maj     = home_penalties_maj,
      opp_hits              = home_hits,
      score_before_last_goal= - score_before_last_goal
    )) %>%
  mutate(
    result = case_when(score > opp_score  ~ "Win",
                       score == opp_score ~ "Tie",
                       score < opp_score  ~ "Loss"),
    points = case_when(regular_length_game == T & result == "Win" ~ 2,
                       regular_length_game == T & result == "Loss" ~ 0,
                       regular_length_game == F & result == "Loss" ~ 1,
                       T ~ 0)
  ) %>%
  group_by(team, season) %>% arrange(team, season, game_date) %>%
  mutate(
    result_lag                = lag(result),
    score_lag                 = lag(score),
    opp_score_lag             = lag(opp_score),
    ended_in_overtime_lag     = lag(ended_in_overtime),
    ended_in_shootout_lag     = lag(ended_in_shootout),
    period_max_lag            = lag(period_max),
    faceoffs_lag              = lag(faceoffs),
    corsi_events_lag          = lag(corsi_events),
    fenwick_events_lag        = lag(fenwick_events),
    opp_fenwick_events_lag    = lag(opp_fenwick_events),
    blocked_lag               = lag(blocked),
    shots_lag                 = lag(shots),
    missed_lag                = lag(missed),
    penalties_lag             = lag(penalties),
    penalties_min_lag         = lag(penalties_min),
    penalties_maj_lag         = lag(penalties_maj),
    hits_lag                  = lag(hits),
    regular_length_game_lag   = lag(regular_length_game),
    last_goal_seconds_lag     = lag(last_goal_seconds),
    last_goal_frac_reg_lag    = lag(last_goal_frac_reg),
    last_goal_frac_actual_lag = lag(last_goal_frac_actual),
    month                     = month(game_date),
    score_before_last_goal_lag= lag(score_before_last_goal)
  ) %>%
  ungroup() %>%
  arrange(team, game_date)

#table(df.game.results.long$points)


### Add estimates of possession time ---- 

# Filter relevant events for estimating possession
df.possession.filtered <- df %>%
  select(game_id, event_type, event_team_abbr, period_seconds_remaining, period) %>%
  filter(event_type %in% c("SHOT", "GOAL", "HIT", "TAKEAWAY", "GIVEAWAY", "BLOCK", "MISS", "FACEOFF")) %>%
  mutate(
    event_team  = ifelse(is.na(event_team_abbr), NA, event_team_abbr),
    event_time  = as.numeric(period_seconds_remaining)
    )

# Create estimated possession segments
df.possession <- df.possession.filtered %>%
  group_by(game_id) %>%
  arrange(game_id, period, desc(event_time)) %>%
  mutate(next_team = lead(event_team),
         next_time = lead(event_time),
         time_diff = abs(event_time - next_time)) %>%
  filter(!is.na(next_team), event_team == next_team)

# Summarize estimated possession time per team
df.possession.summary <- df.possession %>%
  group_by(game_id, event_team) %>%
  summarise(estimated_possession_seconds = sum(time_diff, na.rm = TRUE)) %>%
  arrange(desc(estimated_possession_seconds))


# Add it in...
df.game.results.long <- df.game.results.long %>% 
  left_join(df.possession.summary, by=c("game_id" = "game_id","team" = "event_team")) %>%
    rename(possession = estimated_possession_seconds) %>%
  left_join(df.possession.summary, by=c("game_id" = "game_id","opp_team" = "event_team")) %>%
    rename(possession_opp = estimated_possession_seconds) %>%
  mutate(possession_frac = possession/(possession+possession_opp))




## Team-by-season... ----

# a df that can hold win/loss outcomes for every season...

df.season <- df.game.results.long %>%
  group_by(team, season) %>% arrange(team, season, game_date) %>%
  mutate(
    points_regular = ifelse(season_type=="R", points, NA),
    points_playoff = ifelse(season_type=="P", points, NA),
    fenwick_regular = ifelse(season_type=="R", fenwick_events, NA),
    hits_regular = ifelse(season_type=="R", hits, NA)
  ) %>%
  summarize(
    points_regular_season   = sum(points_regular, na.rm = T),
    points_playoff_season   = sum(points_playoff, na.rm = T),
    fenwick_regular_season  = mean(fenwick_regular, na.rm = T),
    hits_regular_season     = mean(hits_regular, na.rm = T),
    possession_frac_season  = mean(possession_frac, na.rm = T)
  ) %>%
  group_by(team) %>%
  summarize(
    season                     = season,
    points_regular_season      = points_regular_season,
    points_playoff_season      = points_playoff_season,
    fenwick_regular_season     = fenwick_regular_season,
    hits_regular_season        = hits_regular_season,
    possession_frac_season     = possession_frac_season,
    points_regular_season_lag  = lag(points_regular_season),
    points_playoff_season_lag  = lag(points_playoff_season),
    possession_frac_season_lag = lag(possession_frac_season)
  )

## how many points is typical in a given season...
#summary(df.season$points_regular_season)


# SAVE ----

write_fst(df,                   paste0("Data/step02_df.fst"), compress = 50)
write_fst(df.game.results,      paste0("Data/step02_game_results.fst"), compress = 50)
write_fst(df.game.results.long, paste0("Data/step02_game_results_long.fst"), compress = 50)
write_fst(df.season,            paste0("Data/step02_season.fst"), compress = 50)




