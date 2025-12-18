if (!require("pacman")) install.packages("pacman")
library(pacman);p_load(tidyverse, ggplot2, readr, fixest, broom, hockeyR, sportyR);
# SETUP ----

library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)

## themes ----

mytheme <- theme_minimal() + theme(
  legend.position = "none",
  panel.grid.minor.y = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank()
)

# hockeyR ----

## See https://github.com/danmorse314/hockeyR?tab=readme-ov-file


#install.packages("devtools")
#devtools::install_github("danmorse314/hockeyR")
library(hockeyR)
#install.packages("sportyR")
library(sportyR)

library(pacman);p_load(fst, purrr);


## which seasons... 
## The available data goes back to the 2010-2011 season...
seasons <- c(2010:2023)
seasonsdebug <- c(2010:2022)

# Get play-by-play for each season... 
getpbp <- function(season){
  getthisseason = paste0(season,"-",season+1)
  df <- load_pbp(getthisseason)
  write_fst(df, paste0("pbpdata/pbp_",season,".fst"), compress = 50)
}
## Don't run this if not needed
look = getpbp(2022) # run a specific season (2009 is not available)
#walk(seasons, getpbp)  # run all seasons

# Once saved locally, just use this...
readpbp <- function(season){
  df <- read_fst(paste0("pbpdata/pbp_",season,".fst"), as.data.table = FALSE)
  return(df)
}
## Read in a specific season...
df.2023 <- readpbp(2023)

## Read in ALL seasons as a list (note, seasons may have different columns)
df.seasons <- set_names(
   map(seasons, ~ read_fst(paste0("pbpdata/pbp_", .x, ".fst"), as.data.table = FALSE)),
   paste0("df.", seasons)
 )

df.seasons_debug <- set_names(
  map(seasonsdebug, ~ read_fst(paste0("pbpdata/pbp_", .x, ".fst"), as.data.table = FALSE)),
  paste0("df.", seasonsdebug)
)
## Can access a specific season this way...
df.2023 <- df.seasons[["df.2023"]] 


## Actively coding here ----

## Good idea to just use one season first... get code down before moving on to multiple seasons
df.2023 <- readpbp(2023)


## Capture game-specific info ----

# number of games in season...
length(unique(as.numeric(df.2023$game_id)))


unique(df.2033$event_type)

# time of last goal?
df.2023_goals <- df.2023 %>% 
  filter(event_type %in% "GOAL")

## Sharks fn ----

sharks_df <- look %>%
  filter(home_name == "San Jose Sharks" | away_name == "San Jose Sharks")

sharks_df$date_time <- as.POSIXct(sharks_df$date_time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

is_sorted <- all(sharks_df$date_time == sort(sharks_df$date_time))

sharks_short_df <- sharks_df %>%
  filter(event_type %in% c("GAME_SCHEDULED", "SHOT", "MISSED_SHOT", "GOAL", "PERIOD_END", "GAME_END"))

sharks_short_df <- sharks_short_df %>%
  select(event_id, event_type, event, secondary_type, event_team, event_team_type, period, period_seconds, period_seconds_remaining, game_seconds, game_seconds_remaining, home_score, away_score, game_winning_goal, game_id, period_type, ordinal_num, period_time, period_time_remaining, date_time, home_final, away_final, season, season_type, game_date, game_start, game_end, game_length, home_name, away_name)

sharks_short_df <- sharks_short_df %>%
  mutate(new_game_id = dense_rank(game_id))

sharks_short_df <- sharks_short_df %>%
  mutate(
    win = case_when(
      home_final == away_final ~ "Tie",
      home_name == "San Jose Sharks" & home_final > away_final ~ "Win",
      away_name == "San Jose Sharks" & away_final > home_final ~ "Win",
      TRUE ~ "Loss"
    )
  )

ties_df <- sharks_short_df %>%
  filter(win %in% "Tie")

tie_game_ids <- sharks_short_df %>%
  filter(win == "Tie") %>%
  pull(game_id) %>%
  unique()

shootout_win_ids <- sharks_short_df %>%
  filter(game_id %in% tie_game_ids, period_type %in% "SHOOTOUT", event_type == "GOAL") %>%
  group_by(game_id) %>%
  summarise(
    total_goals = n(),
    sharks_goals = sum(event_team == "San Jose Sharks"),
    .groups = "drop"
  ) %>%
  filter(sharks_goals > total_goals / 2) %>%
  pull(game_id)

shootout_loss_ids <- sharks_short_df %>%
  filter(game_id %in% tie_game_ids, period_type %in% "SHOOTOUT", event_type == "GOAL") %>%
  group_by(game_id) %>%
  summarise(
    total_goals = n(),
    sharks_goals = sum(event_team == "San Jose Sharks"),
    .groups = "drop"
  ) %>%
  filter(sharks_goals < total_goals / 2) %>%
  pull(game_id)

sharks_short_df <- sharks_short_df %>%
  mutate(
    win = case_when(
      win == "Tie" & game_id %in% shootout_win_ids ~ "ShootoutWin",
      win == "Tie" & game_id %in% shootout_loss_ids ~ "ShootoutLoss",
      TRUE ~ win
    )
  )

next_game_lookup <- sharks_short_df %>%
  select(new_game_id, win) %>%
  distinct(new_game_id, .keep_all = TRUE)

next_game_lookup <- next_game_lookup %>%
  mutate(new_game_id = new_game_id - 1) %>%  # Shift to match with df's new_game_id
  rename(next_game = win)

final_sharks_df <- sharks_short_df %>%
  left_join(next_game_lookup, by = "new_game_id")

final_sharks_df <- final_sharks_df %>%
  filter(!is.na(next_game))


## Generalized fn ----

look$date_time <- as.POSIXct(look$date_time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

all_short_df <- look %>%
  filter(event_type %in% c("GAME_SCHEDULED", "SHOT", "MISSED_SHOT", "GOAL", "PERIOD_END", "GAME_END")) %>%
  select(event_id, event_type, event, secondary_type, event_team, event_team_type, period, period_seconds,
         period_seconds_remaining, game_seconds, game_seconds_remaining, home_score, away_score, 
         game_winning_goal, game_id, period_type, ordinal_num, period_time, period_time_remaining, 
         date_time, home_final, away_final, season, season_type, game_date, game_start, game_end, 
         game_length, home_name, away_name)

all_teams <- unique(c(look$home_name, look$away_name))

process_team <- function(team_name) {
  team_df <- all_short_df %>%
    filter(home_name == team_name | away_name == team_name) %>%
    mutate(new_game_id = dense_rank(game_id)) %>%
    mutate(
      win = case_when(
        home_final == away_final ~ "Tie",
        home_name == team_name & home_final > away_final ~ "Win",
        away_name == team_name & away_final > home_final ~ "Win",
        TRUE ~ "Loss"
      )
    )
  
  tie_game_ids <- team_df %>%
    filter(win == "Tie") %>%
    pull(game_id) %>%
    unique()
  
  shootout_win_ids <- team_df %>%
    filter(game_id %in% tie_game_ids, period_type == "SHOOTOUT", event_type == "GOAL") %>%
    group_by(game_id) %>%
    summarise(
      total_goals = n(),
      team_goals = sum(event_team == team_name),
      .groups = "drop"
    ) %>%
    filter(team_goals > total_goals / 2) %>%
    pull(game_id)
  
  shootout_loss_ids <- team_df %>%
    filter(game_id %in% tie_game_ids, period_type == "SHOOTOUT", event_type == "GOAL") %>%
    group_by(game_id) %>%
    summarise(
      total_goals = n(),
      team_goals = sum(event_team == team_name),
      .groups = "drop"
    ) %>%
    filter(team_goals < total_goals / 2) %>%
    pull(game_id)
  
  team_df <- team_df %>%
    mutate(
      win = case_when(
        win == "Tie" & game_id %in% shootout_win_ids ~ "ShootoutWin",
        win == "Tie" & game_id %in% shootout_loss_ids ~ "ShootoutLoss",
        TRUE ~ win
      )
    )
  
  next_game_lookup <- team_df %>%
    select(new_game_id, win) %>%
    distinct(new_game_id, .keep_all = TRUE) %>%
    mutate(new_game_id = new_game_id - 1) %>%
    rename(next_game = win)
  
  final_team_df <- team_df %>%
    left_join(next_game_lookup, by = "new_game_id") %>%
    filter(!is.na(next_game)) %>%
    mutate(team = team_name)
  
  return(final_team_df)
}

final_all_teams_df <- map_dfr(all_teams, process_team)

simple_final_df <- final_all_teams_df %>%
  filter(event_type == "GAME_END") %>%
  select(new_game_id, win, next_game, team)

shootouts_df <- simple_final_df %>%
  filter(win %in% c("ShootoutWin", "ShootoutLoss"))

ols_df <- shootouts_df %>%
  mutate(
    win_numeric = ifelse(win == "ShootoutWin", 1, 0),
    next_game_numeric = ifelse(next_game %in% c("Win", "ShootoutWin"), 1, 0)
  )

ols_model <- lm(next_game_numeric ~ win_numeric, data = ols_df)

summary(ols_model)

## all years fn ----

process_season <- function(season_df) {
  season_df$date_time <- as.POSIXct(season_df$date_time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  
  year <- season_df$season[1]
  
  all_short_df <- season_df %>%
    filter(event_type %in% c("GAME_SCHEDULED", "SHOT", "MISSED_SHOT", "GOAL", "PERIOD_END", "GAME_END")) %>%
    select(event_id, event_type, event, secondary_type, event_team, event_team_type, period, period_seconds,
           period_seconds_remaining, game_seconds, game_seconds_remaining, home_score, away_score, 
           game_winning_goal, game_id, period_type, ordinal_num, period_time, period_time_remaining, 
           date_time, home_final, away_final, season, season_type, game_date, game_start, game_end, 
           game_length, home_name, away_name)
  
  all_teams <- unique(c(season_df$home_name, season_df$away_name))
  
  process_team <- function(team_name) {
    team_df <- all_short_df %>%
      filter(home_name == team_name | away_name == team_name) %>%
      mutate(new_game_id = dense_rank(game_id)) %>%
      mutate(
        win = case_when(
          home_final == away_final ~ "Tie",
          home_name == team_name & home_final > away_final ~ "Win",
          away_name == team_name & away_final > home_final ~ "Win",
          TRUE ~ "Loss"
        )
      )
    
    tie_game_ids <- team_df %>%
      filter(win == "Tie") %>%
      pull(game_id) %>%
      unique()
    
    shootout_stats <- team_df %>%
      filter(game_id %in% tie_game_ids, period_type == "SHOOTOUT", event_type == "GOAL") %>%
      group_by(game_id) %>%
      summarise(
        total_goals = n(),
        team_goals = sum(event_team == team_name),
        .groups = "drop"
      )
    
    shootout_win_ids <- shootout_stats %>%
      filter(team_goals > total_goals / 2) %>%
      pull(game_id)
    
    shootout_loss_ids <- shootout_stats %>%
      filter(team_goals < total_goals / 2) %>%
      pull(game_id)
    
    team_df <- team_df %>%
      mutate(
        win = case_when(
          win == "Tie" & game_id %in% shootout_win_ids ~ "ShootoutWin",
          win == "Tie" & game_id %in% shootout_loss_ids ~ "ShootoutLoss",
          TRUE ~ win
        )
      )
    
    next_game_lookup <- team_df %>%
      select(new_game_id, win) %>%
      distinct(new_game_id, .keep_all = TRUE) %>%
      mutate(new_game_id = new_game_id - 1) %>%
      rename(next_game = win)
    
    final_team_df <- team_df %>%
      left_join(next_game_lookup, by = "new_game_id") %>%
      filter(!is.na(next_game)) %>%
      mutate(team = team_name)
    
    return(final_team_df)
  }
  
  final_all_teams_df <- map_dfr(all_teams, process_team)
  
  simple_final_df <- final_all_teams_df %>%
    filter(event_type == "GAME_END") %>%
    select(new_game_id, win, next_game, team) %>%
    mutate(year = year)
  
  return(simple_final_df)
}

df_simple_2015 <- process_season(df.seasons[["df.2015"]])
df_simple_2010 <- process_season(df.seasons[["df.2010"]])
df_simple_2023 <- process_season(df.seasons[["df.2023"]])

combine_seasons <- function(df_list) {
  map_df(df_list, process_season)
}

df_all_years <- combine_seasons(df.seasons_debug)

shootouts_all_df <- df_all_years %>%
  filter(win %in% c("ShootoutWin", "ShootoutLoss"))

ols_all_shootout_df <- shootouts_all_df %>%
  mutate(
    win_numeric = ifelse(win == "ShootoutWin", 1, 0),
    next_game_numeric = ifelse(next_game %in% c("Win", "ShootoutWin"), 1, 0)
  )

ols_all_shootout_model <- lm(next_game_numeric ~ win_numeric + factor(team) + factor(year), data = ols_all_shootout_df)


summary(ols_all_shootout_model)

ols_all_df <- df_all_years %>%
  mutate(
    win_numeric = ifelse(win %in% c("Win", "ShootoutWin"), 1, 0),
    next_game_numeric = ifelse(next_game %in% c("Win", "ShootoutWin"), 1, 0)
  )

ols_all_model <- lm(next_game_numeric ~ win_numeric + factor(team) + factor(year), data = ols_all_df)

summary(ols_all_model)

## Shot map ----

# get single game
game <- df.2023 %>%
  filter(game_date == "2024-04-24" & home_abbr == "TOR")

# grab team logos & colors
team_logos <- hockeyR::team_logos_colors %>%
  filter(team_abbr == unique(game$home_abbr) | team_abbr == unique(game$away_abbr)) %>%
  # add in dummy variables to put logos on the ice
  mutate(x = ifelse(full_team_name == unique(game$home_name), 50, -50),
         y = 0)

# add transparency to logo
transparent <- function(img) {
  magick::image_fx(img, expression = "0.3*a", channel = "alpha")
}

# get only shot events
fenwick_events <- c("MISSED_SHOT","SHOT","GOAL")

shots <- game %>% filter(event_type %in% fenwick_events) %>%
  # adding team colors
  left_join(team_logos, by = c("event_team_abbr" = "team_abbr"))


# create shot plot
geom_hockey("nhl") +
  ggimage::geom_image(
    data = team_logos,
    aes(x = x, y = y, image = team_logo_espn),
    image_fun = transparent, size = 0.22, asp = 2.35
    ) +
  geom_point(
    data = shots,
    aes(x_fixed, y_fixed),
    size = 6,
    color = shots$team_color1,
    shape = ifelse(shots$event_type == "GOAL", 19, 1)
    ) +
  labs(
    title = glue::glue("{unique(game$away_name)} at {unique(game$home_name)}"),
    subtitle = glue::glue(
    "{unique(game$game_date)}\n
    {unique(shots$away_abbr)} {unique(shots$away_final)} - {unique(shots$home_final)} {unique(shots$home_abbr)}"
    ),
    caption = "" #data from hockeyR | plot made with sportyR"
    ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = .9)
    )

