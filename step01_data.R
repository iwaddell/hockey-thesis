
# SETUP ----

if (!require("pacman")) install.packages("pacman")
library(pacman);p_load(tidyverse, ggplot2, readr, fixest, broom, hockeyR, sportyR, fst, purrr);

#install.packages("sportyR")
library(sportyR)

# hockeyR ----

## See https://github.com/danmorse314/hockeyR?tab=readme-ov-file

#install.packages("devtools")
#devtools::install_github("danmorse314/hockeyR")
library(hockeyR)


## The available data goes back to the 2010-2011 season...
seasons <- c(2010:2023)

# Get play-by-play for each season... 
getpbp <- function(season){
  getthisseason = paste0(season,"-",season+1)
  df <- load_pbp(getthisseason)
  write_fst(df, paste0("pbpdata/pbp_",season,".fst"), compress = 50)
}
## Don't run this if not needed
switch = 0
if (switch==1) {
  look = getpbp(2022) # run a specific season (2009 is not available)
} else if (switch==2) {
  walk(seasons, getpbp)  # run all seasons
}

# Once saved locally, just use this...
readpbp <- function(season){
  df <- read_fst(paste0("pbpdata/pbp_",season,".fst"), as.data.table = FALSE)
  return(df)
}
## Read in a specific season...
switch = 0
if (switch==1) {
  df.2023 <- readpbp(2023)
}

# Read in ALL seasons as a list (note, seasons may have different columns)
switch = 0
if (switch==1) {
  df.seasons <- set_names(
    map(seasons, ~ read_fst(paste0("pbpdata/pbp_", .x, ".fst"), as.data.table = FALSE)),
    paste0("df.", seasons)
  )
}
# If setting up this way, can access a specific season this way...
#df.2023 <- df.seasons[["df.2023"]]







