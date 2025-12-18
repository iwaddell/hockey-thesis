if (!require("pacman")) install.packages("pacman")
library(pacman);p_load(tidyverse, ggplot2, readr, fixest, broom, fst);

df.curfew <- read.csv("NHL1/game.csv") 


# make var gameNumHome and gameNumAway for consecutive game splayed by the same team











