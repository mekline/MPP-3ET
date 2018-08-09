library(readr)
library(dplyr)
library(zoo)
log = read_tsv("log.txt", col_names=FALSE)
log <- log %>%
  mutate(type = 'Manner') %>%
  mutate(trialNo = ifelse(X1 == "traintrialno", X2-100,
                          ifelse(X1 == "trialno", X2, NA))) %>%
  mutate(startTrial = ifelse(X1 == "start time", X2, NA)) %>%
  mutate(newMovies = ifelse(X1 == "newmovieset", X2, NA)) %>%
  mutate(startTrial = na.locf(startTrial))



log = rbind(log, logpath)


log <- log %>%
  select(c("leftMovie", "rightMovie", "startTime","gotleft", "gotright", "kbrelease", "soundopen", 
           "leftopen", "rightopen","counter0", 
             "counter100", "movieover", "closeleft", "closeright"))