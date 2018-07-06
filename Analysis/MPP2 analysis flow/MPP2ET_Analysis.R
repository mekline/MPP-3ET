# This script is looking at the eyetracking data of MPP2ET. We're going to read 
# in each participant's trials one by one, append them to each specific phase
# (Practice, Main, Extend), and then append them all to a dataframe. We're going
# to loop through each participant. 

#install.packages("devtools")
#devtools::install_github("jwdink/eyetrackingR")
#install.packages('sqldf')
library("eyetrackingR")
library("plyr")
library("dplyr")
library("lme4")
library("ggplot2")
library("Matrix")
#install.packages("stringr")
library("stringr")

setwd('~/Desktop/MannerPathPriming-3ET')


############################
# Getting ready
############################


# Reading in subject list
subjects <- read.csv("all_participants_MPP3ET.csv")

# Declaring an empty df to append to
allData <- data.frame(Date=as.Date(character()),
                              File=character(), 
                              User=character(), 
                              stringsAsFactors=FALSE) 

df_data_table <- data.frame(Date=as.Date(character()),
                            File=character(), 
                            User=character(), 
                            stringsAsFactors=FALSE) 

df_timestamps <- data.frame(Date=as.Date(character()),
                            File=character(), 
                            User=character(), 
                            stringsAsFactors=FALSE) 


# Declaring my function

## Declaring function trial_time, where a is a given value and f a timestamp
trial_time <- function(x) {
  f = df_timestamps$system_time_stamp
  a = x
  
  ## search for the maximum timestamp where the timestamp is equal to or less than value a  
  maxless <- max(f[f <= a])
  # find out which value that is
  y = which(f == maxless)
  z = as.character(df_timestamps$point_description[y])
  
  ## if the vector is empty, return the timestamp of the minimum value bigger than a,
  ## if the vector is not empty, return the vector
  if(identical(z,character(0))) {
    y = min(which(f > a))
    temp = as.character(df_timestamps$point_description[y])
    return(temp)
  } else {
    return(z)
  }
}

# Declaring empty variables
subj.folders <- list.dirs(recursive = FALSE)
file.names_practice <- NULL
file.names_main <- NULL
file.names_extend <- NULL

# This function returns an array of both the start and end portions of the current trial 
# (e.g., "Start Trial 7, AmbigVideo 7" (or whatever)), but it gives me an error. So, I'm including
# the code incase anyone wants to use it later. 
# trial_time1 <- function(x) {
#   
#   f = df_timestamps$system_time_stamp
#   a = x
#   maxless <- max(f[f <= a])
#   # find out which value that is
#   y = which(f == maxless)
#   z = as.character(df_timestamps$point_description[y])
#   
#   z1 = as.character(df_timestamps$point_description[y+1])
#   z2 <- paste(z1, z, sep=",")
#   
#   #x <- cbind(x, newColumn = z2)
#   
#   return(z2)
# }


############################
# LOOPING THROUGH ALL THE SUBJECTS (1 subject at a time)
############################

for(i in subj.folders){
  ############################
  # Importing files specific to participant
  ############################
  
  # Gettting all the practice trials for 1 subject
  file.names_practice_temp <- list.files(path = i, recursive = FALSE, full.names = TRUE, pattern = ".*\\Practice_.*.csv")
  file.names_practice <- c(file.names_practice, file.names_practice_temp)

  # Getting all the main trials for 1 subject
  file.names_main_temp <- list.files(path = i, recursive = FALSE, full.names = TRUE, all.files = FALSE, pattern = ".*(Main|noBias)_.*\\.csv$")
  x = (file.names_main_temp)
  if(identical(x,character(0))) {
    NULL
  }
  else {
    file.names_main <- c(file.names_main, file.names_main_temp)
  }

  # Getting all the extend trials for 1 subject
  file.names_extend_temp <- list.files(path = i, full.names = TRUE, recursive = FALSE, pattern = ".*\\Extend_.*.csv")
  file.names_extend <- c(file.names_extend, file.names_extend_temp)
  
  # Importing timestamps
  timestamps <- list.files(path = i, pattern="^timestamps.*\\.csv", full.names = TRUE )
  print(timestamps)
 
  if(identical(timestamps,character(0))) {
    NULL
  }
  else {
    temp <- read.csv(timestamps, header = TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
    df_timestamps <- rbind(df_timestamps, temp)
  }
}

############################
# LOOKING AT PRACTICE TRIALS
############################

for(i in subj.folders){
  # Reading in data table
  data_table <- list.files(path = i, pattern=".*\\.dat", full.names=TRUE)
  if(identical(data_table, character(0))) {
    NULL
  }
  else {
    temp <- read.delim(data_table, header=TRUE, sep=",")
    df_data_table <- rbind(df_data_table, temp)
  }

  
  ## makes an empty data frame
  subjData <- data.frame(Date=as.Date(character()),
                         File=character(), 
                         User=character(), 
                         stringsAsFactors=FALSE) 

  df_practice <- data.frame(Date=as.Date(character()),
                            File=character(), 
                            User=character(), 
                            stringsAsFactors=FALSE)
  
  ## adds all the eye-tracking data of the practice runs in the data frame
  for(file in file.names_practice){
    temp <- read.csv(file, header = TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
    df_practice <-rbind(df_practice, temp)
  }

  # Cleaning up the data to get it in the form I want
  colnames(df_practice)[which(names(df_practice) == "description")] <- "trialNo"
  df_practice$L_valid <- as.factor(df_practice$L_valid)
  df_practice$R_valid <- as.factor(df_practice$R_valid)
  df_practice$system_time_stamp <- df_practice$system_time_stamp - 1500000000000000
  df_practice$phase <- 'Practice'

  # Merging together dat_table and trials to get correctness
  df_practice$trialNo <- as.factor(ifelse(df_practice$trialNo == "All_of_Practice_1", "1",
                                          ifelse(df_practice$trialNo == "All_of_Practice_2", "2",
                                                 ifelse(df_practice$trialNo == "All_of_Practice_3", "3",
                                                        ifelse(df_practice$trialNo == "All_of_Practice_4", "4", "Error")))))

  subjData <- rbind(subjData, df_practice)
}
  

############################
# LOOKING AT MAIN TRIALS
############################

for(i in subj.folders){

  ## making an empty data frame
  df_main <- data.frame(Date=as.Date(character()),
                        File=character(),
                        User=character(),
                        stringsAsFactors=FALSE)
  
  ## adds all the eyetracking data for the main trials in the data frame
  for(file in file.names_main){
    temp <- read.csv(file, header = TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
    df_main <- rbind(df_main, temp)
  }

  # Cleaning up the data to get it in the form I want
  colnames(df_main)[which(names(df_main) == "description")] <- "trialNo"
  df_main$L_valid <- as.factor(df_main$L_valid)
  df_main$R_valid <- as.factor(df_main$R_valid)
  df_main$system_time_stamp <- df_main$system_time_stamp - 1500000000000000
  df_main$phase <- 'Main'

  # Merging together dat_table and trials to get correctness
  df_main$trialNo <- as.factor(ifelse(df_main$trialNo == "All_of_noBias_trial_1", "1",
                               ifelse(df_main$trialNo == "All_of_noBias_trial_2", "2",
                               ifelse(df_main$trialNo == "All_of_noBias_trial_3", "3",
                               ifelse(df_main$trialNo == "All_of_noBias_trial_4", "4",
                               ifelse(df_main$trialNo == "All_of_Main_trial_5", "5",
                               ifelse(df_main$trialNo == "All_of_Main_trial_6", "6",
                               ifelse(df_main$trialNo == "All_of_Main_trial_7", "7",
                               ifelse(df_main$trialNo == "All_of_Main_trial_8", "8", "Error")))))))))

  ## adds practice data and main trials data together in one data frame
  subjData <- rbind(subjData, df_main)
}

##########################
# LOOKING AT EXTEND TRIALS
##########################

for(i in subj.folders){

  # Reading in extend trial CSVs
  ## making an empty data frame
  df_extend <- data.frame(Date=as.Date(character()),
                          File=character(),
                          User=character(),
                          stringsAsFactors=FALSE)
  
  ## adds all the eyetracking data of the extend trials in the data frame
  for(file in file.names_extend){
    temp <- read.csv(file, header = TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
    if("trialNo" %in% colnames(temp)) {
      temp <- subset(temp, select=-c(trialNo))
    }
    df_extend <-rbind(df_extend, temp)
  }

  # Cleaning up the data to get it in the form I want
  colnames(df_extend)[which(names(df_extend) == "description")] <- "trialNo"
  df_extend$L_valid <- as.factor(df_extend$L_valid)
  df_extend$R_valid <- as.factor(df_extend$R_valid)
  df_extend$system_time_stamp <- df_extend$system_time_stamp - 1500000000000000
  df_extend$phase <- 'Extend'

  # Merging together dat_table and trials to get correctness
  df_extend$trialNo <- as.factor(ifelse(df_extend$trialNo == "All_of_Extend_trial_5", "13",
                                 ifelse(df_extend$trialNo == "All_of_Extend_trial_6", "14",
                                 ifelse(df_extend$trialNo == "All_of_Extend_trial_7", "7",
                                 ifelse(df_extend$trialNo == "All_of_Extend_trial_8", "8",
                                 ifelse(df_extend$trialNo == "All_of_Extend_trial_1", "9",
                                 ifelse(df_extend$trialNo == "All_of_Extend_trial_2", "10",
                                 ifelse(df_extend$trialNo == "All_of_Extend_trial_3", "11",
                                 ifelse(df_extend$trialNo == "All_of_Extend_trial_4", "12", "Error")))))))))

  subjData <- rbind(subjData, df_extend)

}

############################
# THIS IS WHERE THE FOR LOOP ENDS
############################



############################
# ANALYSES TO START AFTER EXPORTING ONE GIANT-ASS DF
############################

# Reformatting data table
colnames(df_data_table)[1] <- c("subjectID")
df_data_table$trialNo <- as.factor(df_data_table$trialNo)

# Reformatting timestamps
df_timestamps$system_time_stamp <- df_timestamps[,2] - 1500000000000000

# Merging data table with allData
allData <- dplyr::full_join(subjData, df_data_table, by=c("subjectID", "trialNo"))

# Reformatting allData
allData$subjectID <- as.factor(allData$subjectID)

# Defining a trackloss column
allData$Trackloss_column <- ifelse(allData$L_valid == '1' & allData$R_valid == '1', FALSE, 
                            ifelse(allData$L_valid == '0' & allData$R_valid == '1', TRUE,
                            ifelse(allData$L_valid == '1' & allData$R_valid == '0', TRUE,
                            ifelse(allData$L_valid == '0' & allData$R_valid == '0', TRUE, 'Error'))))
allData$Trackloss_column <- as.logical(allData$Trackloss_column)

# Averaging together L and R eyes
allData$X <- rowMeans(subset(allData, select = c(6, 9)), na.rm = TRUE)
allData$Y <- rowMeans(subset(allData, select = c(7, 10)), na.rm = TRUE)

# Applying it to the dataframe for trials, it's taking a long time, so we'll apply it in sections
# a <- lapply(allData$system_time_stamp, trial_time)
# allData$Trial_description <- a

# Adding AOI for Practice
allData %>%
  group_by(Condition, subjectID, trialNo) %>% 
  mutate(lookPractice = ifelse(phase == "Practice" & trialNo == "1" & X < 1.250 & X > 0.67 & Y > 0.1963 & Y < 0.6313, as.logical(TRUE), 
                          ifelse(phase == "Practice" & trialNo == "2" & X < 0.605 & X > 0.25 & Y > 0.1963 & Y < 0.6313, as.logical(TRUE), 
                          ifelse(phase == "Practice" & trialNo == "3" & X < 0.605 & X > 0.25 & Y > 0.1963 & Y < 0.6313, as.logical(TRUE), 
                          ifelse(phase == "Practice" & trialNo == "4" & X < 0.605 & X > 0.25 & Y > 0.1963 & Y < 0.6313, as.logical(TRUE), as.logical(FALSE)))))) %>%
  mutate(lookNotPractice = ifelse(phase == "Practice" & trialNo == "1" & X < 0.605 & X > 0.25 & Y > 0.1963 & Y < 0.6313, as.logical(TRUE), 
                           ifelse(phase == "Practice" & trialNo == "2" & X < 1.250 & X > 0.67 & Y > 0.1963 & Y < 0.6313, as.logical(TRUE), 
                           ifelse(phase == "Practice" & trialNo == "3" & X < 1.250 & X > 0.67 & Y > 0.1963 & Y < 0.6313, as.logical(TRUE), 
                           ifelse(phase == "Practice" & trialNo == "4" & X < 1.250 & X > 0.67 & Y > 0.1963 & Y < 0.6313, as.logical(TRUE), as.logical(FALSE)))))) -> allData
  

# Adding AOI for Manner Bias and Test Bias
allData %>%
  group_by(Condition, subjectID, trialNo) %>% 
  mutate(lookMannerBias = ifelse(VerbDomain == "Motion" & pathSideBias == "L" & X < 0.605 & X > 0.25 & Y > 0.1963 & Y < 0.6313, as.logical(FALSE),
                          ifelse(VerbDomain == "Motion" & pathSideBias == "R" & X < 1.250 & X > 0.67 & Y > 0.1963 & Y < 0.6313, as.logical(FALSE),
                          ifelse(VerbDomain == "Motion" & mannerSideBias == "L" & X < 0.605 & X > 0.25 & Y > 0.1963 & Y < 0.6313, as.logical(TRUE),
                          ifelse(VerbDomain == "Motion" & mannerSideBias == "R" & X < 1.250 & X > 0.67 & Y > 0.1963 & Y < 0.6313, as.logical(TRUE), as.logical(NA)))))) %>% 
  mutate(lookMannerTest = ifelse(VerbDomain == "Motion" & pathSideTest == "L" & X < 0.605 & X > 0.25 & Y > 0.1963 & Y < 0.6313, as.logical(FALSE),
                          ifelse(VerbDomain == "Motion" & pathSideTest == "R" & X < 1.250 & X > 0.67 & Y > 0.1963 & Y < 0.6313, as.logical(FALSE),
                          ifelse(VerbDomain == "Motion" & mannerSideTest == "L" & X < 0.605 & X > 0.25 & Y > 0.1963 & Y < 0.6313, as.logical(TRUE),
                          ifelse(VerbDomain == "Motion" & mannerSideTest == "R" & X < 1.250 & X > 0.67 & Y > 0.1963 & Y < 0.6313, as.logical(TRUE), as.logical(NA)))))) -> allData

# Adding AOI for Manner Bias and Test Bias
allData %>%
  group_by(Condition, subjectID, trialNo) %>% 
  mutate(lookPathBias = ifelse(VerbDomain == "Motion" & pathSideBias == "L" & X < 0.605 & X > 0.25 & Y > 0.1963 & Y < 0.6313, as.logical(TRUE),
                          ifelse(VerbDomain == "Motion" &pathSideBias == "R" & X < 1.250 & X > 0.67 & Y > 0.1963 & Y < 0.6313, as.logical(TRUE),
                          ifelse(VerbDomain == "Motion" &mannerSideBias == "L" & X < 0.605 & X > 0.25 & Y > 0.1963 & Y < 0.6313, as.logical(FALSE),
                          ifelse(VerbDomain == "Motion" &mannerSideBias == "R" & X < 1.250 & X > 0.67 & Y > 0.1963 & Y < 0.6313, as.logical(FALSE), as.logical(NA)))))) %>% 
  mutate(lookPathTest = ifelse(VerbDomain == "Motion" &pathSideTest == "L" & X < 0.605 & X > 0.25 & Y > 0.1963 & Y < 0.6313, as.logical(TRUE),
                          ifelse(VerbDomain == "Motion" &pathSideTest == "R" & X < 1.250 & X > 0.67 & Y > 0.1963 & Y < 0.6313, as.logical(TRUE),
                          ifelse(VerbDomain == "Motion" &mannerSideTest == "L" & X < 0.605 & X > 0.25 & Y > 0.1963 & Y < 0.6313, as.logical(FALSE),
                          ifelse(VerbDomain == "Motion" &mannerSideTest == "R" & X < 1.250 & X > 0.67 & Y > 0.1963 & Y < 0.6313, as.logical(FALSE), as.logical(NA)))))) -> allData

# Adding AOI for Generalization Test
allData %>%
  group_by(Condition, subjectID, trialNo) %>% 
  mutate(lookActionBias = ifelse(VerbDomain == "CoS" & pathSideBias == "L" & X < 0.605 & X > 0.25 & Y > 0.1963 & Y < 0.6313, as.logical(FALSE),
                          ifelse(VerbDomain == "CoS" & pathSideBias == "R" & X < 1.250 & X > 0.67 & Y > 0.1963 & Y < 0.6313, as.logical(FALSE),
                          ifelse(VerbDomain == "CoS" & mannerSideBias == "L" & X < 0.605 & X > 0.25 & Y > 0.1963 & Y < 0.6313, as.logical(TRUE),
                          ifelse(VerbDomain == "CoS" & mannerSideBias == "R" & X < 1.250 & X > 0.67 & Y > 0.1963 & Y < 0.6313, as.logical(TRUE), as.logical(FALSE)))))) -> allData 


############################
# CREATING A SUBSET DF OF PRACTICE TEST TRIALS
############################

allPractice <- filter(allData, phase=="Practice")

# Applying it to the dataframe for trials, it's taking a long time, so we'll apply it in sections
a <- lapply(allPractice$system_time_stamp, trial_time)
allPractice$Trial_description <- a

allPractice %>%
  filter(str_detect(Trial_description, "testVideos")) -> df_practice_test

df_practice_test$lookPractice <- as.logical(df_practice_test$lookPractice)

# Reformatting allData
df_practice_test$subjectID <- as.factor(df_practice_test$subjectID)

# Defining a trackloss column
df_practice_test$Trackloss_column <- ifelse(df_practice_test$L_valid == '1' & df_practice_test$R_valid == '1', FALSE, 
                                            ifelse(df_practice_test$L_valid == '0' & df_practice_test$R_valid == '1', TRUE,
                                                   ifelse(df_practice_test$L_valid == '1' & df_practice_test$R_valid == '0', TRUE,
                                                          ifelse(df_practice_test$L_valid == '0' & df_practice_test$R_valid == '0', TRUE, 'Error'))))
df_practice_test$Trackloss_column <- as.logical(df_practice_test$Trackloss_column)

# Defining column for looks to anything other than the AOI's
df_practice_test$lookNotAOI <- ifelse(df_practice_test$Trackloss_column == "FALSE" & df_practice_test$lookPractice == "FALSE" & df_practice_test$lookNotPractice == "FALSE", TRUE, FALSE)

# Starting to use eyetrackingR
data <- make_eyetrackingr_data(df_practice_test, 
                               participant_column = "subjectID",
                               trial_column = "trialNo",
                               time_column = "system_time_stamp",
                               trackloss_column = "Trackloss_column",
                               aoi_columns = c("lookPractice", "lookNotPractice", "lookNotAOI"),
                               treat_non_aoi_looks_as_missing = FALSE
)

# Aggregating by subjectID to get a proportion of looks to screen by AOI
response_window_agg_by_sub_practice <- make_time_window_data(data, aois = c("lookPractice", "lookNotPractice"), summarize_by = c("Condition"))
response_window_agg_by_sub_practice$Condition[response_window_agg_by_sub_practice$subjectID == "pilot_0725"] <- "Path"
response_window_agg_by_sub_practice <- response_window_agg_by_sub_practice[-c(3,6),]

############################
# GRAPHS FOR PRACTICE
############################


## Analyzing pilot data, when are the children looking at which AOI in the practice trials ##

# rank the data, based on subjectID and trialNo
data <- data %>% 
  group_by(subjectID, trialNo) %>% 
  mutate(Rank = dense_rank(system_time_stamp)) %>%
  arrange(subjectID, trialNo, Rank)

data <- make_eyetrackingr_data(data, 
                               participant_column = "subjectID",
                               trial_column = "trialNo",
                               time_column = "system_time_stamp",
                               trackloss_column = "Trackloss_column",
                               aoi_columns = c("lookPractice", "lookNotPractice", "lookNotAOI"),
                               treat_non_aoi_looks_as_missing = FALSE
)

# rezero system time stamps, so that at every trial start, the system time stamp is 0
response_window <- subset_by_window(data, window_start_msg = 1, msg_col = "Rank", rezero= TRUE, remove= FALSE)

# aggregate across trials within subjects in time analysis
response_time <- make_time_sequence_data(response_window, time_bin_size = 500000,
                                         #predictor_columns = c("Condition"),
                                         aois = c("lookPractice", "lookNotPractice", "lookNotAOI")
)



# visualize time results
plottingmeandata <- response_time %>%
  group_by(AOI, TimeBin) %>%
  summarize(meanProp = mean(Prop, na.rm = TRUE)) %>%
  mutate(subjectID = 'Mean')

plottingtrialdata <- response_time %>%
  group_by(AOI, TimeBin, trialNo) %>%
  summarize(meanProp = mean(Prop, na.rm = TRUE))


plottinginddata <- response_time %>%
  group_by(subjectID, AOI, TimeBin) %>%
  summarize(meanProp = mean(Prop, na.rm = TRUE))

# plot data for the mean
ggplot(plottingmeandata, aes(x=TimeBin, y=meanProp, color = AOI)) +
    facet_wrap(~subjectID) +
    geom_line()

# plot data per individual
ggplot(plottinginddata, aes(x=TimeBin, y=meanProp, color = AOI)) +
  facet_wrap(~subjectID) +
  geom_line()

# plot data per trial
ggplot(plottingtrialdata, aes(x=TimeBin, y=meanProp, color = AOI)) +
  facet_wrap(~trialNo) +
  geom_line()
  
                          


ggsave("/Users/Lotte/Documents/Github/MannerPathPriming-2ET/Analysis/figs/pilot_practice_lookingtimesAOI_kid.png")

# Creating bar graph
ggplot(data=response_window_agg_by_sub_practice, aes(x=Condition, y=Prop, fill=AOI)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") + 
  ylab("Proportion of looks to correct video") +
  theme(axis.title = element_text(size=18),
        axis.text.x  = element_text(size=18),
        axis.text.y = element_text(size=18),
        plot.title = element_text(size=18, face="bold")) 

ggsave("pilot_practice_kid_and_adult.png")







############################
# CREATING A SUBSET DF OF MAIN TEST TRIALS
############################

allMain_test <- filter(allData, phase=="Main")

# Applying it to the dataframe for trials, it's taking a long time, so we'll apply it in sections
a <- lapply(allMain_test$system_time_stamp, trial_time)
allMain_test$Trial_description <- a

allMain_test <- allMain_test[grep("testVideos|biasTest", allMain_test$Trial_description),]
allMain_test$trialNo <- as.factor(allMain_test$trialNo)
allMain_test$lookMannerBias <- as.logical(allMain_test$lookMannerBias)
allMain_test$lookMannerTest <- as.logical(allMain_test$lookMannerTest)
allMain_test$lookPathBias <- as.logical(allMain_test$lookPathBias)
allMain_test$lookPathTest <- as.logical(allMain_test$lookPathTest)
allMain_test$system_time_stamp <- as.numeric(allMain_test$system_time_stamp)

# IDK IF THIS IS KOSHER OR NOT
## only keep the unique rows from the input
allMain_test %>% 
  distinct(trialNo, system_time_stamp, .keep_all = TRUE) -> allMain_test

# Starting to use eyetrackingR
data <- make_eyetrackingr_data(allMain_test, 
                               participant_column = "subjectID",
                               trial_column = "trialNo",
                               item_columns = "itemID",
                               time_column = "system_time_stamp",
                               trackloss_column = "Trackloss_column",
                               aoi_columns = c("lookMannerBias", "lookMannerTest", "lookPathBias", "lookPathTest"),
                               treat_non_aoi_looks_as_missing = FALSE
)

# Cleaning data with 25% trackloss
response_window_clean <- clean_by_trackloss(data = data, trial_prop_thresh = .25)

############################
# GRAPHS FOR MAIN
############################

# Aggregating by Condition to get a proportion of looks to screen by AOI
response_window_agg_by_sub_main <- make_time_window_data(response_window_clean, aois = c("lookMannerBias", "lookMannerTest", "lookPathBias", "lookPathTest"), summarize_by = c("Condition"))
response_window_agg_by_sub_main$phase <- ifelse(response_window_agg_by_sub_main$AOI == "lookMannerBias" | response_window_agg_by_sub_main$AOI == "lookPathBias", "Bias Test", 
                                    ifelse(response_window_agg_by_sub_main$AOI == "lookMannerTest" | response_window_agg_by_sub_main$AOI == "lookPathTest", "Verb Test", "Error"))

# Getting an SE for each response (MannerBias, etc.)
response_window_agg_by_sub_main$Condition <- factor(response_window_agg_by_sub_main$Condition, levels=c("Manner", "Path"))
describe_main_1 <- describe_data(response_window_agg_by_sub_main, describe_column = "Prop", group_columns = c("AOI"))
describe_main <- describe_data(response_window_agg_by_sub_main, describe_column = "Prop", group_columns = c("Condition", "AOI"))
describe_main$SD <- describe_main_1$SD
describe_main$Var <- describe_main_1$Var
describe_main$se <- describe_main$SD/sqrt(describe_main$N)
response_window_agg_by_sub_main <- merge(response_window_agg_by_sub_main, describe_main, by=c("Condition", "AOI"))

# Making a bar graph
## gives a graph comparing the bias test and the verb test
ggplot(data=response_window_agg_by_sub_main, aes(x=Condition, y=Prop, fill=AOI)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") + 
  ylab("Proportion of looks to Manner") +
  guides(fill=guide_legend(title=NULL)) +
  geom_errorbar(aes(ymin=Prop-se, ymax=Prop+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  theme(axis.title = element_text(size=12),
        axis.text.x  = element_text(size=12),
        axis.text.y = element_text(size=12),
        plot.title = element_text(size=18, face="bold"),
        strip.text.x = element_text(size=18),
        legend.text = element_text(size = 12),
        legend.position="top") +
  scale_fill_manual(values=c("#F65D57", "#CC3399", "#00B4B8", "#3399FF"), 
                    breaks=c("lookMannerBias", "lookMannerTest", "lookPathBias", "lookPathTest"),
                    labels=c("Manner Bias", "Manner Test", "Path Bias", "Path Test")) +
  facet_wrap(~phase)
ggsave("maintrials_kid_and_adult.png", width=5.5, height=3,units = "in")


# Remaking a dataframe for only the Bias Test line graph
allMain_test_1 <- allMain_test[grep("biasTest", allMain_test$Trial_description),]
allMain_test_1$trialNo <- as.factor(allMain_test_1$trialNo)
allMain_test_1$lookMannerBias <- as.logical(allMain_test_1$lookMannerBias)
allMain_test_1$lookPathBias <- as.logical(allMain_test_1$lookPathBias)
allMain_test_1$system_time_stamp <- as.numeric(allMain_test_1$system_time_stamp)

# IDK IF THIS IS KOSHER OR NOT
allMain_test_1 %>% 
  distinct(trialNo, system_time_stamp, .keep_all = TRUE) -> allMain_test_1

# Starting to use eyetrackingR
data <- make_eyetrackingr_data(allMain_test_1, 
                               participant_column = "subjectID",
                               trial_column = "trialNo",
                               item_columns = "itemID",
                               time_column = "system_time_stamp",
                               trackloss_column = "Trackloss_column",
                               aoi_columns = c("lookMannerBias", "lookPathBias"),
                               treat_non_aoi_looks_as_missing = FALSE
)

# Cleaning data with 25% trackloss
response_window_clean <- clean_by_trackloss(data = data, trial_prop_thresh = .25)

# Aggregating by trialNo to get a proportion of looks to screen by AOI
response_window_agg_by_sub_main_trialNo <- make_time_window_data(response_window_clean, aois = c("lookMannerBias"), summarize_by = c("trialNo", "Condition"))
response_window_agg_by_sub_main_trialNo$phase <- ifelse(response_window_agg_by_sub_main_trialNo$AOI == "lookMannerBias" | response_window_agg_by_sub_main_trialNo$AOI == "lookPathBias", "Bias Test", 
                                                        ifelse(response_window_agg_by_sub_main_trialNo$AOI == "lookMannerTest" | response_window_agg_by_sub_main_trialNo$AOI == "lookPathTest", "Verb Test", "Error"))

# Getting an SE for each trial
describe_main_trialNo <- describe_data(response_window_agg_by_sub_main_trialNo, describe_column = "Prop", group_columns = c("trialNo"))
response_window_agg_by_sub_main_trialNo$se <- describe_main_trialNo$SD/sqrt(describe_main_trialNo$N)
response_window_agg_by_sub_main_trialNo$Condition <- factor(response_window_agg_by_sub_main_trialNo$Condition, levels=c("Manner", "Path"))

# Making the by-trial line graph
ggplot(data=response_window_agg_by_sub_main_trialNo, aes(x=trialNo, y=Prop, color=Condition, group = Condition)) +
  geom_line() + 
  geom_point() +
  ylab("Proportion of looks to Manner") +
  geom_errorbar(aes(ymin=Prop-se, ymax=Prop+se), width=0.25) +
  scale_x_discrete(name="Trial",
                   breaks=c("5", "6", "7", "8"),
                   labels=c("1", "2", "3", "4")) +
  theme(axis.title.y = element_text(size=12),
        axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=16),
        axis.text.y = element_text(size=16),
        plot.title = element_text(size=16, face="bold"),
        strip.text.x = element_text(size=16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.position="top") +
  facet_wrap(~phase)
ggsave("maintrials_biasTest_trialno_kid_and_adult.png", units="in", width=4, height = 3)





############################
# CREATING A SUBSET DF OF GENERALIZATION TEST TRIALS
############################
allExtend <- filter(allData, phase=="Extend")

# Applying it to the dataframe for trials, it's taking a long time, so we'll apply it in sections
a <- lapply(allExtend$system_time_stamp, trial_time)
allExtend$Trial_description <- a

allExtend <- allExtend[grep("biasTest_Extend", allExtend$Trial_description),]

allExtend$lookActionBias <- as.logical(allExtend$lookActionBias)

# IDK IF THIS IS KOSHER OR NOT
allExtend %>% 
  distinct(trialNo, system_time_stamp, .keep_all = TRUE) -> allExtend_test

# Starting to use eyetrackingR
data <- make_eyetrackingr_data(allExtend_test, 
                               participant_column = "subjectID",
                               trial_column = "trialNo",
                               item_columns = "itemID",
                               time_column = "system_time_stamp",
                               trackloss_column = "Trackloss_column",
                               aoi_columns = c("lookActionBias"),
                               treat_non_aoi_looks_as_missing = FALSE
)

# Cleaning data with 25% trackloss
response_window_clean <- clean_by_trackloss(data = data, trial_prop_thresh = .25)

############################
# GRAPHS FOR GENERALIZATION
############################

# Aggregating by subjectID to get a proportion of looks to screen by AOI
# Also getting a SE for each subject

response_window_agg_by_sub_extend_1 <- make_time_window_data(data, aois = c("lookActionBias"), summarize_by = c("Condition", "subjectID"))
response_window_agg_by_sub_extend <- make_time_window_data(data, aois = c("lookActionBias"), summarize_by = c("Condition"))
described_extend <- describe_data(response_window_agg_by_sub_extend_1, describe_column = "Prop", group_columns = c("Condition"))
described_extend$se <- described_extend$SD/sqrt(described_extend$N)
response_window_agg_by_sub_extend <- merge(response_window_agg_by_sub_extend, described_extend, by=c("Condition"))

# Creating a bar graph
ggplot(data=response_window_agg_by_sub_extend, aes(x=Condition, y=Prop, fill = Condition)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") + 
  geom_errorbar(aes(ymin=Prop-se, ymax=Prop+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  ylab("Proportion of looks to Action") +
  theme(axis.title = element_text(size=12),
        axis.text.x  = element_text(size=12),
        axis.text.y = element_text(size=12),
        plot.title = element_text(size=12, face="bold"),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13),
        legend.position = "top")
ggsave("extendtrials_bar_kid_and_adult.png", units="in", width=3, height=3)


# Aggregating by subjectID to get a proportion of looks to screen by AOI
# Also getting an SE for each trial
response_window_agg_by_sub_extend_trialNo <- make_time_window_data(data, aois = c("lookActionBias"), summarize_by = c("Condition", "trialNo"))
response_window_agg_by_sub_extend_trialNo$trialNo <- factor(response_window_agg_by_sub_extend_trialNo$trialNo, levels=c("7", "8", "9", "10", "11", "12", "13", "14"))
described_extend <- describe_data(response_window_agg_by_sub_extend_trialNo, describe_column = "ArcSin", group_columns = c("trialNo"))
response_window_agg_by_sub_extend_trialNo$se <- described_extend$SD/sqrt(described_extend$N)

# Creating by-trial line graph

response_window_agg_by_sub_extend_trialNo$Condition <- factor(response_window_agg_by_sub_extend_trialNo$Condition, levels=c("Manner", "Path"))

ggplot(data=response_window_agg_by_sub_extend_trialNo, aes(x=trialNo, y=Prop, group=Condition, color=Condition)) +
  geom_line() +
  geom_point() +
  ylab("Proportion of looks to Action") +
  geom_errorbar(aes(ymin=Prop-se, ymax=Prop+se), width=0.25) +
  ylim(0, 0.8) +
  theme(axis.title = element_text(size=18),
        axis.text.x  = element_text(size=18),
        axis.text.y = element_text(size=18),
        plot.title = element_text(size=18, face="bold")) 
ggsave("extendtrials_trialno_kid_and_adult.png")






############################
# ACTUAL ANALYSIS
############################
mainExtend <- bind_rows(allMain_test, allExtend_test)

# Starting to use eyetrackingR
data <- make_eyetrackingr_data(mainExtend, 
                               participant_column = "subjectID",
                               trial_column = "trialNo",
                               time_column = "system_time_stamp",
                               trackloss_column = "Trackloss_column",
                               aoi_columns = c("lookActionBias", "lookMannerBias", "lookMannerTest", "lookPathBias", "lookPathTest"),
                               treat_non_aoi_looks_as_missing = FALSE
)

# Cleaning data with 25% trackloss
response_window_clean <- clean_by_trackloss(data = data, trial_prop_thresh = .25)

response_window_agg_analysis <- make_time_window_data(response_window_clean, 
                                             aois= c("lookActionBias", "lookMannerBias", "lookMannerTest", "lookPathBias", "lookPathTest"), 
                                             predictor_columns=c("Condition","subjectID"))

# got an error: could not find function "spread", but the dplyr package is installed
response_window_agg_analysis %>%
  spread(AOI, Prop) %>%
  group_by(subjectID)-> test

# removes incomplete cases
response_window_agg_analysis <- na.omit(response_window_agg_analysis)

response_window_agg_analysis %>%
  group_by(subjectID) %>%
  tidyr::spread(AOI, Prop) -> test

# analyses the effect of different conditions on the looking times to path in the test trials
M1 <- lmer(lookPathTest ~ Condition +
             (1 | subjectID), 
            data=test)

summary(M1)
anova(M1)

library(lmerTest)

# analyses the effect of different conditions on the looking times to action in the bias test
M2 <- lmer(lookActionBias ~ Condition +
            (1|subjectID),
            data=test)

summary(M2)
anova(M2)



