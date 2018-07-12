###############################
# THE SCRIPT
###############################

#
# Put description here
#

###############################
# Preliminaries
###############################
#Run these two lines the first time only to install eyetrackingR
#install.packages("devtools")
#devtools::install_github("jwdink/eyetrackingR")
#install.packages("eyetrackingR")
#install.packages('sqldf')
library("eyetrackingR")
library("plyr")
library("lme4")
library("ggplot2")
library("Matrix")
library("tidyr")
library("stringr")
library("sqldf")
library("dplyr")
#install.packages('bootstrap')
library("bootstrap")
library("testthat")

#=======CUSTOM FUNS=======#
validate.names = function(df){
  rtn = df
  valid_column_names = make.names(names=names(df), unique=TRUE, allow_ = TRUE)
  names(rtn) = valid_column_names
  rtn
}

mean.na.rm <- function(mylist){
  return(mean(mylist, na.rm=TRUE))
}

bootup <- function(mylist){
  foo <- bootstrap(mylist, 1000, mean.na.rm)
  return(quantile(foo$thetastar, 0.975)[1])
}
bootdown <- function(mylist){
  foo <- bootstrap(mylist, 1000, mean.na.rm)
  return(quantile(foo$thetastar, 0.025)[1])
}

se <- function(mylist){
  return(sd(mylist)/sqrt(length(mylist)))
}
#=========================#


kids_to_process <- c('2018-07-05_ChildPilot1_x2',
                     '2018-07-05_ChildPilot2',
                     '2018-07-05_ChildPilot3',
                     '2018-07-06_ChildPilot4',
                     '2018-07-09_ChildPilot5',
                     '2018-07-09_ChildPilot6',
                     '2018-07-10_ChildPilot7x4',
                     '2018-07-10_ChildPilot8',
                     '2018-07-11_ChildPilot9')
#Set your directories

myRepo = '~/Desktop/MannerPathPriming-3ET'
analysisDir = paste(myRepo, '/Analysis/Olivia W. MPP3ET',sep='')
dataDir = paste(myRepo, '/Data',sep='')

########
context('Check that directory was set correctly and participant file loads!')
expect_true(dir.exists(myRepo))
########


###############################
# LOAD DATA
###############################

setwd(myRepo)
pData <- read.csv('all_participants_MPP3ET.csv', stringsAsFactors = FALSE)

#load participantdata
pData <- read.csv('all_participants_MPP3ET.csv', stringsAsFactors = FALSE)

pData <- pData %>%
  filter(SubjectID %in% kids_to_process)%>%
  mutate(subjectID = str_to_lower(SubjectID))%>%
  select(-SubjectID)

context('Check all participants were found in the csv')
expect_that(length(unique(pData$subjectID)), equals(length(kids_to_process)))

#Load data for all participants from their data folders
DatData <- data.frame(NULL)
TimestampData <- data.frame(NULL)
GazeData <- data.frame(NULL)
kids_to_process <- str_to_lower(kids_to_process)

for(ID in pData$subjectID){
  myDataDir <- paste(dataDir,ID, sep='/')
  setwd(myDataDir)
  print(ID)
  
  myDatFile <- paste('data_MPP3ET_',ID, '.dat', sep='')
  myTimestampFile <- paste('timestamps_MPP3ET_',ID,'.csv',sep='')
  myMainGazeFiles <- list.files(path = myDataDir, full.names = TRUE, pattern = ".*\\Main_.*.csv")
  myPracticeGazeFiles <- list.files(path = myDataDir, full.names = TRUE, pattern = ".*\\Practice_.*.csv")
  
  myDatData <- read.csv(myDatFile, stringsAsFactors = FALSE)
  
  myTimestampData <- read.csv(myTimestampFile, stringsAsFactors = FALSE)
   
  myGazeData <- data.frame(NULL)
  
  for (f in myMainGazeFiles){
    thisGazeData = read.csv(f, stringsAsFactors = FALSE)
    thisGazeData$filename = f
    thisGazeData$phaseGaze = 'Main'
    if(nrow(myGazeData) == 0) {
      
      myGazeData = thisGazeData
      
    } else{
      myGazeData = bind_rows(myGazeData, thisGazeData)
    }
  }
  
  for (f in myPracticeGazeFiles){
    thisGazeData = read.csv(f, stringsAsFactors = FALSE)
    thisGazeData$filename = f
    thisGazeData$phaseGaze = 'Practice'
    myGazeData = bind_rows(myGazeData, thisGazeData)
  }
  #Add everything to the big DFs!
  if (nrow(DatData) == 0){ #special case for 1st round
    DatData = myDatData
    TimestampData = myTimestampData
    GazeData = myGazeData
  } else {
    DatData = bind_rows(DatData, myDatData)
    TimestampData = bind_rows(TimestampData, myTimestampData)
    GazeData = bind_rows(GazeData, myGazeData)
  }
}

setwd(analysisDir)

###############################
# PROCESS/RECODE DATA
###############################

#Save original lengths to check not rows get dropped
tr = nrow(TimestampData)
gr = nrow(GazeData)
dr = nrow(DatData)

TimestampData <- TimestampData %>%
  mutate(point_description = ifelse(str_count(point_description, "Practice"),
                                    point_description,
                                    paste("Main_", point_description, sep="")))%>%
  mutate(subjectID = str_to_lower(subjectID))%>%
  separate(point_description, c("phaseTimestamp", "trialNo", "description"), extra = 'merge', remove = FALSE)

GazeData <- GazeData %>%
  mutate(subjectID = str_to_lower(subjectID))

DatData <- DatData %>%
  select(c("SubjectNo","Date","Time","VerbDomain","Condition","trialNo","itemID" ,"verbName","verbMeaning",
           "mannerSideBias", "pathSideBias","mannerSideTest", "pathSideTest")) %>%
  mutate_if(is.factor, as.character) %>%
  mutate(subjectID = str_to_lower(SubjectNo)) %>%
  mutate(itemID = as.character(itemID))%>%
  select(-SubjectNo) %>%
  mutate(ExperimentPhase = 'Main') %>%
  mutate(targetSideBias = ifelse(Condition == 'Manner', mannerSideBias,pathSideBias)) %>%
  mutate(targetSideTest = ifelse(Condition == 'Manner', mannerSideTest, pathSideTest))
  
  
#Manually add Pratice lines to the Dat files - parameters always the same! Target on the right for trial 1, target on left for trial 2
#NOTE: This may add a trial the child didn't actually do (ie if second practice trial
#wasn't run, the new version), but this will be fine bc it won't correspond to any timestamps
pract1 <- DatData %>%
  group_by(subjectID)%>%
  summarise_all(first)%>%
  mutate(trialNo = 1, itemID = 'practice1', verbName = 'NA', ExperimentPhase = 'Practice',
         verbMeaning = 'book', mannerSideBias = 'NA', pathSideBias = 'NA',
         mannerSideTest = 'NA', pathSideTest = 'NA', targetSideBias = 'NA', targetSideTest = 'R')

pract2 <- DatData %>%
  group_by(subjectID)%>%
  summarise_all(first)%>%
  mutate(trialNo = 2, itemID = 'practice2', verbName = 'NA', ExperimentPhase = 'Practice',
         verbMeaning = 'ball', mannerSideBias = 'NA', pathSideBias = 'NA',
         mannerSideTest = 'NA', pathSideTest = 'NA', targetSideBias = 'NA', targetSideTest = 'L')

DatData$pathSideTest<-as.numeric(as.character(DatData$pathSideTest))
pract1$pathSideTest<-as.numeric(as.character(pract1$pathSideTest))
pract2$pathSideTest<-as.numeric(as.character(pract2$pathSideTest))
#pathSideTest for pract1 and 2 are NA as a result, they are converted first to as.character so that they are not ranked from big to small, and then to numerics
#To resolve error message: Error in bind_rows_(x,.id): Column 'pathSideTest'can't be converted from numeric to character
#comments by Olivia
DatData <- bind_rows(DatData, pract1) 
DatData <- bind_rows(DatData, pract2)  

#Make sure all dfs stayed the right length
expect_equal(sum(tr, gr, dr) + 2*length(kids_to_process), 
             sum(nrow(DatData), nrow(GazeData), nrow(TimestampData))) 

#Update correct DatData length
dr = nrow(DatData)
###############################
# MERGE DATA (ahhhh!)
###############################

#Merge all subject- and trial-level data, dropping fake trials!
AllSubjData <- merge(DatData, pData, by="subjectID") %>%
  mutate(trialNo = ifelse(ExperimentPhase == 'Main', as.numeric(trialNo), as.numeric(trialNo)-100)) %>%
  filter(!(Experiment == 'Pilot3 - NEW - SHORT'& trialNo == -98))
  

expect_equal(length(unique(AllSubjData$subjectID)), length(kids_to_process))
ar = nrow(AllSubjData)

#Get start times (of the whole experiment) to normalize clock variables!
#(Values are sufficiently large they sometimes crash excel)
startTime <- TimestampData %>%
  mutate(subjectID = str_to_lower(subjectID))%>%
  group_by(subjectID) %>%
  arrange(system_time_stamp)%>%
  filter(description == 'Start')%>%
  select(subjectID, system_time_stamp)%>%
  mutate(expStartTime = system_time_stamp) %>%
  select(-system_time_stamp) %>%
  mutate('stringexpStartTime' = as.character(expStartTime))

GazeData = merge(GazeData, startTime, by=c("subjectID"), all.x=TRUE) 
expect_equal(nrow(GazeData), gr)

GazeData <- GazeData %>%
  mutate(GazeDataStringTime = as.character(system_time_stamp)) %>%
  mutate(adjusted_time = system_time_stamp - expStartTime) %>%
  select(-c(device_time_stamp, system_time_stamp, expStartTime)) %>%
  separate(description, c("x","y","z", "w", "trialNo")) %>% 
  select(-c(x,y,z,w))
  
#Reshape TimestampData
TimestampData = merge(TimestampData, startTime, by = c("subjectID"))
TimestampData <- TimestampData %>%
  mutate(TimestampDataStringTime = as.character(system_time_stamp))%>%
  mutate(adjusted_start_time = system_time_stamp - expStartTime) %>%
  select(-c(system_time_stamp, expStartTime, point_description)) %>%
  arrange(subjectID, adjusted_start_time) %>%
  mutate(phaseTimestamp = factor(phaseTimestamp, levels=c("Practice","Main")))%>%
  mutate(timeGroupings = paste(subjectID, phaseTimestamp, trialNo)) %>%
  group_by(timeGroupings) %>%
  mutate(adjusted_end_time = lead(adjusted_start_time, order_by = timeGroupings)) %>%
  mutate(next_description = lead(description, order_by = timeGroupings)) %>% #NOTE: these give nonsensical values between edge cases, watch out...
  ungroup() %>%
  select(-timeGroupings) %>%
  #Doing some cleanup on those bad edge cases
  mutate(next_description = ifelse(description == 'SameVerbTest_compareVideo2_end', 
                                  'TRIAL END', next_description)) %>%
  mutate(adjusted_end_time = ifelse(description == 'SameVerbTest_compareVideo2_end', 
                                 adjusted_start_time, adjusted_end_time)) %>%
  mutate(segment_length_in_sec = (adjusted_end_time-adjusted_start_time)/1000000) #For checking things are the right length....

#Column cleanup
TimestampData <- TimestampData %>%
  mutate(trial_sanitycheck = paste(description, next_description, sep='-TO-'))%>%
  select(-c(next_description))

#Make sure all dfs stayed the right length
expect_equal(sum(tr, gr, dr, ar), 
             sum(nrow(DatData), nrow(GazeData), nrow(TimestampData), nrow(AllSubjData)))

#Make sure trial numberings all match!
#####
#####
#NOTE: This line may produce a warning 'NAs introduced by coercion' - this will happen
#bc for out-of-pattern Timestamps that leave a string in the trialNo column
#####
#####
TimestampData <- TimestampData %>%
  mutate(trialNo = ifelse(phaseTimestamp == 'Main', as.numeric(trialNo), as.numeric(trialNo)-100))

GazeData <- GazeData %>%
  mutate(trialNo = ifelse(phaseGaze == 'Main', as.numeric(trialNo), as.numeric(trialNo)-100))

DatData <- DatData %>% #Unneccessary except for checksums below
  mutate(trialNo = ifelse(ExperimentPhase == 'Main', as.numeric(trialNo), as.numeric(trialNo)-100))


#Now do a cool SQL merge to find the timestamp window that each gazepoint belongs to!
#(Takes a while)
#Note that this operation should NOT drop any GazeData observations. 
# This drops lines :( TimestampedGazeData = sqldf("select * from GazeData f1 inner join TimestampData f2 
TimestampedGazeData = sqldf("select * from GazeData f1 outer left join TimestampData f2 
            on (f1.adjusted_time > f2.adjusted_start_time 
            and f1.adjusted_time<= f2.adjusted_end_time
            and f1.subjectID == f2.subjectID
            and f1.PhaseGaze == f2.PhaseTimestamp
            and f1.trialNo == f2.trialNo) ")

TimestampedGazeData <- TimestampedGazeData%>%
  validate.names() %>% #(See fn at beginning of file, handles duplicated names gen'd by sqldf)
  mutate(ExperimentPhase = phaseGaze) %>%
  select("subjectID","ExperimentPhase", "trialNo","L_valid", "L_x","L_y","R_valid", "R_x", "R_y",
         "stringexpStartTime","GazeDataStringTime",
         "adjusted_time", "adjusted_start_time","adjusted_end_time",
         "segment_length_in_sec","description" )

#And merge on the Trial level data!
AllData <- NULL
AllData <- merge(TimestampedGazeData, AllSubjData, by=c("subjectID", "ExperimentPhase", "trialNo"), all.x=TRUE, all.y=TRUE)

#Check number of unique subjects & trialNos
ts_length <- TimestampedGazeData %>%
  group_by(subjectID, ExperimentPhase, trialNo) %>%
  dplyr::summarize(numlines = n())

as_length <- AllSubjData %>%
  group_by(subjectID, ExperimentPhase, trialNo) %>%
  dplyr::summarize(numlines = n())

#Make sure all dfs stayed the right length
table(GazeData$subjectID)
table(AllData$subjectID)

expect_equal(gr, nrow(AllData))
expect_equal(length(unique(AllData$subjectID)), length(kids_to_process))

#Similarly, all trials should be present
expect_equal(nrow(AllSubjData), nrow(unique(AllData[c("subjectID", "ExperimentPhase","trialNo")])))

#########################
# FORMAT for eyetrackingr package
#########################

AllData <- AllData %>%
  mutate(Trackloss = !R_valid & !L_valid)%>%
  mutate(Trackloss = ifelse(is.na(Trackloss),TRUE, Trackloss))%>%
  mutate(trialNo = as.numeric(trialNo))%>%
  mutate(trialNo = ifelse(ExperimentPhase == 'Main', trialNo, trialNo-100))%>%
  mutate(Gaze_x = rowMeans(cbind(R_x, L_x), na.rm=TRUE)) %>%
  mutate(Gaze_y = rowMeans(cbind(R_y, L_y), na.rm=TRUE)) %>%
  separate(description, c('probeType','probeSegment'), extra = "merge")

#TEST all timebins (at least in the Main exp) should be the right length for their segment. Check for problems here
segments <- AllData %>%
  group_by(probeSegment, ExperimentPhase)%>%
  dplyr::summarize(meanlen = mean(segment_length_in_sec, na.rm=TRUE), selen = se(segment_length_in_sec))
View(segments)

#Add by-probe time windows so that eyetrackingr can find them!
AllData <- AllData %>%
  dplyr::group_by(subjectID,trialNo,probeType) %>%
  dplyr::mutate(start_time_by_probe = min(adjusted_start_time))%>%
  dplyr::mutate(end_time_by_probe = max(adjusted_end_time))%>%
  ungroup()
  
#Add AOIs
# LEFT- liberal
# LEFT moviebox
# RIGHT - liberal
# RIGHT moviebox
# Center moviebox
#NOTE these AOIS are in relative numbers (0,0 to 1,1), and are accurate
#for display on our 1280x1040 T60; but maybe not on yours (the PTB help
#code has some pixel-based calculations!)
aois = read.csv('aoi_t60_LionRoom.csv', stringsAsFactors = FALSE)
for (i in 1:nrow(aois)) {
  AllData = add_aoi(data=AllData, aoi_dataframe = aois[i,], 
                x_col= "Gaze_x", y_col= "Gaze_y", 
                aoi_name = aois[i,]$AOIName)}

#Add Derived AOIS (using the known target side!)
AllData <- AllData %>%
  mutate(targetSide = ifelse(probeType == 'Bias', targetSideBias, targetSideTest))%>%
  mutate(In_Target_Box = ifelse(targetSide == 'L', Left_Box, Right_Box))%>%
  mutate(In_Target_Side = ifelse(targetSide == 'L', Left_Side, Right_Side)) %>%
  mutate(In_NonTarget_Box = ifelse(targetSide == 'R', Left_Box, Right_Box))%>%
  mutate(In_NonTarget_Side = ifelse(targetSide == 'R', Left_Side, Right_Side)) %>%
  mutate(In_Manner_Box = ifelse(Condition == "Manner", In_Target_Box, In_NonTarget_Box)) %>%
  mutate(In_Manner_Side = ifelse(Condition == "Manner", In_Target_Side, In_NonTarget_Side))

#For dropping misbehaving timepoints (see below)

AllData <- AllData %>%
  filter(!(subjectID =='child_pilot_03282018_10_3am'& trialNo ==6 & adjusted_time ==493068505))

ERData <- make_eyetrackingr_data(AllData, 
                               participant_column = "subjectID",
                               trial_column = "trialNo",
                               time_column = "adjusted_time",
                               trackloss_column = "Trackloss",
                               aoi_columns = c('Left_Box','Right_Box',
                                               'Center_Box','Left_Side','Right_Side',
                                               'In_Target_Box','In_Target_Side',
                                               'In_NonTarget_Box','In_NonTarget_Side',
                                               'In_Manner_Box','In_Manner_Side'),
                               treat_non_aoi_looks_as_missing = FALSE)

####
####
#NOTE: A waring that 'your dataset has a column called Time' may occur (I used this for hour of testing on days
#where we had multiple kids'); This is fine, as described in the message that column will be renamed/saved
#NOTE: If you get an error about 'trial_column not unique within participants' and a table, this
#may indicate that two timestamps are registering as being at the same time (I had exactly 1 such measurement
#in the pilot set.  Best bet for now is to manually exclude that timepoint.  See above for syntax!)
####

#########################
# TESTS for eyetrackingr package (Don't skip!)
#########################

#During segments when a video is always playing on the right or left, we should see more
#looks in those regions!!!!
leftlooks = describe_data(ERData, describe_column = "Left_Box", group_columns = "probeSegment")
rightlooks = describe_data(ERData, describe_column = "Right_Box", group_columns = "probeSegment")
checklooks = filter(bind_rows("Left_Box" = leftlooks, "Right_Box" = rightlooks, .id = 'AOI'), 
       probeSegment == 'left_video' | probeSegment == 'right_video')
expect_true(all.equal(checklooks$Mean, c(1, 0, 0, 1), tolerance = 0.15))

#The descriptions of trackloss on each trial should make sense given what you know about the participants!
TL_Descriptives = trackloss_analysis(ERData)
View(TL_Descriptives)

#########################
# IN THE FUTURE, All subject descriptives should be calculated here so they can be reported!
#########################

#########################
# SUBSETTING DATA
#########################

ERData_zeroed = subset_by_window(ERData, window_start_col = "start_time_by_probe", window_end_col = "end_time_by_probe", rezero = TRUE)

Probe_Data = filter(ERData_zeroed, probeType == 'SameVerbTest' | probeType == 'Bias')
Probe_Data <- clean_by_trackloss(data = Probe_Data, trial_prop_thresh = .25)


#########################
# IN THE FUTURE, All post-data-cleaning subject descriptives should be calculated here so they can be reported!
#########################
#Assess Trackloss
trackloss_clean <- trackloss_analysis(data = Probe_Data)
trackloss_clean_subjects <- unique(trackloss_clean[, c('subjectID','TracklossForParticipant')])
mean(1 - trackloss_clean_subjects$TracklossForParticipant)
sd(1- trackloss_clean_subjects$TracklossForParticipant)

final_summary <- describe_data(Probe_Data, 'Left_Box', c('subjectID','Condition'))
mean(final_summary$NumTrials)
sd(final_summary$NumTrials)


#########################
# GRAPHS (It's very exciting!)
#########################

#A function to generate the section-by-section plot, showing first looks-to-left (for single
#presentation), then looks to manner/target

MakeSpaghetti <- function(eyedata, pt, ep){
  these_LR_looks <- filter(eyedata, probeType == pt, ExperimentPhase == ep,
                          probeSegment %in% c('left_video','right_video'))
  these_comp1 <- filter(eyedata, probeType == pt, ExperimentPhase == ep,
                        probeSegment %in% c('compareVideo1_start','compareVideo1_still'))
  these_comp2 <- filter(eyedata, probeType == pt, ExperimentPhase == ep,
                        probeSegment %in% c('compareVideo2_start','compareVideo2_still'))

  LR_seq <- make_time_sequence_data(these_LR_looks, time_bin_size = 100000, 
                                         predictor_columns = c("Condition"),
                                         aois = "Left_Side",
                                         summarize_by = "subjectID")
  comp1_seq <- make_time_sequence_data(these_comp1, time_bin_size = 100000, 
                                         predictor_columns = c("Condition"),
                                         aois = c("In_Manner_Side"),
                                         summarize_by = "subjectID")
  comp2_seq <- make_time_sequence_data(these_comp2, time_bin_size = 100000, 
                                         predictor_columns = c("Condition"),
                                         aois = c("In_Manner_Side"),
                                         summarize_by = "subjectID")
  
  this_seqdata = bind_rows("LR" = LR_seq, 
                       "Comp1" = comp1_seq,
                       "Comp2" = comp2_seq,.id='ResponseWindow')
  
  this_seqdata <- this_seqdata %>%
    mutate(ResponseWindow = factor(ResponseWindow))%>%
    mutate(ResponseWindow = factor(ResponseWindow, levels(ResponseWindow)[c(3,1,2)])) %>%
    mutate(Time_in_Sec = Time/1000000) %>%
    filter(!is.na(Prop))%>%
    group_by(Condition, ResponseWindow, TimeBin, Time_in_Sec) %>%
    dplyr::summarize(themean = mean(Prop, na.rm=TRUE))

   
  print('get here!')
  
  this_plot <- ggplot(data = this_seqdata, aes(y=themean,x=Time_in_Sec,color=Condition)) +
    geom_line(stat="identity") +
    #geom_errorbar(aes(ymin=ci_down, ymax=ci_up), colour="black", width=.1, position=position_dodge(1.5)) + #Why point 9? Hell if I know!
    facet_wrap(~ResponseWindow, scales = "free_x") +
    geom_line(y=0.5, color='black')
  
  return(list(this_seqdata, this_plot))

}


#Run this function, then print to the console to see the graph!
foo = MakeSpaghetti(Probe_Data, 'SameVerbTest','Main')
foo = MakeSpaghetti(Probe_Data, 'Bias','Main')
foo




######OLD CODE BELOW HERE, DON"T USE ##############
############################
# LOOKING AT MAIN TRIALS
############################

df_111_main <- data.frame(Date=as.Date(character()),
                              File=character(), 
                              User=character(), 
                              stringsAsFactors=FALSE) 
for(file in file.names_main){
  temp <- read.csv(file, header = TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  df_111_main <-rbind(df_111_main, temp)
}

#cleaning up the data to get it in the form I want
colnames(df_111_main)[which(names(df_111_main) == "description")] <- "trialNo"
df_111_main$L_valid <- as.factor(df_111_main$L_valid)
df_111_main$R_valid <- as.factor(df_111_main$R_valid)
df_111_main$system_time_stamp <- df_111_main$system_time_stamp - 1500000000000000

#defining a trackloss column
df_111_main$Trackloss_column <- as.factor(ifelse(df_111_main$L_valid == '1' & df_111_main$R_valid == '1', TRUE, 
                                         ifelse(df_111_main$L_valid == '0' & df_111_main$R_valid == '1', FALSE,
                                         ifelse(df_111_main$L_valid == '1' & df_111_main$R_valid == '0', FALSE,
                                         ifelse(df_111_main$L_valid == '0' & df_111_main$R_valid == '0', FALSE, 'Error')))))


#read in timestamps
df_timestamps <- read.csv("~/Documents/Github/MannerPathPriming-2ET/Data/Melissa_111/timestamps_MPPCREATION_Melissa_111.csv", header = TRUE, stringsAsFactors=FALSE, fileEncoding = "latin1")
df_timestamps$subjectID <- as.factor(df_timestamps$subjectID)
df_timestamps$system_time_stamp <- df_timestamps[,2] - 1500000000000000

#applying it to the dataframe for main trials
a <- lapply(df_111_main$system_time_stamp, trial_time)
df_111_main$Trial_description <- a

#averaging together L and R eyes
df_111_main$X <- rowMeans(subset(df_111_main, select = c(6, 9)), na.rm = TRUE)
df_111_main$Y <- rowMeans(subset(df_111_main, select = c(7, 10)), na.rm = TRUE)

#merging together dat_table and trials to get correctness
df_111_main$trialNo <- as.factor(ifelse(df_111_main$trialNo == "All_of_Main_trial_5", "5", 
                                 ifelse(df_111_main$trialNo == "All_of_Main_trial_6", "6",
                                 ifelse(df_111_main$trialNo == "All_of_Main_trial_7", "7",
                                 ifelse(df_111_main$trialNo == "All_of_Main_trial_8", "8", "Error")))))
df_111_main_aoi$Trackloss_column <- as.logical(df_111_main_aoi$Trackloss_column)

df_111_main <- merge(df_111_main, dat_table, by="trialNo")

#Making sure they're looking at the correct video or not
df_111_main_aoi %>%
  group_by(Condition, subjectID, trialNo) %>% 
  mutate(correctBias = ifelse(Condition == 'Path' & pathSideBias == "L" & X < 0.605 & X > 0.25 & Y > 0.1963 & Y < 0.6313, TRUE,
                   ifelse(Condition == 'Path' & pathSideBias == "R" & X < 1.250 & X > 0.67 & Y > 0.1963 & Y < 0.6313, TRUE,
                   ifelse(Condition == 'Manner' & mannerSideBias == "L" & X < 0.605 & X > 0.25 & Y > 0.1963 & Y < 0.6313, TRUE,
                   ifelse(Condition == 'Manner' & mannerSideBias == "R" & X < 1.250 & X > 0.67 & Y > 0.1963 & Y < 0.6313, TRUE, FALSE))))) %>% 
  mutate(correctTest = ifelse(Condition == 'Path' & pathSideTest == "L" & X < 0.605 & X > 0.25 & Y > 0.1963 & Y < 0.6313, TRUE,
                       ifelse(Condition == 'Path' & pathSideTest == "R" & X < 1.250 & X > 0.67 & Y > 0.1963 & Y < 0.6313, TRUE,
                       ifelse(Condition == 'Manner' & mannerSideTest == "L" & X < 0.605 & X > 0.25 & Y > 0.1963 & Y < 0.6313, TRUE,
                       ifelse(Condition == 'Manner' & mannerSideTest == "R" & X < 1.250 & X > 0.67 & Y > 0.1963 & Y < 0.6313, TRUE, FALSE))))) -> df_111_main_aoi

#adding an AOI column for Incorrect looks BIAS to screen
df_111_main_aoi$incorrectBias <- ifelse(df_111_main_aoi$correctBias == TRUE, FALSE,
                                 ifelse(df_111_main_aoi$correctBias == FALSE, TRUE, 'Error'))

df_111_main_aoi$incorrectBias <- as.logical(df_111_main_aoi$incorrectBias)

#adding an AOI column for Incorrect looks TEST to screen
df_111_main_aoi$incorrectTest <- ifelse(df_111_main_aoi$correctTest == TRUE, FALSE,
                                 ifelse(df_111_main_aoi$correctTest == FALSE, TRUE, 'Error'))

df_111_main_aoi$incorrectTest <- as.logical(df_111_main_aoi$incorrectTest)


#starting to use eyetrackingR
data <- make_eyetrackingr_data(df_111_main_aoi, 
                               participant_column = "subjectID",
                               trial_column = "trialNo",
                               time_column = "system_time_stamp",
                               trackloss_column = "Trackloss_column",
                               aoi_columns = c("correctBias", "incorrectBias", "correctTest", "incorrectTest"),
                               treat_non_aoi_looks_as_missing = TRUE
)

#aggregating by subjectID to get a proportion of looks to screen by AOI, with only trial 5
esponse_window_agg_by_sub <- make_time_window_data(data, aois = c("correctBias", "incorrectBias", "correctTest", "incorrectTest"), summarize_by = "subjectID")

#creating plots
ggplot(data=response_window_agg_by_sub, aes(x=AOI, y=Prop)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") + 
  ylab("Proportion of looks to screen") +
  ggtitle("Path Condition") +
  theme(axis.title = element_text(size=18),
        axis.text.x  = element_text(size=18),
        axis.text.y = element_text(size=18),
        plot.title = element_text(size=18, face="bold")) +
  scale_x_discrete(breaks=c("Correct", "Incorrect"),
                   labels=c("Path", "Manner"))
ggsave("melissa_path_main_trials.png")

##########################
# LOOKING AT EXTEND TRIALS
##########################

#reading in extend trial CSVs
df_111_extend <- data.frame(Date=as.Date(character()),
                          File=character(), 
                          User=character(), 
                          stringsAsFactors=FALSE) 
for(file in file.names_extend){
  temp <- read.csv(file, header = TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  df_111_extend <-rbind(df_111_extend, temp)
}

#cleaning up the data to get it in the form I want
colnames(df_111_extend)[which(names(df_111_extend) == "description")] <- "trialNo"
df_111_extend$L_valid <- as.factor(df_111_extend$L_valid)
df_111_extend$R_valid <- as.factor(df_111_extend$R_valid)
df_111_extend$system_time_stamp <- df_111_extend$system_time_stamp - 1500000000000000

#defining a trackloss column
df_111_extend$Trackloss_column <- as.factor(ifelse(df_111_extend$L_valid == '1' & df_111_extend$R_valid == '1', TRUE, 
                                         ifelse(df_111_extend$L_valid == '0' & df_111_extend$R_valid == '1', FALSE,
                                         ifelse(df_111_extend$L_valid == '1' & df_111_extend$R_valid == '0', FALSE,
                                         ifelse(df_111_extend$L_valid == '0' & df_111_extend$R_valid == '0', FALSE, 'Error')))))

a <- lapply(df_111_extend$system_time_stamp, trial_time)
df_111_extend$Trial_description <- a

#averaging together L and R eyes
df_111_extend$X <- rowMeans(subset(df_111_extend, select = c(6, 9)), na.rm = TRUE)
df_111_extend$Y <- rowMeans(subset(df_111_extend, select = c(7, 10)), na.rm = TRUE)

#merging together dat_table and trials to get correctness
df_111_extend$trialNo <- as.factor(ifelse(df_111_extend$trialNo == "All_of_Extend_trial_5", "13", 
                                ifelse(df_111_extend$trialNo == "All_of_Extend_trial_6", "14",
                                ifelse(df_111_extend$trialNo == "All_of_Extend_trial_7", "7",
                                ifelse(df_111_extend$trialNo == "All_of_Extend_trial_8", "8",
                                ifelse(df_111_extend$trialNo == "All_of_Extend_trial_1", "9",
                                ifelse(df_111_extend$trialNo == "All_of_Extend_trial_2", "10",
                                ifelse(df_111_extend$trialNo == "All_of_Extend_trial_3", "11",
                                ifelse(df_111_extend$trialNo == "All_of_Extend_trial_4", "12", "Error")))))))))

df_111_extend <- merge(df_111_extend, dat_table, by="trialNo")

#Making sure they're looking at the correct video or not
subjID_aoi_extend <- read.csv("~/Documents/Github/MannerPathPriming-2ET/Analysis/subjID_aoi_extend.csv", header = TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
subjID_aoi_extend$trialNo <- as.factor(subjID_aoi_extend$trialNo)
df_111_extend_aoi <- add_aoi(df_111_extend, aoi_dataframe = subjID_aoi_extend, 'X', 'Y', aoi_name="Correct", x_min_col = "X_min",
                           x_max_col = "X_max", y_min_col = "Y_min", y_max_col = "Y_max")

#adding an AOI column for Incorrect looks to screen
df_111_extend_aoi$Incorrect <- ifelse(df_111_extend_aoi$Correct == TRUE, FALSE,
                                    ifelse(df_111_extend_aoi$Correct == FALSE, TRUE, 'Error'))
df_111_extend_aoi$Incorrect <- as.logical(df_111_extend_aoi$Incorrect)

#defining a trackloss column
df_111_extend_aoi$Trackloss_column <- ifelse(df_111_extend_aoi$L_valid == '1' & df_111_extend_aoi$R_valid == '1', FALSE, 
                                           ifelse(df_111_extend_aoi$L_valid == '0' & df_111_extend_aoi$R_valid == '1', TRUE,
                                                  ifelse(df_111_extend_aoi$L_valid == '1' & df_111_extend_aoi$R_valid == '0', TRUE,
                                                         ifelse(df_111_extend_aoi$L_valid == '0' & df_111_extend_aoi$R_valid == '0', TRUE, 'Error'))))
df_111_extend_aoi$Trackloss_column <- as.logical(df_111_extend_aoi$Trackloss_column)
#starting to use eyetrackingR
data <- make_eyetrackingr_data(df_111_extend_aoi, 
                               participant_column = "subjectID",
                               trial_column = "trialNo",
                               time_column = "system_time_stamp",
                               trackloss_column = "Trackloss_column",
                               aoi_columns = c("Correct", "Incorrect"),
                               treat_non_aoi_looks_as_missing = TRUE
)

#aggregating by subjectID to get a proportion of looks to screen by AOI, with only trial 5
data_summary <- describe_data(data, 
                              describe_column='Correct', group_columns=c('subjectID'))
response_window_agg_by_sub <- make_time_window_data(data, aois = c("Correct", "Incorrect"), summarize_by = "subjectID")

#creating plots
ggplot(data=response_window_agg_by_sub, aes(x=AOI, y=Prop)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") + 
  ylab("Proportion of looks to screen") +
  ggtitle("Path Condition") +
  theme(axis.title = element_text(size=18),
        axis.text.x  = element_text(size=18),
        axis.text.y = element_text(size=18),
        plot.title = element_text(size=18, face="bold")) +
  scale_x_discrete(breaks=c("Correct", "Incorrect"),
                   labels=c("Outcome", "Action"))
ggsave("extendtrials_melissa_path.png")


