#This doc/folder is for understanding where and why timing is going all wrong in the MPP-ET
#experiments! This issue dates back to 2ET (where we thought it was a fluke/dataloss issue)
#but thanks to manual timing by Olivia, we know that the pilot data collected summer 2018
#(through at least 7/20/18)

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


#Set directories
myRepo = '~/Dropbox/_DB_Projects/MannerPath-2ET/MannerPathPriming-3ET'
#myRepo = '~/Desktop/MannerPathPriming-3ET'
analysisDir = paste(myRepo, '/Analysis/Timing Debug',sep='')
dataDir = paste(myRepo, '/Data',sep='')

########
context('Check that directory was set correctly and participant file loads!')
expect_true(dir.exists(myRepo))
setwd(dataDir)
########

########
#Get the relevant files: child data, plus some testing.
########

alldirs <- data.frame(list.dirs())
names(alldirs) <- c('ID')
childdata <- alldirs %>%
  filter(str_detect(tolower(ID), 'child')) %>%
  mutate(datatype='Child')


notr <- alldirs %>%
  filter(str_detect(tolower(ID), 'notraining')) %>%
  mutate(datatype = 'notraining')

Olivia <- alldirs %>%
  filter(str_detect(tolower(ID), 'olivia')) %>%
  mutate(datatype = 'Dummytesting')

dt <- alldirs %>%
  filter(str_detect(tolower(ID), 'dummytesting')) %>%
  mutate(datatype = 'Dummytesting')

alldirs <- rbind(Olivia, dt, childdata, notr)

alldirs <- alldirs %>%
  mutate(ID = str_remove(ID, './')) #%>%
  #filter(!(ID %in% c('2018-07-10_ChildPilot7x4', '2018-07-10_ChildPilot8'))) #Remove bc their multi-defined names screw things up! 

###############################
# LOAD DATA
###############################

TimestampData <- data.frame(NULL)
DatData <- data.frame(NULL)

for(myID in alldirs$ID){
  
  myDatFile <- paste(dataDir, '/', myID, '/data_MPP3ET_',myID, '.dat', sep='')
  myTimestampFile <- paste(dataDir, '/', myID, '/timestamps_MPP3ET_', myID,'.csv',sep='')

  
  if(file.exists(myDatFile) & file.exists(myTimestampFile)){ #IF BOTH FILES EXIST
    print(paste('found files for ', myID))
    myDatData <- read.csv(myDatFile, stringsAsFactors = FALSE)
      
    myTimestampData <- read.csv(myTimestampFile, stringsAsFactors = FALSE)
    myTimestampData$dataType <- ifelse(str_detect(tolower(myID), '2018-07-23'),'childNoQT', 
                                ifelse(str_detect(tolower(myID), '2018-07-25'),'childNoQT', 
                                ifelse(str_detect(tolower(myID), 'child'),'child', 
                                ifelse(str_detect(tolower(myID), 'noqt'),'noQT',
                                ifelse(str_detect(tolower(myID), 'notraining'),'notraining','OTHER')))))
    #Add everything to the big DFs!
    if (nrow(TimestampData) == 0){ #special case for 1st round
      TimestampData = myTimestampData
      DatData = myDatData
    } else {
      TimestampData = bind_rows(TimestampData, myTimestampData)
      DatData = bind_rows(DatData, myDatData)
    }
  }
  else{print(paste(myID, " not added, file not found"))}
}

setwd(analysisDir)

TimestampData <- TimestampData %>%
  mutate(point_description = ifelse(str_count(point_description, "Practice"),
                                    point_description,
                                    paste("Main_", point_description, sep="")))%>%
  mutate(subjectID = str_to_lower(subjectID))%>%
  separate(point_description, c("phaseTimestamp", "trialNo", "description"), extra = 'merge', remove = FALSE)

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

#Clean DatData
DatData <- DatData %>%
  select(c("SubjectNo","Date","Time","VerbDomain","Condition","trialNo","itemID" ,"verbName","verbMeaning",
           "mannerSideBias", "pathSideBias","mannerSideTest", "pathSideTest")) %>%
  mutate_if(is.factor, as.character) %>%
  mutate(subjectID = str_to_lower(SubjectNo)) %>%
  mutate(itemID = as.character(itemID))%>%
  select(-SubjectNo) %>%
  mutate(ExperimentPhase = 'Main') %>%
  mutate(targetSideBias = ifelse(Condition == 'Manner', mannerSideBias,pathSideBias)) %>%
  mutate(targetSideTest = ifelse(Condition == 'Manner', mannerSideTest, pathSideTest)) %>%
  mutate(trialNo = ifelse(ExperimentPhase == 'Main', as.numeric(trialNo), as.numeric(trialNo)-100))

#Merge them!
AllData <- merge(TimestampData, DatData, by=c("subjectID", "trialNo"), all.x=TRUE, all.y=TRUE)


#Filter to suspected troublesome segments
AllData <- AllData %>%
  filter(description %in% c('SameVerbTest_compareVideo1_start', 
                            'Bias_compareVideo1_start', 
                            'SameVerbTest_compareVideo2_start', 
                            'Bias_compareVideo2_start',
                            'Training_1_video',
                            'Training_2_video',
                            'Training_3_video',
                            'Training_4_video')) %>%
  filter(ExperimentPhase == 'Main')%>%
  filter(phaseTimestamp == 'Main')%>%
  mutate(segType = ifelse(str_detect(description, 'Training'), "Training",
                          ifelse(str_detect(description, 'Bias'), "Bias","SameVerb")))

#Look at all segment types
ggplot(data = AllData, aes(x=segment_length_in_sec, fill=Condition)) +
  geom_histogram(binwidth = 0.1) +
  facet_wrap(~Condition*segType, nrow=2)
#Conclusion: Huh - in both the testing phases and the training segments, manner videos tend to lag, path tends
#to be both shorter and peakier (ie correct)

#Look at each participant group - did doing just the biastests or removing QT solve the problem? 
ggplot(data = AllData, aes(x=segment_length_in_sec, fill=Condition)) +
  geom_histogram(binwidth = 0.1) +
  facet_wrap(~dataType*segType, ncol=3, scales="free_y")

