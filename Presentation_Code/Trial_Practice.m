function [response] = Trial_Practice(trialNo)

%%%%%%%%%%%%%%%%%%%%%%%%
%PRELIMINARIES
%%%%%%%%%%%%%%%%%%%%%%%%

global WINDOW_PARAMS RESOURCEFOLDER TOBII EYETRACKER USE_EYETRACKER DATAFOLDER EXPERIMENT SUBJECT timeCell

%To start, play a fun sound! (It takes a bit for the trial to start and
%the kiddos get bored...)
bells = strcat(RESOURCEFOLDER, '/audio_mpp2ET/general/bells.wav');
Play_Sound(bells,0);
    
if (trialNo == 1)
    targetword = 'ball';
elseif (trialNo == 2)
    targetword = 'book';
end

soundtoplay_letsWatchMore = strcat(RESOURCEFOLDER, '/audio/aa_motivation/letswatchmore.wav');
movietoplay_practice_a = strcat(RESOURCEFOLDER,'/Movies_Practice/practice_',num2str(trialNo),'a.mp4');
movietoplay_practice_b = strcat(RESOURCEFOLDER,'/Movies_Practice/practice_',num2str(trialNo),'b.mp4');
movietoplay_practice_c = strcat(RESOURCEFOLDER,'/Movies_Practice/practice_',num2str(trialNo),'c.mp4');
movietoplay_practice_d = strcat(RESOURCEFOLDER,'/Movies_Practice/practice_',num2str(trialNo),'d.mp4');
movietoplay_practice_e = strcat(RESOURCEFOLDER,'/Movies_Practice/practice_',num2str(trialNo),'e.mp4');
movietoplay_practice_distr = strcat(RESOURCEFOLDER,'/Movies_Practice/practice_',num2str(trialNo),'_distr.mp4');       

%%%%%%%%%%%%%%%%%%%%%%%%
%GET READY 
%%%%%%%%%%%%%%%%%%%%%%%%

Show_Blank;  
Get_Attention();
        
%%%%%%%%%%%%%%%%%%%%%%%%
%FOUR CENTERED PRACTICE VIDEOS     
%%%%%%%%%%%%%%%%%%%%%%%%
        
if USE_EYETRACKER
    GazeData = EYETRACKER.get_gaze_data; %dummy call to make sure we clear & collect new data
end

%Make array of stuff to show 
T = table({movietoplay_practice_a; movietoplay_practice_b; movietoplay_practice_c; movietoplay_practice_d},...
    'VariableNames',{'Movie'});

for i=1:4      
            
    timeCell(end+1,:) = timeStamp(['Practice_', num2str(trialNo), '_Training_', num2str(i), '_audio']);         
    Play_Sound(strcat(RESOURCEFOLDER, '/audio/aa_nouns/',targetword,'1.wav'), 'toBlock');                
    Show_Blank;

    timeCell(end+1,:) = timeStamp(['Practice_', num2str(trialNo), '_Training_', num2str(i), '_video']);
    PlayCenterMovie(T.Movie{i}, 'ownsound', 1, 'border', 1);
    WaitSecs(0.50);

    Show_Blank;

    if USE_EYETRACKER
        GazeData1 = EYETRACKER.get_gaze_data; 
        FullGazeData = horzcat(GazeData, GazeData1); %Concatenate to save gaze data in all one big file
    end
end        

        

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%LEARNING TEST
%%%%%%%%%%%%%%%%%%%%%%%%%%%
Show_Blank;
timeCell(end+1,:) = timeStamp(['Practice_', num2str(trialNo), '_SameVerbTest_audio']); 
Play_Sound(strcat(RESOURCEFOLDER, '/audio/aa_nouns/',targetword,'2.wav'), 'toBlock');                

%%%
%LEFT
timeCell(end+1,:) = timeStamp(['Practice_', num2str(trialNo), '_SameVerbTest_left_video']); 

if trialNo == 1
    PlaySideMovies(movietoplay_practice_distr,'','border', 1);
elseif trialNo == 2
    PlaySideMovies(movietoplay_practice_e,'','border', 1);
end

WaitSecs(0.50);
Show_Blank;

if USE_EYETRACKER
    GazeData1 = EYETRACKER.get_gaze_data; 
    FullGazeData = horzcat(GazeData, GazeData1); %Concatenate to save gaze data in all one big file
end

%%%
%RIGHT
timeCell(end+1,:) = timeStamp(['Practice_', num2str(trialNo), '_SameVerbTest_right_video']); 

if trialNo == 1
    PlaySideMovies('', movietoplay_practice_e,'border', 1);
elseif trialNo == 2
    PlaySideMovies('',movietoplay_practice_distr,'border', 1);
end

WaitSecs(0.50);
Show_Blank;

if USE_EYETRACKER
    GazeData1 = EYETRACKER.get_gaze_data; 
    FullGazeData = horzcat(GazeData, GazeData1); %Concatenate to save gaze data in all one big file
end

%%%
% COMPARE 1
Play_Sound(strcat(RESOURCEFOLDER, '/audio/aa_nouns/',targetword,'2.wav'), 0);  
timeCell(end+1,:) = timeStamp(['Practice_', num2str(trialNo) '_SameVerbTest_compareVideo1_start']); 
if trialNo == 1
    PlaySideMovies(movietoplay_practice_distr, movietoplay_practice_e,'border', 1);
elseif trialNo== 2
    PlaySideMovies(movietoplay_practice_e,movietoplay_practice_distr,'border', 1);
end

timeCell(end+1,:) = timeStamp(['Practice_',  num2str(trialNo) '_SameVerbTest_compareVideo1_still']);
WaitSecs(3);
Show_Blank;
timeCell(end+1,:) = timeStamp(['Practice_',  num2str(trialNo) '_SameVerbTest_compareVideo1_end']);

if USE_EYETRACKER
    GazeData1 = EYETRACKER.get_gaze_data; 
    FullGazeData = horzcat(GazeData, GazeData1); %Concatenate to save gaze data in all one big file
end

%%%
% COMPARE 2
Play_Sound(strcat(RESOURCEFOLDER, '/audio/aa_nouns/',targetword,'2.wav'), 0);  
timeCell(end+1,:) = timeStamp(['Practice_', num2str(trialNo) '_SameVerbTest_compareVideo2_start']); 
if trialNo == 1
    PlaySideMovies(movietoplay_practice_distr, movietoplay_practice_e, 'border', 1);
elseif trialNo== 2
    PlaySideMovies(movietoplay_practice_e,movietoplay_practice_distr, 'border', 1);
end

timeCell(end+1,:) = timeStamp(['Practice_',  num2str(trialNo) '_SameVerbTest_compareVideo2_still']);
WaitSecs(3);
Show_Blank;
timeCell(end+1,:) = timeStamp(['Practice_',  num2str(trialNo) '_SameVerbTest_compareVideo2_end']);

if USE_EYETRACKER
    GazeData1 = EYETRACKER.get_gaze_data; 
    FullGazeData = horzcat(GazeData, GazeData1); %Concatenate to save gaze data in all one big file
end      
              
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SAVE THE DATA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Interesting noises & pix for non-boringness
Play_Sound(bells,0);
Show_Image([RESOURCEFOLDER '/rewardpix/cuteanimals.0' num2str(trialNo + 16) '.jpeg']);
%Save gaze trial data as MAT, and add to the big CSV
description = ['All_of_Practice_trial_' num2str(trialNo)]; %description of this timeperiod

if USE_EYETRACKER
    save([DATAFOLDER, '/gaze_' EXPERIMENT '_' SUBJECT '_' description '.mat'], 'GazeData');
    SaveGazeData(FullGazeData, description);
end

%saving timestamps
timeTable = cell2table(timeCell(2:end,:));
timeTable.Properties.VariableNames = timeCell(1,:);
%And save the file!
filename = [DATAFOLDER, '/timestamps_' EXPERIMENT '_' SUBJECT '.csv'];
writetable(timeTable, filename);
    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
% END PRACTICE TRAINING                             
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    