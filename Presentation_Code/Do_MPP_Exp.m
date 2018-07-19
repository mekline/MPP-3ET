
function Do_MPP_Exp()

%We are adapting MPP to the eyetracker, very exciting!
%MPP specific objects
global expStart expTime CONDITION TOEXTEND EXTENDCONDITION
global MAIN_ITEMS EXT_ITEMS ntrials timeCell RESOURCEFOLDER

%Some numeric versions of condition names for indexing into tables...

switch CONDITION
    case 'Manner'
        conditionno = 1;
    case 'Path'
        conditionno = 3;
    case 'Action'
        conditionno = 6;
    case 'Effect'
        conditionno = 5; %(yes #s reversed between domains for now)
end

switch TOEXTEND
    case 'NoExtend'
        toExtend = 0;
    otherwise
        toExtend = 1;
end

% switch SET
%     case 'Set1'
%         Set = 1;
%     case 'Set2'
%         Set = 2;
% end
       
        
        

try
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%%%%%%%%%%%%%%%%%
    %READ IN AND RANDOMIZE ALL FILES FOR THE EXPERIMENT!
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    vidInfo = readtable('MPP_videos.csv');
    MAIN_ITEMS = vidInfo(vidInfo.List == conditionno,:);
    if toExtend %Assign the other-domain set! Since we don't do any learning, arbitrarily get Manner or Action
        if(conditionno == 1 || conditionno == 3) %Start with MannerPath, move to Action
            EXTENDCONDITION = 'Action';
            EXT_ITEMS = vidInfo(vidInfo.List == 6,:);
        elseif (conditionno == 5 || conditionno == 6) %Start with ActionEffect, move to Manner
            EXTENDCONDITION = 'Manner';
            EXT_ITEMS = vidInfo(vidInfo.List == 1,:);
        end
    end
    
%     %Randomize the order of the items, but dividing into 2 sets
%     %corresponding to (counterbalanced) at-home practice
%     VERB1 = vidInfo(strcmp(vidInfo.verbName,'glipping'),:);
%     VERB2 = vidInfo(strcmp(vidInfo.verbName,'krading'),:);
%     VERB3 = vidInfo(strcmp(vidInfo.verbName,'torging'),:);
%     VERB4 = vidInfo(strcmp(vidInfo.verbName,'birking'),:);
%     MAIN_ITEMS1 = vertcat(VERB1, VERB2, VERB3, VERB4);
%     MAIN_ITEMS1 = MAIN_ITEMS1(MAIN_ITEMS1.List == conditionno,:);
%     
%     VERB5 = vidInfo(strcmp(vidInfo.verbName,'zarking'),:);
%     VERB6 = vidInfo(strcmp(vidInfo.verbName,'dacking'),:);
%     VERB7 = vidInfo(strcmp(vidInfo.verbName,'pimming'),:);
%     VERB8 = vidInfo(strcmp(vidInfo.verbName,'molking'),:);
%     MAIN_ITEMS2 = vertcat(VERB5, VERB6, VERB7, VERB8);
%     MAIN_ITEMS2 = MAIN_ITEMS2(MAIN_ITEMS2.List == conditionno,:);
%     
%             
%     MAIN_ITEMS1 = MAIN_ITEMS1(randperm(height(MAIN_ITEMS1)), :);
%     MAIN_ITEMS2 = MAIN_ITEMS2(randperm(height(MAIN_ITEMS2)), :);
%     
%     if Set == 1
%         MAIN_ITEMS = vertcat(MAIN_ITEMS1, MAIN_ITEMS2);
%     elseif Set == 2
%         MAIN_ITEMS = vertcat(MAIN_ITEMS2, MAIN_ITEMS1);
%     end
  
    %Permute item order randomly
    MAIN_ITEMS = MAIN_ITEMS(randperm(height(MAIN_ITEMS)), :);
    if toExtend
        EXT_ITEMS = EXT_ITEMS(randperm(height(EXT_ITEMS)), :);
    end
    
    
    
    %Randomize the side presentation of the items
    %(Counterbalancing note: left video always plays first, assignment is
    %to play either the M or P video first.  Remember that we use
    %manner/path as the example case, you might be showing action/effect
    %really...
    
    %verbose code for readable output!
   
    screenside = ['LR';'LR';'LR';'LR';'RL';'RL';'RL';'RL'];
    
    screenside = screenside(randperm(8),:);
    MAIN_ITEMS.BiasManner = screenside(:,1);
    MAIN_ITEMS.BiasPath = screenside(:,2);
    
    screenside = screenside(randperm(8),:);
    MAIN_ITEMS.TestManner = screenside(:,1);
    MAIN_ITEMS.TestPath = screenside(:,2);
    
    if toExtend
        screenside = screenside(randperm(8),:);
        EXT_ITEMS.BiasManner = screenside(:,1);
        EXT_ITEMS.BiasPath = screenside(:,2);
        
        screenside = screenside(randperm(8),:);
        EXT_ITEMS.TestManner = screenside(:,1);
        EXT_ITEMS.TestPath = screenside(:,2);
    end
    
    %%%%%%%%%%%%%%%%%%%%%
    % EXPERIMENT STARTS HERE
    %%%%%%%%%%%%%%%%%%%%%
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %Save a header file to the data file so it will be easier to read!
   % WriteToDataFile({'header','asdf','putcolumnnameshere', 12}); %use curly braces, ANYthing could be in there!
    %Since MK is someimes confused: WriteToDataFile is a common resource
    %function!  Below, we use WriteMPPTrial.m for common format!
    
    WriteToDataFile({'SubjectNo',...
        'Date',...
        'Time',...
        'VerbDomain',...
        'Condition',...
        'trialNo',...
        'itemID',...
        'verbName',...
        'verbMeaning',...
        'mannerSideBias',...
        'pathSideBias',...
        'mannerSideTest',...
        'pathSideTest',...
        'totalTime',...
        'xxxxxxxxx',...
        'expStartTime',...
        'trainingStartTime',...
        'trainingEndTime',...
        'finalTestStart',...
        'finalTestEnd',...
        'ambigVid',...
        'mBiasVid',...
        'pBiasVid',...
        'trainVid1',...
        'trainVid2',...
        'trainVid3',...
        'mTestVid',...
        'pTestVid',...
        'ambigAudioFuture',...
        'ambigAudioPast',...
        'trainAudioFuture1',...
        'trainAudioPast1',...
        'trainAudioFuture2',...
        'trainAudioPast2',...
        'trainAudioFuture3',...
        'trainAudioPast3',...
        'whichOneAudio',...
        'letsFindAudio'});
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    expStart = GetSecs;
    disp(expStart);
    
    timeCell = {'subjectID', 'system_time_stamp', 'point_description'};
    timeCell(end+1,:) = timeStamp('Experiment_Start');

    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % 2 TRIALS OF PRACTICE TRAINING
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
    for i=1:1 %SHORTER VERSION, PILOT 3/19 
    %for i=1:2
         Trial_Practice(i);
         display(i)
     end
  
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % N TRIALS OF WITHIN-FIELD PRIMING or VERB LEARNING
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    %How many trials?
    ntrials = height(MAIN_ITEMS); 

%     All WithBias trials! (6/9)
    for i=1:ntrials
        disp(i)
        disp(MAIN_ITEMS.verbName(i))
        Trial_Omnibus(i,'WithBias');
        expTrial = GetSecs;
        expTime = expTrial - expStart;       
        Write_MPP_Trial(i, MAIN_ITEMS);
    end
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % N TRIALS OF CROSS-FIELD TESTING
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    if strcmp(TOEXTEND,'Extend')
        for i=(ntrials+1):(2*ntrials)
            Trial_Omnibus(i, 'BiasOnly');
            expTrial = GetSecs;
            expTime = expTrial - expStart;
            Write_MPP_Trial(i, EXT_ITEMS);
        end
    end
    

    %Experiment ended cleanly! Cleanup and shut down
    Closeout_PTool();
        

catch 
    Closeout_PTool();
    psychrethrow(psychlasterror);
    
end

end

     