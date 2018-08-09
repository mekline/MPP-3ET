function cellObj = timeStamp(label)
%A convenience function to save MPP timestamps the way I want! (could be 
%added to the Tobii-ptb toolbox...)

global TOBII SUBJECT USE_EYETRACKER

disp(label);

if USE_EYETRACKER
    cellObj = {SUBJECT, TOBII.get_system_time_stamp, label}; 
else
    mytime = GetSecs*1000000;
    cellObj = {SUBJECT, mytime, label}; 
end


end