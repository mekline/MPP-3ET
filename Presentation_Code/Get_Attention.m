function Get_Attention()
%Wraps up the command line info & code for laughing baby
global RESOURCEFOLDER;

    recenter_movie = strcat(RESOURCEFOLDER, '/movies/babylaugh.mov');
    disp('Press space to advance once participant is oriented to the screen');
    PlayCenterMovie(recenter_movie, 'ownsound', 1);
    Show_Blank; 