# DANGER DANGER DANGER

As of 7/18/18, the presentation script contains an unknown bug that causes the video presentation to execute at slightly different speeds for the two conditions (!!!!). Remove this note when the problem is fixed. 

# MPP-Eyetracker

This version of MPP has been refactored so that common code for psychtoolbox and the Tobii are in a shared library with other extensions (like MPP-Concepts!). 

The necessary libraries (to put in the RESOURCEFOLDER path) are at:

PTB-Helper: https://github.com/mekline/PTB_HelperFuns

Tobii-PsychToolBox: https://github.com/mekline/Tobii-PsychToolBox

Also put the audio, audio_MPP2ET, & movies folders in RESOURCEFOLDER

## Testing Notes

Remember not to commit exact ages into this repository: enter PPI via the [Subject Form](https://goo.gl/forms/7zJcjGcmWtXX77ku2) (link won't work unless you are signed in to the klinelab account), and use [SaltyDates](https://mekline.shinyapps.io/saltydates/) to get approx values to enter in the CSV. 
