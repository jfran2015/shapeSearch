%function shapeSearch(subNum, runNum)
subNum = 55; %tk remove and uncomment function call
runNum = 3; %tk remove and uncomment function call
%-----------------------------------------------------------------------
% Script: shapeSearch.m
% Author: Justin Frandsen
% Date: 07/12/2023
% Description: Matlab script that presents experiment exploring how
%              learning influences attention involving scene synatx.
%
% Additional Comments:
% - Must be in the correct folder to run script.
% - Make sure eyetracker is powered on and connected.
% - Make sure shape_location_type.mat & shape_position.mat are saved and
%   ready
%
% Usage:
% - type function name with subject number and run number (seperated by a
%   comma in parentheses).(e.g., shapeSearch(222, 1)).
% - Script will output a .csv file containing behavioral data, a .mat file
%   containing all matlab script variables, and a .edf file containing
%   eyetracking data.
%-----------------------------------------------------------------------

% =========================================================================
% =============== Settings! ===============================================
% =========================================================================
% Global Variables
bxFileFormat            = 'sceneShapeSearchSub%.3dRun%.2d.csv';
eyeFileFormat           = 'S%.3dR%.1d.edf';
bxOutputFolder          = 'output/bxData';
eyetrackingOutputFolder = 'output/eyeData';
imageFolder             = 'scenes';
nonsidedShapes          = 'Stimuli/transparent_black';
shapesTLeft             = 'Stimuli/Black_Left_T';
shapesTRight            = 'Stimuli/Black_Right_T';

% Task variables
trialsPerRun            = 60;% must be a multiple of 4
totalTargets            = 4;
totalDistractors        = 18;
stimuliSizeRect         = [0, 0, 240, 240]; %This rect contains the size of the shapes that are presented

% PTB Settings
WinNum                  = 0;

% Eyelink settings
dummymode               = 1; %set 0 if using eyetracking, set 1 if not eyetracking (will use mouse position instead)

% =========================================================================
% =============== Start of code! ==========================================
% =========================================================================

% Create output file names
bxFileName = sprintf(bxFileFormat, subNum, runNum);  %name of bx(behavioral) file
edfFileName = sprintf(eyeFileFormat, subNum, runNum); %name of the edf(eyetracking) file

% Test if bx output file already exists
bxOutputFileList = dir(fullfile(bxOutputFolder, '*.csv'));
for j = 1:length(bxOutputFileList)
    existing_file_name = bxOutputFileList(j).name;
    if existing_file_name == bxFileName
        error('Suject bx file already exists. If you want to run again with the same subject number you will need to delete the corresponding output file.');
    end
end

% Test if .edf file already exists
eyeOutputFileList = dir(fullfile(eyetrackingOutputFolder, '*.csv'));
for j = 1:length(eyeOutputFileList)
    existing_file_name = eyeOutputFileList(j).name;
    if existing_file_name == bxFileName
        error('Suject eyetracking file already exists. If you want to run again with the same subject number you will need to delete the corresponding output file.');
    end
end

ClockRandSeed;

% Initilize PTB window
[w, rect] = pfp_ptb_init; %call this function which contains all the screen initilization.
[width, height] = Screen('WindowSize', WinNum); %get the width and height of the screen
Screen('BlendFunction', w, 'GL_SRC_ALPHA', 'GL_ONE_MINUS_SRC_ALPHA'); %allows the .png files to be transparent

%set font, size, and style for w
old_font = Screen('TextFont', w, 'Arial');
old_size = Screen('TextSize', w, 35);
old_style = Screen('TextStyle', w, 1);

%create central fixation location
winfixsize = 50;
winfix = [-winfixsize -winfixsize, winfixsize, winfixsize];
winfix = CenterRect(winfix, rect);

% Load in images
DrawFormattedText(w, 'Loading Images...', 'center', 'center');
Screen('Flip', w);

% Load all .jpg files in the scenes folder.
[allScenesFilePaths, allScenesTextures] = imageStimuliImport(imageFolder, '*.jpg', w);

% Load in shape stimuli
[sortedNonsidedShapesFilePaths, sortedNonsidedShapesTextures] = imageStimuliImport(nonsidedShapes, '*.png', w, true);
[sortedLeftShapesFilePaths, sortedLeftShapesTextures] = imageStimuliImport(shapesTLeft, '*.png', w, true);
[sortedRightShapesFilePaths, sortedRightShapesTextures] = imageStimuliImport(shapesTRight, '*.png', w, true);

% =========================================================================
% =============== Initialize the eyetracker! ==============================
% =========================================================================
if dummymode == 0
    el=EyelinkInitDefaults(w); %starts with EyeLink default settings
    
    el.backgroundcolour = GrayIndex(el.window); %background color of calibration display
    el.msgfontcolour  = WhiteIndex(el.window); %font color for calibration display
    el.imgtitlecolour = WhiteIndex(el.window); %tile color for calibration display jf what is tile color
    el.targetbeep = 0; %doesn't beep after each target when calibrating
    el.calibrationtargetcolour = WhiteIndex(el.window); %color of circle/target used in calibration display
    
    %determines the size of the circle/target used for calibration
    el.calibrationtargetsize = 2;
    el.calibrationtargetwidth = 0.75;
    
    EyelinkUpdateDefaults(el); %update EyeLink settings based on what you just defined above
    
    %the following does some checks and aborts if something isn't right
    %with the EyeLink-computer connection
    if ~EyelinkInit(dummymode)
        fprintf('Eyelink Init aborted.\n');
        Eyelink('Shutdown');
        Screen('CloseAll')
        return;
    end
    
    i = Eyelink('Openfile', edfFileName);
    if i~=0
        fprintf('Cannot create EDF file ''%s'' ', edfFileName);
        Eyelink('Shutdown');
        Screen('CloseAll')
        return;
    end
    
    if Eyelink('IsConnected')~=1 && ~dummymode
        Eyelink('Shutdown');
        Screen('CloseAll')
        return;
    end
    
    %these add some info for logging in the EDF file
    Eyelink('command', 'add_file_preamble_text ''Recorded by EyelinkToolbox demo-experiment'''); %this is old and probably doesn't need to be here :-)
    Eyelink('command','screen_pixel_coords = %ld %ld %ld %ld', 0, 0, width-1, height-1);
    Eyelink('message', 'DISPLAY_COORDS %ld %ld %ld %ld', 0, 0, width-1, height-1);
    
    %sets up some more EyeLink settings
    Eyelink('command', 'calibration_type = HV9'); %9-point calibration
    Eyelink('command', 'generate_default_targets = YES'); %use default targets for calibration (you just defined what default is above)
    %Eyelink('Command', 'calibration_area_proportion .85 .85'); %calibrate and 85% of the screen extent (the circles/targets only go 85% of the way out from center -- you only need to calibrate the useful extent of the monitor)
    %Eyelink('Command', 'validation_area_proportion  .85 .85'); %validate at 85% of the screen extend (see above) ^^^^^^^
    Eyelink('command', 'saccade_velocity_threshold = 35'); %threshold for computing/defining saccades in the EDF file (only effects what the EDF file logs)
    Eyelink('command', 'saccade_acceleration_threshold = 9500'); %threshold for computing/defining saccades in the EDF file (only effects what the EDF file logs)
    
    % set EDF file contents using the file_sample_data and
    % file-event_filter commands
    % set link data thtough link_sample_data and link_event_filter
    Eyelink('command', 'file_event_filter = LEFT,RIGHT,FIXATION,SACCADE,BLINK,MESSAGE,BUTTON,INPUT');
    %jf this code links the data so we could use it online like gaze contingient paradigms (I am currently unsure if I will use this);
    Eyelink('command', 'link_event_filter = LEFT,RIGHT,FIXATION,SACCADE,BLINK,MESSAGE,BUTTON,INPUT');
    
    % check the software version
    % add "HTARGET" to record possible target data for EyeLink Remote
    Eyelink('command', 'file_sample_data  = LEFT,RIGHT,GAZE,HREF,AREA,HTARGET,GAZERES,STATUS,INPUT');
    Eyelink('command', 'link_sample_data  = LEFT,RIGHT,GAZE,GAZERES,AREA,HTARGET,STATUS,INPUT');
    
    
    %some more basic settings
    [v,vs] = Eyelink('GetTrackerVersion');
    fprintf('Running experiment on a ''%s'' tracker.\n', vs );
    vsn = regexp(vs,'\d','match');
    Eyelink('command', 'button_function 5 "accept_target_fixation"');
    
    EyelinkDoTrackerSetup(el); %apply the above settings
    
    % do a final check of calibration using driftcorrection
    %EyelinkDoDriftCorrection(el);
end

%load
shapeLocationTypes = load('shape_location_types.mat');
shapePositions = load('shape_positions.mat');

%load variables for where the shapes are located and what postion theyre in
randomizor = fullRandomizor(trialsPerRun, allScenesTextures, sortedNonsidedShapesTextures, totalTargets);

if isfield(randomizor, 'randomizor')
    randomizor = randomizor.randomizor;
end

this_subj_this_run = randomizor.(sprintf('subj%d', subNum)).(sprintf('run%d', runNum)); %method of getting into the struct

% =========================================================================
% =============== Beginning of task! ======================================
% =========================================================================
%variables that will be saved out
rtAll = zeros(1, trialsPerRun);
responses = cell(1, trialsPerRun);
tDirectionTarget = cell(1, trialsPerRun);
accuracy = zeros(1, trialsPerRun);
fileName = cell(1, trialsPerRun);
thisRunTrialNumbers = 1:trialsPerRun;
subNumForOutput(1:trialsPerRun) = subNum;

allTrialsFixationMatrix = [];

% Create a cell array to store eye movement data for each trial
eyeMovementData = cell(1, trialsPerRun);

%-------------------Instructions----------------------------------------
DrawFormattedText(w, 'For a <sideways T with bar on left> press z\n and for a <sideways T with bar on right> press /', 'center', 'center')
Screen('Flip', w);

%this look waits until the spacebar is pressed to continue
start = 0;
while start==0
    [key_time,key_code]=KbWait([], 2);
    resp = find(key_code);
    if resp(1) == KbName('SPACE') || resp(1) == KbName('space')
        start = 1;
    end
end

if dummymode==0 %if you are actually eye tracking (and not using mouse position, which you might do if just testing the script)
    Eyelink('Command', 'set_idle_mode'); %eye tracker will go idle and wait
    WaitSecs(0.1);
    
    Eyelink('Command', 'driftcorrect_cr_disable = OFF'); %allow online drift correction on tracker computer
    Eyelink('Command', 'normal_click_dcorr = ON'); %use the normal click method to activate and apply drift correction
    Eyelink('Command', 'online_dcorr_maxangle = 5.0'); %only allow drift correction when the different between measured and corrected eye position is 5 degrees visual angle or less
    
    % tk I just changed this. I'm hoping this displays the fixation point
    % on the screen but I'm not sure how it will work later on with the image sent to the eyetracker
    Eyelink('command', 'clear_screen %d', 0);
    Eyelink('command', 'draw_box %d %d %d %d 15', winfix(1),winfix(2),winfix(3),winfix(4));
    
    
    eye_used = Eyelink('EyeAvailable'); %I think this just determines, for online reading of eye position, which eye to use if you are recording from both at the same time. We only ever record from one at a time so it tends to never be relevant
    if eye_used == 2
        eye_used = 1;
    end
end

if dummymode == 0
    Eyelink('Command', 'set_idle_mode'); %eye tracker will go idle and wait
    WaitSecs(0.05);
    
    Eyelink('StartRecording'); % Start recording eye data
    WaitSecs(0.1); % Allow a brief moment for the tracker to start recording
end

if dummymode == 1 %if you are instead not measuring eye position and using the mouse like eye position (usually to test/debug a script)
    ShowCursor(['Arrow']); %show mouse position as an arrowhead (which you'll need to be able to see in order to simulate eye position by moving it with the mouse)
end

possibleLocations = [1 2 3 4];
allTargets = this_subj_this_run.allTargets;
%targetLocationTypeRandomizor = this_subj_this_run.targetLocationTypeRandomizor;
% =============== Task for loop ===========================================
for trialNum = 1:4 %trialsPerRun
    if dummymode==0
        error=Eyelink('checkrecording');
        if(error~=0)
            break;
        end
        % Send trial identification message to Eyelink
        Eyelink('Message', 'TRIALID %d', trialNum);
        
        % Send trial status message to Eyelink
        Eyelink('command', 'record_status_message "TRIAL %d/%d"', trialNum, trialsPerRun);
    end
    
    % this gets all of the variables from the prerandomized struct for this subject this run into variable that are easier to use.
    sceneInds = this_subj_this_run.cBSceneOrder(trialNum);
    targetInds = this_subj_this_run.cBTargetOrder(trialNum);
    thisTrialIncorrectTargetLocation = this_subj_this_run.cBIncorrectTargetLocation(trialNum);
    thisTrialExtraTarget = this_subj_this_run.cBExtraTargetTrials(trialNum);
    targetPosition = this_subj_this_run.cBOrigionalTargetPosition(trialNum);
    targetChoice = this_subj_this_run.cBTargetChoice(trialNum);
    tDirectionThisTrial = this_subj_this_run.tDirectionAllTrials(trialNum, :);
    thisTrialDistractors = this_subj_this_run.allDistractorsAllTrials(trialNum, :);
    
    oldsize = Screen('TextSize', w, 60); %make the font size a little bigger when drawing the fixation cross
    
    DrawFormattedText(w, '+', 'center', 'center', [255,255,255]); %draws the fixation cross (a plus-sign) at the center of the screen
    
    Screen('Flip', w); %draws fixation cross 1
    
    Screen('DrawTexture', w, sortedNonsidedShapesTextures(targetInds)); %tk change the size later to reflect the true size on the trial
    
    %================= Check for fixation on first cross! =================
    
    
    ReadyToBegin = 0; %set this variable to zero to begin the trial, and it will need to turn to 1 when central fixation is acquired to proceed
    
    while ReadyToBegin == 0
        if dummymode==0 %the following grabs the x and y position of the eye if tracking
            
            %if not recording (because the connection to the tracker
            %was interrupted/broken), break out of the loop
            error=Eyelink('CheckRecording');
            if(error~=0)
                break;
            end
            
            if Eyelink('NewFloatSampleAvailable') > 0 %if a new read on eye position is avaialble
                evt = Eyelink('NewestFloatSample'); %grab the new sample as a variable "evt" This is a structured variable with multiple parts
                evt.gx; %this part of variable evt is the x-position of measured eye position (in pixels)
                evt.gy; %this part of variable evt is the y-position of measured eye position (in pixels)
                if eye_used ~= -1 %if you are actually measusing from the eye
                    x = evt.gx(eye_used+1); %x will now be the measured eye position in the x-dimension
                    y = evt.gy(eye_used+1); %y will now be the measured eye position in the y-dimension
                    if x~=el.MISSING_DATA && y~=el.MISSING_DATA && evt.pa(eye_used+1)>0 %if eyes are not closed (otherwise measured eye position would not be a valid number, which would cause subsequent logical statements using the variable to fail)
                        mx = x; %update variable mx to measured eye position in the x-dimension
                        my = y; %update variable my to measured eye position in the y-dimension
                    end
                end
            end
        else
            [mx, my]=GetMouse(w); %if using the mouse position to simulate eye position, define mx and my in terms of the position of the mouse cursor
        end
        % check if the position obtained is in the fixation window
        fix = mx > winfix(1) &&  mx < winfix(3) && ...
            my > winfix(2) && my < winfix(4); %this logical statement will be true (==1) if measured eye position (mx and my) are inside the box you defined around the fixation cross
        if fix == 1 %if measured eye position (mx and my) is inside the central box around fixation
            fixstart = GetSecs; %define the time that the fixation started
            while fix == 1 %while eye position is still inside the central box (since you later update the variable within the while loop, it can become zero and not be true if eye position later falls outside of the central box)
                %this next part here updates measured eye position as
                %you did before you entered into this while loop. You
                %will keep updating eye position until (a) the time in
                %the box reaches a critical threshold or (b) eye
                %position moves outside of the central box
                if dummymode==0
                    error=Eyelink('CheckRecording');
                    if(error~=0)
                        break;
                    end
                    
                    if Eyelink('NewFloatSampleAvailable') > 0
                        evt = Eyelink('NewestFloatSample');
                        evt.gx;
                        evt.gy;
                        if eye_used ~= -1
                            x = evt.gx(eye_used+1);
                            y = evt.gy(eye_used+1);
                            if x~=el.MISSING_DATA && y~=el.MISSING_DATA && evt.pa(eye_used+1)>0
                                mx = x;
                                my = y;
                            end
                        end
                    end
                else
                    [mx, my]=GetMouse(w);
                end
                
                fix = mx > winfix(1) &&  mx < winfix(3) && ...
                    my > winfix(2) && my < winfix(4); %updates the fix variable that reflects whether measured eye position is inside the central box. If not, this now becomes 0 and will cause the while loop to end when it next loops around
                fixduration = GetSecs - fixstart; %variable reflecting how long it has been since eye position was first measured inside the central fixation (current clock time minus the time at which eye position was first measured inside the central box) window
                if fixduration >= 0.5 %if eye position has been inside the central fixation box for 0.5 seconds
                    ReadyToBegin = 1; %fixation has been acquired and we're ready to move on and show the stimulus array
                    break
                end
            end
        elseif fix == 0
            continue
        end
    end
    
    Screen('Flip', w); %draws what the target will be
    
    if dummymode == 0
        Eyelink('Message', 'Target Presentation');
    end
    
    DrawFormattedText(w, '+', 'center', 'center', [255,255,255]); %draws the fixation cross (a plus-sign) at the center of the screen
    oldsize = Screen('TextSize', w, 40); %change the font size back to 40 after you draw the fixation cross
    WaitSecs(1); %keep target on screen for 1 sec
    Screen('Flip', w); %draws second fixation cross
    
    if dummymode == 0
        Eyelink('Message', 'Fixation Cross');
    end
    
    % Draw background scene
    Screen('DrawTexture', w, allScenesTextures(sceneInds), [], rect); %cue scene to be flipped
    WaitSecs(1); %keep fixation cross on screen for 0.5 sec
    
    %this gives back the indicies that match the targetPosition
    allPositionInds = find(shapeLocationTypes.locationTypes(sceneInds, :) == targetPosition);
    
    %sometimes position inds gives back 2 indicies (because sometimes the scenes has two locations that match). targetChoice is a variable that is
    %random each trial and used to randomly select one of the two inds (both are valid)
    if length(allPositionInds) > 1
        if targetChoice == 1
            positionInds = allPositionInds(1);
        else
            positionInds = allPositionInds(2);
        end
    else
        positionInds = allPositionInds;
    end
    
    %if thisTrialIncorrectTargetLocation == 1 that means its an incorrect trial(a trial where the target appears the the incorrect location).
    if thisTrialIncorrectTargetLocation == 1
        incorrectLocations = setdiff(possibleLocations, allPositionInds); %get inds for all locations where it isn't a correct location
        positionInds = randsample(incorrectLocations, 1);
    end
    
    tDirection = tDirectionThisTrial(positionInds); %this variable is used to determine which direction the T faces
    shapeSizeAndPosition = shapePositions.savedPositions{sceneInds, positionInds};
    if tDirection == 1 %if t direction is 1 the target
        Screen('DrawTexture', w, sortedRightShapesTextures(targetInds), [], shapeSizeAndPosition);
        tDirectionTarget{trialNum} = 'R'; %this variable is used to output which direction the T faced
    elseif tDirection == 2
        Screen('DrawTexture', w, sortedLeftShapesTextures(targetInds), [], shapeSizeAndPosition);
        tDirectionTarget{trialNum} = 'L'; %this variable is used to output which direction the T faced
    end
    
    if thisTrialExtraTarget == 1
        trialInd = randsample(1:3, 1);
        targetInd = randsample(1:3, 1);
        possibleDistractorTargets = setdiff(allTargets(1, :), targetInds);
        distractorTarget = possibleDistractorTargets(targetInd);
        thisTrialDistractors(trialInd) = distractorTarget;
    end
    
    distractorPositions = setdiff(possibleLocations, positionInds);
    for position = 1:length(distractorPositions)
        distractorTDirection = tDirectionThisTrial(distractorPositions(position));
        shapeSizeAndPosition = shapePositions.savedPositions{sceneInds, distractorPositions(position)};
        thisDistractor = thisTrialDistractors(position);
        if distractorTDirection == 1
            Screen('DrawTexture', w, sortedRightShapesTextures(thisDistractor), [], shapeSizeAndPosition);
        elseif distractorTDirection == 2
            Screen('DrawTexture', w, sortedLeftShapesTextures(thisDistractor), [], shapeSizeAndPosition);
        end
    end
    
    stimOnsetTime = Screen('Flip', w); %this flip displays the scene with all four shapes
    
    if dummymode == 0
        Eyelink('Message', 'SYNCTIME');
    end
    
    
    %     current_display = Screen('GetImage', w); %test if this sends the image I want to the eyetracker
    %
    %     imwrite(current_display, 'FileName.png'); %consider including this script for later analysis of the images (I could also program up another script for saving the images quickly later if I want them)
    %
    if dummymode == 0
        %         transferStatus = Eyelink('ImageTransfer', 'FileName.png');
        %         if transferStatus ~= 0
        %             fprintf('*****Image transfer Failed*****-------\n');
        %         end
        
        % Send an integration message so that an image can be loaded as
        % overlay backgound when performing Data Viewer analysis.  This
        % message can be placed anywhere within the scope of a trial (i.e.,
        % after the 'TRIALID' message and before 'TRIAL_RESULT')
        % See "Protocol for EyeLink Data to Viewer Integration -> Image
        % Commands" section of the EyeLink Data Viewer User Manual.
        Eyelink('Message', 'Scene Presentation: %s', allScenesFilePaths(sceneInds));
        
    end
    
    %response
    response = 'nan'; % sets response to nan, so that if no response is given it's listed as a nan
    RT = NaN; % sets rt to nan, so that if no response is given it's listed as a nan
    startTime = GetSecs(); % Gets the time when this line was run
    
    fixationMatrix = [];  % Initialize an empty fixation matrix
    fixationStartTime = 0; % Initialize the start time of the current fixation
    isFixating = false;  % Initialize isFixating here
    previousFixationRect = 0; % Initialize the previously fixated rectangle
    currentFixationRect = 0; % Initialize the currently fixated rectangle
    fixationTimeThreshold = 100;
    
    while GetSecs() - startTime <= 15 %15 represents seconds.
        [key_is_down, secs, key_code] = KbCheck;
        if key_is_down
            responseKey = KbName(key_code);
            if strcmp(responseKey, 'z') || strcmp(responseKey, '/?') %checks to see if the response key is z or /. If not it keeps looping
                response = responseKey;
                RT = round((secs - stimOnsetTime) * 1000);
                
                if dummymode == 0
                    Eyelink('Message', 'Key pressed');
                end
                
                break;
            end
        end
        
        if dummymode==0
            error=Eyelink('CheckRecording');
            if(error~=0)
                break;
            end
            if Eyelink('NewFloatSampleAvailable') > 0
                evt = Eyelink('NewestFloatSample');
                evt.gx;
                evt.gy;
                if eye_used ~= -1
                    x = evt.gx(eye_used+1);
                    y = evt.gy(eye_used+1);
                    if x~=el.MISSING_DATA && y~=el.MISSING_DATA && evt.pa(eye_used+1)>0
                        mx = x;
                        my = y;
                    end
                end
            end
        else
            [mx, my]=GetMouse(w); %get mouse position to replace the eyetracking during dummymode
        end
        
        % Check if the current gaze position is within any of the interest areas
        
        for interestArea = 1:4
            if IsInRect(mx, my, shapePositions.savedPositions{sceneInds, interestArea})
                currentFixationRect = interestArea;
                break;  % Exit loop if a valid interest area is found
            else
                currentFixationRect = 0; % No interest area fixated
            end
        end
        
        % Handle fixation state transitions
        if currentFixationRect ~= previousFixationRect
            if isFixating
                fixationEndTime = GetSecs();
                fixationDuration = (fixationEndTime - fixationStartTime)*1000;
                fixationMatrix = [fixationMatrix; fixationDuration, previousFixationRect, trialNum, thisTrialExtraTarget, thisTrialIncorrectTargetLocation];
            end
            
            fixationStartTime = GetSecs();
            isFixating = true;
            previousFixationRect = currentFixationRect;
        elseif isFixating
            % Check if the fixation time in the current interest area is over
            fixationEndTime = GetSecs();
            fixationDuration = (fixationEndTime - fixationStartTime)*1000;
            if fixationDuration >= fixationTimeThreshold
                fixationMatrix = [fixationMatrix; fixationDuration, previousFixationRect, trialNum, thisTrialExtraTarget, thisTrialIncorrectTargetLocation];
                isFixating = false;
            end
        end
        
    end
    
    % Outside the loop, handle the last fixation if it's ongoing
    if isFixating
        fixationEndTime = GetSecs();
        fixationDuration = (fixationEndTime - fixationStartTime)*1000;
        fixationMatrix = [fixationMatrix; fixationDuration, previousFixationRect, trialNum, thisTrialExtraTarget, thisTrialIncorrectTargetLocation];
    end
    
   
    
    
    if strcmp(response, 'nan')
        textToShow = 'Too slow!';
        accuracy(trialNum) = 2;
    elseif strcmp(response, '/?') && strcmp(tDirectionTarget{trialNum}, 'R')
        textToShow = 'Correct!';
        accuracy(trialNum) = 1;
    elseif strcmp(response, 'z') && strcmp(tDirectionTarget{trialNum}, 'L')
        textToShow = 'Correct!';
        accuracy(trialNum) = 1;
    else
        textToShow = 'Incorrect!';
        accuracy(trialNum) = 0;
    end
    
     %add accuracy to the fixation matrix at this point
    allTrialsFixationMatrix = [allTrialsFixationMatrix; fixationMatrix];
    
    DrawFormattedText(w, textToShow, 'center', 'center');
    Screen('Flip', w);
    if dummymode == 0
        Eyelink('Message', 'Response Screen');
    end
    
    WaitSecs(0.5);
    
    Screen('FillRect', w, [127, 127, 127]);%el.backgroundcolour);
    Screen('Flip', w);
    
    if dummymode == 0
        Eyelink('Message', 'BLANK_SCREEN');
    end
    % adds 100 msec of data to catch final events
    WaitSecs(0.1);
    
    fileName{trialNum} = allScenesFilePaths(sceneInds);
    responses{trialNum} = response;
    rtAll(trialNum) = RT;
end

DrawFormattedText(w, 'Saving Data...', 'center', 'center');
Screen('Flip', w);

if dummymode==0
    WaitSecs(1.0);
    Eyelink('StopReccording')
    Eyelink('Command', 'set_idle_mode'); %set tracking to idle
    WaitSecs(0.5);
    Eyelink('CloseFile'); %close the EDF file
    
    % grab the EDF file from the EyeLink computer (shows text messages
    % in command window based on whether file retrieval was successful
    % or not)
    cd output/eyeData
    try
        fprintf('Receiving data file ''%s''\n', edfFileName);
        status=Eyelink('ReceiveFile');
        if status > 0
            fprintf('ReceiveFile status %d\n', status);
        end
        if 2==exist(edfFileName, 'file')
            fprintf('Data file ''%s'' can be found in ''%s''\n', edfFileName, pwd);
        end
    catch
        fprintf('Problem receiving data file ''%s''\n', edfFileName);
    end
    cd ../..
    
    Eyelink('Shutdown'); %shutdown Matlab connection to EyeLink
end

outputData = {'sub_num' 'file_name' 'trial_num'  'rt' 'response' 't_direction' 'accuracy';};
for col = 1:4 %trialsPerRun
    outputData{col+1, 1} = subNumForOutput(1, col);
    outputData{col+1, 2} = fileName(1, col);
    outputData{col+1, 3} = thisRunTrialNumbers(1, col);
    outputData{col+1, 4} = rtAll(1, col);
    outputData{col+1, 5} = responses(1, col);
    outputData{col+1, 6} = tDirectionTarget{1, col};
    outputData{col+1, 7} = accuracy(1, col);
end

% Convert cell to a table and use first row as variable names
% outputTable = cell2table(outputData(2:end,:), 'VariableNames', outputData(1,:));

% Write the table to a CSV file
% Output is working but it is commeted out for now so I don't have a bunch
% saved csv files that I have to go and delete
% writetable(outputTable, fullfile(bxOutputFolder, bxFileName));

pfp_ptb_cleanup
%end