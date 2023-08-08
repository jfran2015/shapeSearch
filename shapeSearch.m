%function shapeSearch(subNum, runNum)
subNum = 1; %tk remove and uncomment function call
runNum = 1; %tk remove and uncomment function call
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

%===================Beginning of real script============================
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
trialsPerRun          = 60;% 72 must be a multiple of 4
totalTargets          = 4;
totalDistractors      = 18;
stimuliSizeRect       = [0, 0, 240, 240]; %This rect contains the size of the shapes that are presented
%stimuliLocationMatrix = [1000, 100, 1000, 100]; %this matrix can be used to move the stimuli. This will be replaced
%stimuliScaler         = .25; %you can multiply the size Rect by this to grow or shrink the size of the stimuli.

% PTB Settings
WinNum = 0;

% Eyelink settings
dummymode = 1; %set 0 if using eyetracking, set 1 if not eyetracking (will use mouse position instead)

% Create output file names
bxFileName = sprintf(bxFileFormat, subNum, runNum);  %name of bx(behavioral) file
edfFileName = sprintf(eyeFileFormat, subNum, runNum); %name of the edf(eyetracking) file

% Test if output file already exists
outputFileList = dir(fullfile(bxOutputFolder, '*.csv'));
for j = 1:length(outputFileList)
    existing_file_name = outputFileList(j).name;
    if existing_file_name == bxFileName
        error('Suject already exists. If you want to run again with the same subject number you will need to delete the corresponding output file.');
    end
end

% Initilize PTB window
[w, rect] = pfp_ptb_init;
[width, height] = Screen('WindowSize', WinNum);
Screen('BlendFunction', w, 'GL_SRC_ALPHA', 'GL_ONE_MINUS_SRC_ALPHA'); %allows the .png files to be transparent
%set font, size, and style for my_window
old_font = Screen('TextFont', w, 'Arial');
old_size = Screen('TextSize', w, 35);
old_style = Screen('TextStyle', w, 1);

%create central fixation location
winfixsize = 50;
winfix = [-winfixsize -winfixsize, winfixsize, winfixsize];
winfix = CenterRect(winfix, rect);

% Initilize Eyetracker
if dummymode == 0
    el = eyelink_init(w, edfFileName);
end

% Load in images
DrawFormattedText(w, 'Loading Images...', 'center', 'center');
Screen('Flip', w);

% Load all .jpg files in the scenes folder.
[allScenesFilePaths, allScenesTextures] = imageStimuliImport(imageFolder, '*.jpg', w);

% Load in shape stimuli
[sortedNonsidedShapesFilePaths, sortedNonsidedShapesTextures] = imageStimuliImport(nonsidedShapes, '*.png', w, true);
[sortedLeftShapesFilePaths, sortedLeftShapesTextures] = imageStimuliImport(shapesTLeft, '*.png', w, true);
[sortedRightShapesFilePaths, sortedRightShapesTextures] = imageStimuliImport(shapesTRight, '*.png', w, true);

% %randomize presentation order
% trialOrder = 1:4; %tk change to 1:length(allScenesTextures);
% trialOrderFull = repmat(trialOrder, 1, 2); % Repeat the trialOrder vector twice
% trialOrderFull = trialOrderFull(randperm(length(trialOrderFull))); % Shuffle the elements randomly

%load
shapeLocationTypes = load('shape_location_types.mat');
shapePositions = load('shape_positions.mat');

%load variables for where the shapes are located and what postion theyre in
randomizor = fullRandomizor(trialsPerRun, allScenesTextures, sortedNonsidedShapesTextures, totalTargets);
this_subj_this_run = randomizor.(sprintf('subj%d', subNum)).(sprintf('run%d', runNum)); %method of getting into the struct
%==============================Beginning of task========================
%variables that will be saved out
rtAll = [];
responses = {};
tDirectionTarget = {};
accuracy = [];
fileName = {};
thisRunTrialNumbers = 1:trialsPerRun;
subNumForOutput(1:trialsPerRun) = subNum;

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

%eyetracking code will go here
possibleLocations = [1 2 3 4];
allTargets = this_subj_this_run.allTargets;
% =============== Task for loop ===========================================
for trialNum = 1:trialsPerRun
    sceneInds = this_subj_this_run.cBSceneOrder(trialNum);
    targetInds = this_subj_this_run.cBTargetOrder(trialNum);
    thisTrialIncorrectTargetLocation = this_subj_this_run.cBIncorrectTargetLocation(trialNum);
    thisTrialExtraTarget = this_subj_this_run.cBExtraTargetTrials(trialNum);
    targetPosition = this_subj_this_run.cBOrigionalTargetPosition(trialNum);
    targetChoice = this_subj_this_run.cBTargetChoice(trialNum);
    tDirectionThisTrial = this_subj_this_run.tDirectionAllTrials(trialNum, :);
    thisTrialDistractors = this_subj_this_run.allDistractorsAllTrials(trialNum, :);
    
    oldsize = Screen('TextSize', w,60); %make the font size a little bigger when drawing the fixation cross
    DrawFormattedText(w, '+', 'center', 'center', [255,255,255]); %draws the fixation cross (a plus-sign) at the center of the screen
    Screen('Flip', w);
    
    if thisTrialExtraTarget == 1 && thisTrialIncorrectTargetLocation == 1
        textToDisplay = 'Extra target present and Incorrect location';
    elseif thisTrialExtraTarget == 1 && thisTrialIncorrectTargetLocation == 0
        textToDisplay = 'Extra target present';
    elseif thisTrialExtraTarget == 0 && thisTrialIncorrectTargetLocation == 1
        textToDisplay = 'Incorrect location';
    elseif thisTrialExtraTarget == 0 && thisTrialIncorrectTargetLocation == 0
        textToDisplay = 'Normal Trial';
    end
    
    DrawFormattedText(w, textToDisplay, 50, 200, [255,255,255]); %tk remove this later
    Screen('DrawTexture', w, sortedNonsidedShapesTextures(targetInds)); %tk change the size later to reflect the true size on the trial
    start = 0;
    
    WaitSecs(1); %TK change to check if they're fixated
    
    Screen('Flip', w);
    while start==0 % tk delete this entire loop later
        [key_time,key_code]=KbWait([], 2);
        resp = find(key_code);
        if resp(1) == KbName('SPACE') || resp(1) == KbName('space')
            start = 1;
        end
    end
    % tk uncomment out for full experiment
    %     DrawFormattedText(w, '+', 'center', 'center', [255,255,255]); %draws the fixation cross (a plus-sign) at the center of the screen
    %     WaitSecs(1);
    %
    %     Screen('Flip', w);
    
    % Draw background scene
    Screen('DrawTexture', w, allScenesTextures(sceneInds), [], rect);
    
    if targetPosition == 1
        positionInds = find(shapeLocationTypes.locationTypes(sceneInds, :) == 1);
    elseif targetPosition == 2
        positionInds = find(shapeLocationTypes.locationTypes(sceneInds, :) == 2);
    elseif targetPosition == 3
        positionInds = find(shapeLocationTypes.locationTypes(sceneInds, :) == 3);
    end
    
    if length(positionInds) > 1
        if targetChoice == 1
            positionInds = positionInds(1);
        else
            positionInds = positionInds(2);
        end
    end
    
    if thisTrialIncorrectTargetLocation == 1
        incorrectLocations = setdiff(possibleLocations, positionInds);
        positionInds = randsample(incorrectLocations, 1);
    end
    
    tDirection = tDirectionThisTrial(positionInds);
    shapeSizeAndPosition = shapePositions.savedPositions{sceneInds, positionInds};
    if tDirection == 1
        Screen('DrawTexture', w, sortedRightShapesTextures(targetInds), [], shapeSizeAndPosition);
        tDirectionTarget{end+1} = 'R';
    elseif tDirection == 2
        Screen('DrawTexture', w, sortedLeftShapesTextures(targetInds), [], shapeSizeAndPosition);
        tDirectionTarget{end+1} = 'L';
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
    
    WaitSecs(1);
    stimOnsetTime = Screen('Flip', w);
    
    %response
    response = 'nan';
    RT = NaN;
    startTime = GetSecs();
    while GetSecs() - startTime <= 15
        [key_is_down, secs, key_code] = KbCheck;
        if key_is_down
            responseKey = KbName(key_code);
            if strcmp(responseKey, 'z') || strcmp(responseKey, '/?')
                response = responseKey;
                RT = round((secs - stimOnsetTime) * 1000);
                break;
            end
        end
    end
    
    if strcmp(response, 'nan')
        textToShow = 'Too slow!';
        accuracy(end+1) = 2;
    elseif strcmp(response, '/?') && strcmp(tDirectionTarget{trialNum}, 'R')
        textToShow = 'Correct!';
        accuracy(end+1) = 1;
    elseif strcmp(response, 'z') && strcmp(tDirectionTarget{trialNum}, 'L')
        textToShow = 'Correct!';
        accuracy(end+1) = 1;
    else
        textToShow = 'Incorrect!';
        accuracy(end+1) = 0;
    end
    
    DrawFormattedText(w, textToShow, 'center', 'center');
    Screen('Flip', w);
    WaitSecs(0.5);
    
    fileName{end+1} = allScenesFilePaths(sceneInds); %tk preallocate before final version
    responses{end+1} = response; %tk preallocate before final version
    rtAll(end+1) = RT; %tk preallocate before final version
end

DrawFormattedText(w, 'Saving Data...', 'center', 'center');
Screen('Flip', w);

outputData = {'sub_num' 'file_name' 'trial_num'  'rt' 'response' 't_direction' 'accuracy';};
for col = 1:trialsPerRun
    outputData{col+1, 1} = subNumForOutput(1, col);
    outputData{col+1, 2} = fileName(1, col);
    outputData{col+1, 3} = thisRunTrialNumbers(1, col);
    outputData{col+1, 4} = rtAll(1, col);
    outputData{col+1, 5} = responses(1, col);
    outputData{col+1, 6} = tDirectionTarget{1, col};
    outputData{col+1, 7} = accuracy(1, col);
end

% Convert cell to a table and use first row as variable names
outputTable = cell2table(outputData(2:end,:), 'VariableNames', outputData(1,:));

% Write the table to a CSV file
% Output is working but it is commeted out for now so I don't have a bunch
% saved csv files that I have to go and delete
% writetable(outputTable, fullfile(bxOutputFolder, bxFileName));

pfp_ptb_cleanup
%end