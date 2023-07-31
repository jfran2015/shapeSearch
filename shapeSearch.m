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
totalTrials           = 8;%must be a multiple of 4
totalTargets          = 4;
totalDistractors      = 18;
stimuliSizeRect       = [0, 0, 240, 240]; %This rect contains the size of the shapes that are presented
stimuliLocationMatrix = [1000, 100, 1000, 100]; %this matrix can be used to move the stimuli. This will be replaced
stimuliScaler         = .25; %you can multiply the size Rect by this to grow or shrink the size of the stimuli.

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
[allScenesFilePaths, allScenesTextures] = image_stimuli_import(imageFolder, '*.jpg', w);

% Load in shape stimuli
[sortedNonsidedShapesFilePaths, sortedNonsidedShapesTextures] = image_stimuli_import(nonsidedShapes, '*.png', w, true);
[sortedLeftShapesFilePaths, sortedLeftShapesTextures] = image_stimuli_import(shapesTLeft, '*.png', w, true);
[sortedRightShapesFilePaths, sortedRightShapesTextures] = image_stimuli_import(shapesTRight, '*.png', w, true);

%randomize presentation order
trialOrder = 1:4; %tk change to 1:length(allScenesTextures);
trialOrderFull = repmat(trialOrder, 1, 2); % Repeat the trialOrder vector twice
trialOrderFull = trialOrderFull(randperm(length(trialOrderFull))); % Shuffle the elements randomly

%load variables for where the shapes are located and what postion theyre in
shapeLocationTypes = load('shape_location_types.mat');
shapePositions = load('shape_positions.mat');

shapeLocationTypes.locationTypes = cell2mat(shapeLocationTypes.locationTypes);

%set the 4 targets for this participant
allTargets = randsample(1:length(sortedLeftShapesTextures), totalTargets);
doubleTargetLocation = randi([1, 3]);
targetLocationTypeRandomizor = [1, 2, 3, doubleTargetLocation];
randomizedOrder = targetLocationTypeRandomizor(randperm(length(targetLocationTypeRandomizor)));
allTargets(2, :) = randomizedOrder;

allDistractors = setdiff(1:length(sortedLeftShapesTextures), allTargets(1, :), 'stable');

%condition radomization
%possible conditions:
% - 1 = Target in correct location and additionaltarget is present
% - 2 = Target is in wrong location with addition target,
% - 3 = Target is in correct location with no additional target
% - 4 = Target is in wrong location with no additional target
conditions = [1, 2, 3, 4];

%determines where the target location will be
targetPosition = [1, 2, 3, doubleTargetLocation];

%how to choose between if there's two possible locations
targetChoice = [1, 1, 2, 2];

%index for the distractors
distractorsInds = 1:totalDistractors;

%index for all scenes
SceneList = 1:length(allScenesTextures);

%radomizor for trial types
extraTargetTrials = [0 0 0 1 0 0 0 1];

%randomizor for if target is in correct position


cBExtraTargetTrials = counterBalancer(extraTargetTrials, 72);
cBIncorrectTargetLocation = counterBalancer(extraTargetTrials, 72);
cBConditions = counterBalancer(conditions, 72); %I needed a number divisible by 12 because of the nature of the counterbalancing. 72 is arbetrary
cBTargetPosition = counterBalancer(targetPosition, 72);
cBTargetChoice = counterBalancer(targetChoice, 72); %just a variable for choosing if we use the first or second position if for example if it could appear in position 1 or 4
cBDistractors = counterBalancer(distractorsInds, 72);
%cBSceneOrder = counterBalancer(SceneList, 72);
cBSceneOrder = [1 2 3 4 1 2 3 4];

%deterimines which direction the t faces
targetTDirection = [1, 1, 2, 2];
tDirectionAllTrials = zeros(totalTrials, 4);
for trialNum = 1:totalTrials
    tDirectionAllTrials(trialNum, :) = targetTDirection(randperm(length(targetTDirection)));
end

cBTargetOrder = [];
cBOrigionalTargetPosition = [];
choice = 1;
for numTargets = 1:length(cBTargetPosition)
    inds = find(allTargets(2, :) == cBTargetPosition(numTargets));
    if length(inds) > 1
        if choice == 1
            inds = inds(choice);
            choice = 2;
        elseif choice == 2
            inds = inds(choice);
            choice = 1;
        end
    end
    cBTargetOrder(end+1) = allTargets(1, inds);
    cBOrigionalTargetPosition(end+1) = allTargets(2, inds);
end

allDistractorsAllTrials = [];
for k = 1:12 % 6 reps in the inner loop go into 72 (the random number I picked for number of trials to test these with, so 12 reps)
    tempDistractors = allDistractors;
    
    if exist('oneTrialDistractors','var')
        clear oneTrialDistractors
    end
    
    for i = 1:6 %there are 18 distractors 18/3 = 6, so thats why 6 repitions
        if exist('oneTrialDistractors','var')
            tempDistractors = setdiff(tempDistractors, oneTrialDistractors);
        end
        distractorsInds = randsample(1:length(tempDistractors), 3);
        oneTrialDistractors = tempDistractors(distractorsInds);
        allDistractorsAllTrials(end+1, :) = oneTrialDistractors;
    end
end

%==============================Beginning of task========================
%variables that will be saved out
rtAll = [];
responses = {};
tDirectionTarget = {};
accuracy = [];
fileName = {};
thisRunTrialNumbers = 1:totalTrials;
subNumForOutput(1:totalTrials) = subNum;

% Create a cell array to store eye movement data for each trial
eyeMovementData = cell(1, totalTrials);

%-------------------Instructions---------------------------------------------
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
% =============== Task for loop ===========================================
for trialNum = 1:totalTrials
    thisTrialScene = cBSceneOrder(trialNum);
    thisTrialTarget = cBTargetOrder(trialNum);
    
    oldsize = Screen('TextSize', w,60); %make the font size a little bigger when drawing the fixation cross
    DrawFormattedText(w, '+', 'center', 'center', [255,255,255]); %draws the fixation cross (a plus-sign) at the center of the screen
    Screen('Flip', w);
    
    if cBExtraTargetTrials(trialNum) == 1 && cBIncorrectTargetLocation(trialNum) == 1
        textToDisplay = 'Extra target present and Incorrect location';
    elseif cBExtraTargetTrials(trialNum) == 1 && cBIncorrectTargetLocation(trialNum) == 0
        textToDisplay = 'Extra target present';
    elseif cBExtraTargetTrials(trialNum) == 0 && cBIncorrectTargetLocation(trialNum) == 1
        textToDisplay = 'Incorrect location';
    elseif cBExtraTargetTrials(trialNum) == 0 && cBIncorrectTargetLocation(trialNum) == 0
        textToDisplay = 'Normal Trial';
    end
    
    DrawFormattedText(w, textToDisplay, 50, 200, [255,255,255]); %tk remove this later
    Screen('DrawTexture', w, sortedNonsidedShapesTextures(thisTrialTarget)); %tk change the size later to reflect the true size on the trial
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
    Screen('DrawTexture', w, allScenesTextures(cBSceneOrder(trialNum)), [], rect);
    
    if cBTargetPosition(trialNum) == 1
        positionInds = find(shapeLocationTypes.locationTypes(thisTrialScene, :) == 1);
    elseif cBTargetPosition(trialNum) == 2
        positionInds = find(shapeLocationTypes.locationTypes(thisTrialScene, :) == 2);
    elseif cBTargetPosition(trialNum) == 3
        positionInds = find(shapeLocationTypes.locationTypes(thisTrialScene, :) == 3);
    end
    
    if length(positionInds) > 1
        if cBTargetChoice(trialNum) == 1
            positionInds = positionInds(1);
        else
            positionInds = positionInds(2);
        end
    end
    
    if cBIncorrectTargetLocation(trialNum) == 1
        incorrectLocations = setdiff(possibleLocations, positionInds);
        positionInds = randsample(incorrectLocations, 1);
    end
    
    targetTDirection = tDirectionAllTrials(trialNum, positionInds);
    shapeSizeAndPosition = shapePositions.savedPositions{thisTrialScene, positionInds};
    if targetTDirection == 1
        Screen('DrawTexture', w, sortedRightShapesTextures(thisTrialTarget), [], shapeSizeAndPosition);
        tDirectionTarget{end+1} = 'R';
    elseif targetTDirection == 2
        Screen('DrawTexture', w, sortedLeftShapesTextures(thisTrialTarget), [], shapeSizeAndPosition);
        tDirectionTarget{end+1} = 'L';
    end
    
    if cBExtraTargetTrials(trialNum) == 1
        trialInd = randsample(1:3, 1);
        targetInd = randsample(1:3, 1);
        possibleDistractorTargets = setdiff(allTargets(1, :), thisTrialTarget);
        distractorTarget = possibleDistractorTargets(targetInd);
        allDistractorsAllTrials(trialNum, trialInd) = distractorTarget;
    end
    
    distractorPositions = setdiff(possibleLocations, positionInds);
    for position = 1:length(distractorPositions)
        distractorTDirection = tDirectionAllTrials(trialNum, distractorPositions(position));
        shapeSizeAndPosition = shapePositions.savedPositions{thisTrialScene, distractorPositions(position)};
        thisDistractor = allDistractorsAllTrials(trialNum, position);
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
    
    fileName{end+1} = allScenesFilePaths(thisTrialScene); %tk preallocate before final version
    responses{end+1} = response; %tk preallocate before final version
    rtAll(end+1) = RT; %tk preallocate before final version
end

DrawFormattedText(w, 'Saving Data...', 'center', 'center');
Screen('Flip', w);

outputData = {'sub_num' 'file_name' 'trial_num'  'rt' 'response' 't_direction' 'accuracy';};
for col = 1:totalTrials
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