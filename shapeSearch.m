%function shapeSearch(subNum, runNum)
subNum = 1; %tk remove and uncomment function call
runNum = 1; %tk remove and uncomment function call
%-----------------------------------------------------------------------
% Script: shapeSearch.m
% Author: Justin Frandsen
% Date: 07/12/2023
% Description: Matlab script that presents experiment exploring how
%              learning influences scene grammar.
%
% Additional Comments:
% - Must be in the correct folder to run script.
% - Make sure eyetracker is powered on and connected.
% - At beginning of task you will be prompted to enter run number
%   Each Participant should have xx runs.
% - Make sure shape_location_type.mat & shape_position.mat are saved and
%   ready
%
% Usage:
% - type function name with subject number and run number (seperated bya
%   comma in parentheses
%   (e.g., shapeSearch(222, 1)).
% - Script will output a .csv file containing behavioral data and a
%   .edf file containing eyetracking data.
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

%set the 4 targets for this participant
allTargets = randsample(1:length(sortedLeftShapesTextures), totalTargets);
targetLocationTypeRandomizor = [1, 2, 3, randi([1, 3])];
randomizedOrder = targetLocationTypeRandomizor(randperm(length(targetLocationTypeRandomizor)));
allTargets(2, :) = randomizedOrder;
% Calculate how many times the targets will need repeated
totalTargetRepitions = totalTrials/totalTargets;
% Create a vector repeating the target indexes
allTargetsRepeated = repmat(allTargets, 1, totalTargetRepitions);
% Shuffle the elements of the vector
% Randomly reorder the columns
numColumns = size(allTargetsRepeated, 2);
randomColumnOrder = randperm(numColumns);
allTargetsShuffled = allTargetsRepeated(:, randomColumnOrder);

% totalDistractorRepitions = totalTrials/totalDistractors; Impliment when
% we have more trials
allDistractors = setdiff(1:length(sortedLeftShapesTextures), allTargets(1, :), 'stable');
% allDistractorsRepeated = repmat(allDistractors, 1, 2); %tk add totalDistractorRepitions to third agrument
% allDistractorsShuffled = allDistractorsRepeated(randperm(length(allDistractorsRepeated)));

% x=1;
% y=3;
% distractorTable = zeros(totalTrials, 3);
% for trialNum = 1:totalTrials
% distractorTable(trialNum, :) = allDistractorsShuffled(x:y);
%     x = x+3;
%     y = y+3;
% end

%condition radomization
%possible conditions:
% - 1 = Target in correct location and additionaltarget is present
% - 2 = Target is in wrong location with addition target,
% - 3 = Target is in correct location with no additional target
% - 4 = Target is in wrong location with no additional target
conditions = [1, 2, 3, 4];

%deterimines which direction the t faces
direction = [1, 1, 2, 2];

%determines where the target location will be
targetPosition = [1, 2, 3];

distractors = 1:22;

counterBalancedConditions = counterBalancer(conditions, 72);
counterBalancedDirection = counterBalancer(direction, 72);
counterBalancedTargetPosition = counterBalancer(targetPosition, 72);
counterBalancedDistractors = counterBalancer(distractors, 72);


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


% =============== Task for loop ===========================================
for trialNum = 1:totalTrials
    thisTrialScene = trialOrderFull(trialNum);
    thisTrialTarget = allTargetsShuffled(1, trialNum);
    targetPositionValue = allTargetsShuffled(2, trialNum);
    
    
    oldsize = Screen('TextSize', w,60); %make the font size a little bigger when drawing the fixation cross
    DrawFormattedText(w, '+', 'center', 'center', [255,255,255]); %draws the fixation cross (a plus-sign) at the center of the screen
    Screen('Flip', w);
    Screen('DrawTexture', w, sortedNonsidedShapesTextures(thisTrialTarget)); %tk change the size later to reflect the true size on the trial
    WaitSecs(1); %TK change to check if they're fixated
    
    Screen('Flip', w);
    DrawFormattedText(w, '+', 'center', 'center', [255,255,255]); %draws the fixation cross (a plus-sign) at the center of the screen
    WaitSecs(1);
    
    Screen('Flip', w);
    
    % Draw background scene
    Screen('DrawTexture', w, allScenesTextures(thisTrialScene), [], rect);
    
    
%     distractorIndex = 1;
%     for col = 1:4
%         if targetPositionValue == 1 && strcmp(shapeLocationTypes.locationTypes{thisTrialScene, col}, '1!')
%             matchingCondition = true;
%         elseif targetPositionValue == 2 && strcmp(shapeLocationTypes.locationTypes{thisTrialScene, col}, '2@')
%             matchingCondition = true;
%         elseif targetPositionValue == 3 && strcmp(shapeLocationTypes.locationTypes{thisTrialScene, col}, '3#')
%             matchingCondition = true;
%         else
%             matchingCondition = false;
%         end
%         
%         if matchingCondition % Draw the target
%             shapeSize = shapePositions.savedPositions{thisTrialScene, col};
%             if directionRandomizor(col) == 1
%                 Screen('DrawTexture', w, sortedLeftShapesTextures(thisTrialTarget), [], shapeSize);
%                 tDirectionTarget{end+1} = 'L'; %tk preallocate before the final version
%             elseif directionRandomizor(col) == 2
%                 Screen('DrawTexture', w, sortedRightShapesTextures(thisTrialTarget), [], shapeSize);
%                 tDirectionTarget{end+1} = 'R'; %tk preallocate before the final version
%             end
%         end
%     end
    
    
    WaitSecs(1);
    stimOnsetTime = Screen('Flip', w);
    
    %response
    response = 'nan';
    RT = NaN;
    startTime = GetSecs();
    while GetSecs() - startTime <= 20
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