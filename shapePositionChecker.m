%function shapePositionChecker(sceneTypeMain0Practice1)
%-----------------------------------------------------------------------
% Script: ShapePositionChecker.m
% Author: Justin Frandsen
% Date: 09/26/2023
% Description:
% -
% Usage:
% -
% -
% -
%-----------------------------------------------------------------------
sceneTypeMain0Practice1 = 0;

% settings
sceneFolderPractice = 'Stimuli/scenes/practiceScenes';
scenesFolderMain = 'Stimuli/scenes/mainScenes';
shapesFolder = 'stimuli/shapes/transparent_black';

% Initilize PTB window
[w, rect] = pfp_ptb_init;
[width, height] = Screen('WindowSize', 0);

Screen('BlendFunction', w, 'GL_SRC_ALPHA', 'GL_ONE_MINUS_SRC_ALPHA'); %allows the .png files to be transparent

if sceneTypeMain0Practice1 == 0
    shapeLocationTypes = load('trialDataFiles/shape_location_types_main.mat');
    shapePositions = load('trialDataFiles/shape_positions_main.mat');
    %mistakes = load('trialDataFiles/mistakes_main.mat');
    [scenes_file_path_matrix, scenes_texture_matrix] = imageStimuliImport(scenesFolderMain, '', w);
elseif sceneTypeMain0Practice1 == 1
    shapeLocationTypes = load('trialDataFiles/shape_location_types_practice.mat');
    shapePositions = load('trialDataFiles/shape_positions_practice.mat');
    %mistakes = load('trialDataFiles/mistakes_practice.mat');
    [scenes_file_path_matrix, scenes_texture_matrix] = imageStimuliImport(sceneFolderPractice, '', w);
else
    error('Input for sceneTypeMain0Practice1 must be either 1 or 0!')
end

locationTypes = shapeLocationTypes.locationTypes;
savedPositions = shapePositions.savedPositions;

% Set initial position of the texture
textureSize = [0, 0, 240, 240]; % Adjust the size of the texture as desired
scaler = 1;
textureMover = [0, 0, 0, 0];
x = 0;
y = 0;

mistakes = [];
mistakeCounter = 0;

%load in stimuli
[~, stimuli_texture_matrix] = imageStimuliImport(shapesFolder, '*.png', w);

for scene_num = 1:length(scenes_texture_matrix)
    for positionNum = 1:4
        this_shape = stimuli_texture_matrix(randsample(1:22, 1));
        
        
        % '1 = Wall, 2 = Floor, 3 = Counter'
        thisSceneLocationType = locationTypes(scene_num, positionNum);
        thisScenePosition = savedPositions{scene_num, positionNum};
        
        if thisSceneLocationType == 1
            textForDisplay = 'Posisition Type: Wall';
        elseif thisSceneLocationType == 2
            textForDisplay = 'Posisition Type: Floor';
        elseif thisSceneLocationType == 3
            textForDisplay = 'Posisition Type: Counter';
        else
            disp('You did this wrong')
        end
        
        Screen('DrawTexture', w, scenes_texture_matrix(scene_num), [], rect);
        Screen('DrawTexture', w, this_shape, [], thisScenePosition);
        Screen('DrawText', w, textForDisplay)
        Screen('Flip', w);
        
        while true
            % Wait for a response
            [~, keyCode, ~] = KbWait([], 2);
            keyChar = KbName(keyCode);
            
            % Check if the response is valid (1, 2, or 3)
            if any(strcmp(keyChar, {'y', 'n', 'Y', 'N'}))
                if any(strcmp(keyChar, {'n', 'N'}))
                    running = true;
                    while running
                        WaitSecs(0.5);
                        running = true;
                        this_shape = stimuli_texture_matrix(randsample(1:22, 1));
                        while running == true
                            % Check for keyboard events
                            [keyIsDown, ~, keyCode] = KbCheck;
                            if keyIsDown && keyCode(KbName('space'))
                                running = false; % Break the loop if spacebar is pressed
                            elseif keyIsDown && keyCode(KbName('w'))
                                y = y-2;
                                textureMover(2) = y;
                                textureMover(4) = y;
                            elseif keyIsDown && keyCode(KbName('s'))
                                y = y+2;
                                textureMover(2) = y;
                                textureMover(4) = y;
                            elseif keyIsDown && keyCode(KbName('a'))
                                x = x-2;
                                textureMover(1) = x;
                                textureMover(3) = x;
                            elseif keyIsDown && keyCode(KbName('d'))
                                x = x+2;
                                textureMover(1) = x;
                                textureMover(3) = x;
                            elseif keyIsDown && keyCode(KbName('=+'))
                                scaler = scaler+.1;
                            elseif keyIsDown && keyCode(KbName('-_'))
                                if scaler > .1
                                    scaler = scaler-.01;
                                end
                            elseif keyIsDown && keyCode(KbName('o'))
                                disp(scenes_file_path_matrix(scene_num))
                            elseif keyIsDown && keyCode(KbName('m'))
                                if positionNum > 1
                                    positionNumForDisplay = positionNum-1;
                                    sceneNumForDisplay = scene_num;
                                elseif positionNum == 1
                                    positionNumForDisplay = 4;
                                    sceneNumForDisplay = scene_num - 1;
                                end
                                fprintf("ScenePosition(%d, %d)", sceneNumForDisplay, positionNumForDisplay)
                                mistakeCounter = mistakeCounter + 1;
                                mistakes(mistakeCounter, 1) = sceneNumForDisplay;
                                mistakes(mistakeCounter, 2) = positionNumForDisplay;
                            elseif keyIsDown && keyCode(KbName('ESCAPE'))
                                pfp_ptb_cleanup
                            end
                            
                            % Draw texture at new position
                            position = textureSize * scaler + textureMover;
                            Screen('DrawTexture', w, scenes_texture_matrix(scene_num), [], rect);
                            Screen('DrawTexture', w, this_shape, [], position);
                            Screen('Flip', w);   
                        end
                        savedPositions{scene_num, positionNum} = position;
                        DrawFormattedText(w, '1 = Wall, 2 = Floor, 3 = Counter', 'center', 'center')
                        Screen('Flip', w);
                        while true
                            % Wait for a response
                            [~, keyCode, ~] = KbWait([], 2);
                            keyChar = KbName(keyCode);
                            
                            % Check if the response is valid (1, 2, or 3)
                            if any(strcmp(keyChar, {'1!', '2@', '3#'}))
                                locationTypes(scene_num, positionNum) = keyChar;
                                break; % Break out of the response loop
                            end
                        end
                    end
                elseif any(strcmp(keyChar, {'y', 'Y'}))
                    break; % Break out of the response loop
                end
            end
        end
    end
end


if sceneTypeMain0Practice1 == 0
    save trialDataFiles/shape_positions_main1.mat savedPositions
    save trialDataFiles/shape_location_types_main1.mat locationTypes
    %save trialDataFiles/mistakes_main.mat mistakes
elseif sceneTypeMain0Practice1 == 1
    save trialDataFiles/shape_positions_practice1.mat savedPositions
    save trialDataFiles/shape_location_types_practice1.mat locationTypes
    %save trialDataFiles/mistakes_practice.mat mistakes
else
    error('Input for sceneTypeMain0Practice1 must be either 1 or 0!')
end