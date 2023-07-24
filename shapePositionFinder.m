function ShapePositionFinder()
%-----------------------------------------------------------------------
% Script: ShapePositionFinder.m
% Author: Justin Frandsen
% Date: 07/21/2023
% Description:
% - Matlab script used to place images on their correct locations in scenes
%   and output rects for placing them in the main experiment.
% Usage:
% - Use W,A,S,D to move the image across the screen. Use - & + to increase
%   or decrease the size of the shapes, and use space to save that position
%   and size. After saved it will ask if the shape was on the wall, floor,
%   or counter.
% - This function saves the rects for each image in a rect containing
%   the location and size of each object. Each row represents each scene.
% - A second .mat file will be saved containing the responses to if the
%   shape was on the floor, counter, or wall.
%-----------------------------------------------------------------------

% settings
image_folder = 'scenes';
stimuli_folder = 'Stimuli/transparent_black';

% Initilize PTB window
[w, rect] = pfp_ptb_init;
[width, height] = Screen('WindowSize', 0);

Screen('BlendFunction', w, 'GL_SRC_ALPHA', 'GL_ONE_MINUS_SRC_ALPHA'); %allows the .png files to be transparent

%load all .jpg files in the images directory.
[scenes_file_path_matrix, scenes_texture_matrix] = image_stimuli_import(image_folder, '*.jpg', w);

%load in stimuli
[stimuli_file_path_matrix, stimuli_texture_matrix] = image_stimuli_import(stimuli_folder, '*.png', w);

% Set initial position of the texture
textureSize = [0, 0, 240, 240]; % Adjust the size of the texture as desired
scaler = 1;
textureMover = [0, 0, 0, 0];
x = 0;
y = 0;

savedPositions = cell(4, 4); %tk change first for to length(scenes_texture_matrix)
locationTypes = cell(4, 4); %tk first 4 to length(scenes_texture_matrix)

%loop for presenting scenes and their
for scene_num = 1:4 %tk change to length(scenes_texture_matrix)
    for i = 1:4
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
            elseif keyIsDown && keyCode(KbName('ESCAPE'))
                pfp_ptb_cleanup
            end
            
            % Draw texture at new position
            position = textureSize * scaler + textureMover;
            Screen('DrawTexture', w, scenes_texture_matrix(scene_num), [], rect);
            Screen('DrawTexture', w, this_shape, [], position);
            Screen('Flip', w);
            
            
        end
        savedPositions{scene_num, i} = position;
        DrawFormattedText(w, '1 = Wall, 2 = Floor, 3 = Counter', 'center', 'center')
        Screen('Flip', w);
        while true
            % Wait for a response
            [~, keyCode, ~] = KbWait([], 2);
            keyChar = KbName(keyCode);
            
            % Check if the response is valid (1, 2, or 3)
            if any(strcmp(keyChar, {'1!', '2@', '3#'}))
                locationTypes{scene_num, i} = keyChar;
                break; % Break out of the response loop
            end
        end
    end
end

for row = 1:length(locationTypes)
    for col = 1:4
        if strcmp(locationTypes{row, col}, '1!')
            locationTypes{row, col} = 1;
        elseif strcmp(locationTypes{row, col}, '2@')
            locationTypes{row, col} = 2;
        elseif strcmp(locationTypes{row, col}, '3#')
            locationTypes{row, col} = 3;
        end
    end
end

locationTypes = cell2mat(locationTypes);

pfp_ptb_cleanup;

save shape_positions.mat savedPositions
save shape_location_types.mat locationTypes
end