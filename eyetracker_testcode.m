% settings
output_file_format = 'scene_shape_search_%.3d.csv';
output_folder = 'output';
image_folder = 'images';
stimuli_folder = 'Stimuli';
number_of_trials = 4;
WinNum = 0;

% eyelink settings
dummymode = 0;

% shuffle the seed for the random number generator
rng('shuffle')

% magic_cleanup = onCleanup(@pfp_ptb_cleanup);

% prompt to get subject number
prompt = {'Enter Subject Number:'};
default = {'0'};
title = 'Setup Info';
LineNo = 1;
answer = inputdlg(prompt, title, LineNo, default);
[subjno_Str, Task] = deal(answer{:});

output_file_name = sprintf(output_file_format, str2num(subjno_Str));

% create section that checks if output exists for that subject yet
output_file_list = dir(fullfile(output_folder, '*.csv'));

edfFileName = ['Scene_grammer_subj' subjno_Str '.edf'];

for j = 1:length(output_file_list)
    existing_file_name = output_file_list(j).name;
    if existing_file_name == output_file_name
        error('Subject already exists. If you want to run again with the same subject number, you will need to delete the corresponding output file.');
    end
end

[my_window, my_rect] = pfp_ptb_init;
[width, height] = Screen('WindowSize', WinNum);

% =========================================================================
% =============== Load in images! =========================================
% =========================================================================

% load all .jpg files in the images directory.
[scenes_file_path_matrix, scenes_texture_matrix] = image_stimuli_import(image_folder, '*.jpg', my_window);

% load in stimuli
[stimuli_file_path_matrix, stimuli_texture_matrix] = image_stimuli_import(stimuli_folder, '*.png', my_window);

% =========================================================================
% =============== Initialize the eyetracker! ==============================
% =========================================================================

if dummymode == 0 % if you are actually eye tracking
    el = EyelinkInitDefaults(my_window);
    
    el.backgroundcolour = BlackIndex(el.window);
    el.msgfontcolour = WhiteIndex(el.window);
    el.imgtitlecolour = WhiteIndex(el.window);
    el.targetbeep = 0;
    el.calibrationtargetcolour = WhiteIndex(el.window);
    
    el.calibrationtargetsize = 2;
    el.calibrationtargetwidth = 0.75;
    
    EyelinkUpdateDefaults(el);
    
    if ~EyelinkInit(dummymode)
        fprintf('Eyelink Init aborted.\n');
        Eyelink('Shutdown');
        Screen('CloseAll')
        return;
    end
    
    i = Eyelink('Openfile', edfFileName);
    if i ~= 0
        fprintf('Cannot create EDF file ''%s'' ', edfFileName);
        Eyelink('Shutdown');
        Screen('CloseAll')
        return;
    end
    
    if Eyelink('IsConnected') ~= 1 && ~dummymode
        Eyelink('Shutdown');
        Screen('CloseAll')
        return;
    end
    
    Eyelink('command', 'add_file_preamble_text ''Recorded by EyelinkToolbox demo-experiment''');
    Eyelink('command', 'screen_pixel_coords = %ld %ld %ld %ld', 0, 0, width - 1, height - 1);
    Ey


% ... Existing code ...

% Create a cell array to store eye movement data for each trial
eye_movement_data = cell(1, number_of_trials);

% This loop runs the main task
for trial = 1:number_of_trials
    img_num = order_matrix(trial);  % Get the image number for the current trial
    
    % Draw the image on the screen
    Screen('DrawTexture', my_window, scenes_texture_matrix(img_num), [], my_rect);
    
    % Start recording eye movements for the current trial
    if dummymode == 0
        Eyelink('Message', 'TRIALID %d', trial);
        Eyelink('command', 'record_status_message "TRIAL %d/%d"', trial, number_of_trials);
        Eyelink('StartRecording');
    end
    
    % Present the image on the screen
    Screen('Flip', my_window);
    
    % Wait for a certain duration to collect eye movement data
    % Adjust the duration according to your experiment's requirements
    WaitSecs(2);  % Example: Collect eye movement data for 2 seconds
    
    % Stop recording eye movements for the current trial
    if dummymode == 0
        Eyelink('StopRecording');
    end
    
    % Retrieve and store the eye movement data for the current trial
    if dummymode == 0
        eye_movement_data{trial} = Eyelink('GetQueuedData');
    end
    
    % ... Perform other trial-related tasks ...
end

% Save the eye movement data to a file
if dummymode == 0
    eye_movement_file = fullfile(output_folder, output_file_name);
    save(eye_movement_file, 'eye_movement_data');
end

% ... Continue with the rest of the code ...