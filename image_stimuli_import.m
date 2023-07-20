
function[file_path_matrix, texture_matrix] = image_stimuli_import(file_directory, file_type, my_window)

my_files = dir(fullfile(file_directory, file_type));
file_path_matrix = string(zeros(length(my_files), 1)); %matrix that contains all image file paths
texture_matrix = zeros(length(my_files), 1); %matrix that contains all the textures of the image files
for k = 1:length(my_files)
    base_file_name = my_files(k).name;
    full_file_path = string(fullfile(file_directory, base_file_name));
    
    
    
    if file_type == '*.png'
        [loaded_img, ~, alpha] = imread(full_file_path);
        loaded_img(:, :, 4) = alpha;
        texture_matrix(k) = Screen('MakeTexture', my_window, loaded_img);
        file_path_matrix(k, 1) = full_file_path;
    else
        loaded_img = imread(full_file_path);
        texture_matrix(k) = Screen('MakeTexture', my_window, loaded_img);
        file_path_matrix(k, 1) = full_file_path;
    end 
end

end