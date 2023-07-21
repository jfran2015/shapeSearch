function[file_path_matrix, texture_matrix] = image_stimuli_import(file_directory, file_type, my_window, sort_logical)
%-------------------------------------------------------------------------
% Script: detectKeyPressed.m
% Author: Justin Frandsen
% Date: 07/20/2023
% Description: Matlab Script that is used for importing image files and
%              turing them into textures.
%
% Usage:
% - input the file directory, file type (must be a string with * at
%   beginning (i.e., '*.png') and ptb window.
% - Script will output a matrix containing the file paths for the images
%   and a matrix containing textures which can be displayed using ptb
% - If sort is true then when it will sort the textures and paths 
%   matrixes based on numbers in the file names 
%-------------------------------------------------------------------------
if nargin < 4
    sort_logical = false;
end

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

if sort_logical == true
    numbersSort = regexp(file_path_matrix, 'Shape(\d+)', 'tokens');
    numbersSort = cellfun(@(x) str2double(x{1}), numbersSort);
    [~,sortedIndices] = sort(numbersSort);
    new_texture_matrix = texture_matrix(sortedIndices, :);
    new_file_path_matrix = file_path_matrix(sortedIndices);
    
    texture_matrix = new_texture_matrix;
    file_path_matrix = new_file_path_matrix;
end

end