function[filePathMatrix, textureMatrix, imgMatrix] = imageStimuliImport(fileDirectory, fileType, w, sortLogical)
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
    sortLogical = false;
end

myFiles = dir(fullfile(fileDirectory, fileType));
filePathMatrix = string(zeros(length(myFiles), 1)); %matrix that contains all image file paths
textureMatrix = zeros(length(myFiles), 1); %matrix that contains all the textures of the image files
imgMatrix = zeros(length(myFiles), 1);
for k = 1:length(myFiles)
    baseFileName = myFiles(k).name;
    fullFilePath = string(fullfile(fileDirectory, baseFileName));
    
    
    
    if fileType == '*.png'
        [loadedImg, ~, alpha] = imread(fullFilePath);
        loadedImg(:, :, 4) = alpha;
    else
        loadedImg = imread(fullFilePath);
        
        
    end
    imgMatrix(k)= loadedImg;
    textureMatrix(k) = Screen('MakeTexture', w, loadedImg);
    filePathMatrix(k, 1) = fullFilePath;
end

if sortLogical == true
    numbersSort = regexp(filePathMatrix, 'Shape(\d+)', 'tokens');
    numbersSort = cellfun(@(x) str2double(x{1}), numbersSort);
    [~,sortedIndices] = sort(numbersSort);
    newTextureMatrix = textureMatrix(sortedIndices, :);
    newFilePathMatrix = filePathMatrix(sortedIndices);
    
    textureMatrix = newTextureMatrix;
    filePathMatrix = newFilePathMatrix;
end

end