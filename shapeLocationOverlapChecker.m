%function shapeLocationOverlapChecker()
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

shapeLocationTypes = load('shape_location_types.mat');
shapePositions = load('shape_positions.mat');

for sceneNum = 1:length(shapePositions)
    for i = 1:4
        
    end
end