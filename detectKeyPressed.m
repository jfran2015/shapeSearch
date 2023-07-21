function detectKeyPressed()
%-----------------------------------------------------------------------
% Script: detectKeyPressed.m
% Author: Justin Frandsen
% Date: 07/20/2023
% Description: Matlab Script that when run outputs the name of the keyboard
%              key that you pressed
%
% Usage:
% - type function into Command Window
% - Script will output keypress into Command Window
%-----------------------------------------------------------------------
    WaitSecs(1);
    [~, keyCode] = KbWait();
    keyPressed = KbName(keyCode);
    disp(['Key pressed: ', keyPressed]);
end
