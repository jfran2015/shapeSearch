% Specify the shape size
shapeSize = [0, 0, 240, 240];
WinNum=0;

% Specify the display dimensions
displayWidth = 1920;
displayHeight = 1080;

% Calculate the position coordinates for the shape
left = (displayWidth - shapeSize(3)) / 2;    % Horizontal center
top = (displayHeight - shapeSize(4)) / 2;    % Vertical center
right = left + shapeSize(3);
bottom = top + shapeSize(4);

% Display the shape at the specified location
[my_window, my_rect] = pfp_ptb_init;
[width, height] = Screen('WindowSize', WinNum);
Screen('FillRect', my_window, [255 255 255], [left, top, right, bottom]);
Screen('Flip', my_window);
WaitSecs(4.0);

% Specify the T shape dimensions
lineWidth = 4;
lineLength = 160;

% Calculate the position coordinates for the T shape
centerX = displayWidth / 2;    % Horizontal center
centerY = displayHeight / 2;   % Vertical center

% Define the T shape coordinates
horizontalLine = [centerX - lineLength/2, centerY, centerX + lineLength/2, centerY];
verticalLine = [centerX, centerY - lineLength/2, centerX, centerY + lineLength/2];
tCoordinates = [horizontalLine; verticalLine];

% Display the T shape at the specified location
Screen('DrawLines', my_window, tCoordinates, lineWidth, [255 255 255]);
Screen('Flip', my_window);
KbWait;

pfp_ptb_cleanup;