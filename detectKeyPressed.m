function detectKeyPressed()
    [~, keyCode] = KbWait();
    keyPressed = KbName(keyCode);
    disp(['Key pressed: ', keyPressed]);
end
