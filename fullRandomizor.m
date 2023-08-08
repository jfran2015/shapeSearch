function randomizor = fullRandomizor(trialsPerRun, sceneTextures, shapeTextures, totalTargets)


if exist('randomizor.mat', 'file')
    randomizor = load('randomizor.mat');
else
    % Initialize the main struct to hold 100 sub-structs (subjects)
    randomizor = struct();
    
    totalSubj = 100;
    
    % Number of runs
    totalRuns = 6;
    
    for subject = 1:totalSubj
        subStructName = sprintf('subj%d', subject);
        
        % Create the subject struct
        subjectStruct = struct();
        
        sceneInds = [1:96]';
        incorrectTrials = [0 0 0 1 0 0 0 1]';
        extraTargetTrials = [1 0 1 1 0 1 0 0]';
        
        combinedTrialConditions = [incorrectTrials, extraTargetTrials];
        
        
        allTrials = [];
        for i = 1:4
            allConditions = [];
            if i == 1
                numberOfRepitions = (length(sceneInds)/length(incorrectTrials))-(32/length(combinedTrialConditions));
                allConditions = zeros(32, 2);
            else
                numberOfRepitions = length(sceneInds)/length(incorrectTrials);
            end
            
            for reps = 1:numberOfRepitions
                rowInds = randperm(length(combinedTrialConditions));
                shuffledConditions = combinedTrialConditions(rowInds, :);
                allConditions = vertcat(allConditions, shuffledConditions);
            end
            
            scenesShuffledInds = randperm(length(sceneInds));
            scenesShuffled = sceneInds(scenesShuffledInds);
            scenesAndConditions = [scenesShuffled, allConditions];
            
            allTrials = vertcat(allTrials, scenesAndConditions);
        end
        
        allTrialsStart = 1;
        allTrialsEnd = trialsPerRun;
        
        for run = 1:totalRuns
            runStructName = sprintf('run%d', run);
            runStruct = struct();
            
            % Add variables to the run struct
            %set the 4 targets for this participant
            allTargets = randsample(1:length(shapeTextures), totalTargets);
            doubleTargetLocation = randi([1, 3]);
            targetLocationTypeRandomizor = [1, 2, 3, doubleTargetLocation];
            randomizedOrder = targetLocationTypeRandomizor(randperm(length(targetLocationTypeRandomizor)));
            allTargets(2, :) = randomizedOrder;
            
            allDistractors = setdiff(1:length(shapeTextures), allTargets(1, :), 'stable');
            
            %condition radomization
            %possible conditions:
            % - 1 = Target in correct location and additionaltarget is present
            % - 2 = Target is in wrong location with addition target,
            % - 3 = Target is in correct location with no additional target
            % - 4 = Target is in wrong location with no additional target
            %conditions = [1, 2, 3, 4];
            
            %determines where the target location will be
            targetPosition = [1, 2, 3, doubleTargetLocation];
            
            %how to choose between if there's two possible locations
            targetChoice = [1, 1, 2, 2];
            
            %index for all scenes
            SceneList = 1:length(sceneTextures);
            
            %radomizor for trial types
            extraTargetTrials = [0 0 0 1 0 0 0 1];
            
            %randomizor for if target is in correct position
            
            
            %cBExtraTargetTrials = counterBalancer(extraTargetTrials, trialsPerRun);
            %cBIncorrectTargetLocation = counterBalancer(extraTargetTrials, trialsPerRun); %I needed a number divisible by 12 because of the nature of the counterbalancing. 72 is arbetrary
            cBTargetPosition = counterBalancer(targetPosition, trialsPerRun);
            cBTargetChoice = counterBalancer(targetChoice, trialsPerRun); %just a variable for choosing if we use the first or second position if for example if it could appear in position 1 or 4
            %cBSceneOrder = counterBalancer(SceneList, trialsPerRun);
            
            
            
            %deterimines which direction the t faces
            tDirection = [1, 1, 2, 2];
            tDirectionAllTrials = zeros(trialsPerRun, 4);
            for trialNum = 1:trialsPerRun
                tDirectionAllTrials(trialNum, :) = tDirection(randperm(length(tDirection)));
            end
            
            cBTargetOrder = [];
            cBOrigionalTargetPosition = [];
            choice = 1;
            for numTargets = 1:length(cBTargetPosition)
                inds = find(allTargets(2, :) == cBTargetPosition(numTargets));
                if length(inds) > 1
                    if choice == 1
                        inds = inds(choice);
                        choice = 2;
                    elseif choice == 2
                        inds = inds(choice);
                        choice = 1;
                    end
                end
                cBTargetOrder(end+1) = allTargets(1, inds);
                cBOrigionalTargetPosition(end+1) = allTargets(2, inds);
            end
            
            allDistractorsAllTrials = [];
            for k = 1:12 % 6 reps in the inner loop go into 72 (the random number I picked for number of trials to test these with, so 12 reps)
                tempDistractors = allDistractors;
                
                if exist('oneTrialDistractors','var')
                    clear oneTrialDistractors
                end
                
                for i = 1:6 %there are 18 distractors 18/3 = 6, so thats why 6 repitions
                    if exist('oneTrialDistractors','var')
                        tempDistractors = setdiff(tempDistractors, oneTrialDistractors);
                    end
                    distractorsInds = randsample(1:length(tempDistractors), 3);
                    oneTrialDistractors = tempDistractors(distractorsInds);
                    allDistractorsAllTrials(end+1, :) = oneTrialDistractors;
                end
            end
            sceneOrder = allTrials(allTrialsStart:allTrialsEnd, 1);
            incorrectTargetLocation = allTrials(allTrialsStart:allTrialsEnd, 2);
            extraTargetTrials = allTrials(allTrialsStart:allTrialsEnd, 3);
            
            allTrialsStart = allTrialsStart + trialsPerRun;
            allTrialsEnd = allTrialsEnd + trialsPerRun;
            
            runStruct.('cBSceneOrder') = sceneOrder;
            runStruct.('cBIncorrectTargetLocation') = incorrectTargetLocation;
            runStruct.('cBExtraTargetTrials') = extraTargetTrials;
            runStruct.('allDistractorsAllTrials') = allDistractorsAllTrials; % You can set initial values if needed
            runStruct.('cBOrigionalTargetPosition') = cBOrigionalTargetPosition;
            runStruct.('cBTargetOrder') = cBTargetOrder;
            runStruct.('tDirectionAllTrials') = tDirectionAllTrials;
            runStruct.('cBTargetChoice') = cBTargetChoice;
            runStruct.('allTargets') = allTargets;
            
            % Add the run struct to the subject struct
            subjectStruct.(runStructName) = runStruct;
        end
        % Add the subject struct to the main struct
        randomizor.(subStructName) = subjectStruct;
    end
end
end