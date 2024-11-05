
trialSceneInfo = zeros(1, 5);

matlabfiles = dir(fullfile('output/matlabData', '*.mat'));

for file_num = 1:length(matlabfiles)

    file_name = matlabfiles(file_num).name;

    load(fullfile('output/matlabData', file_name))

    for trial_num = 1:length(outputData)-1
        thisTrialTargetPosition = outputData(trial_num+1, 9);
        thisTrialTargetPosition = thisTrialTargetPosition{1}{1};

        positions = shapePositions.savedPositions;
        if ~isempty(thisTrialTargetPosition)
            for positionnum = 1:length(positions)
                for i = 1:4
                    test=thisTrialTargetPosition==positions{positionnum, i};
                    if test
                        trialSceneInfo = vertcat(trialSceneInfo, [subNum, runNum, trial_num, positionnum, i]);
                    end
                end
            end
        end
    end
    clearvars -except trialSceneInfo matlabfiles     %deletes all variables except X in workspace
end

writematrix(trialSceneInfo, 'output/sceneInfo.csv')

clear all
close all