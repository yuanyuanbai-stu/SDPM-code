clc 
clear
close all

% ====== 第一步：载入数据并选取第1列 ======
load GNSS_20220201
data = GNSS_20220201;
data_single = data(:, 1);  % 获取第1列

% ====== 第二步：标准化处理 ======
trainRatio = 0.72;
numTimeStepsTrain = floor(trainRatio * numel(data_single));

mu = mean(data_single(1:numTimeStepsTrain));
sig = std(data_single(1:numTimeStepsTrain));
dataNorm = (data_single - mu) / sig;

% ====== 第三步：构造训练和测试序列 ======
XTrain = dataNorm(1:numTimeStepsTrain-1)';
YTrain = dataNorm(2:numTimeStepsTrain)';

XTest = dataNorm(numTimeStepsTrain:end-1)';
YTest = dataNorm(numTimeStepsTrain+1:end)';

% ====== 第四步：定义 LSTM 网络结构 ======
layers = [ ...
    sequenceInputLayer(1)
    lstmLayer(100,'OutputMode','sequence')
    fullyConnectedLayer(1)
    regressionLayer];

options = trainingOptions('adam', ...
    'MaxEpochs', 250, ...
    'GradientThreshold', 1, ...
    'InitialLearnRate', 0.005, ...
    'Verbose', 0);

% ====== 第五步：训练网络 ======
net = trainNetwork(XTrain, YTrain, layers, options);

% ====== 第六步：预测并还原为真实数值 ======
% 测试集预测
YPred = predict(net, XTest, 'MiniBatchSize', 1);
YPredRescaled = YPred * sig + mu;
YTestRescaled = YTest * sig + mu;

% 训练集预测（用于完整输出）
YTrainPred = predict(net, XTrain, 'MiniBatchSize', 1);
YTrainRescaled = YTrainPred * sig + mu;

% ====== 第七步：拼接完整预测值 ======
fullPrediction = [YTrainRescaled, YPredRescaled];        % 365个
fullGroundTruth = data_single(2:end);                    % 目标是从第2天开始

% ====== 第八步：计算并保存 RMSE ======
rmse = sqrt(mean((YPredRescaled - YTestRescaled).^2));

% 保存结果
results.prediction = YPredRescaled;
results.groundTruth = YTestRescaled;
results.rmse = rmse;
results.fullPrediction = fullPrediction';
results.fullGroundTruth = fullGroundTruth;

% 显示部分预测结果和 RMSE
disp('测试集预测结果（前10个）：');
disp(results.prediction(1:10));
fprintf('测试集均方根误差 (RMSE)：%.4f\n', results.rmse);

% ====== 第九步：绘图（包含训练段 + 测试段） ======
figure
plot(fullGroundTruth, 'k-', 'LineWidth', 1.2, 'DisplayName','实际值')
hold on
plot(fullPrediction, 'r--', 'LineWidth', 1.2, 'DisplayName','预测值（训练+测试）')
xline(numTimeStepsTrain, 'b--', 'LineWidth', 1.2, 'DisplayName','训练/测试分界')
xlabel('时间步')
ylabel('GNSS 值')
title('GNSS 第1列时间序列的完整 LSTM 预测')
legend
grid on

% ====== 第十步：保存结果 ======
save('GNSS_LSTM_full_results.mat', 'results');
