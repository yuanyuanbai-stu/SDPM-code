clc 
clear
close all

% ====== 第一步：载入数据并选取第1列 ======
load GNSS_20220201
data = GNSS_20220201(:, 1);
N = length(data);

% 归一化
mu = mean(data);
sigma = std(data);
dataNorm = (data - mu) / sigma;

% 构造训练输入和目标（滑动单步预测）
X = num2cell(dataNorm(1:end-1)');
Y = num2cell(dataNorm(2:end)');

% 构建简单RNN网络
layers = [
    sequenceInputLayer(1)
    fullyConnectedLayer(10)
    tanhLayer
    fullyConnectedLayer(1)
    regressionLayer
];

% 设置训练选项
options = trainingOptions('adam', ...
    'MaxEpochs', 200, ...
    'GradientThreshold', 1, ...
    'InitialLearnRate', 0.005, ...
    'Verbose', false);

% 模型训练
net = trainNetwork(X, Y, layers, options);

% 整体预测（对整个数据做一次）
YPredNorm = predict(net, X, 'MiniBatchSize', 1);

% 还原归一化
YPred = cell2mat(YPredNorm) * sigma + mu;
YTrue = data(2:end);  % 真实值，从第二个点开始

% 计算RMSE
rmse = sqrt(mean((YPred - YTrue).^2));

% 打包结果
resultStruct = struct();
resultStruct.PredictedValues = YPred;
resultStruct.RMSE = rmse;

% 可视化
figure;
plot(2:length(data), YTrue, 'b-', 'DisplayName', '真实值');
hold on;
plot(2:length(data), YPred, 'r--', 'DisplayName', '预测值');
legend;
xlabel('时间步');
ylabel('数据值');
title(['整体序列预测（RMSE = ', num2str(rmse, '%.4f'), '）']);
grid on;