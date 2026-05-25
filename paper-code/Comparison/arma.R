rm(list = ls())
library(readxl)
GNSS_20220201 <- read_excel("D:/采集数据/ARMA/13218.xlsx")


# 1. 提取第1列时间序列
data_series <- as.numeric(GNSS_20220201[[1]])

# 2. 划分训练集和测试集
n_total <- length(data_series)
n_train <- floor(0.77 * n_total)
train_data <- data_series[1:n_train]
test_data <- data_series[(n_train + 1):n_total]

# 3. 拟合 ARMA 模型（自动选择 p 和 q）
model <- auto.arima(train_data, d = 0, seasonal = FALSE)

# 4. 获取训练拟合值（fitted）和测试预测值（forecast）
fitted_train <- as.numeric(fitted(model))  # 训练集上的拟合值
forecast_result <- forecast(model, h = length(test_data))
predicted_test <- as.numeric(forecast_result$mean)

# 5. 拼接完整预测序列
predicted_full <- c(fitted_train, predicted_test)

# 6. 计算预测阶段的均方根误差（只在测试区间）
rmse <- sqrt(mean((predicted_test - test_data)^2))

# 7. 保存结果到变量
ARMA_Result <- list(
  predicted_train = fitted_train,
  predicted_test = predicted_test,
  full_prediction = predicted_full,
  actual = data_series,
  rmse = rmse,
  model_order = arimaorder(model),
  model_summary = summary(model)
)

# 8. 可视化：完整预测 vs 原始数据
plot(data_series, type = "l", col = "blue", lwd = 2,
     main = sprintf("ARMA 拟合与预测结果 (RMSE = %.4f)", rmse),
     ylab = "值", xlab = "时间")
lines(predicted_full, col = "red", lwd = 2, lty = 2)
abline(v = n_train, col = "gray", lty = 3)  # 分割线：训练/测试
legend("topleft", legend = c("原始数据", "ARMA预测", "训练/测试分界"),
       col = c("blue", "red", "gray"), lty = c(1, 2, 3), lwd = 2)

library(openxlsx)

# 1. 创建对比表格
comparison_df <- data.frame(
  TimeIndex = 1:length(data_series),
  Actual = data_series,
  Predicted = ARMA_Result$full_prediction
)

# 2. 保存到指定文件夹（注意使用正斜杠 / 或双反斜杠 \\）
write.xlsx(
  comparison_df,
  file = "D:/采集数据/ARMA/13218ARMA_Prediction_Result.xlsx",
  overwrite = TRUE
)

# 查看数据（可选）
print(comparison_df)