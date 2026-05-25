library(readxl)

# 读取数据
generate <- read_excel("D:/采集数据/数值模拟/generate.xlsx")
ts_data <- data.matrix(generate)

# ------------------- 高分辨率 PNG 输出 -------------------
png(
  filename = "D:/4_41.png",
  width = 3000,
  height = 1200,
  res = 300,
  type = "cairo"
)

# ------------------- 全局绘图参数 -------------------
par(
  family = "Times",
  mfrow = c(5, 2),
  mar = c(0.2, 0.2, 0.2, 0.2),   # 极小边距
  oma = c(0, 0, 0, 0),          # 去掉外边距
  las = 1,
  mgp = c(2, 0.3, 0),
  tcl = -0.2,
  xaxs = "i",
  yaxs = "i"
)

# 颜色向量
colors_vec <- c(
  "#4E79A7", "#4E79A7", "#4E79A7", "#4E79A7", "#4E79A7",
  "#4E79A7", "#4E79A7", "#4E79A7", "#4E79A7", "#4E79A7"
)

# ------------------- 绘制子图 -------------------
for (i in 1:ncol(ts_data)) {
  
  y <- ts_data[, i]
  if (all(is.na(y))) next
  
  # 纵轴范围向下/向上取整到小数点后一位
  y_min <- floor(min(y, na.rm = TRUE) * 10) / 10
  y_max <- ceiling(max(y, na.rm = TRUE) * 10) / 10
  
  # 横轴范围加空白 (5% padding)
  x_min <- 1
  x_max <- length(y)
  x_pad <- 0.05 * (x_max - x_min)
  xlim <- c(x_min - x_pad, x_max + x_pad)
  
  # 纵轴加空白 (5% padding)
  y_pad <- 0.04 * (y_max - y_min)
  ylim <- c(y_min - y_pad, y_max + y_pad)
  
  # 横轴刻度（5个等分）
  x_ticks <- seq(1, length(y), length.out = 2)
  # 纵轴刻度（5个等分）
  y_ticks <- seq(y_min, y_max, length.out = 2)
  
  plot(
    y,
    type = "l",
    col = colors_vec[i],
    lwd = 3,
    xlab = "",
    ylab = "",
    xaxt = "n",
    yaxt = "n",
    xlim = xlim,
    ylim = ylim
  )
  
  # 横轴：5个刻度，数字不显示
  axis(1, at = x_ticks, labels = FALSE, tcl = 0.3, lwd = 2)
  # 纵轴：5个刻度，数字不显示
  axis(2, at = y_ticks, labels = FALSE, tcl = 0.3, lwd = 2)
  
  # 外边框线加粗
  box(lwd = 2)
}

# ------------------- 关闭 PNG 设备 -------------------
dev.off()







#svg输出
library(readxl)

# 读取数据
generate <- read_excel("D:/采集数据/数值模拟/generate.xlsx")
ts_data <- data.matrix(generate)

# ------------------- SVG 矢量图输出 -------------------
svg(
  filename = "D:/4_41.svg",
  width = 10,   # 单位：英寸（可根据需要调整）
  height = 4
)

# ------------------- 全局绘图参数 -------------------
par(
  family = "Times",     # 建议用 Times（兼容性更好）
  mfrow = c(5, 2),      # 5行2列
  mar = c(0.3, 0.8, 0.2, 0.2),
  oma = c(0, 0, 0, 0),
  las = 1,
  mgp = c(2.3, 0.5, 0),
  tcl = -0.2,
  xaxs = "i",
  yaxs = "i"
)

# 颜色向量
colors_vec <- c(
  "#4E79A7", "#4E79A7", "#4E79A7", "#4E79A7", "#4E79A7",
  "#4E79A7", "#4E79A7", "#4E79A7", "#4E79A7", "#4E79A7"
)

# ------------------- 绘制子图 -------------------
for (i in 1:ncol(ts_data)) {
  
  y <- ts_data[, i]
  if (all(is.na(y))) next
  
  # 纵轴范围
  y_min <- floor(min(y, na.rm = TRUE) * 10) / 10
  y_max <- ceiling(max(y, na.rm = TRUE) * 10) / 10
  
  # 横轴范围 + padding
  x_min <- 1
  x_max <- length(y)
  x_pad <- 0.02 * (x_max - x_min)
  xlim <- c(x_min - x_pad, x_max + x_pad)
  
  # 纵轴 padding
  y_pad <- 0.04 * (y_max - y_min)
  ylim <- c(y_min - y_pad, y_max + y_pad)
  
  # 刻度
  x_ticks <- seq(1, length(y), length.out = 2)
  y_ticks <- seq(y_min, y_max, length.out = 0)
  
  plot(
    y,
    type = "l",
    col = colors_vec[i],
    lwd = 3,
    xlab = "",
    ylab = "",
    xaxt = "n",
    yaxt = "n",
    xlim = xlim,
    ylim = ylim
  )
  
  # 坐标轴
  axis(1, at = x_ticks, labels = FALSE, tcl = 0.3, lwd = 2)
  axis(2, at = y_ticks, labels = FALSE, tcl = 0.3, lwd = 2)
  
  # 边框
  box(lwd = 2)
}

# ------------------- 关闭设备 -------------------
dev.off()








#3-2图，3.19晚上新改代码
# ------------------- 3-2 图（最终 + padding版） -------------------
library(readxl)

# 读取数据
moni <- read_excel("D:/3.17图/数据/模拟/rmse.xlsx")
ts_data <- data.matrix(moni)

# ------------------- SVG 矢量图输出 -------------------
svg(
  filename = "D:/4_42.svg",
  width = 10,   # 单位：英寸（可根据需要调整）
  height = 4
)

# ------------------- 全局绘图参数 -------------------
par(
  family = "Times",     # 建议用 Times（兼容性更好）
  mfrow = c(5, 2),      # 5行2列
  mar = c(0.5, 2.5, 0.3, 1.0),
  oma = c(0, 0, 0, 0),
  las = 1,
  mgp = c(2.3, 0.5, 0),
  tcl = -0.2,
  xaxs = "i",
  yaxs = "i"
)

colors_vec <- rep("#F28E2B", ncol(ts_data))

# ------------------- 纵轴范围 -------------------
y_ranges <- list(
  c(0, 0.5),
  c(0, 1.0),
  c(0, 1.5),
  c(0, 0.4),
  c(0, 1.2),
  c(0, 0.8),
  c(0, 0.3),
  c(0, 0.8),
  c(0, 0.5),
  c(0, 0.5)
)

# 刻度数量
y_ticks_num <- c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2)

# ------------------- 绘图 -------------------
for (i in 1:ncol(ts_data)) {
  
  y <- ts_data[, i]
  if (all(is.na(y))) next
  
  # ---------- 横轴 ----------
  x_base <- c(0, 200)
  x_range <- diff(x_base)
  x_pad <- 0.02 * x_range
  xlim <- c(x_base[1] - x_pad, x_base[2] + x_pad)
  
  x_ticks <- seq(0, 200, length.out = 2)
  
  # ---------- 纵轴 ----------
  y_base <- y_ranges[[i]]
  y_range <- diff(y_base)
  y_pad <- 0.04 * y_range
  ylim <- c(y_base[1] - y_pad, y_base[2] + y_pad)
  
  y_ticks <- seq(y_base[1], y_base[2], length.out = y_ticks_num[i])
  
  plot(
    y,
    type = "l",
    col = colors_vec[i],
    lwd = 3,
    xlab = "",
    ylab = "",
    xaxt = "n",
    yaxt = "n",
    xlim = xlim,
    ylim = ylim
  )
  
  # 横轴刻度（基于原始范围）
  axis(1,
       at = x_ticks,
       labels = FALSE,
       tcl = 0.3,
       lwd = 2)
  
  # 纵轴刻度（基于原始范围）
  axis(2,
       at = y_ticks,
       labels = FALSE,
       tcl = 0,
       lwd = 0)
  
  box(lwd = 2)
}

dev.off()



#3-3图，3.19晚上新改代码
# ------------------- 3-3 图（单图最终版） -------------------
library(readxl)

# 读取数据
sd <- read_excel("D:/3.17图/数据/模拟/sd.xlsx")
ts_data <- data.matrix(sd)

# 提取唯一一列
y <- ts_data[, 1]

# ------------------- SVG 矢量图输出 -------------------
svg(
  filename = "D:/4_43.svg",
  width = 5,   # 单位：英寸（可根据需要调整）
  height = 4
)

# ------------------- 全局参数 -------------------
par(
  family = "TNR",
  mar = c(0, 0, 0, 0),   # 单图适当增加边距
  las = 1,
  mgp = c(2.5, 0.6, 0),
  tcl = -0.3,
  xaxs = "i",
  yaxs = "i"
)

# ------------------- 横轴 -------------------
x_base <- c(0, 200)
x_pad <- 0.05 * diff(x_base)
xlim <- c(x_base[1] - x_pad, x_base[2] + x_pad)

x_ticks <- seq(0, 200, length.out = 5)

# ------------------- 纵轴 -------------------
y_base <- c(0, 2100)
y_pad <- 0.04 * diff(y_base)
ylim <- c(y_base[1] - y_pad, y_base[2] + y_pad)

y_ticks <- seq(0, 2100, length.out = 4)

# ------------------- 绘图 -------------------
plot(
  y,
  type = "l",
  col = "#59A14F",
  lwd = 2.5,
  xlab = "",
  ylab = "",
  xaxt = "n",
  yaxt = "n",
  xlim = xlim,
  ylim = ylim
)

# 横轴
axis(1,
     at = x_ticks,
     labels = FALSE,
     tcl = 0.3,
     lwd = 2.0)

# 纵轴
axis(2,
     at = y_ticks,
     labels = FALSE,
     tcl = 0.3,
     lwd = 2.0)

# 边框
box(lwd = 2.0)

# ------------------- 关闭设备 -------------------
dev.off()




#3-4图，3.19晚上新改代码
# ------------------- 3-4 图（双列数据最终版） -------------------
library(readxl)

# 读取数据
pzhi <- read_excel("D:/3.17图/数据/模拟/pzhi.xlsx")
ts_data <- data.matrix(pzhi)

# ------------------- SVG 矢量图输出 -------------------
svg(
  filename = "D:/4_44.svg",
  width = 5,   # 单位：英寸（可根据需要调整）
  height = 4
)

# ------------------- 全局参数 -------------------
par(
  family = "TNR",
  mar = c(0, 0, 0, 0),   # 单图适当增加边距
  las = 1,
  mgp = c(2.5, 0.6, 0),
  tcl = -0.3,
  xaxs = "i",
  yaxs = "i"
)

# ------------------- 横轴 -------------------
x_base <- c(0, 200)
x_pad <- 0.05 * diff(x_base)
xlim <- c(x_base[1] - x_pad, x_base[2] + x_pad)
x_ticks <- seq(0, 200, length.out = 5)

# ------------------- 纵轴 -------------------
y_base <- c(0, 1)
y_pad <- 0.04 * diff(y_base)
ylim <- c(y_base[1] - y_pad, y_base[2] + y_pad)
y_ticks <- seq(0, 1, length.out = 6)

# ------------------- 绘图 -------------------
# 绘制第一列
plot(
  ts_data[1:185, 1],            # 第一列，长度 185
  type = "l",
  col = "#F28E2B",
  lwd = 2.0,
  xlab = "",
  ylab = "",
  xaxt = "n",
  yaxt = "n",
  xlim = xlim,
  ylim = ylim
)

# 绘制第二列
lines(
  ts_data[1:190, 2],            # 第二列，长度 190
  col = "#59A14F",
  lwd = 2.0
)

# ------------------- 坐标轴 -------------------
# 横轴
axis(1,
     at = x_ticks,
     labels = FALSE,
     tcl = 0.3,
     lwd = 2.0)

# 纵轴
axis(2,
     at = y_ticks,
     labels = FALSE,
     tcl = 0.3,
     lwd = 2.0)

# ------------------- 边框 -------------------
box(lwd = 2.0)

# ------------------- 关闭设备 -------------------
dev.off()

























library(ggplot2)
library(readxl)
library(gridExtra)
library(grid)

# 读取数据
rmse_station_all <- read_excel("D:/采集数据/数值模拟/moni_rmse.xlsx")
combined_sd_y <- read_excel("D:/采集数据/数值模拟/moni_sd.xlsx")

# 数据
t_rmse <- 1:nrow(rmse_station_all)
t_sd <- 1:nrow(combined_sd_y)
y_rmse <- rmse_station_all[[1]]
y_sd <- combined_sd_y[[1]]

unpredictability_marker_x <- min(87, length(y_rmse))
fluctuation_marker_x <- min(91, length(y_sd))

t <- 1:200
signal_x <- 91
critical_x <- 100

# Times New Roman
windowsFonts(TNR = windowsFont("Times New Roman"))

# 公共主题：刻度朝外，不显示数字，边框
common_theme <- theme_classic() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),              # 刻度数字不显示
    axis.ticks.length = unit(0.08, "cm"),    # 刻度短一点
    axis.ticks = element_line(size = 0.6),   # 刻度粗一点
    panel.border = element_rect(colour = "black", fill = NA, size = 1.2) # 边框加粗
  )


# 5 个刻度线位置
x_breaks_5 <- function(x) seq(min(x), max(x), length.out = 5)
y_breaks_5 <- function(y) seq(min(y), max(y), length.out = 5)

# 信号线 p1
p1 <- ggplot() +
  annotate("segment", x = signal_x, xend = signal_x, y = 0, yend = 1, color = "white", size = 1.2) +
  coord_cartesian(xlim = c(1, 200), ylim = c(0, 1.5)) +
  scale_x_continuous(breaks = x_breaks_5(1:200), expand = expansion(mult = c(0.03, 0.03))) +
  scale_y_continuous(breaks = y_breaks_5(c(0, 1.5)), expand = c(0, 0)) +
  common_theme

# 临界线 p2
p2 <- ggplot() +
  annotate("segment", x = critical_x, xend = critical_x, y = 0, yend = 1, color = "white", size = 1.2) +
  coord_cartesian(xlim = c(1, 200), ylim = c(0, 1.5)) +
  scale_x_continuous(breaks = x_breaks_5(1:200), expand = expansion(mult = c(0.03, 0.03))) +
  scale_y_continuous(breaks = y_breaks_5(c(0, 1.5)), expand = c(0, 0)) +
  common_theme

# RMSE 曲线 p3
df_rmse <- data.frame(t = t_rmse, y = y_rmse)
p3 <- ggplot(df_rmse, aes(x = t, y = y)) +
  geom_line(color = "#FF7F00", size = 0.8) +
  coord_cartesian(xlim = c(1, max(t_rmse)), ylim = range(y_rmse)*c(0.98, 1.05)) +
  scale_x_continuous(breaks = x_breaks_5(t_rmse), expand = expansion(mult = c(0.03, 0.03))) +
  scale_y_continuous(breaks = y_breaks_5(y_rmse), expand = expansion(mult = c(0.05, 0.1))) +
  common_theme

# SD 曲线 p4
df_sd <- data.frame(t = t_sd, y = y_sd)
p4 <- ggplot(df_sd, aes(x = t, y = y)) +
  geom_line(color = "#1A9850", size = 0.8) +
  coord_cartesian(xlim = c(1, max(t_sd)), ylim = range(y_sd)*c(0.98, 1.05)) +
  scale_x_continuous(breaks = x_breaks_5(t_sd), expand = expansion(mult = c(0.03, 0.03))) +
  scale_y_continuous(breaks = y_breaks_5(y_sd), expand = expansion(mult = c(0.05, 0.1))) +
  common_theme

# 合并四张图
grid.arrange(p3, p4, p1, p2, ncol = 1)



#2026.1.27改

library(ggplot2)
library(readxl)
library(gridExtra)
library(grid)

# 读取数据
rmse_station_all <- read_excel("D:/采集数据/数值模拟/moni_rmse.xlsx")
combined_sd_y <- read_excel("D:/采集数据/数值模拟/moni_sd.xlsx")

# 数据
t_rmse <- 1:nrow(rmse_station_all)
t_sd <- 1:nrow(combined_sd_y)
y_rmse <- rmse_station_all[[1]]
y_sd <- combined_sd_y[[1]]

unpredictability_marker_x <- min(87, length(y_rmse))
fluctuation_marker_x <- min(91, length(y_sd))

t <- 1:200
signal_x <- 91
critical_x <- 100

# Times New Roman
windowsFonts(TNR = windowsFont("Times New Roman"))

# 公共主题：刻度朝外，不显示数字，边框
common_theme <- theme_classic() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),              # 刻度数字不显示
    axis.ticks.length = unit(0.12, "cm"),    # 刻度短一点
    axis.ticks = element_line(size = 1.0),   # 刻度粗一点
    panel.border = element_rect(colour = "black", fill = NA, size = 1.6) # 边框加粗
  )


# 5 个刻度线位置
x_breaks_5 <- function(x) seq(min(x), max(x), length.out = 5)
y_breaks_5 <- function(y) seq(min(y), max(y), length.out = 5)

# RMSE 曲线 p3
df_rmse <- data.frame(t = t_rmse, y = y_rmse)
p3 <- ggplot(df_rmse, aes(x = t, y = y)) +
  geom_line(color = "#FF7F00", size = 1.2) +
  coord_cartesian(xlim = c(1, max(t_rmse)), ylim = range(y_rmse)*c(0.98, 1.05)) +
  scale_x_continuous(breaks = x_breaks_5(t_rmse), expand = expansion(mult = c(0.03, 0.03))) +
  scale_y_continuous(breaks = y_breaks_5(y_rmse), expand = expansion(mult = c(0.05, 0.1))) +
  common_theme

# SD 曲线 p4
df_sd <- data.frame(t = t_sd, y = y_sd)
p4 <- ggplot(df_sd, aes(x = t, y = y)) +
  geom_line(color = "#1A9850", size = 1.2) +
  coord_cartesian(xlim = c(1, max(t_sd)), ylim = range(y_sd)*c(0.98, 1.05)) +
  scale_x_continuous(breaks = x_breaks_5(t_sd), expand = expansion(mult = c(0.03, 0.03))) +
  scale_y_continuous(breaks = y_breaks_5(y_sd), expand = expansion(mult = c(0.05, 0.1))) +
  common_theme

# 合并四张图
grid.arrange(
  p3,
  nullGrob(),   # 中间空白
  p4,
  ncol = 1,
  heights = c(1, 0.08, 1)  # 中间数值越大，空白越大
)