library(readxl)

# 读取数据
GNSS <- read_excel("D:/3.17图/数据/11222/GNSS.xlsx")
ts_data <- data.matrix(GNSS)


# ------------------- SVG 矢量图输出 -------------------
svg(
  filename = "D:/4_41.svg",
  width = 5,   # 单位：英寸（可根据需要调整）
  height = 4
)

# ------------------- 全局绘图参数 -------------------
par(
  family = "Times",     # 建议用 Times（兼容性更好）
  mfrow = c(4, 2),      # 5行2列
  mar = c(0.3, 0.8, 0.2, 0.2),
  oma = c(0, 0, 0, 0),
  las = 1,
  mgp = c(2.3, 0.5, 0),
  tcl = -0.2,
  xaxs = "i",
  yaxs = "i"
)

colors_vec <- rep("#4E79A7", ncol(ts_data))

# ------------------- 纵轴范围（保留原有结构，统一改为0-1） -------------------
# 适配12列数据，全部改为0-1范围
y_ranges <- list(
  c(0, 1), c(0, 1), c(0, 1), c(0, 1), 
  c(0, 1), c(0, 1), c(0, 1), c(0, 1)
)

# 刻度数量：全部改为6个刻度（适配0-1范围）
y_ticks_num <- rep(2, 8)

# ------------------- 绘图 -------------------
for (i in 1:ncol(ts_data)) {
  
  y <- ts_data[, i]
  if (all(is.na(y))) next
  
  # ---------- 横轴 ----------
  x_base <- c(0, 370)  # 修改为0-400
  x_range <- diff(x_base)
  x_pad <- 0.05 * x_range
  xlim <- c(x_base[1] - x_pad, x_base[2] + x_pad)
  
  x_ticks <- seq(0, 370, length.out = 2)  # 修改为0-400，5个刻度
  
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
    lwd = 2,
    xlab = "",
    ylab = "",
    xaxt = "n",
    yaxt = "n",
    xlim = xlim,
    ylim = ylim
  )
  
  # 横轴刻度（基于原始范围）
  #axis(1,
  #     at = x_ticks,
  #     labels = FALSE,
  #     tcl = 0.3,
  #     lwd = 2)
  
  # 纵轴刻度（基于原始范围）
  #axis(2,
  #     at = y_ticks,
  #     labels = FALSE,
  #     tcl = 0.3,
  #     lwd = 2)
  
  box(lwd = 2)
}

dev.off()



# 清理环境
rm(list = ls())

library(readxl)

# 读取数据
rmse <- read_excel("D:/3.17图/数据/11222/rmse.xlsx")
ts_data <- data.matrix(rmse)

# ------------------- SVG 矢量图输出 -------------------
svg(
  filename = "D:/4_42.svg",
  width = 5,   # 单位：英寸（可根据需要调整）
  height = 4
)

# ------------------- 全局绘图参数 -------------------
par(
  family = "Times",     # 建议用 Times（兼容性更好）
  mfrow = c(4, 2),      # 5行2列
  mar = c(0.3, 0.8, 0.2, 0.2),
  oma = c(0, 0, 0, 0),
  las = 1,
  mgp = c(2.3, 0.5, 0),
  tcl = -0.2,
  xaxs = "i",
  yaxs = "i"
)

colors_vec <- rep("#F28E2B", ncol(ts_data))

# ------------------- 纵轴范围（保留原有结构，统一改为0-1） -------------------
# 适配12列数据，全部改为0-1范围
y_ranges <- list(
  c(0, 1.0),
  c(0, 0.6),
  c(0, 1.2),
  c(0, 0.5),
  c(0, 0.5),
  c(0, 0.4),
  c(0, 0.8),
  c(0, 0.8)
)

# 刻度数量
y_ticks_num <- c(2, 2, 2, 2, 2, 2, 2, 2)

# ------------------- 绘图 -------------------
for (i in 1:ncol(ts_data)) {
  
  y <- ts_data[, i]
  if (all(is.na(y))) next
  
  # ---------- 横轴 ----------
  x_base <- c(0, 370)  # 修改为0-400
  x_range <- diff(x_base)
  x_pad <- 0.05 * x_range
  xlim <- c(x_base[1] - x_pad, x_base[2] + x_pad)
  
  x_ticks <- seq(0, 370, length.out = 2)  # 修改为0-400，5个刻度
  
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
    lwd = 2,
    xlab = "",
    ylab = "",
    xaxt = "n",
    yaxt = "n",
    xlim = xlim,
    ylim = ylim
  )
  
  # 横轴刻度（基于原始范围）
  #axis(1,
  #     at = x_ticks,
  #     labels = FALSE,
  #     tcl = 0.3,
  #     lwd = 2)
  
  # 纵轴刻度（基于原始范围）
  #axis(2,
  #     at = y_ticks,
  #     labels = FALSE,
  #     tcl = 0.3,
  #     lwd = 2)
  
  box(lwd = 2)
}

dev.off()



#4-3图，3.19晚上新改代码
# ------------------- 4-3 图（单图最终版） -------------------
library(readxl)

# 读取数据
sd <- read_excel("D:/3.17图/数据/11222/sd.xlsx")
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
  mgp = c(2.3, 0.5, 0),
  tcl = -0.3,
  xaxs = "i",
  yaxs = "i"
)


# ------------------- 横轴 -------------------
x_base <- c(0, 370)
x_pad <- 0.05 * diff(x_base)
xlim <- c(x_base[1] - x_pad, x_base[2] + x_pad)

x_ticks <- seq(0, 370, length.out = 5)

# ------------------- 纵轴 -------------------
y_base <- c(0, 1000)
y_pad <- 0.04 * diff(y_base)
ylim <- c(y_base[1] - y_pad, y_base[2] + y_pad)

y_ticks <- seq(0, 1000, length.out = 6)

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
#axis(1,
#     at = x_ticks,
#     labels = FALSE,
#     tcl = 0.3,
#     lwd = 2.5)

# 纵轴
#axis(2,
#     at = y_ticks,
#     labels = FALSE,
#     tcl = 0.3,
#     lwd = 2.5)

# 边框
box(lwd = 2)

# ------------------- 关闭设备 -------------------
dev.off()




#4-4图，3.19晚上新改代码
# ------------------- 4-4 图（双列数据最终版） -------------------
library(readxl)

# 读取数据
pzhi <- read_excel("D:/3.17图/数据/11222/pzhi.xlsx")
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
x_base <- c(0, 355)
x_pad <- 0.03 * diff(x_base)
xlim <- c(x_base[1] - x_pad, x_base[2] + x_pad)
x_ticks <- seq(0, 355, length.out = 2)

# ------------------- 纵轴 -------------------
y_base <- c(0, 1)
y_pad <- 0.04 * diff(y_base)
ylim <- c(y_base[1] - y_pad, y_base[2] + y_pad)
y_ticks <- seq(0, 1, length.out = 3)

# ------------------- 绘图 -------------------
# 绘制第一列
plot(
  ts_data[1:355, 1],            # 第一列，长度 185
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
  ts_data[1:355, 2],            # 第二列，长度 190
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