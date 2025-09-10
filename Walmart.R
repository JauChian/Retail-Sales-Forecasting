# load required r packages / 載入所需套件
library(IRdisplay)
library(magrittr)
library(tidyverse)
library(scales)
library(gridExtra)
library(forecast)
library(tseries)
library(ggthemes)
library(ggplot2)

# set working directory / 設定工作目錄
setwd("C:/Users/User/Desktop/Unitec/2024Sem2/8811 Data Analytics & Intelligence/Retail Sales Forecasting/")

# Read data / 讀取資料
ur = read.csv("Walmart.csv", header = T)

# 查看数据框的结构 / Check structure of the dataframe
str(ur)

# 查看数据的前几行 / Preview first few rows
head(ur)

# 查看缺失值情况 / Check for missing values
sum(is.na(ur))

# 查看所有变量的描述性统计 / Summary statistics of all variables
summary(ur)

# 查看每家店的数量 / Get unique store IDs
unique_stores = unique(ur$Store)
print(unique_stores)

# 计算有多少家不同的店 / Count number of unique stores
num_stores = length(unique_stores)
print(paste("There are", num_stores, "unique stores in the dataset."))

# 按商店分组，查看每家店的 Weekly_Sales 的描述性统计
# Group by store and compute descriptive statistics of Weekly_Sales
ur %>% group_by(Store) %>% 
  summarise(Min_Sales = min(Weekly_Sales),
            Max_Sales = max(Weekly_Sales),
            Mean_Sales = mean(Weekly_Sales),
            Median_Sales = median(Weekly_Sales),
            Sales_SD = sd(Weekly_Sales))

# 按商店绘制 Weekly_Sales 的箱线图
# Boxplot of Weekly_Sales by Store
ggplot(ur, aes(x = factor(Store), y = Weekly_Sales)) +
  geom_boxplot() +
  labs(title = "Weekly Sales Distribution by Store", x = "Store", y = "Weekly Sales")

# 将日期转换为 Date 格式 / Convert Date column to Date format
ur$Date = as.Date(ur$Date, format = "%d-%m-%Y")

# 按 Store 绘制销售趋势折线图
# Line plot of Weekly Sales trend by Store
ggplot(ur, aes(x = Date, y = Weekly_Sales, color = factor(Store))) +
  geom_line() +
  labs(title = "Weekly Sales Trend by Store", x = "Date", y = "Weekly Sales")

# 按商店绘制销售与温度的散点图
# Scatter plot of Weekly Sales vs Temperature by Store
ggplot(ur, aes(x = Temperature, y = Weekly_Sales, color = factor(Store))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # 添加线性回归线 / Add regression line
  labs(title = "Sales vs Temperature by Store", x = "Temperature", y = "Weekly Sales")

# 确认 Date 列是否转换成功 / Check Date column type
str(ur$Date)

# 将 Date 列从字符类型转换为日期格式 / Ensure Date column is converted to Date
ur$Date = as.Date(ur$Date, format = "%d-%m-%Y")

# 提取月份 / Extract month from Date
ur$Month = format(ur$Date, "%m-%Y")

# 检查月份列的前几行 / Preview first few rows of Month column
head(ur$Month)

# 检查月份是否正确生成 / Count number of unique months
length(unique(ur$Month))

# 按商店绘制每月销售趋势
# Line plot of Monthly Sales trend by Store
ggplot(ur, aes(x = Month, y = Weekly_Sales, color = factor(Store))) +
  geom_line() +
  scale_x_discrete(breaks = unique(ur$Month)[seq(1, length(unique(ur$Month)), by = 3)]) +
  labs(title = "Monthly Sales Trend by Store", x = "Month", y = "Weekly Sales")

# 使用 ggplot2 按商店和节假日标志绘制销售箱线图
# Boxplot of Sales on Holidays vs Non-Holidays by Store
ggplot(ur, aes(x = factor(Holiday_Flag), y = Weekly_Sales, fill = factor(Store))) +
  geom_boxplot() +
  labs(title = "Sales on Holidays vs Non-Holidays by Store",
       x = "Holiday Flag (0 = Non-Holiday, 1 = Holiday)",
       y = "Weekly Sales") +
  scale_fill_discrete(name = "Store") +
  theme_minimal()

# 假设 "Store" 是代表商店的列名，先过滤 Store 1 的数据
# Filter Store 1 data
ur_store1 = ur %>% filter(Store == 1)

# 查看 Store 1 的数据 / Preview Store 1 data
head(ur_store1)

# 确认有多少行数据 / Check number of rows
nrow(ur_store1) 

# 检查 Store 1 的销售数据是否存在 / Summary of Weekly_Sales for Store 1
head(ur_store1$Weekly_Sales)
summary(ur_store1$Weekly_Sales)

# 转换 Date 列为日期格式 / Convert Date for Store 1
ur_store1$Date = as.Date(ur_store1$Date, format = "%d-%m-%Y")

# 确保 Store 1 的销售数据从第 5 周开始
# Create time series object for Store 1 (start from week 5, 2010)
ur_store1_sales = ts(ur_store1$Weekly_Sales, frequency = 52, start = c(2010, 5))

# 检查时间序列对象是否正确创建 / Inspect time series object
ur_store1_sales

# 对 Store 1 的销售数据进行 STL 分解
# STL decomposition for Store 1 Weekly Sales
ur_store1_stl = stl(ur_store1_sales, s.window = "periodic")

# 绘制 STL 分解结果 / Plot STL decomposition
autoplot(ur_store1_stl)

# 基于 STL 分解进行未来 52 周的预测
# Forecast next 52 weeks based on STL decomposition
ur_store1_forecast = forecast(ur_store1_stl, h = 52, level = 80)

# 绘制预测结果 / Plot forecast
autoplot(ur_store1_forecast)


####################################################
# EDA函数，分析每个特征与 Weekly_Sales 的关系
# EDA function to analyze relationship between features and Weekly_Sales
eda_analysis <- function(data) {
  
  # 描述性统计 / Descriptive statistics
  print("Descriptive Statistics for Weekly_Sales")
  print(summary(data$Weekly_Sales))
  
  # 1. Weekly_Sales vs Temperature
  ggplot(data, aes(x = Temperature, y = Weekly_Sales)) +
    geom_point() +
    geom_smooth(method = "lm", color = "red") +
    labs(title = "Sales vs Temperature", x = "Temperature", y = "Weekly Sales")
  
  # 2. Weekly_Sales vs Fuel_Price
  ggplot(data, aes(x = Fuel_Price, y = Weekly_Sales)) +
    geom_point() +
    geom_smooth(method = "lm", color = "blue") +
    labs(title = "Sales vs Fuel Price", x = "Fuel Price", y = "Weekly Sales")
  
  # 3. Weekly_Sales vs Holiday_Flag
  ggplot(data, aes(x = factor(Holiday_Flag), y = Weekly_Sales)) +
    geom_boxplot() +
    labs(title = "Sales on Holidays vs Non-Holidays", 
         x = "Holiday Flag (0 = Non-Holiday, 1 = Holiday)", y = "Weekly Sales")
  
  # 4. Weekly_Sales vs CPI
  ggplot(data, aes(x = CPI, y = Weekly_Sales)) +
    geom_point() +
    geom_smooth(method = "lm", color = "green") +
    labs(title = "Sales vs CPI", x = "CPI", y = "Weekly Sales")
  
  # 5. Weekly_Sales vs Unemployment
  ggplot(data, aes(x = Unemployment, y = Weekly_Sales)) +
    geom_point() +
    geom_smooth(method = "lm", color = "purple") +
    labs(title = "Sales vs Unemployment", x = "Unemployment", y = "Weekly Sales")
  
}

# 预测函数，使用时间序列模型进行未来销售预测
# Forecast function using time series model for future sales
forecast_sales <- function(data, store_number) {
  
  # 过滤数据，只分析特定商店 / Filter data for given store
  data_store = data %>% filter(Store == store_number)
  
  # 将 Date 列转换为日期格式 / Convert Date column
  data_store$Date = as.Date(data_store$Date, format = "%d-%m-%Y")
  
  # 创建时间序列对象 / Create time series object
  sales_ts = ts(data_store$Weekly_Sales, frequency = 52, start = c(2010, 5))  # 假设从2010年第5周开始 / Assume from week 5, 2010
  
  # STL 分解 / STL decomposition
  stl_decomposition = stl(sales_ts, s.window = "periodic")
  
  # 绘制 STL 分解结果 / Plot STL decomposition
  autoplot(stl_decomposition) + 
    labs(title = paste("STL Decomposition for Store", store_number))
  
  # 预测未来52周 / Forecast next 52 weeks
  forecast_result = forecast(stl_decomposition, h = 52)
  
  # 绘制预测结果 / Plot forecast
  autoplot(forecast_result) + 
    labs(title = paste("52-week Sales Forecast for Store", store_number))
  
  # 返回预测结果 / Return forecast results
  return(forecast_result)
}

# 调用 EDA 函数分析 Store 1 / Run EDA for Store 1
eda_analysis(ur %>% filter(Store == 1))

# 调用预测函数，预测 Store 1 的销售 / Run forecast for Store 1
forecast_sales(ur, 1)
