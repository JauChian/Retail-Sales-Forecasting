# Walmart Sales Forecasting (R + Python)

This repository contains an end‑to‑end analysis of Walmart weekly retail sales. It combines **R** (EDA + STL decomposition) and **Python** (STL decomposition, SARIMA/SARIMAX, Prophet) to explore patterns and generate forecasts.

> Notebook: `Walmart.ipynb`

---

## Dataset
- **Source file(s):** Walmart.csv
- **Granularity:** Weekly sales per store
- **Typical fields:** `Store`, `Date`, `Weekly_Sales`, `Holiday_Flag`, `Temperature`, `Fuel_Price`, `CPI`, `Unemployment`

---

## Tech Stack
- **Python libs detected:** datetime, itertools, keras, math, matplotlib, numpy, pandas, pmdarima, prophet, seaborn, sklearn, statsmodels, tensorflow, warnings
- **R (separate script recommended):** `tidyverse`, `ggplot2`, `forecast`, `lubridate`

---

## What This Project Does
- Exploratory Data Analysis (store distributions, weekly/monthly trends)
- Holiday vs non‑holiday comparisons
- Time series decomposition (trend / seasonality / residuals) via STL
- Forecasting with STL decomposition, SARIMA/SARIMAX, Prophet
- Visualizations suitable for dashboard prototypes (store filter, time range)

---

## Key Results (placeholders)
- **Boxplot:** `figures/boxplot.png`
- **Trend / Decomposition:** `figures/stl_decomposition.png`
- **Forecast:** `figures/forecast.png`


## Method Notes
- **Data frequency:** weekly; ensure a continuous weekly index (fill missing weeks when modeling).
- **Seasonality:** retail sales typically show annual seasonality → SARIMA with m=52 or STL with frequency=52.
- **Validation:** hold‑out the last k weeks for evaluation (e.g., MAPE/RMSE).



