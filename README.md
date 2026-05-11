# Forecasting and Risk Management of Equity Index Volatility

Comparative study of implied volatility, GARCH-family models and HAR-RV forecasts on the S&P 500 using R.

## Overview

This project investigates the dynamics of equity index volatility through several complementary quantitative finance frameworks:

- Implied volatility (VIX)
- Conditional volatility models (GARCH family)
- Realized volatility models (HAR-RV)

The objective is to evaluate whether econometric volatility models can outperform market-implied volatility forecasts and to assess their performance in both volatility forecasting and tail-risk management applications.

The analysis is conducted on the S&P 500 index using daily data from 2015 onward.

---

## Main Features

### Data Processing
- Downloading S&P 500 and VIX data from Yahoo Finance
- Log-return construction
- Standardization and cleaning
- Outlier detection
- Stationarity analysis

### Realized Volatility
- Rolling realized volatility estimation
- Multiple horizons:
  - 5-day
  - 10-day
  - 21-day
  - 63-day

### Implied Volatility
- VIX integration
- Volatility Risk Premium (VRP)
- Implied vs realized volatility analysis

### Volatility Models
- GARCH(1,1)
- EGARCH(1,1)
- GJR-GARCH(1,1)
- Student-t innovations
- HAR-RV model

### Forecasting Framework
- Rolling out-of-sample forecasts
- Moving-window re-estimation
- Train/test split:
  - Train: 2015–2021
  - Test: 2022+

### Forecast Evaluation
- Mean Squared Error (MSE)
- Mean Absolute Error (MAE)
- QLIKE loss

### Statistical Model Comparison
- Diebold-Mariano tests
- Forecast significance analysis

### Risk Management
- Value-at-Risk (VaR)
- Expected Shortfall (ES)
- Kupiec unconditional coverage test
- Christoffersen independence test

---

## Key Results

### Forecasting Performance
- All econometric volatility models outperform the VIX benchmark.
- GARCH Student-t is the strongest performer among GARCH-family models.
- HAR-RV significantly outperforms all competing models across MSE, MAE and QLIKE metrics.

### Risk Management
- EGARCH provides the best VaR backtesting performance.
- Tail-risk forecasting quality differs from volatility forecasting quality.

### Economic Interpretation
The results suggest that:
- realized-volatility persistence contains substantial predictive information,
- implied volatility embeds forward-looking market expectations,
- asymmetric volatility dynamics improve tail-risk estimation.

---

## Repository Structure

```text
project/
│
├── R/
│   ├── data_cleaning.R
│   ├── realized_volatility.R
│   ├── implied_volatility.R
│   ├── garch_models.R
│   ├── har_rv_model.R
│   ├── forecast_evaluation.R
│   ├── diebold_mariano_tests.R
│   └── risk_backtesting.R
│
├── data/
│
├── figures/
│
├── report/
│
└── main.R