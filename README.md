# Time Series Forecasting with ARIMA, ETS, and Decomposition Methods in R

This project focuses on advanced time series forecasting using various models, including ARIMA, ETS, and STL decomposition. The analysis focuses on datasets from different domains, including Olympic sprint times, global population, GDP, tourism, and industrial production.

## Objectives
- Model time series data using ARIMA, ETS, and decomposition methods (STL, Holt-Winters)
- Compare forecasting models based on accuracy and residual diagnostics
- Forecast future values and evaluate assumptions (e.g., stationarity, transformations)

## Tools & Libraries
- **Language:** R
- **Libraries:** fpp3, ggplot2, dplyr, tsibble, feasts, forecast

## Project Highlights
- **Olympic Sprint Times (1896–2016):** Visualized winning times and forecasted 2020 Olympic results.
- **Afghanistan Population Trends:** Analyzed population data and compared trend models.
- **Tourism Arrivals (1981–2012):** Compared multiple time series models for tourism data.
- **U.S. GDP Forecasting:** Applied ARIMA and ETS models, including Box-Cox transformations.
- **Australian Industrial Production:** Forecasted industrial production and assessed model accuracy.

## Techniques Used
- Regression modeling
- Time series decomposition (STL)
- ARIMA modeling
- Exponential smoothing (ETS, Holt-Winters)
- Box-Cox transformation
- Forecast accuracy evaluation (RMSE, MAE)

## Structure
- `.Rmd` notebook with full analysis, plots, and commentary.
- `.R` script for the forecasting pipeline.
- Visual outputs: trend lines, residual plots, decomposition graphs, forecast intervals.

## Skills Demonstrated
- Advanced forecasting and model comparison
- Time series transformation and stationarity testing
- Interpretation of residuals and diagnostics
- Communication of statistical results through visualizations
