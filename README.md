# Time Series Forecasting with ARIMA, ETS, and Decomposition Methods in R

## Project Overview

This project evaluates classical time series forecasting methods, including ARIMA, ETS / Holt-Winters, and STL decomposition, across diverse real-world datasets such as GDP, population, tourism, industrial production, and Olympic sprint times.

The goal is to compare model behavior, accuracy, and assumptions under different data characteristics, using diagnostic checks and data transformations (e.g., Box–Cox) to ensure robust forecasts. This project demonstrates the application of advanced time series techniques to multiple domains, highlighting both predictive performance and interpretability.

## Technologies
- R (fpp3, forecast, tsibble, feasts, ggplot2, dplyr)

## Techniques Used
- Regression modeling
- Time series decomposition (STL)
- ARIMA modeling
- Exponential smoothing (ETS, Holt-Winters)
- Box-Cox transformation
- Forecast accuracy evaluation (RMSE, MAE)
- Residual diagnostics


## Project Highlights
- **Olympic Sprint Times (1896–2016):** Visualized winning times and forecasted 2020 Olympic results.
- **Afghanistan Population Trends:** Analyzed population data and compared trend models.
- **Tourism Arrivals (1981–2012):** Compared multiple time series models for tourism data.
- **U.S. GDP Forecasting:** Applied ARIMA and ETS models, including Box-Cox transformations.
- **Australian Industrial Production:** Forecasted industrial production and assessed model accuracy.

## Results and conclusions

The project demonstrates that different time series models perform best under different conditions:

- **ARIMA models:** Effectively captured stationary or differenced economic series (e.g., GDP, industrial production).  
- **ETS models:** Performed well on datasets with strong trend and seasonality (e.g., tourism arrivals).  
- **STL decomposition:** Allowed clear separation of trend, seasonality, and remainder components, improving interpretability.  
- **Box–Cox transformations and diagnostic checks:** Improved forecast stability and validated model assumptions.

Overall, the study highlights the importance of model selection based on data characteristics, rigorous evaluation of residuals, and visualizations to support forecast interpretation. This project showcases practical applications of classical time series forecasting techniques across multiple domains.
