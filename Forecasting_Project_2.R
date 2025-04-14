library(fpp3)
library(latex2exp)
library(readr)
library(fable)
library(tsibble)
library(feasts)
library(fpp2)
library(purrr)
library(fpp3)

#Olympic Games data
ggplot(olympic_running, aes(x = Year, y = Time, color = as.factor(Length), linetype = Sex)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Figure 1: Olympic Winning Times for Track Events",
    x = "Year",
    y = "Winning Time in seconds",
    color = "Event Length in meters",
    linetype = "Gender"
  )

#Figure 1 illustrates the winning times for various track events for the Olympic Games starting from the late 1896 to 2016. A distinct feature of the plot are the gaps in data around the 1910s and the 1930s/40s. Due to the geo-political state of the world during that time, those scheduled Olympic Games were cancelled. This is because those time periods happen to coincide with the outbreak of World War 1 and World War 2. Another feature is the slight downward trend present in the event, especially in the 10000 meter event. This could be attributed to advancements in training equipment and competition, this trend is not only limited to running times but some other sports as well. The last distinct attribute of the plot is the difference in running times between men and women. Which can be attributed to biological/biomechanical differences in men and women.

ggplot(olympic_running, aes(x = Year, y = Time, color = as.factor(Length), linetype = Sex)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Figure 2: Olympic Winning Times for Track Events (Fitted Regression Line)",
    x = "Year",
    y = "Winning Time in seconds",
    color = "Event Length in meters",
    linetype = "Gender")
    
#Figure 2 is the exact same plot as Figure 1 but with a regression line fitted to it. As was partially observed in Figure 1 a downward trend in most events is evident, which means that winning times have been decreasing.
    
    filtered_data <- olympic_running |>
      filter(!is.na(Time)) |> 
      group_by(Length, Sex) |>
      filter(n() >= 2) |>              
      filter(diff(range(Time, na.rm = TRUE)) > 0) |> 
      ungroup()
    
    slopes <- filtered_data |> 
      group_by(Length, Sex) |> 
      summarise(
        rate_per_year = coef(lm(Time ~ Year))[1],
        .groups = "drop"
      )
    olympic_running_tibble <- as_tibble(olympic_running)
    
    filtered_data <- olympic_running_tibble |>
      filter(!is.na(Time)) |> 
      group_by(Length, Sex) |>
      filter(n() >= 2) |>              
      filter(diff(range(Time, na.rm = TRUE)) > 0) |> 
      ungroup()
    
    slopes <- filtered_data |> 
      group_by(Length, Sex) |> 
      summarise(
        rate_per_year = coef(lm(Time ~ Year))[2],  # Extract the slope (rate per year)
        .groups = "drop"
      )
    
    average_decrease_per_year <- olympic_running_tibble |>
      filter(!is.na(Time)) |>
      group_by(Year) |>
      summarise(
        avg_rate = mean(coef(lm(Time ~ Year))[1], na.rm = TRUE),  # Average slope for that year
        .groups = "drop"
      )
    
    ggplot(average_decrease_per_year, aes(x = Year, y = avg_rate)) +
      geom_line() +
      geom_point() +
      labs(
        title = "Figure 3: Average Rate of Decrease in Olympic Track Event Winning Times ",
        x = "Year",
        y = "Average Rate of Decrease (seconds/year)"
      )
    
    
##Figure 3 shows a graph of the average rate of decrease in seconds in winning times per year. The plot shows rapid performance improvements of around 450 seconds/year by the 1920s. Improvements slowed to about 200 seconds per year from the 1930s to 1980s which could be attributed to the Great Depression and World War 2. Around the 1980s, advances in technology and global participation revived improvement rates to 400 seconds/year, stabilizing in the 2000s.
    
    olympic_running_clean <- olympic_running |>
      filter(!is.na(Time) & !is.na(Year))
    
    linear_model <- lm(Time ~ Year, data = olympic_running_clean)
    
    olympic_running_with_residuals <- olympic_running_clean |>
      mutate(residuals = residuals(linear_model))
    
    ggplot(olympic_running_with_residuals, aes(x = Year, y = residuals)) +
      geom_point() +
      labs(
        title = "Figure 4: Residuals of Fitted Winning Times",
        x = "Year",
        y = "Residuals"
      ) 
    
#Figure 4 shows the differences between observed and predicted winning times per year. A linear model generates predicted winning times for each year. The residuals represent how far off the model's predictions are: points near zero mean good predictions, while larger residuals indicate poorer fits. The residuals are not randomly scattered but show patterns, indicating the linear model may not capture the underlying trends.
    
    olympic_running_clean <- olympic_running |>
      filter(!is.na(Time) & !is.na(Year))
    
    #Unique combinations of event length and sex
    unique_events <- olympic_running_clean |>
      distinct(Length, Sex)
    
    #Initialize an empty data frame to store predictions
    predictions <- tibble(Length = numeric(),
                          Sex = character(),
                          Predicted_Winning_Time = numeric())
    
    #Loop through each unique event and predict 2020 winning time
    for (i in 1:nrow(unique_events)) {
      length <- unique_events$Length[i]
      sex <- unique_events$Sex[i]
      
      event_data <- olympic_running_clean |>
        filter(Length == length, Sex == sex)
      
      linear_model <- lm(Time ~ Year, data = event_data)
      newdata <- data.frame(Year = 2020)
      predicted_time <- predict(linear_model, newdata)
      
      predictions <- predictions |>
        add_row(Length = length, Sex = sex, Predicted_Winning_Time = predicted_time)
    }
    
    # View the predicted times for 2020
    print(predictions)
    
#The tsibble above shows the predicted winning times for the year 2020, each unique event is displayed by seperating gender and the length of the event. An assumption made is a linear relationship between Year and Time for each event, with independent observations and sufficient data for reliable predictions. It also assumes that historical trends remain consistent through 2020.
    
#Global Economy data (analysis for Afghanistan)
#At first the data was imported from the "global_economy".
    
    data("global_economy")
    afghanistan_data <- global_economy %>%
      filter(Country == "Afghanistan") %>%
      select(Year, Population)
#After plotting, it was revealed that the Afghanistan data has no seasonality. However, it has a a strong increasing trend which is slightly non linear. The non-linearity pattern occurs in the time of the Soviet-Afghan war which occured in 1980-89. In this time these seemed to be a steep population decline and later after the war the population started increasing again.
    
#Plot the data
    afghanistan_data %>%
      as_tsibble(index = Year)%>%
      autoplot(Population) +
      labs(title = "Annual Population of Afganistan", x = "Year", y = "Population") +
      theme_minimal()
    
    
#The linear model was fitted to trended afghanistan_data.
    
#Fit linear model and compare this to piecewise l
#linear model
    linear_model <- afghanistan_data %>%
      model(linear_trend = TSLM(Population ~ trend()))
    
#Later the piecewise model was fitted as well. The approach to this model required 3 different trends for the data before war, during the war and after which was achieved by applying mutete() and specifying trend preriods.
#fit piecewise model
    afghanistan_data2 <- global_economy %>%
      filter(Country == "Afghanistan") %>%
      select(Year, Population)
    
    afghanistan_data2 <- afghanistan_data2 %>%
      mutate(
        trend1 = Year - 1970,  # Main trend variable (for the entire time period)
        trend2 = if_else(Year >= 1980, Year - 1980, 0),  # Trend after 1980
        trend3 = if_else(Year >= 1989, Year - 1989, 0)   # Trend after 1989
      )
    piecewise_model <- afghanistan_data2 %>%
      model(piecewise_trend = TSLM(Population ~ trend1 + trend2 + trend3))
#The report of linear model revealed that trend and intercept of data are significant, the model is significant as p-value is very small. The residual error for the model is 3190000, R2 is 0.838 which evaluates fit of model to data. Adjusted R2 is 0.835 and will be useful for models comparison.
#compare (adjusted R2, resudual error. r2, visual comparison, residuals)
    report(linear_model)

#The report of piecewise model shows that the intercept and 3 trends are significant and that model is significant (all p-values are small). The residual error (RSE) is 301000, R2 0.999 and adjusted R2 is 0.999.
report(piecewise_model)
    
#To plot the data both models were first augumented, mutatate() was applied and later data was binded together. Later autoplot() of models was fitted on actual data. The plot confirm the results reflected by the reports() of model. Piecewise model provides a fit very close to actual values which is supported by R2 being 0.999 (showing very small difference from actual population) and its fit is closer than linear models fit (linear model's R2 is 0.835). The RSE predictive accuracy is also higher for piecewise model which shows that this model is more accurate. Adjusted R2 of Piecewise model is also higher for piecewise, this value is used for models comparisons as it does account for number of predictors (in contrary to R2) indicating that piecewise model is better choise. However, it is worth to mention that very close fit to training data may result in model overfitting in predictions.
    
# Plot actual vs fitted values for both
# Augment the linear model with fitted values and add a "Model" column
    linear_augmented <- augment(linear_model) %>%
      mutate(Model = "Linear Model")
    
# Augment the piecewise model with fitted values and add a "Model" column
    piecewise_augmented <- augment(piecewise_model) %>%
      mutate(Model = "Piecewise Model")
# Combine both augmented datasets
    augmented_data <- bind_rows(linear_augmented, piecewise_augmented)
    
# Now, plot actual vs fitted values= better graph
    augmented_data %>%
      autoplot(Population) +
      geom_line(aes(y = .fitted, color = Model), size = 1) +
      labs(title = "Actual vs Fitted Values",
           x = "Year", y = "Population", color = "Model") +
      scale_color_manual(values = c("Linear Model" = "blue", "Piecewise Model" = "red"))
    
#Later the forecasts for 5 years ahead was generated for both models. In case of the piecewise model, the trends had to be extracted and converted to tibble and tsibble with index = Year first, later the forecast was generated. Next an autoplot of of actual data, forecasts, and prediction intervals (80% and 95%) was fitted. From the autoplots it is visible that piecewise model's predictions naturally follow the flow of population data, whereas in linear model those appear much lower that the data trend. This means that linear model seems to not capture trend of actual data thus may be too simple. Moreover, prediction intervals describing uncertainty associated with forecasts are wider for linear model, thus forecasts has larger margine of error. Meanwhile, the piecewise model's predictions are narrow, indicating small margin of errors in forecasts. Taking into consideration both model fit measures and visual aspects of models, the piecewise model is a better choice for afghanistan population data.
    
#The accuracy measures such as MSE, RMSE could not be generated as the prediction is 5 years ahead of data so the training set is not available.
    
    
#forecast mean = point forecast
#models
#linear
    linear_forecast <- linear_model %>%
      forecast(h = 5, level = c(80, 95))
    
#piecewise
    future_years_piecewise <- tibble(Year = 2018:2022) %>%
      mutate(
        trend1 = Year - 1970,   # trend1 for the entire period
        trend2 = pmax(Year - 1980, 0),  # trend2 for years after 1980
        trend3 = pmax(Year - 1989, 0)   # trend3 for years after 1989
      )
    future_years_piecewise_tsibble <- future_years_piecewise %>%
      as_tsibble(index = Year)
    
    piecewise_forecast <- piecewise_model %>%
      forecast(new_data = future_years_piecewise_tsibble, level = c(80, 95))
    
    autoplot(afghanistan_data, series = "Actual") +
      autolayer(linear_forecast, series = "Forecast", 
                level = c(80, 95), alpha = 0.3) +  # Alpha controls transparency of the interval
      labs(title = "Population Forecast for Afghanistan with Prediction Intervals",
           x = "Year",
           y = "Population") +
      theme_minimal()
    
    autoplot(afghanistan_data, series = "Actual") +
      autolayer(piecewise_forecast, series = "Forecast", 
                level = c(80, 95), alpha = 0.3) +  # Alpha controls transparency of the interval
      labs(title = "Piecewise Population Forecast for Afghanistan with Prediction Intervals",
           x = "Year",
           y = "Population") +
      theme_minimal()
    
#Arrivals Data
    
    unique(aus_arrivals$Origin)
    
#Filter again for New Zealand arrivals based on exact match
    aus_nz <- aus_arrivals %>% filter(Origin == "NZ")
    # Plot the time series for New Zealand arrivals
    ggplot(aus_nz, aes(x = Quarter, y = Arrivals)) +
      geom_line() +
      labs(title = "Quarterly Arrivals to Australia from New Zealand",
           x = "Quarter",
           y = "Number of Arrivals") +
      theme_minimal()
    
#Trend: Clear upward trend, with arrivals increasing significantly over time.  
#Seasonality: Strong seasonal patterns with periodic peaks and troughs each year.  
#Level: Shifted upward; early values below 200,000, later exceeding 300,000.  
#Variation: Increasing amplitude over time, indicating **multiplicative seasonality**.  
#Stationarity: Non-stationary due to the upward trend and growing seasonal variation.  
    
#These features justify using the Holt-Winters multiplicative method to handle the trend and seasonality.  
    
# Split the data into training and test sets
    aus_train <- aus_nz %>%
      filter(Quarter < yearquarter("2010 Q4"))  # Training set excludes the last 2 years
    aus_test <- aus_nz %>%
      filter(Quarter >= yearquarter("2010 Q4"))  # Test set includes the last 2 years
    
    # Fit Holt-Winters' multiplicative model to the training set
    hw_multiplicative_model <- aus_train %>%
      model(
        HW = ETS(Arrivals ~ error("M") + trend("A") + season("M"))
      )
    
# Generate forecasts for the test period
    hw_forecasts <- hw_multiplicative_model %>%
      forecast(h = "2 years")  # Forecast horizon matches the test set
# Plot the forecasts against the actual data
    hw_forecasts %>%
      autoplot(aus_nz, level = NULL) +
      labs(
        title = "Holt-Winters' Multiplicative Method Forecast",
        x = "Quarter",
        y = "Number of Arrivals"
      ) +
      theme_minimal()
    

#Why is Multiplicative Seasonality Necessary?
#Seasonal variation grows proportionally with the trend, as seen in the increasing amplitude over time.  
#Additive seasonality cannot capture this proportional relationship, making the multiplicative approach more appropriate.  
#The Holt-Winters' multiplicative method effectively models the upward trend and scaling seasonal effects, aligning closely with the observed data.  

# Fit ETS model to the training set
ets_model <- aus_train %>%
  model(
    ETS = ETS(Arrivals)
  )

# Generate forecasts for the test period
ets_forecasts <- ets_model %>%
  forecast(h = "2 years")  # Forecast horizon matches the test set

# Plot the forecasts against the actual data
ets_forecasts %>%
  autoplot(aus_nz, level = NULL) +
  labs(
    title = "ETS Model Forecast",
    x = "Quarter",
    y = "Number of Arrivals"
  ) +
  theme_minimal()


# Log-transform the series (ensure we are using the training data)
aus_log_train <- aus_train %>%
  mutate(Log_Arrivals = log(Arrivals))

# Fit an additive ETS model to the log-transformed data (no need to specify trend and season)
ets_log_model <- aus_log_train %>%
  model(
    ETS = ETS(Log_Arrivals)  # Additive model by default
  )

# Generate forecasts for the test period (on the log scale)
ets_log_forecasts <- ets_log_model %>%
  forecast(h = "2 years")

# Back-transform the forecasts to the original scale (exp to undo the log transformation)
ets_log_forecasts <- ets_log_forecasts %>%
  mutate(.mean = exp(.mean))  # Only back-transform the 'mean' column

# Merge the forecasts with the actual data (aus_nz)
forecast_with_actuals_log <- ets_log_forecasts %>%
  left_join(aus_nz, by = "Quarter")

# Plot the forecasts along with the actual data
ggplot(forecast_with_actuals_log, aes(x = Quarter)) +
  geom_line(aes(y = Arrivals), color = "blue") +  # Actual data
  geom_line(aes(y = .mean), color = "red") +  # Forecasts
  labs(
    title = "Additive ETS Model on Log-Transformed Data",
    x = "Quarter",
    y = "Number of Arrivals"
  ) +
  theme_minimal()


seasonal_naive_model <- aus_train %>%
  model(
    seasonal_naive = SNAIVE(Arrivals ~ season())
  )

# Generate forecasts for the test period
seasonal_naive_forecasts <- seasonal_naive_model %>%
  forecast(h = "2 years")

# Plot the forecasts against the actual data
seasonal_naive_forecasts %>%
  autoplot(aus_nz, level = NULL) +
  labs(
    title = "Seasonal Naive Method Forecast",
    x = "Quarter",
    y = "Number of Arrivals"
  ) +
  theme_minimal()

# STL decomposition on the log-transformed data
aus_log_train_stl <- aus_log_train %>%
  model(stl = STL(Log_Arrivals))  # Apply STL decomposition

# Extract the seasonally adjusted data (trend + remainder)
aus_log_trend_remainder <- aus_log_train_stl %>%
  components() %>%
  mutate(Seasonally_Adjusted = trend + remainder) %>%
  select(Quarter, Seasonally_Adjusted)

# Fit an ETS model to the seasonally adjusted (trend + remainder) data
ets_seasonally_adjusted_model <- aus_log_trend_remainder %>%
  model(
    ETS = ETS(Seasonally_Adjusted)
  )

# Generate forecasts for the test period (on the seasonally adjusted scale)
ets_seasonally_adjusted_forecasts <- ets_seasonally_adjusted_model %>%
  forecast(h = "2 years")

# Back-transform the forecasts to the original scale (exp to undo the log transformation)
ets_seasonally_adjusted_forecasts <- ets_seasonally_adjusted_forecasts %>%
  mutate(.mean = exp(.mean))  # Only back-transform the 'mean' column

# Merge the forecasts with the actual data (aus_nz)
forecast_with_actuals <- ets_seasonally_adjusted_forecasts %>%
  left_join(aus_nz, by = "Quarter")

# Plot the forecasts along with the actual data
ggplot(forecast_with_actuals, aes(x = Quarter)) +
  geom_line(aes(y = Arrivals), color = "blue") +  # Actual data
  geom_line(aes(y = .mean), color = "red") +  # Forecasts
  labs(
    title = "ETS Model on Seasonally Adjusted Data (Log-Transformed)",
    x = "Quarter",
    y = "Number of Arrivals"
  ) +
  theme_minimal()

# Accuracy for Holt-Winters' Multiplicative Method
    accuracy(hw_forecasts, aus_test)
    
# Accuracy for ETS model
    accuracy(ets_forecasts, aus_test)
    
# Accuracy for Additive ETS model on log-transformed data
#accuracy(ets_log_forecasts, aus_test)
    
# Accuracy for Seasonal Naive method
    accuracy(seasonal_naive_forecasts, aus_test)
    
# Correctly merge the forecasts with actual data for STL + ETS model
    forecast_with_actuals_stl <- ets_seasonally_adjusted_forecasts %>%
      mutate(.mean = exp(.mean)) %>%
      left_join(aus_test, by = "Quarter")
    
# Now calculate accuracy using the actual 'Arrivals' and the forecasted '.mean'
#accuracy(forecast_with_actuals_stl, aus_test)
    
# For Holt-Winters' multiplicative method
    hw_residuals <- residuals(hw_multiplicative_model)
    autoplot(hw_residuals) + ggtitle("Holt-Winters' Residuals")
    
# For ETS model
    ets_residuals <- residuals(ets_model)
    autoplot(ets_residuals) + ggtitle("ETS Model Residuals")
    
# For Additive ETS model on log-transformed data
    ets_log_residuals <- residuals(ets_log_model)
    autoplot(ets_log_residuals) + ggtitle("Additive ETS Residuals (Log-transformed)")
    
# For Seasonal Naive method
    seasonal_naive_residuals <- residuals(seasonal_naive_model)
    autoplot(seasonal_naive_residuals) + ggtitle("Seasonal Naive Method Residuals")
    
# For STL + ETS on seasonally adjusted data
    ets_seasonally_adjusted_residuals <- residuals(ets_seasonally_adjusted_model)
    autoplot(ets_seasonally_adjusted_residuals) + ggtitle("STL + ETS Residuals")
    
    
##GDP data
    
#First, the GDP data was extracted from global economy and plotted using autoplot(). The autoplot revealed that GDP data has an increasing trend, with unusual decrease around 2008, which most likelly happed due to Great Depression. Moreover, the data seems to be slightly exponential, consequently the Box-Cox transformation is required.

    data("global_economy")
    us_gdp <- global_economy %>%
      filter(Country == "United States") %>%
      select(Year, GDP)
    
    
    #plot is slightly exponential so transformation is needed
    us_gdp %>%
      autoplot(GDP) + 
      labs(title = "U.S. GDP", y = "GDP", x = "Year")
    
    
#To make data more linear, Box-Cox transformation with optimal lambda = 0.282 was applied. Later, the transformed GDP data was plotted. The plot shows more linear pattern than the one before transformation, consequently the transformation linearised the data.
    
# Box-Cox transformation with optimal lambda
    us_gdp <- us_gdp %>% as_tsibble(index = Year)
    guerrero_result <- us_gdp %>%
      features(GDP, guerrero)
    guerrero_result #lambda is 0.282
    
    us_gdp_transformed <- us_gdp %>%
      mutate(GDP_transformed = box_cox(GDP, 0.282))
    
# Plot the transformed data
    us_gdp_transformed %>%
      autoplot(GDP_transformed) + 
      labs(title = "Box-Cox Transformed U.S. GDP Autoplot", y = "Transformed GDP", x = "Year")
#arima model derivation of data (I cannot apply it now as series is non stationary)
    
    
#Fitting ARIMA model As it is visible on Box-Cox Transformed U.S. GDP Autoplot the data of GDP has no seasonality, however it has a clear linear trend. As the ARIMA model can only be fited on a stationary data, the trend has to be removed through diffrencing in order to fit the model.To confirm that the data is not-stationary fist it was converted to tsibble and later the KPSS unit root test with significance level = 0.05 was generated, which showed that test statistic is relatively big and p-value is 0.01, which rejects null hypothesis that data is stationary.
    
# kpss transformed data
    us_gdp_tsibble <- as_tsibble(us_gdp_transformed, index = Year)
    #kpss before diff
    us_gdp_tsibble %>%
      features(GDP_transformed, unitroot_kpss) %>%
      print()
    
#Consequently, the data was differenced for the first time, and later unit root test with significance level = 0.05 was generated once again. Now, the optput showed p-valie 0.01 which is less than significance level of 0.05 and test statistic is small which fails to reject null hypothesis that data is stationary.

#kpss after diff1
    us_gdp_tsibble %>%
      mutate(
        diff_gdp_2 = difference(GDP_transformed)       
      ) %>%
      features(diff_gdp_2, unitroot_kpss)
    
#As data is stationary now according to kpss, the ARIMA with default settings can be fitted. As the data was differenced once, the neighbourhood search in ARIMA() was restricted to d=1.

#####now arima
    diff_gdp_data <- us_gdp_tsibble %>%
      mutate(
        diff_gdp_2 = difference(GDP_transformed)       
      )
# fit arima
# Fit ARIMA model limit diffs to 2
    arima_model <- diff_gdp_data %>%
      model(arima = ARIMA(diff_gdp_2 ~ pdq(p = 1:3, d = 1, q = 0:2)))

    
#The default model generated by ARIMA() is ARIMA(2,1,1).

#Report the results of the ARIMA model
    report(arima_model)
#arima combinations To veryfy stationarity of differenced data, the differenced GDP was plotted using gg_tsdisplay. The plot showed that data does not have trend anymore. The ACF plot shows a sinusoidal pattern and spike at lag 1, whereas PACF as well shows a significant spike in lag 1 and later the spikes are not significant.
    
#######both pacf and acf together
    diff_gdp_data <- us_gdp_tsibble %>%
      mutate(
        diff_gdp_2 = difference(GDP_transformed)       
      )
    diff_gdp_data %>%
      gg_tsdisplay(diff_gdp_2, plot_type = "partial") + 
      labs(title = "ACF and PACF of First Differenced GDP")
    
    #As a result, 5 models were fitted ARIMA(1,1,1) to account for acf and pacf lag 1 spike, ARIMA(2,1,1), thus default model generated by arima, ARIMA(0,1,1) (MA model) to test if AR component is required and ARIMA(2,1,2) to check if higher order terms for MA ans AR are needed.As before the d in arima model should be restricted to 1 as the data was differenced once. Later, their AICc values were extracted and compared.
    
    arima_models <- diff_gdp_data %>%
      model(
        arima_1_1 = ARIMA(diff_gdp_2 ~ pdq(p = 1, d = 1, q = 1)),
        arima_2_1 = ARIMA(diff_gdp_2 ~ pdq(p = 2, d = 1, q = 1)),
        arima_0_1 = ARIMA(diff_gdp_2 ~ pdq(p = 0, d = 1, q = 1)),
        arima_1_0 = ARIMA(diff_gdp_2 ~ pdq(p = 1, d = 1, q = 0)),
        arima_2_2 = ARIMA(diff_gdp_2 ~ pdq(p = 2, d = 1, q = 2)),
        
      )
    
#According to AICc values the best model is ARIMA(0,1,1) as it has the lowest value of 650.

    aicc_values <- arima_models %>%
      glance() %>%
      select(.model, AICc) %>%
      arrange(AICc)

    print(aicc_values)
#best arima is with lowest aics= 0,1,1 model
    
#Later, the residual analysis using gg_tsresiduals was performed. The innovation residuals plot shows no pattern so it is likely that it is a white noise. All lags in acf() plot are not significant thus these confirms there is no significant autpocorrenation in data and histogram of residuals seems to achive the highest values around 0 so residuals are approximately normally distributed, thus there is no obvious pattern in residuals and model can be used for forecasting.
    
#residuals for best model
    arima_models %>%
      select(arima_0_1) %>%
      gg_tsresiduals() 
    
#The ljung_box text p-value of 0.233 results in the fact that test fails to reject null hypothesis at significance level of 0.05, thus there is no significant autocorrelation in residuals and thus confirm that residuals are white noise for model ARIMA(0,1,1).
    
    
#test for residuals:
    augmented_residuals <- augment(arima_models) %>%
      filter(.model == "arima_0_1")
    ljung_box_results <- augmented_residuals %>%
      features(.innov, ljung_box, lag = 10, dof = 3)
    ljung_box_results
    
#To forecast, the data was first split to train and test set to allow for evaluation of forecasting accuracy of model ARIMA(0,1,1). The forecasting horizon is 5 and test set has data points thus the aim in to forecast 5 years ahead of 2012.

    #split to train and test
    train_data <- us_gdp_tsibble %>%
      filter(Year <= 2012)
    train_data
    
    test_data <- us_gdp_tsibble %>%
      filter(Year >= 2013)

#Later, the model is fitted once again on train data. Next, forecast is generated and inversed with use of optimal lambda to show accurate forecasted values for transformed data.

    best_model <- train_data %>%
      model(arima_0_1 =ARIMA(GDP_transformed ~ pdq(p = 0, d = 1, q = 1)))
    
    forecast_data_arima <- best_model %>%
      forecast(h = 5)
    
    forecast_data_arima_inv <- forecast_data_arima %>%
      mutate(inv_mean = inv_box_cox(.mean, lambda = 0.282))
    
#The ets model was also fitted on train data and the forecast is made for 5 years ahead of 2012 data. However, this time the model is fitted on non-transformed data thus actual data for US GDP. The model parameters were selected by ETS() functions and are ETS(M,A,N). #ets model

    train_data2 <- us_gdp %>%
      filter(Year <= 2012)
    train_data2
    
    test_data2 <- us_gdp %>%
      filter(Year >= 2013)

    ets_model <- train_data2 %>%
      model(ETS(GDP))#fit on non-transromed data
    report(ets_model) #ETS(MAN) model was fitted
    
    
    report(ets_model) #ETS(MAN) model was fitted
    
    
    forecast_ets <- ets_model %>%
      forecast(h = 5) #no transformation so no inverse
    
#Later, the models were used to forecast on test data and accuracy measures were generated using accuracy(). All the errors are much smaller for for arima indicating the model captures data pattern better, gives errors of smaller magnitude, thus gives better forecasts. Moreover, after plotting the ETS model on the whole non-transformed dataset and using gg_tsresiduals the innovation resuduals plot seems to be funnel shaped thus sugessts that residuals variance is heteroscedastic which means model is invalid or fitted to non-stationary data. Consequently, ARIMA(0,1,1) is better model for prediction of US GDP.
    
    ets_forecast <- ets_model %>%
      forecast(new_data = test_data)
    
    arima_forecast <- best_model %>%
      forecast(new_data = test_data2)
    arima_accuracy <- accuracy(arima_forecast, test_data)
    arima_accuracy
    
    ets_accuracy <- accuracy(ets_forecast, test_data2)
    ets_accuracy

#train set acc
    best_model_acc <- train_data %>%
      model(arima_1_3 =ARIMA(GDP_transformed ~ pdq(p = 1, d = 2, q = 3)))%>%
      accuracy()
    best_model_acc
 
    ets_model_acc <- train_data2 %>%
      model(ETS(GDP)) %>%
      accuracy() #fit on non-transromed data
    ets_model_acc
    
    ets_model_full <- us_gdp %>%
      model(ETS(GDP))
    
    ets_model_full %>%
      gg_tsresiduals() 

## The Australian production of electricity is used for this exercise.
#Load data and look at the plot:
    electricity_production <- aus_production %>% select(Quarter, Electricity)
    electricity_production %>%
      autoplot(Electricity) +
      ggtitle("Original Electricity Data")
    
#Turns out the data is in good shape with clear seasonality and a clear trend showing a slight exponential growth pattern. Transformation is not necessary but a Box-Cox transformation could make the trend to be more towards normal distribution. In this case, Box-Cox with guerrero method could be used as a tool to automatically adjust data to a more linear pattern, and slightly reduce variance in latter records:
    
#Box-Cox lambda choosing with guerrero method, and transformation
    lambda <- electricity_production %>%
      features(Electricity, features = guerrero) %>%
      pull(lambda_guerrero)
    
    electricity_production <- electricity_production %>%
      mutate(Electricity_transformed = box_cox(Electricity, lambda))
    
    electricity_production %>%
      autoplot(Electricity_transformed) +
      ggtitle("Box-Cox Transformed Electricity Data")

#Data is not stationary and has a clear seasonality, therefore on top of regular differencing, seasonal differencing is needed as well. ndiffs will be used to yield stationary data:
    
    
    electricity_production_diff <- electricity_production %>%
      mutate(Electricity_transformed_diff = difference(Electricity_transformed, differences = ndiffs(electricity_production$Electricity_transformed)))
    
# Seasonal differencing (lag=4 for quarterly data)
    electricity_production_seasonal_diff <- electricity_production_diff %>% mutate(Electricity_transformed_seasonal_diff = difference(Electricity_transformed_diff, lag = 4))
    
# Plot
    electricity_production_seasonal_diff %>% autoplot(Electricity_transformed_seasonal_diff) + ggtitle("Seasonally Differenced Box-Cox Transformed Electricity Production Data")

#As the resulting plot shows the resulting data to have stationary mean and non-stationary variance.

#First, auto_arima is used observed for determining ARIMA combinations:
    
# Fit auto arima
    auto_model <- electricity_production_seasonal_diff %>% model(auto_arima = ARIMA(Electricity_transformed_seasonal_diff))
    
# Report
    report(auto_model) 
    aicc_values <- auto_model %>% glance() %>% select(.model, AICc) %>% arrange(AICc)
    
#It turns out both d and q parameters should stay at 0, this may make sense as the data is already seasonally differenciated. Thus 4 models with different p were tested:
    
# Fit multiple ARIMA models
    arima_models <- electricity_production_seasonal_diff %>%
      model(
        arima_1_0 = ARIMA(Electricity_transformed_seasonal_diff ~ pdq(p = 1, d = 0, q = 0)),
        arima_2_0 = ARIMA(Electricity_transformed_seasonal_diff ~ pdq(p = 2, d = 0, q = 0)),
        arima_3_0 = ARIMA(Electricity_transformed_seasonal_diff ~ pdq(p = 3, d = 0, q = 0)),
        arima_4_0 = ARIMA(Electricity_transformed_seasonal_diff ~ pdq(p = 4, d = 0, q = 0))
      )
# Compare AICc values
    aicc_values <- arima_models %>%
      glance() %>%
      select(.model, AICc) %>%
      arrange(AICc)
    
    print(aicc_values)
    
#Turns out arima(4,0,0) is the best model with AICc of 1314.
    
#best model can be explored by inspecting residual's plot and ACF plot
    
# Fit the ARIMA(4,0,0) model
    best_model <- electricity_production_seasonal_diff %>%
      model(arima_4_0_0 = ARIMA(Electricity_transformed_seasonal_diff ~ pdq(4,0,0)))
    
# Diagnostic testing on the residuals
    residuals <- best_model %>%
      augment() %>%
      filter(.model == "arima_4_0_0")
    
# Plot
    residuals %>%
      autoplot(.resid) +
      ggtitle("Residuals of ARIMA(4,0,0) Model")
    
# ACF
    residuals %>%
      ACF(.resid) %>%
      autoplot() +
      ggtitle("ACF of Residuals of ARIMA(4,0,0) Model")
    
    
#Both plots resemble white noise despite some none-pattern significantly correlated lags appearing in the ACF plot. Therefore the model can be considered the best-fit model.
#Forecast on transformed data:
    
    forecasts <- best_model %>% forecast(h = "24 months")
# Plot the forecasts
    forecasts %>% autoplot(electricity_production_seasonal_diff, level = NULL) + ggtitle("Transformed 24-Month Forecast of Electricity Production") + xlab("Time") + ylab("Electricity Production (Transformed)")
    
# Forecast on Origional Data:
    
# Extract the .mean values
    forecasts_modified <- forecasts %>%
      as_tibble() %>%
      select(Quarter, .mean) %>%
      rename(Electricity_transformed_seasonal_diff = .mean)
    
# Reverse seasonal differencing
    last_seasonal_values <- tail(electricity_production_diff$Electricity_transformed_diff, 4)
    forecasts_modified <- forecasts_modified %>%
      mutate(Electricity_transformed_diff = cumsum(Electricity_transformed_seasonal_diff) + rep(last_seasonal_values, length.out = n()))
    
# Reverse differencing
    last_diff_values <- tail(electricity_production$Electricity_transformed, ndiffs(electricity_production$Electricity_transformed))
    forecasts_modified <- forecasts_modified %>%
      mutate(Electricity_transformed = cumsum(Electricity_transformed_diff) + rep(last_diff_values, length.out = n()))
    
# Reverse Box-Cox transformation
    forecasts_modified <- forecasts_modified %>%
      mutate(Electricity_forecast = inv_box_cox(Electricity_transformed, lambda))
    
# Format the forecast to match the original data format
    inv_forecast <- forecasts_modified %>%
      select(Quarter, Electricity_forecast) %>%
      rename(Electricity = Electricity_forecast)
    
# Combine the original data and the forecast
    combined_data <- bind_rows(electricity_production, inv_forecast)
    
# Plot the combined data with highlighted forecast
    combined_data %>%
      ggplot(aes(x = Quarter, y = Electricity)) +
      geom_line(data = electricity_production, aes(x = Quarter, y = Electricity), color = "blue") +
      geom_line(data = inv_forecast, aes(x = Quarter, y = Electricity), color = "red") +
      ggtitle("Electricity Production with 24-Month Forecast") +
      xlab("Time") +
      ylab("Electricity Production")
    
    
# Fit the ETS model to original data
    ets_model <- electricity_production %>%
      model(ETS(Electricity))
    
    forecasts_ets <- ets_model %>%
      forecast(h = "24 months")
    
# Extract the .mean values
    forecasts_ets_modified <- forecasts_ets %>%
      as_tibble() %>%
      select(Quarter, .mean) %>%
      rename(Electricity = .mean)
    
# Combine the original data and the forecast
    combined_data <- bind_rows(forecasts_ets_modified, inv_forecast)
    
    combined_data %>%
      ggplot(aes(x = Quarter, y = Electricity)) +
      geom_line(data = forecasts_ets_modified, aes(x = Quarter, y = Electricity), color = "blue") +
      geom_line(data = inv_forecast, aes(x = Quarter, y = Electricity), color = "red") +
      ggtitle("Comparison of 24-Month Forecasts") +
      xlab("Time") +
      ylab("Electricity Production") 
    
    
#Red is the ARIMA forecast, and blue is the ETS forecast. These forecasts are in similar shape showing good capture of seasonality, the ARIMA forecasting is showing a downward trend while the ETS is remaining at the same level. There is no other significant difference between the two models.
    