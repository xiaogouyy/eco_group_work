install.packages("BatchGetSymbols")
library(BatchGetSymbols)
library(dplyr)
library(e1071)
library(readxl)


setwd('/Users/lyy/Library/Mobile Documents/com~apple~CloudDocs/eco group work')


####2.1 Convert price series to log return series
# Define parameters for getting data
first.date <- as.Date("2010-02-01")
last.date <- as.Date("2024-02-01")
freq <- "monthly" 

# Get data
sp500_data <- BatchGetSymbols(tickers = "^GSPC", 
                              first.date = first.date, 
                            last.date = last.date,
                            freq = freq)
NVDA_data <- BatchGetSymbols(tickers = "NVDA", 
                                     first.date = first.date, 
                                     last.date = last.date, 
                                     freq = freq)
KO_data <- BatchGetSymbols(tickers = "KO", 
                                     first.date = first.date, 
                                     last.date = last.date, 
                                     freq = freq)
JNJ_data <- BatchGetSymbols(tickers = "JNJ", 
                                     first.date = first.date, 
                                     last.date = last.date, 
                                     freq = freq)
MCD_data <- BatchGetSymbols(tickers = "MCD", 
                                     first.date = first.date, 
                                     last.date = last.date, 
                                     freq = freq)
GOOGL_data <- BatchGetSymbols(tickers = "GOOGL", 
                                     first.date = first.date, 
                                     last.date = last.date, 
                                     freq = freq)

##Convert price series to log return series
#extracrt data into dataframe
nvda_df <- NVDA_data$df.tickers
ko_df <- KO_data$df.tickers
jnj_df <- JNJ_data$df.tickers
mcd_df <- MCD_data$df.tickers
googl_df <- GOOGL_data$df.tickers
sp500_df <- sp500_data$df.tickers

df_nvda_clean <- na.omit(nvda_df)
df_ko_clean <- na.omit(ko_df)
df_jnj_clean <- na.omit(jnj_df)
df_mcd_clean <- na.omit(mcd_df)
df_googl_clean <- na.omit(googl_df)
df_sp500_clean <- na.omit(sp500_df)

##calculate log return
df_nvda_clean <- df_nvda_clean %>%
  dplyr::mutate(
    Price_lag1 = lag(price.adjusted, n = 1),    
    return1 = (price.adjusted / Price_lag1),           
    log_return_nvda = log(return1),
    percentage_log_ret_nvda = log_return_nvda *100
    ) %>% na.omit()

df_ko_clean <- df_ko_clean %>%
  dplyr::mutate(
    Price_lag2 = lag(price.adjusted, n = 1),    
    return2 = (price.adjusted / Price_lag2),           
    log_return_ko = log(return2),  
    percentage_log_ret_ko = log_return_ko *100
  ) %>% na.omit()


df_jnj_clean <- df_jnj_clean %>%
  dplyr::mutate(
    Price_lag3 = lag(price.adjusted, n = 1),    
    return3 = (price.adjusted / Price_lag3),           
    log_return_jnj = log(return3),   
    percentage_log_ret_jnj = log_return_jnj *100
  ) %>% na.omit()


df_mcd_clean <- df_mcd_clean %>%
  dplyr::mutate(
    Price_lag4 = lag(price.adjusted, n = 1),    
    return4 = (price.adjusted / Price_lag4),           
    log_return_mcd = log(return4),       
    percentage_log_ret_mcd = log_return_mcd *100
  ) %>% na.omit()


df_googl_clean <- df_googl_clean %>%
  dplyr::mutate(
    Price_lag5 = lag(price.adjusted, n = 1),    
    return5 = (price.adjusted / Price_lag5),           
    log_return_googl = log(return5),   
    percentage_log_ret_googl = log_return_googl *100
  ) %>% na.omit()

####2.2 Plot ACF and PACF
install.packages("ggplot2")
install.packages("forecast")
library(ggplot2)
library(forecast)

##ACF plot for five stocks
acf(df_nvda_clean$log_return_nvda, 
    main = "ACF of NVDA Log Returns", xlim = c(1, 20)) 

acf(df_ko_clean$log_return_ko, 
    main = "ACF of KO Log Returns", xlim = c(1, 20)) 

acf(df_jnj_clean$log_return_jnj, 
    main = "ACF of JNJ Log Returns", xlim = c(1, 20)) 

acf(df_mcd_clean$log_return_mcd, 
    main = "ACF of MCD Log Returns", xlim = c(1, 20)) 

acf(df_googl_clean$log_return_googl, 
    main = "ACF of GOOGL Log Returns", xlim = c(1, 20)) 
acf(df_googl_clean$log_return_googl, 
    main = "ACF of GOOGL Log Returns", 48) 

##PACF plot for five stocks
pacf(df_nvda_clean$log_return_nvda, 
     main = "PACF of NVDA Log Returns", xlim = c(1, 20))

pacf(df_ko_clean$log_return_ko, 
     main = "PACF of KO Log Returns", xlim = c(1, 20))

pacf(df_jnj_clean$log_return_jnj, 
    main = "PACF of JNJ Log Returns", xlim = c(1, 20))

pacf(df_mcd_clean$log_return_mcd, 
     main = "PACF of MCD Log Returns", xlim = c(1, 20))

pacf(df_googl_clean$log_return_googl, 
     main = "PACF of GOOGL Log Returns", xlim = c(1, 20))

pacf(df_googl_clean$log_return_googl, 
     main = "PACF of GOOGL Log Returns", 48)

## Ljung-Box test using log return series
#Ljung-Box test for NVDA's series.
Box.test(df_nvda_clean$log_return_nvda, lag=12, type = "Ljung-Box")
Box.test(df_nvda_clean$log_return_nvda, lag=2, type = "Ljung-Box")

#Ljung-Box test for KO's series.
Box.test(df_ko_clean$log_return_ko, type = "Ljung-Box", lag = 12)
Box.test(df_ko_clean$log_return_ko, type = "Ljung-Box", lag = 2)

#Ljung-Box test for JNJ's series.
Box.test(df_jnj_clean$log_return_jnj, type = "Ljung-Box", lag = 12)
Box.test(df_jnj_clean$log_return_jnj, type = "Ljung-Box", lag = 2)

#Ljung-Box test for MCD's series.
Box.test(df_mcd_clean$log_return_mcd, type = "Ljung-Box", lag = 12)
Box.test(df_mcd_clean$log_return_mcd, type = "Ljung-Box", lag = 2)

#Ljung-Box test for GOOGL's series.
Box.test(df_googl_clean$log_return_googl, type = "Ljung-Box", lag = 12)
Box.test(df_googl_clean$log_return_googl, type = "Ljung-Box", lag = 2)

####2.3 Build ARMA model
##Identification
#ADF test for NVDA
library(tseries)
adf.test(df_nvda_clean$log_return_nvda)
#ADF test for KO
adf.test(df_ko_clean$log_return_ko)
#ADF test for JNJ
adf.test(df_jnj_clean$log_return_jnj)
#ADF test for MCD
adf.test(df_mcd_clean$log_return_mcd)
#ADF test for GOOGL
adf.test(df_googl_clean$log_return_googl)

##Estimation
#KO's optimal ARMA model
arma_model_ko <- auto.arima(df_ko_clean$log_return_ko, seasonal = TRUE, 
                             approximation = FALSE, trace = TRUE)
summary(arma_model_ko)

#JNJ's optimal ARMA model
arma_model_jnj <- auto.arima(df_jnj_clean$log_return_jnj, seasonal = TRUE, 
                                approximation = FALSE, trace = TRUE, ic = "aic")
summary(arma_model_jnj)

#MCD's optimal ARMA model
arma_model_mcd <- auto.arima(df_mcd_clean$log_return_mcd, seasonal = TRUE, 
                                approximation = FALSE, trace = TRUE)
summary(arma_model_mcd)

#GOOGL's optimal ARMA model
arma_model_googl <- auto.arima(df_googl_clean$log_return_googl, seasonal = TRUE, 
                               approximation = FALSE, trace = TRUE)
summary(arma_model_googl)

##Diagnostic check
#get residuals
residuals_arma_ko <- residuals(arma_model_ko)
residuals_arma_jnj <- residuals(arma_model_jnj)
residuals_arma_mcd <- residuals(arma_model_mcd)
residuals_arma_googl <- residuals(arma_model_googl)

#Ljung-Box test for residuals autocorrelation in KO's model
Box.test(residuals_arma_ko, lag = 12, type = "Ljung-Box")

#Ljung-Box test for residuals autocorrelation in JNJ's model
Box.test(residuals_arma_jnj, lag = 12, type = "Ljung-Box")

#Ljung-Box test for residuals autocorrelation in MCD's model
Box.test(residuals_arma_mcd, lag = 13, type = "Ljung-Box")

#Ljung-Box test for residuals autocorrelation in GOOGL's model
Box.test(residuals_arma_googl, lag = 11, type = "Ljung-Box")

#ACF and PACF plot for residuals
acf(residuals_arma_ko, main = "ACF of KO ARMA Residuals", xlim = c(1,12))
acf(residuals_arma_jnj, main = "ACF of JNJ ARMA Residuals", xlim = c(1,12))
acf(residuals_arma_mcd, main = "ACF of MCD ARMA Residuals", xlim = c(1,12))
acf(residuals_arma_googl, main = "ACF of GOOGL ARMA Residuals", xlim = c(1,12))

pacf(residuals_arma_ko, main = "PACF of KO ARMA Residuals", xlim = c(1,12))
pacf(residuals_arma_jnj, main = "PACF of JNJ ARMA Residuals", xlim = c(1,12))
pacf(residuals_arma_mcd, main = "PACF of MCD ARMA Residuals", xlim = c(1,12))
pacf(residuals_arma_googl, main = "PACF of GOOGL ARMA Residuals", xlim = c(1,12))


####2.4 Build CAPM models
#get risk free rate fed rate
install.packages("tidyquant")  # yahoo finance dont have fed. use this instead
library(tidyquant)

#get fed data
fed_rate <- tq_get("FEDFUNDS", get = "economic.data", 
                   from = "2010-04-01", to = "2024-01-05")

#add fed into stocks' dataframes, calculate ri-rf, and rm-rf
df_sp500_clean <- df_sp500_clean %>%
  mutate(Rm = log(price.adjusted / lag(price.adjusted))*100
  )%>% na.omit()

df_nvda_clean <- df_nvda_clean %>%
  dplyr::mutate(
    fedrate = fed_rate$price,
    y_nvda = percentage_log_ret_nvda - fedrate,
    rm = df_sp500_clean$Rm,
    x_nvda = rm-fedrate
  ) 

df_ko_clean <- df_ko_clean %>%
  dplyr::mutate(
    fedrate = fed_rate$price,
    y_ko = percentage_log_ret_ko - fedrate,
    rm = df_sp500_clean$Rm,
    x_ko = rm-fedrate
  ) 

df_jnj_clean <- df_jnj_clean %>%
  dplyr::mutate(
    fedrate = fed_rate$price,
    y_jnj = percentage_log_ret_jnj - fedrate,
    rm = df_sp500_clean$Rm,
    x_jnj = rm-fedrate
  ) 

df_mcd_clean <- df_mcd_clean %>%
  dplyr::mutate(
    fedrate = fed_rate$price,
    y_mcd = percentage_log_ret_mcd - fedrate,
    rm = df_sp500_clean$Rm,
    x_mcd = rm-fedrate
  ) 

df_googl_clean <- df_googl_clean %>%
  dplyr::mutate(
    fedrate = fed_rate$price,
    y_googl = percentage_log_ret_googl - fedrate,
    rm = df_sp500_clean$Rm,
    x_googl = rm-fedrate
  ) 

##run the regression
#NVDA's model
lm_nvda <- lm(y_nvda ~ x_nvda, data = df_nvda_clean) 
summary(lm_nvda)

#KO's model
lm_ko <- lm(y_ko ~ x_ko, data = df_ko_clean) 
summary(lm_ko)

#JNJ's model
lm_jnj <- lm(y_jnj ~ x_jnj, data = df_jnj_clean) 
summary(lm_jnj)

#MCD's model
lm_mcd <- lm(y_mcd ~ x_mcd, data = df_mcd_clean) 
summary(lm_mcd)

#GOOGL's model
lm_googl <- lm(y_googl ~ x_googl, data = df_googl_clean) 
summary(lm_googl)


####2.5 Forecasting
##out of sample forcasting
KO_actual <- BatchGetSymbols(tickers = "KO", 
                           first.date = "2024-01-01", 
                           last.date = "2025-02-01", 
                           freq = freq)
JNJ_actual <- BatchGetSymbols(tickers = "JNJ", 
                            first.date = "2024-01-01", 
                            last.date = "2025-02-01", 
                            freq = freq)
MCD_actual <- BatchGetSymbols(tickers = "MCD", 
                            first.date = "2024-01-01", 
                            last.date = "2025-02-01", 
                            freq = freq)
GOOGL_actual <- BatchGetSymbols(tickers = "GOOGL", 
                              first.date = "2024-01-01", 
                              last.date = "2025-02-01", 
                              freq = freq)


ko_df_actual <- KO_actual$df.tickers
jnj_df_actual <- JNJ_actual$df.tickers
mcd_df_actual <- MCD_actual$df.tickers
googl_df_actual <- GOOGL_actual$df.tickers


ko_df_actual <- ko_df_actual %>%
  dplyr::mutate(
    Price_lag_k = lag(price.adjusted, n = 1),    
    return_k = (price.adjusted / Price_lag_k),           
    log_return_ko_actual = log(return_k),  
  ) %>% na.omit()


jnj_df_actual <- jnj_df_actual %>%
  dplyr::mutate(
    Price_lagj = lag(price.adjusted, n = 1),    
    returnj = (price.adjusted / Price_lagj),           
    log_return_jnj_actual = log(returnj),   
  ) %>% na.omit()


mcd_df_actual <- mcd_df_actual %>%
  dplyr::mutate(
    Price_lagm = lag(price.adjusted, n = 1),    
    returnm = (price.adjusted / Price_lagm),           
    log_return_mcd_actual = log(returnm),       
  ) %>% na.omit()


googl_df_actual <- googl_df_actual %>%
  dplyr::mutate(
    Price_lagg = lag(price.adjusted, n = 1),    
    returng = (price.adjusted / Price_lagg),           
    log_return_googl_actual = log(returng),   
  ) %>% na.omit()


# Forecast the next 12 months
forecast_values_ko <- forecast(arma_model_ko, h = 12)$mean

forecast_values_jnj <- forecast(arma_model_jnj, h = 12)$mean

forecast_values_mcd <- forecast(arma_model_mcd, h = 12)$mean

forecast_values_googl <- forecast(arma_model_googl, h = 12)$mean

# Calculate the accuracy of out-of-sample forecast
accuracy(forecast_values_ko, ko_df_actual$log_return_ko_actual)
accuracy(forecast_values_jnj, jnj_df_actual$log_return_jnj_actual)
accuracy(forecast_values_mcd, mcd_df_actual$log_return_mcd_actual)
accuracy(forecast_values_googl,googl_df_actual$log_return_googl_actual)

## in sample forecast
fitted_values_ko <- fitted(arma_model_ko)
fitted_values_jnj <- fitted(arma_model_jnj)
fitted_values_mcd <- fitted(arma_model_mcd)
fitted_values_googl <- fitted(arma_model_googl)

# Extract the last 12 fitted values
last_12_fitted_ko <- tail(fitted_values_ko, 12)
last_12_fitted_jnj <- tail(fitted_values_jnj, 12)
last_12_fitted_mcd <- tail(fitted_values_mcd, 12)
last_12_fitted_googl <- tail(fitted_values_googl, 12)

# Extract actual last 12 values
actual_last_12_ko <- tail(df_ko_clean$log_return_ko, 12)
actual_last_12_jnj <- tail(df_jnj_clean$log_return_jnj, 12)
actual_last_12_mcd <- tail(df_mcd_clean$log_return_mcd, 12)
actual_last_12_googl <- tail(df_googl_clean$log_return_googl, 12)
# Calculate accuracy of in-sample forecast
accuracy(last_12_fitted_ko, actual_last_12_ko)
accuracy(last_12_fitted_jnj, actual_last_12_jnj)
accuracy(last_12_fitted_mcd, actual_last_12_mcd)
accuracy(last_12_fitted_googl, actual_last_12_googl)

