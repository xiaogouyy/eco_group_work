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
    main = "ACF of NVDA Log Returns", xlim = c(1, 12)) 

acf(df_ko_clean$log_return_ko, 
    main = "ACF of KO Log Returns", xlim = c(1, 12)) 

acf(df_jnj_clean$log_return_jnj, 
    main = "ACF of JNJ Log Returns", xlim = c(1, 12)) 

acf(df_mcd_clean$log_return_mcd, 
    main = "ACF of MCD Log Returns", xlim = c(1, 12)) 

acf(df_googl_clean$log_return_googl, 
    main = "ACF of GOOGL Log Returns", xlim = c(1, 12)) 
acf(df_googl_clean$log_return_googl, 
    main = "ACF of GOOGL Log Returns", 48) 

##PACF plot for five stocks
pacf(df_nvda_clean$log_return_nvda, 
     main = "PACF of NVDA Log Returns", xlim = c(1, 12))

pacf(df_ko_clean$log_return_ko, 
     main = "PACF of KO Log Returns", xlim = c(1, 12))

pacf(df_jnj_clean$log_return_jnj, 
    main = "PACF of JNJ Log Returns", xlim = c(1, 12))

pacf(df_mcd_clean$log_return_mcd, 
     main = "PACF of MCD Log Returns", xlim = c(1, 12))

pacf(df_googl_clean$log_return_googl, 
     main = "PACF of GOOGL Log Returns", xlim = c(1, 12))

pacf(df_googl_clean$log_return_googl, 
     main = "PACF of GOOGL Log Returns", 48)

## Ljung-Box test using log return series
#Ljung-Box test for NVDA's series.
Box.test(df_nvda_clean$log_return_nvda, lag=12, type = "Ljung-Box")

#Ljung-Box test for KO's series.
Box.test(df_ko_clean$log_return_ko, type = "Ljung-Box", lag = 2+2+10)

#Ljung-Box test for JNJ's series.
Box.test(df_jnj_clean$log_return_jnj, type = "Ljung-Box", lag = 2+2+10)

#Ljung-Box test for MCD's series.
Box.test(df_mcd_clean$log_return_mcd, type = "Ljung-Box", lag = 2+2+10)
Box.test(df_mcd_clean$log_return_mcd, type = "Ljung-Box", lag = 12)

#Ljung-Box test for GOOGL's series.
Box.test(df_googl_clean$log_return_googl, type = "Ljung-Box", lag = 1+1+10)

##time series plots
plot(df_nvda_clean$ref.date, df_nvda_clean$log_return_nvda, type = "l",
     xlab = "Date", ylab = "log return", main = "NVDA log return time series")

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
#JNJ's ARMA model
arma_model_jnj <- auto.arima(df_jnj_clean$log_return_jnj, seasonal = TRUE, 
                                approximation = FALSE, trace = TRUE)
summary(arma_model_jnj)

#MCD's ARMA model
arma_model_mcd <- auto.arima(df_mcd_clean$log_return_mcd, seasonal = TRUE, 
                                approximation = FALSE, trace = TRUE)
summary(arma_model_mcd)

#GOOGL's ARMA model
arma_model_googl <- auto.arima(df_googl_clean$log_return_googl, seasonal = TRUE, 
                               approximation = FALSE, trace = TRUE)
summary(arma_model_googl)

##Diagnostic check
residuals_arma_jnj <- residuals(arma_model_jnj)
residuals_arma_mcd <- residuals(arma_model_mcd)
residuals_arma_googl <- residuals(arma_model_googl)

#Ljung-Box test for residuals
Box.test(residuals_arma_jnj, lag = 12, type = "Ljung-Box")
Box.test(residuals_arma_mcd, lag = 13, type = "Ljung-Box")
Box.test(residuals_arma_googl, lag = 11, type = "Ljung-Box")

#ACF and PACF plot for residuals
acf(residuals_arma_jnj, main = "ACF of JNJ ARMA Residuals", xlim = c(1,12))
acf(residuals_arma_mcd, main = "ACF of MCD ARMA Residuals", xlim = c(1,12))
acf(residuals_arma_googl, main = "ACF of GOOGL ARMA Residuals", xlim = c(1,12))

pacf(residuals_arma_jnj, main = "ACF of JNJ ARMA Residuals", xlim = c(1,12))
pacf(residuals_arma_mcd, main = "ACF of MCD ARMA Residuals", xlim = c(1,12))
pacf(residuals_arma_googl, main = "ACF of GOOGL ARMA Residuals", xlim = c(1,12))

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


