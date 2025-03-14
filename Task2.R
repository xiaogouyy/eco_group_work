install.packages("BatchGetSymbols")
library(BatchGetSymbols)
library(dplyr)
library(e1071)
library(readxl)


setwd('/Users/lyy/Library/Mobile Documents/com~apple~CloudDocs/eco group work')


####2.1 Convert price series to log return series
# Define parameters for getting data
first.date <- as.Date("2010-02-01")
last.date <- as.Date("2025-03-01")
freq <- "monthly" 

# Get data
sp500_data <- BatchGetSymbols(tickers = "^GSPC", 
                              first.date = first.date, 
                            last.date = "2024-02-01",
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
#extracrt data into dataframes
nvda_df <- NVDA_data$df.tickers
ko_df <- KO_data$df.tickers
jnj_df <- JNJ_data$df.tickers
mcd_df <- MCD_data$df.tickers
googl_df <- GOOGL_data$df.tickers
sp500_df <- sp500_data$df.tickers

#get the data from 2010-02-01 to 2024-02-01 as in sample data, 
#2024-03-01 to 2025-03-01 as out of sample data.
insample_nvda <- head(nvda_df, 169)
insample_ko <- head(ko_df, 169)
insample_jnj <- head(jnj_df, 169)
insample_mcd <- head(mcd_df, 169)
insample_googl <- head(googl_df, 169)

outsample_nvda <- tail(nvda_df, 13)
outsample_ko <- tail(nvda_df, 13)
outsample_jnj <- tail(nvda_df, 13)
outsample_mcd <- tail(nvda_df, 13)
outsample_googl <- tail(nvda_df, 13)

##calculate log return
insample_nvda <- insample_nvda %>%
  dplyr::mutate(
    Price_lag1 = lag(price.adjusted, n = 1),    
    return1 = (price.adjusted / Price_lag1),           
    log_return_nvda = log(return1),
    percentage_log_ret_nvda = log_return_nvda *100
    ) %>% na.omit()

insample_ko <- insample_ko %>%
  dplyr::mutate(
    Price_lag2 = lag(price.adjusted, n = 1),    
    return2 = (price.adjusted / Price_lag2),           
    log_return_ko = log(return2),  
    percentage_log_ret_ko = log_return_ko *100
  ) %>% na.omit()


insample_jnj <- insample_jnj %>%
  dplyr::mutate(
    Price_lag3 = lag(price.adjusted, n = 1),    
    return3 = (price.adjusted / Price_lag3),           
    log_return_jnj = log(return3),   
    percentage_log_ret_jnj = log_return_jnj *100
  ) %>% na.omit()


insample_mcd <- insample_mcd %>%
  dplyr::mutate(
    Price_lag4 = lag(price.adjusted, n = 1),    
    return4 = (price.adjusted / Price_lag4),           
    log_return_mcd = log(return4),       
    percentage_log_ret_mcd = log_return_mcd *100
  ) %>% na.omit()


insample_googl <- insample_googl %>%
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
acf(insample_nvda$log_return_nvda, 
    main = "ACF of NVDA Log Returns", xlim = c(1, 20)) 

acf(insample_ko$log_return_ko, 
    main = "ACF of KO Log Returns", xlim = c(1, 20)) 

acf(insample_jnj$log_return_jnj, 
    main = "ACF of JNJ Log Returns", xlim = c(1, 20)) 

acf(insample_mcd$log_return_mcd, 
    main = "ACF of MCD Log Returns", xlim = c(1, 20)) 

acf(insample_googl$log_return_googl, 
    main = "ACF of GOOGL Log Returns", xlim = c(1, 20)) 
acf(insample_googl$log_return_googl, 
    main = "ACF of GOOGL Log Returns", 48) 

##PACF plot for five stocks
pacf(insample_nvda$log_return_nvda, 
     main = "PACF of NVDA Log Returns", xlim = c(1, 20))

pacf(insample_ko$log_return_ko, 
     main = "PACF of KO Log Returns", xlim = c(1, 20))

pacf(insample_jnj$log_return_jnj, 
    main = "PACF of JNJ Log Returns", xlim = c(1, 20))

pacf(insample_mcd$log_return_mcd, 
     main = "PACF of MCD Log Returns", xlim = c(1, 20))

pacf(insample_googl$log_return_googl, 
     main = "PACF of GOOGL Log Returns", xlim = c(1, 20))

pacf(insample_googl$log_return_googl, 
     main = "PACF of GOOGL Log Returns", 48)

## Ljung-Box test using log return series
#Ljung-Box test for NVDA's series.
Box.test(insample_nvda$log_return_nvda, lag=12, type = "Ljung-Box")
Box.test(insample_nvda$log_return_nvda, lag=2, type = "Ljung-Box")

#Ljung-Box test for KO's series.
Box.test(insample_ko$log_return_ko, type = "Ljung-Box", lag = 4)
Box.test(insample_ko$log_return_ko, type = "Ljung-Box", lag = 3)
Box.test(insample_ko$log_return_ko, type = "Ljung-Box", lag = 10)

#Ljung-Box test for JNJ's series.
Box.test(insample_jnj$log_return_jnj, type = "Ljung-Box", lag = 20)
Box.test(insample_jnj$log_return_jnj, type = "Ljung-Box", lag = 3)
Box.test(insample_jnj$log_return_jnj, type = "Ljung-Box", lag = 4)
#Ljung-Box test for MCD's series.
Box.test(insample_mcd$log_return_mcd, type = "Ljung-Box", lag = 13)
Box.test(insample_mcd$log_return_mcd, type = "Ljung-Box", lag = 7)
Box.test(insample_mcd$log_return_mcd, type = "Ljung-Box", lag = 6)
#Ljung-Box test for GOOGL's series.
Box.test(insample_googl$log_return_googl, type = "Ljung-Box", lag = 12)
Box.test(insample_googl$log_return_googl, type = "Ljung-Box", lag = 2)
Box.test(insample_googl$log_return_googl, type = "Ljung-Box", lag = 20)

####2.3 Build ARMA model
##Identification
#ADF test for NVDA
library(tseries)
adf.test(insample_nvda$log_return_nvda)
#ADF test for KO
adf.test(insample_ko$log_return_ko)
#ADF test for JNJ
adf.test(insample_jnj$log_return_jnj)
#ADF test for MCD
adf.test(insample_mcd$log_return_mcd)
#ADF test for GOOGL
adf.test(insample_googl$log_return_googl)

##Estimation
#KO's optimal ARMA model
arma_model_ko <- auto.arima(insample_ko$log_return_ko, seasonal = TRUE, 
                            max.p = 4,max.q = 4, max.d = 4,
                             approximation = FALSE, trace = TRUE)
summary(arma_model_ko)

#JNJ's optimal ARMA model
arma_model_jnj <- auto.arima(insample_jnj$log_return_jnj, 
                             max.p = 12,max.q = 12, max.d = 12, seasonal = TRUE, 
                                approximation = FALSE, trace = TRUE, ic = "aic")
summary(arma_model_jnj)

#MCD's optimal ARMA model
arma_model_mcd <- auto.arima(insample_mcd$log_return_mcd, seasonal = TRUE, 
                             max.p = 14,max.q = 14, max.d = 14,
                                approximation = FALSE, trace = TRUE)
summary(arma_model_mcd)

##Diagnostic check
#get residuals
residuals_arma_ko <- residuals(arma_model_ko)
residuals_arma_jnj <- residuals(arma_model_jnj)
residuals_arma_mcd <- residuals(arma_model_mcd)

#Ljung-Box test for residuals autocorrelation in KO's model
Box.test(residuals_arma_ko, lag = 30, type = "Ljung-Box")

#Ljung-Box test for residuals autocorrelation in JNJ's model
Box.test(residuals_arma_jnj, lag = 30, type = "Ljung-Box")

#Ljung-Box test for residuals autocorrelation in MCD's model
Box.test(residuals_arma_mcd, lag = 30, type = "Ljung-Box")

#ACF and PACF plot for residuals
acf(residuals_arma_ko, main = "ACF of KO ARMA Residuals", xlim = c(1,30))
acf(residuals_arma_jnj, main = "ACF of JNJ ARMA Residuals", xlim = c(1,30))
acf(residuals_arma_mcd, main = "ACF of MCD ARMA Residuals", xlim = c(1,30))

pacf(residuals_arma_ko, main = "PACF of KO ARMA Residuals", xlim = c(1,30))
pacf(residuals_arma_jnj, main = "PACF of JNJ ARMA Residuals", xlim = c(1,30))
pacf(residuals_arma_mcd, main = "PACF of MCD ARMA Residuals", xlim = c(1,30))
pacf(residuals_arma_googl, main = "PACF of GOOGL ARMA Residuals", xlim = c(1,30))


####2.4 Build CAPM models
#get risk free rate fed rate
install.packages("tidyquant")  # yahoo finance dont have fed. use this instead
library(tidyquant)

#get fed data
fed_rate <- tq_get("FEDFUNDS", get = "economic.data", 
                   from = "2010-03-01", to = "2024-02-05")

#add fed into stocks' dataframes, calculate ri-rf, and rm-rf
sp500_df <- sp500_df %>%
  mutate(Rm = log(price.adjusted / lag(price.adjusted))*100
  )%>% na.omit()

insample_nvda <- insample_nvda %>%
  dplyr::mutate(
    fedrate = fed_rate$price,
    y_nvda = percentage_log_ret_nvda - fedrate,
    rm = sp500_df$Rm,
    x_nvda = rm-fedrate
  ) 

insample_ko <- insample_ko %>%
  dplyr::mutate(
    fedrate = fed_rate$price,
    y_ko = percentage_log_ret_ko - fedrate,
    rm = sp500_df$Rm,
    x_ko = rm-fedrate
  ) 

insample_jnj <- insample_jnj %>%
  dplyr::mutate(
    fedrate = fed_rate$price,
    y_jnj = percentage_log_ret_jnj - fedrate,
    rm = sp500_df$Rm,
    x_jnj = rm-fedrate
  ) 

insample_mcd <- insample_mcd %>%
  dplyr::mutate(
    fedrate = fed_rate$price,
    y_mcd = percentage_log_ret_mcd - fedrate,
    rm = sp500_df$Rm,
    x_mcd = rm-fedrate
  ) 

insample_googl <- insample_googl %>%
  dplyr::mutate(
    fedrate = fed_rate$price,
    y_googl = percentage_log_ret_googl - fedrate,
    rm = sp500_df$Rm,
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
#calculate actual log return for out of sample data
outsample_nvda <- outsample_nvda %>%
  dplyr::mutate(
    Price_lag1out = lag(price.adjusted, n = 1),    
    return1out = (price.adjusted / Price_lag1out),           
    log_return_nvda = log(return1out),
  ) %>% na.omit()

outsample_ko <- outsample_ko %>%
  dplyr::mutate(
    Price_lag2out = lag(price.adjusted, n = 1),    
    return2out = (price.adjusted / Price_lag2out),           
    log_return_ko = log(return2out),  
  ) %>% na.omit()


outsample_jnj <- outsample_jnj %>%
  dplyr::mutate(
    Price_lag3out = lag(price.adjusted, n = 1),    
    return3out = (price.adjusted / Price_lag3out),           
    log_return_jnj = log(return3out),   
  ) %>% na.omit()


outsample_mcd <- outsample_mcd %>%
  dplyr::mutate(
    Price_lag4out = lag(price.adjusted, n = 1),    
    return4out = (price.adjusted / Price_lag4out),           
    log_return_mcd = log(return4out),       
  ) %>% na.omit()


outsample_googl <- outsample_googl %>%
  dplyr::mutate(
    Price_lag5out = lag(price.adjusted, n = 1),    
    return5out = (price.adjusted / Price_lag5out),           
    log_return_googl = log(return5out),   
  ) %>% na.omit()

#make prediction using models built
pred_ko <- predict(object = arma_model_ko,n.ahead = 12)
pred_jnj <- predict(object = arma_model_jnj,n.ahead = 12)
pred_mcd <- predict(object = arma_model_mcd,n.ahead = 12)
pred_googl <- predict(object = arma_model_googl,n.ahead = 12)

#calculate MSE of models
mean((pred_ko$pred - outsample_ko$log_return_ko)^2)
mean((pred_jnj$pred - outsample_jnj$log_return_jnj)^2)
mean((pred_mcd$pred - outsample_mcd$log_return_mcd)^2)

range(insample_ko$log_return_ko)
range(insample_jnj$log_return_jnj)
range(insample_mcd$log_return_mcd)

##compare with naive model
#define naive model
naive_md_ko <- naive(insample_ko$log_return_ko, h=12)
naive_md_jnj <- naive(insample_jnj$log_return_jnj, h=12)
naive_md_mcd <- naive(insample_mcd$log_return_mcd, h=12)

fc_nai_ko <- forecast(naive_md_ko)
fc_nai_jnj <- forecast(naive_md_jnj)
fc_nai_mcd <- forecast(naive_md_mcd)

accuracy(pred_ko$pred, outsample_ko$log_return_ko)
accuracy(fc_nai_ko, outsample_ko$log_return_ko)

accuracy(pred_jnj$pred, outsample_jnj$log_return_jnj)
accuracy(fc_nai_jnj, outsample_jnj$log_return_jnj)

accuracy(pred_mcd$pred, outsample_mcd$log_return_mcd)
accuracy(fc_nai_mcd, outsample_mcd$log_return_mcd)

system("git push --force origin main")

