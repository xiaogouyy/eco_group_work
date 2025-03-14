install.packages("BatchGetSymbols")
install.packages("dplyr")
install.packages("e1071")
install.packages("readxl")

library(BatchGetSymbols)
library(dplyr)
library(e1071)
library(readxl)

##set working location, use your own location
setwd('/Users/lyy/Library/Mobile Documents/com~apple~CloudDocs/eco group work')


#####1.1 and 1.2, regression and T test and F test.
##get data for each company
data= readxl::read_excel('data.xlsx')
df_Apple <- dplyr::filter(data, ticker == "AAPL")
df_AMD <- dplyr::filter(data, ticker=="AMD")
df_MSFT <-dplyr::filter(data, ticker=="MSFT")
df_INTC <-dplyr::filter(data, ticker=="INTC")
df_AMZN <-dplyr::filter(data, ticker=="AMZN")

## change the colum name of Mkt-RF to RM, other wise it will cause errors
colnames(data)[colnames(data) == "Mkt-RF"] <- "RM"
colnames(df_Apple)[colnames(df_Apple) == "Mkt-RF"] <- "RM"
colnames(df_AMD)[colnames(df_AMD) == "Mkt-RF"] <- "RM"
colnames(df_MSFT)[colnames(df_MSFT) == "Mkt-RF"] <- "RM"
colnames(df_AMZN)[colnames(df_AMZN) == "Mkt-RF"] <- "RM"
colnames(df_INTC)[colnames(df_INTC) == "Mkt-RF"] <- "RM"

##calculate price lag, return, Ri, and dependent varaible
df_Apple <- df_Apple %>%
  dplyr::mutate(
    Price_lag1 = lag(price_close, n = 1),    
    return1 = (price_close / Price_lag1),           
    ri1 = log(return1)*100,                          
    y1 = ri1 - RF) 

df_AMD <- df_AMD %>%
  dplyr::mutate(
    Price_lag2 = lag(price_close, n = 1),    
    return2 = (price_close / Price_lag2),           
    ri2 = log(return2)*100,                          
    y2 = ri2 - RF) 

df_AMZN <- df_AMZN %>%
  dplyr::mutate(
    Price_lag3 = lag(price_close, n = 1),    
    return3 = price_close / Price_lag3,           
    ri3 = log(return3)*100,                          
    y3 = ri3 - RF) 

df_INTC <- df_INTC %>%
  dplyr::mutate(
    Price_lag4 = lag(price_close, n = 1),    
    return4 = price_close / Price_lag4,           
    ri4 = log(return4)*100,                          
    y4 = ri4 - RF) 

df_MSFT <- df_MSFT %>%
  dplyr::mutate(
    Price_lag5 = lag(price_close, n = 1),    
    return5 = price_close / Price_lag5,           
    ri5 = log(return5)*100,                          
    y5 = ri5 - RF) 

## run regression for each company
model1 <- lm(y1 ~ RM + SMB + HML, data = df_Apple)
summary(model1)

model2 <-lm(y2 ~ RM + SMB + HML, data = df_AMD)
summary(model2)

model3 <-lm(y3~RM + SMB + HML, data = df_AMZN)
summary(model3)

model4 <-lm(y4~RM + SMB + HML, data = df_INTC)
summary(model4)

model5<-lm(y5~RM+SMB+HML, data= df_MSFT)
summary(model5)

## critical values for t test and F test
qt(1-0.025, df= 295)## 5% critical t
qt(1-0.005, df=295)## 1% critical t
qt(1-0.05,df=295) ## 10% critical t
qf(1-0.05, df1 = 3, df2= 295) ##5% critical f
qf(1-0.01, df1 = 3, df2=295)  ## 1% critical f
qf(1-0.1, df1=3, df2=295) ## 10% critical f

#####1.3 DW test
install.packages("lmtest")  
install.packages("car")
library(lmtest)
library(car)

#Plot ut-ut-1 graph to check for autocorrelation
ut_model1<- residuals(model1)
ut_model2<- residuals(model2)
ut_model3<- residuals(model3)
ut_model4<- residuals(model4)
ut_model5<- residuals(model5)

ut1_model1 <- lag(ut_model1, n=1)  # Lag by 1
ut1_model2 <- lag(ut_model2, n=1)
ut1_model3 <- lag(ut_model3, n=1)
ut1_model4 <- lag(ut_model4, n=1)
ut1_model5 <- lag(ut_model5, n=1)

#ut-ut-1 graph
plot(ut_model1, ut1_model1, main = "ut vs ut-1 for Apple", xlab = "ut", ylab = "ut-1")
plot(ut_model2, ut1_model2, main = "ut vs ut-1 for AMD", xlab = "ut", ylab = "ut-1")
plot(ut_model3, ut1_model3, main = "ut vs ut-1 for AMZN", xlab = "ut", ylab = "ut-1")
plot(ut_model4, ut1_model4, main = "ut vs ut-1 for INTC", xlab = "ut", ylab = "ut-1")
plot(ut_model5, ut1_model5, main = "ut vs ut-1 for MSFT", xlab = "ut", ylab = "ut-1")

#ut-time graph
plot(df_Apple$ref_date[-1], ut_model1, main = "ut vs time for Apple",
     type="l", xlab = "ut", ylab = "time")

plot(df_AMD$ref_date[-1], ut_model2, main = "ut vs time for AMD",
     type="l", xlab = "ut", ylab = "time")

plot(df_AMZN$ref_date[-1], ut_model3, main = "ut vs time for AMZN",
     type="l", xlab = "ut", ylab = "time")

plot(df_INTC$ref_date[-1], ut_model4, main = "ut vs time for INTC",
     type="l", xlab = "ut", ylab = "time")

plot(df_MSFT$ref_date[-1], ut_model5, main = "ut vs time for MSFT",
     type="l", xlab = "ut", ylab = "time")


#DW test
dwtest(model1)
dwtest(model2)
dwtest(model3)
dwtest(model4)
dwtest(model5)


#####1.4White test using LM test
residual1_sq = residuals(model1)^2
residual2_sq = residuals(model2)^2
residual3_sq = residuals(model3)^2
residual4_sq = residuals(model4)^2
residual5_sq = residuals(model5)^2

##deleting missing values
df_apple_clean <- na.omit(df_Apple)
df_AMD_clean <- na.omit(df_AMD)
df_AMZN_clean <- na.omit(df_AMZN)
df_INTC_clean <- na.omit(df_INTC)
df_MSFT_clean <- na.omit(df_MSFT)

##adding residual squrared to data frames
df_apple_clean <- df_apple_clean %>%
  dplyr::mutate(residual_sq = residual1_sq)
df_AMD_clean <- df_AMD_clean %>%
  dplyr::mutate(residual_sq = residual2_sq)
df_AMZN_clean <- df_AMZN_clean %>%
  dplyr::mutate(residual_sq = residual3_sq)
df_INTC_clean <- df_INTC_clean %>%
  dplyr::mutate(residual_sq = residual4_sq)
df_MSFT_clean <- df_MSFT_clean %>%
  dplyr::mutate(residual_sq = residual5_sq)

##Apple's auxiliary regression
model_white1 = lm(residual_sq ~ RM + SMB + HML +
                    I(RM^2) + I(SMB^2) + I(HML^2) + 
                    I(RM*SMB) + I(RM*HML) + I(HML*SMB), 
                  data = df_apple_clean)

##AMD's auxiliary regression
model_white2 = lm(residual_sq ~ RM + SMB + HML +
                    I(RM^2) + I(SMB^2) + I(HML^2) + 
                    I(RM*SMB) + I(RM*HML) + I(HML*SMB),
                  data = df_AMD_clean)
##AMZN's auxiliary regression
model_white3 = lm(residual_sq ~ RM + SMB + HML +
                    I(RM^2) + I(SMB^2) + I(HML^2) + 
                    I(RM*SMB) + I(RM*HML) + I(HML*SMB), 
                  data = df_AMZN_clean)
##INTC's auxiliary regression
model_white4 = lm(residual_sq ~ RM + SMB + HML +
                    I(RM^2) + I(SMB^2) + I(HML^2) + 
                    I(RM*SMB) + I(RM*HML) + I(HML*SMB),
                  data = df_INTC_clean)
##MSFT's auxiliary regression
model_white5 = lm(residual_sq ~ RM + SMB + HML +
                    I(RM^2) + I(SMB^2) + I(HML^2) + 
                    I(RM*SMB) + I(RM*HML) + I(HML*SMB), 
                  data = df_MSFT_clean)

##White test statistic
R21 <- summary(model_white1)$r.squared  # Get R-squared
LM_stat1 <- 299 * R21 #TR^2 test statistic

R22 <- summary(model_white2)$r.squared  
LM_stat2 <- 299 * R22

R23 <- summary(model_white3)$r.squared  
LM_stat3 <- 299 * R23

R24 <- summary(model_white4)$r.squared  
LM_stat4 <- 299 * R24

R25 <- summary(model_white5)$r.squared  
LM_stat5 <- 299 * R25

##White test critical value
chi_critical5 = qchisq(0.95, df = 9)
chi_critical10 = qchisq(0.9, df=9)
chi_critical1 = qchisq(0.99, df=9)
cat("Chi-square critical value at 5% significance level (df=9):", chi_critical5, "\n")
cat("Chi-square critical value at 10% significance level (df=9):", chi_critical10, "\n")
cat("Chi-square critical value at 1% significance level (df=9):", chi_critical1, "\n")

## obtain Heteroscedasticity consistent standard errors and conduct new t test.
install.packages("sandwich")
library(sandwich)
coeftest(model1, vcov = vcovHC(model1, type = "HC0"))
coeftest(model2, vcov = vcovHC(model2, type = "HC0"))
coeftest(model5, vcov = vcovHC(model5, type = "HC0"))

system("git push origin main") 
system("git pull origin main --rebase")
system("git push --force origin main")



