library("moments")
library("RcmdrMisc")
library("ggplot2")
library("moments")
library("fitdistrplus")
library("lmtest")
library("sandwich")
library("car")
library("vcd")
library("car")
library(stargazer)
library("readxl")
library("dplyr")
library("tidyr")
library("tseries")
library("fUnitRoots")
library("knitr")
library("kableExtra")
library("forecast")
library("rugarch")
library("vars")
library("FinTS")

data<-read_excel("OECDdata.xlsx")

data$SWEIP <- as.numeric(data$SWEIP)
data$DENIP<-as.numeric(data$DENIP)

glimpse(data)

#Plot variables of interests and descriptive statistics

data$SWERER_Change<-(log(data$SWERER)-log(lag(data$SWERER,12)))*100
data$FINRER_Change<-(log(data$FINRER)-log(lag(data$FINRER,12)))*100
data$DENRER_Change<-(log(data$DENRER)-log(lag(data$DENRER,12)))*100

data$FINIP_Change<-(log(data$FINIP)-log(lag(data$FINIP,12)))*100
data$SWEIP_Change<-(log(data$SWEIP)-log(lag(data$SWEIP,12)))*100
data$DENIP_Change<-(log(data$DENIP)-log(lag(data$DENIP,12)))*100
data$USIP_Change<-(log(data$USIP)-log(lag(data$USIP,12)))*100

data$SWEInf<-(log(data$SWECPI)-log(lag(data$SWECPI,12)))*100
data$FINInf<-(log(data$FINCPI)-log(lag(data$FINCPI,12)))*100
data$DENInf<-(log(data$DENCPI)-log(lag(data$DENCPI,12)))*100
data$USAInf<-(log(data$USCPI)-log(lag(data$USCPI,12)))*100

data$INF_DIFF_FIN<-(data$FINInf-lag(data$FINInf,1))*100
data$INF_DIFF_SWE<-(data$SWEInf-lag(data$SWEInf,1))*100
data$INF_DIFF_DEN<-(data$DENInf-lag(data$DENInf,1))*100



data$FINI3_diff<-data$FINi3-lag(data$FINi3,1)
data$SWEI3_diff<-data$SWEI3-lag(data$SWEI3,1)
data$DENI3_diff<-data$DENi3-lag(data$DENi3,1)

#2.diff

data$FINI3_diff2<-data$FINI3_diff-lag(data$FINI3_diff,1)
data$SWEI3_diff2<-data$SWEI3_diff-lag(data$SWEI3_diff,1)
data$DENI3_diff2<-data$DENI3_diff-lag(data$DENI3_diff,1)


variables <- c(
  "FINRER_Change", "SWERER_Change", "DENRER_Change",
  "FINDY", "SWEDY", "DENDY",
  "FINIP_Change", "SWEIP_Change", "DENIP_Change",
  "FINInf", "SWEInf", "DENInf",
  "FINi3","SWEI3","DENi3"
)

# Laske tilastot jokaiselle muuttujalle
stats_list <- lapply(
  train[variables],
  function(x) {
    x_no_na <- na.omit(x)    
    data.frame(
      mean      = mean(x_no_na),
      median    = median(x_no_na),
      sd        = sd(x_no_na),
      min       = min(x_no_na),
      q1        = quantile(x_no_na, 0.25),
      q3        = quantile(x_no_na, 0.75),
      max       = max(x_no_na),
      skewness  = skewness(x_no_na),
      kurtosis  = kurtosis(x_no_na),
      n         = length(x_no_na)
    )
  }
)

stats_df <- do.call(rbind, stats_list)
rownames(stats_df) <- variables

num_cols <- setdiff(names(stats_df), "jb_sig")
stats_df[num_cols] <- round(stats_df[num_cols], 3)
stats_df

latex_table <- stats_df %>%
  kable("latex",
        booktabs = TRUE,
        caption = "Descriptive statistics") %>%
  kable_styling(latex_options = c("striped", "hold_position"))

latex_table

library(tidyverse)

data_long6 <- pivot_longer(
  data,
  cols = c(FINRER_Change, FINDY),
  names_to = "variables",
  values_to = "Values"
)

# Calculate ranges for scaling
yL <- range(data_long6$Values[data_long6$variables == "FINDY"], na.rm = TRUE)
yR <- range(data_long6$Values[data_long6$variables == "FINRER_Change"], na.rm = TRUE)

# Plot
ggplot(data_long6, aes(x = time)) +
  geom_line(aes(
    y = ifelse(variables == "FINRER_Change",
               (Values - yR[1]) / (yR[2] - yR[1]) * (yL[2] - yL[1]) + yL[1],
               Values),
    color = variables
  ), linewidth = 1) +
  scale_y_continuous(
    name = "Dividend Yield (%)",
    sec.axis = sec_axis(
      ~ (. - yL[1]) / (yL[2] - yL[1]) * (yR[2] - yR[1]) + yR[1],
      name = "FINRER_Change (%)"
    )
  ) +
  scale_x_date(
    limits = c(as.Date("1997-01-30"), as.Date("2020-12-01")),
    date_labels = "%Y-%m"
  ) +
  theme_minimal() +
  ggtitle("Dividend yield and change in real exchange rate in Finland")+
  theme(legend.position = "bottom")


data_long7 <- pivot_longer(
  data,
  cols = c(SWERER_Change, SWEDY),
  names_to = "variables",
  values_to = "Values"
)

yL_2 <- range(data_long7$Values[data_long7$variables != "SWERER_Change"], na.rm = TRUE)  # SWEDY
yR_2 <- range(data_long7$Values[data_long7$variables == "SWERER_Change"], na.rm = TRUE)  # variable to rescale

# Plot
ggplot(data_long7, aes(x = time)) +  # make sure your column is 'Time'
  geom_line(aes(
    y = ifelse(variables == "SWERER_Change",
               (Values - yR_2[1]) / (yR_2[2] - yR_2[1]) * (yL_2[2] - yL_2[1]) + yL_2[1],
               Values),
    color = variables
  ), linewidth = 1) +
  scale_y_continuous(
    name = "Dividend Yield(%)",
    sec.axis = sec_axis(
      ~ (. - yL_2[1]) / (yL_2[2] - yL_2[1]) * (yR_2[2] - yR_2[1]) + yR_2[1],
      name = "SWERER_Change (%)"
    )
  ) +
  scale_x_date(
    limits = c(as.Date("1997-01-30"), as.Date("2020-12-01")),
    date_labels = "%Y-%m"
  ) +
  theme_minimal() +
  ggtitle("Dividend yield and change in real exchange rate in Sweden")+
  theme(legend.position = "bottom")




data_long8 <- pivot_longer(
  data,
  cols = c(DENRER_Change, DENDY),
  names_to = "variables",
  values_to = "Values"
)

# Calculate ranges for scaling
yL_3 <- range(data_long8$Values[data_long8$variables != "DENRER_Change"], na.rm = TRUE)        # Dividend Yield
yR_3 <- range(data_long8$Values[data_long8$variables == "DENRER_Change"], na.rm = TRUE)  # Variable to rescale

# Plot
ggplot(data_long8, aes(x = time)) +  # make sure column name is 'Time'
  geom_line(aes(
    y = ifelse(variables == "DENRER_Change",
               (Values - yR_3[1]) / (yR_3[2] - yR_3[1]) * (yL_3[2] - yL_3[1]) + yL_3[1],
               Values),
    color = variables
  ), linewidth = 1) +
  scale_y_continuous(
    name = "Dividend Yield (%)",
    sec.axis = sec_axis(
      ~ (. - yL_3[1]) / (yL_3[2] - yL_3[1]) * (yR_3[2] - yR_3[1]) + yR_3[1],
      name = "DENRER_Change (%)"
    )
  ) +
  scale_x_date(
    limits = c(as.Date("1997-01-30"), as.Date("2020-12-01")),
    date_labels = "%Y-%m"
  ) +
  theme_minimal() +
  ggtitle("Dividend yield and change in real exchange rate in Denmark")+
  theme(legend.position = "bottom")
 
jarque.bera.test(data$SWERER)#Data isn't normally distributed
jarque.bera.test(data$FINRER)#Data isn't normally distributed
jarque.bera.test(data$DENRER) #Data is normally distributed

b) Unit root tests

adfTest(train$FINDY, lags = 12, type = "c") #non-stationary
kpss.test(train$FINDY)#Non-Stationary

adfTest(train$SWEDY, lags = 12, type = "c") #non-stationary
kpss.test(train$SWEDY)#Non-Stationary

adfTest(train$DENDY, lags = 12, type = "c") #non-stationary
kpss.test(train$DENDY)#Non-Stationary

adfTest(train$FINRER_Change, lags = 12, type = "c")#non-stationary
kpss.test(train$FINRER_Change)#Stationary

adfTest(train$SWERER_Change, lags = 12, type = "c") #Non-Stationary
kpss.test(train$SWERER_Change)#Stationary

adfTest(train$DENRER_Change, lags = 12, type = "c")#Non-Stationary
kpss.test(train$DENRER_Change)#Stationary

#To make comparison meaningful we take first difference 
for all real exchange rate changes and dividends

data$diff_FINRER_Change <- c(NA, diff(data$FINRER_Change))
data$diff_SWERER_Change <- c(NA, diff(data$SWERER_Change))
data$diff_DENRER_Change <- c(NA, diff(data$DENRER_Change))

data$diff_FINDY <- c(NA, diff(data$FINDY, lag = 1))
data$diff_SWEDY <- c(NA, diff(data$SWEDY, lag = 1))
data$diff_DENDY <- c(NA, diff(data$DENDY, lag = 1))


train <- data %>%
  filter(time < as.POSIXct("2018-01-01"))

test <- data %>%
  filter(time >= as.POSIXct("2018-01-01"))


adfTest(train$diff_FINRER_Change, lags = 12, type = "ct") #Stationary
kpss.test(train$diff_FINRER_Change)#Stationary


adfTest(train$diff_SWERER_Change, lags = 12, type = "ct") #Stationary
kpss.test(train$diff_SWERER_Change)#Stationary


adfTest(train$diff_DENRER_Change, lags = 12, type = "ct") #Stationary
kpss.test(train$diff_DENRER_Change)#Stationary



adfTest(train$diff_FINDY, lags = 12, type = "c") #Stationary
kpss.test(train$diff_FINDY)#Stationary


adfTest(train$diff_SWEDY, lags = 12, type = "c") #Stationary
kpss.test(train$diff_SWEDY)#Stationary


adfTest(train$diff_DENDY, lags = 12, type = "c") #Stationary
kpss.test(train$diff_DENDY)#Stationary




#ACF and PACF plots
par(mfrow=c(3,2))
pacf(na.omit(train$diff_FINDY)) ARMA(0,0)
acf(na.omit(train$diff_FINDY))   
pacf(na.omit(train$diff_SWEDY)) #ARMA(0,0)
acf(na.omit(train$diff_SWEDY))  
pacf(na.omit(train$diff_DENDY)) #ARMA(0,0)
acf(na.omit(train$diff_DENDY))

pacf(na.omit(train$diff_FINRER_Change))  
acf(na.omit(train$diff_FINRER_Change))  
pacf(na.omit(train$diff_SWERER_Change)) 
acf(na.omit(train$diff_SWERER_Change))  
pacf(na.omit(train$diff_DENRER_Change))
acf(na.omit(train$diff_DENRER_Change))


#Using first differences for ARMA. #We are examining relationship between dividend
yield and RER

fit_FINDY <- auto.arima(train$diff_FINDY, ic = "aic", max.p=12, max.q=12, allowmean=TRUE)
summary(fit_FINDY)#ARIMA(0,0,0)
checkresiduals(fit_FINDY)
checkresiduals(fit_FINDY$resid^2) #Variance not constant!! #Some autocorrelation might exist

Box.test(resid(fit_FINDY), lag = 12,type = "Ljung") #Not independently distributed

fit_FIN_RER <- auto.arima(train$diff_FINRER_Change, ic = "aic",max.p=12, max.q=12,allowdrift=TRUE, allowmean=TRUE)
summary(fit_FIN_RER)#ARIMA(0,0,1)
checkresiduals(fit_FIN_RER, lag=12)  #Autocorrelation exists
qqline(residuals(fit_FIN_RER)) 
checkresiduals(fit_FIN_RER$resid^2) #Variance is not constant, heteroskedasticity

Box.test(resid(fit_FIN_RER), lag =12, type = "Ljung")#Autocorrelation exists

fit_SWEDY <- auto.arima(train$diff_SWEDY, ic = "aic",max.p=12, max.q=12,allowdrift=TRUE, allowmean=TRUE)
summary(fit_SWEDY) #ARIMA(0,0,0)
checkresiduals(fit_SWEDY, lag=12) 
checkresiduals(fit_SWEDY$resid^2)          #Variance not constant!
Box.test(resid(fit_SWEDY),lag=12, type = "Ljung") #Independently distributed!

fit_SWE_RER <- auto.arima(train$diff_SWERER_Change, ic = "aic",max.p=12, max.q=12, allowdrift=TRUE, allowmean=TRUE)
summary(fit_SWE_RER)
checkresiduals(fit_SWE_RER, lag=12) #Autocorrelation exists
checkresiduals(fit_SWE_RER$resid^2, lag=12) #Variance is not constant
Box.test(resid(fit_SWE_RER), lag=12, type = "Ljung") #Not independently distributed

fit_DENDY <- auto.arima(train$diff_DENDY, ic = "aic",max.p=12, max.q=12,allowdrift=TRUE, allowmean=TRUE)
summary(fit_DENDY) #ARIMA(0,0,0)
checkresiduals(fit_DENDY, lag=12) #No autocorrelation left
checkresiduals(fit_DENDY$resid^2, lag=12)#Variance constant constant

Box.test(resid(fit_DENDY),lag = 12, type = "Ljung") #Indepenently distributed

fit_DEN_RER <- auto.arima(train$diff_DENRER_Change, ic = "aic",max.p=12, max.q=12,allowdrift=TRUE, allowmean=TRUE)
summary(fit_DEN_RER)
checkresiduals(fit_DEN_RER)#No unexplained autocorrelation
checkresiduals(fit_DEN_RER$resid^2) #Variance is not constant
Box.test(resid(fit_DEN_RER), type = "Ljung")#Independently distributed

fit_FINDY   #ARMA(0,0)
summary(fit_FINDY)
fit_FIN_RER #ARMA(0,1)
fit_SWEDY   #ARMA(0,0)
fit_SWE_RER #ARMA(3,0)
fit_DENDY   #ARMA(0,0)
fit_DEN_RER #ARMA(0,1)

plot(train$diff_FINDY, type="l",ylab ="Difference in dividend yield",xlab="time", main="Finland")
plot(train$diff_SWEDY, type="l",ylab ="Difference in dividend yield",xlab="time", main="Sweden")
plot(train$diff_DENDY, type="l",ylab ="Difference in inflation yield",xlab="time", main="Denmark")

#Forecasting

h <- 36  # ennustetaan 36 kuukautta eteenpäin

forecast_FIN_RER<-predict(fit_FIN_RER,n.ahead = 36)
forecast_FIN_RER
forecast_SWE_RER<-predict(fit_SWE_RER,n.ahead = 36)
forecast_SWE_RER
forecast_DEN_RER<-predict(fit_DEN_RER,n.ahead = 36)
forecast_DEN_RER
forecast_FINDY<-predict(fit_FINDY,n.ahead = 36)
forecast_FINDY
forecast_SWEDY<-predict(fit_SWEDY,n.ahead = 36)
forecast_SWEDY
forecast_DENDY<-predict(fit_DENDY,n.ahead = 36)
forecast_DENDY

#Static forecasts
actuals_FINRER <- test$diff_FINRER_Change[(nrow(test)-35):nrow(test)]

static_FINRER <- numeric(length(actuals_FINRER))
for(i in 1:length(static_FINRER)) {
  
  # Mallin päivittäminen: train + kaikki tiedossa olevat test-arvot tähän asti
  model_i <- Arima(c(train$diff_FINRER_Change, actuals_FINRER[1:(i-1)]), order=c(0,0,1))  # MA(1)
  
  # Ennustetaan seuraava askel
  static_FINRER[i] <- forecast(model_i, h=1)$mean
}

actuals_DENRER <- test$diff_DENRER_Change[(nrow(test)-35):nrow(test)]
static_DENRER <- numeric(length(actuals_DENRER))
for(i in 1:length(static_DENRER)) {
  
  # Mallin päivittäminen: train + kaikki tiedossa olevat test-arvot tähän asti
  model_i <- Arima(c(train$diff_DENRER_Change, actuals_DENRER[1:(i-1)]), order=c(0,0,1))  # MA(1)
  
  # Ennustetaan seuraava askel
  static_DENRER[i] <- forecast(model_i, h=1)$mean
}
forecast_DEN_RER

#Dividend yields

actuals_FINDY <- test$diff_FINDY[1:nrow(test)]
static_FINDY <- numeric(length(actuals_FINDY))
for(i in 1:length(static_FINDY)) {
  
  # Mallin päivittäminen: train + kaikki tiedossa olevat test-arvot tähän asti
  model_i <- Arima(c(train$diff_FINDY, actuals_FINDY[1:(i-1)]), order=c(1,0,1))  # MA(0)
  
  # Ennustetaan seuraava askel
  static_FINDY[i] <- forecast(model_i, h=1)$mean
}

actuals_SWEDY <- test$diff_SWEDY[1:nrow(test)]
static_SWEDY <- numeric(length(actuals_SWEDY))
for(i in 1:length(static_SWEDY)) {
  
  # Mallin päivittäminen: train + kaikki tiedossa olevat test-arvot tähän asti
  model_i <- Arima(c(train$diff_SWEDY, actuals_SWEDY[1:(i-1)]), order=c(0,0,0))  # MA(0)
  
  # Ennustetaan seuraava askel
  static_SWEDY[i] <- forecast(model_i, h=1)$mean
}

actuals_DENDY <- test$diff_DENDY[1:nrow(test)]
static_DENDY <- numeric(length(actuals_DENDY))
for(i in 1:length(static_DENDY)) {
  
  # Mallin päivittäminen: train + kaikki tiedossa olevat test-arvot tähän asti
  model_i <- Arima(c(train$diff_DENDY, actuals_DENDY[1:(i-1)]), order=c(0,0,0))  
  
  # Ennustetaan seuraava askel
  static_DENDY[i] <- forecast(model_i, h=1)$mean
}

static_fc_SWERER = fit_SWE_RER$coef[1]*data$diff_SWERER_Change[228:263]+fit_SWE_RER$coef[2]*data$diff_SWERER_Change[227:262]+fit_SWE_RER$coef[3]*data$diff_SWERER_Change[226:261]

length(data$time)
length(data$diff_SWERER_Change)
length(train$diff_SWERER_Change)
data$SWERER_Change


par(lwd = 2, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)

plot(test$time, test$diff_FINRER_Change,
     type = "l",
     col = "black",
     xlab = "Time",
     ylab = "RER_Change diff",
     main = "Forecast vs Actual (FIN)",
     lwd = 2)
lines(test$time, forecast_FIN_RER$pred, 
      col = "blue", 
      lwd = 2, 
      lty = 1)  # jatkuva viiva
lines(test$time, static_FINRER, 
      col = "red", 
      lwd = 2, 
      lty = 2)  # jatkuva viiva


legend("topright",
       legend = c("Actual", "Dynamic", "Static"),
       col = c("black", "blue", "red"),
       lty = c(1, 1, 2),
       lwd = 2,
       bty = "n")  # bty="n" 



length(train$diff_FINRER_Change)#228
length(data$diff_FINRER_Change) #264


par(lwd = 2, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)

plot(test$time, test$diff_SWERER_Change,
     type = "l",
     col = "black",
     xlab = "Time",
     ylab = "RER_Change diff",
     main = "Forecast vs Actual (SWE)",
     lwd = 2)
lines(test$time, forecast_SWE_RER$pred, 
      col = "blue", 
      lwd = 2, 
      lty = 1)  # jatkuva viiva
lines(test$time, static_fc_SWERER, 
      col = "red", 
      lwd = 2, 
      lty = 2)  # jatkuva viiva
legend("topright",
       legend = c("Actual", "Dynamic", "Static"),
       col = c("black", "blue", "red"),
       lty = c(1, 1, 2),
       lwd = 2,
       bty = "n")  # bty="n" 

#Denmark

static_fc_DENRER = fit_arma_DEN_RER_Change$coef[1]*data$diff_DENRER_Change[228:263]
static_fc_DENRER
par(lwd = 2, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)

plot(test$time, test$diff_DENRER_Change,
     type = "l",
     col = "black",
     xlab = "Time",
     ylab = "RER_Change diff",
     main = "Forecast vs Actual (DEN)",
     lwd = 2)
lines(test$time, forecast_DEN_RER$pred, 
      col = "blue", 
      lwd = 2, 
      lty = 1)  # jatkuva viiva
lines(test$time, static_DENRER, 
      col = "red", 
      lwd = 2, 
      lty = 2)  # jatkuva viiva
legend("topright",
       legend = c("Actual", "Dynamic", "Static"),
       col = c("black", "blue", "red"),
       lty = c(1, 1, 2),
       lwd = 2,
       bty = "n")  # bty="n" 


#Dividend yields

forecast_FINDY<-predict(fit_FINDY,n.ahead = 36)
forecast_FINDY

forecast_SWEDY<-predict(fit_SWEDY,n.ahead = 36)
forecast_SWEDY

forecast_DENDY<-predict(fit_DENDY,n.ahead = 36)
forecast_DENDY

par(mfrow=c(1,1))
par(lwd = 2, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)

plot(test$time, test$diff_FINDY,
     type = "l",
     col = "black",
     xlab = "Time",
     ylab = "Dividend yield diff",
     main = "Forecast vs Actual (FIN)",
     lwd = 2)
lines(test$time, forecast_FINDY$pred, 
      col = "blue", 
      lwd = 2, 
      lty = 1)  # jatkuva viiva
lines(test$time, static_FINDY, 
      col = "red", 
      lwd = 2, 
      lty = 2)  # jatkuva viiva

legend("topright",
       legend = c("Actual", "Dynamic", "Static"),
       col = c("black", "blue","red"),
       lty = c(1, 1, 2),
       lwd = 2,
       bty = "n")  # bty="n" 


par(lwd = 2, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)

plot(test$time, test$diff_SWEDY,
     type = "l",
     col = "black",
     xlab = "Time",
     ylab = "Dividend yield diff",
     main = "Forecast vs Actual (SWE)",
     lwd = 2)
lines(test$time, forecast_SWEDY$pred, 
      col = "blue", 
      lwd = 2, 
      lty = 1)  
lines(test$time, static_SWEDY, 
      col = "red", 
      lwd = 2, 
      lty = 2)  
legend("topright",
       legend = c("Actual", "Dynamic", "Static"),
       col = c("black", "blue","red"),
       lty = c(1, 1, 2),
       lwd = 2,
       bty = "n")  # bty="n" 

par(mfrow=c(1,1))
par(lwd = 2, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)

plot(test$time, test$diff_DENDY,
     type = "l",
     col = "black",
     xlab = "Time",
     ylab = "Dividend yield diff",
     main = "Forecast vs Actual (DEN)",
     lwd = 2)
lines(test$time, forecast_DENDY$pred, 
      col = "blue", 
      lwd = 2, 
      lty = 1)  # jatkuva viiva
lines(test$time, static_DENDY, 
      col = "red", 
      lwd = 2, 
      lty = 2)  

legend("topright",
       legend = c("Actual", "Dynamic", "Static"),
       col = c("black", "blue", "red"),
       lty = c(1, 1, 2),
       lwd = 2,
       bty = "n")  # bty="n" 

#Comparison

#Dynamic models
accuracy(forecast_FIN_RER$pred, test$diff_FINRER_Change)
accuracy(forecast_SWE_RER$pred, test$diff_SWERER_Change)
accuracy(forecast_DEN_RER$pred, test$diff_DENRER_Change)

#Static models

accuracy(static_FINRER, test$diff_FINRER_Change)
accuracy(static_fc_SWERER, test$diff_SWERER_Change)
accuracy(static_DENRER, test$diff_DENRER_Change)
accuracy(static_FINDY, test$diff_FINDY))
accuracy(static_SWEDY, test$diff_SWEDY)
accuracy(static_DENDY, test$diff_DENDY)


#f

#Unit roots for variables that are not tested

#Industrial production

adfTest(train$FINIP_Change, lags = 12, type = "c") #Stationary
kpss.test(train$FINIP_Change) #Stationary

adfTest(train$SWEIP_Change, lags = 12, type = "c") #Stationary
kpss.test(train$SWEIP_Change) #Stationary 

adfTest(train$DENIP_Change, lags = 12, type = "c") #Stationary
kpss.test(train$DENIP_Change) #Stationary

#Inflation

adfTest(train$FINInf, lags = 12, type = "c")#Non-Stationary
kpss.test(train$FINInf) #Non-stationary P-value 0.047

adfTest(train$SWEInf, lags = 12, type = "c")#Non-Stationary
kpss.test(train$SWEInf) #Stationary #Non-stationary

adfTest(train$DENInf, lags = 12, type = "c")#Non-Stationary
kpss.test(train$DENInf) #Non-stationary



#1.difference

adfTest(train$INF_DIFF_FIN, lags = 12, type = "c")#Stationary
kpss.test(train$INF_DIFF_FIN) #Stationary

adfTest(train$INF_DIFF_SWE, lags = 12, type = "c")#Stationary
kpss.test(train$INF_DIFF_SWE)#Stationary

adfTest(train$INF_DIFF_DEN, lags = 12, type = "c")#Stationary
kpss.test(train$INF_DIFF_DEN)#Stationary




#Short-term interest rates

adfTest(train$FINi3, lags = 12, type="c") #Non-Stationary
kpss.test(train$FINi3)#Non-Stationary

adfTest(train$SWEI3, lags = 12, type="c") #Non-stationary
kpss.test(train$SWEI3)#Non-Stationary

adfTest(train$DENi3, lags = 12, type="c") #Non-stationary
kpss.test(train$DENi3)#Non-Stationary


#1.difference

adfTest(train$FINI3_diff, lags = 12, type="c") #Stationary
kpss.test(train$FINI3_diff)#Stationary

adfTest(train$SWEI3_diff, lags = 12, type="c") #Stationary
kpss.test(train$SWEI3_diff) #Stationary

adfTest(train$DENI3_diff, lags = 12, type="c") #Stationary
kpss.test(train$DENI3_diff) #Stationary



#Finland

#Optimal lag length

Var_select<-VARselect(
  na.omit(train[, c("diff_FINDY",
                    "FINIP_Change",
                    "INF_DIFF_FIN",
                    "diff_FINRER_Change",
                    "FINI3_diff")]),
  lag.max = 12
)

Var_select
Var_select$selection #Optimal lag length, aic=12, HQ=1, SC=1, FPE=12


var_model<-VAR(na.omit(train[,c("diff_FINDY","FINIP_Change","INF_DIFF_FIN", "diff_FINRER_Change","FINI3_diff")]), p = 1)

summary(var_model)

stargazer(
  var_model$varresult,
  type = "latex",
  digits = 3
)

#Sweden

#Optimal lag length

Var_select_SWE<-VARselect(
  na.omit(train[, c("diff_SWEDY",
                    "SWEIP_Change",
                    "INF_DIFF_SWE",
                    "diff_SWERER_Change",
                    "SWEI3_diff")]),
  lag.max = 12
)


Var_select_SWE$selection #Optimal lag length => AIC = 3, HQ=2, SC=1, FPE=3

var_model_SWE<-VAR(na.omit(train[,c("diff_SWEDY","SWEIP_Change","INF_DIFF_SWE", "diff_SWERER_Change","SWEI3_diff")]), p = 1)

summary(var_model_SWE)

stargazer(
  var_model_SWE$varresult,
  type = "latex",
  digits = 3
)

#Denmark
Var_select_DEN<-VARselect(
  na.omit(train[, c("diff_DENDY",
                    "DENIP_Change",
                    "INF_DIFF_DEN",
                    "diff_DENRER_Change",
                    "DENI3_diff")]),
  lag.max = 12
)
Var_select_DEN$selection #Optimal lag length, aic=3, HQ=1, SC=1, FPE=3


var_model_DEN<-VAR(na.omit(train[,c("diff_DENDY","DENIP_Change","INF_DIFF_DEN", "diff_DENRER_Change","DENI3_diff")]), p = 1)

summary(var_model_DEN)

stargazer(
  var_model_DEN$varresult,
  type = "latex",
  digits = 3
)


#FIN

# To test whether lagged values of real exchange rate or lagged values of dividend yield are able to explain others


linearHypothesis(
  var_model$varresult$diff_FINRER_Change,
  c("diff_FINDY.l1 = 0") #P-value 0.01091 
  )

#Dividend yield helps to explain first difference of exchange rate changes 


linearHypothesis(
  var_model$varresult$diff_FINDY,
  c("diff_FINRER_Change.l1 = 0") #P-value 0.2329
)

linearHypothesis(
  var_model_SWE$varresult$diff_SWERER_Change,
  c("diff_SWEDY.l1 = 0")
)						
linearHypothesis(
  var_model_SWE$varresult$diff_SWEDY,
  c("diff_SWERER_Change.l1 = 0")
)						

#DEN
linearHypothesis(
  var_model_DEN$varresult$diff_DENRER_Change,
  c("diff_DENDY.l1 = 0")
)  					

linearHypothesis(
  var_model_DEN$varresult$diff_DENDY,
  c("diff_DENRER_Change.l1 = 0") 
)

ir_FIN<-irf(var_model,n.ahead = 12, runs=100)
ir_SWE<-irf(var_model_SWE,n.ahead = 12, runs=100)
ir_DEN<-irf(var_model_DEN,n.ahead = 12, runs=100)
plot(ir_FIN)
plot(ir_SWE)
plot(ir_DEN)

ir_FIN
ir_SWE
ir_DEN

vd_fin = fevd(var_model,n.ahead = 12)
vd_fin
plot(vd_fin)

vd_swe = fevd(var_model_SWE,n.ahead = 12)
vd_swe
plot(vd_swe)

vd_den = fevd(var_model_DEN,n.ahead = 12)
vd_den
plot(vd_den)

# Change the order

var_model_SWE2<-VAR(na.omit(train[,c("SWEIP_Change","INF_DIFF_SWE", "diff_SWERER_Change","diff_SWEDY","SWEI3_diff")]), p = 1)
var_model_FIN2<-VAR(na.omit(train[,c("FINIP_Change","INF_DIFF_FIN", "diff_FINRER_Change","diff_FINDY","FINI3_diff")]), p = 1)
var_model_DEN2<-VAR(na.omit(train[,c("DENIP_Change","INF_DIFF_DEN", "diff_DENRER_Change","diff_DENDY","DENI3_diff")]), p = 1)

linearHypothesis(
  var_model_FIN2$varresult$diff_FINRER_Change,
  c("diff_FINDY.l1 = 0")


linearHypothesis(
  var_model_FIN2$varresult$diff_FINDY,
  c("diff_FINRER_Change.l1 = 0") 
)

linearHypothesis(
  var_model_SWE2$varresult$diff_SWERER_Change,
  c("diff_SWEDY.l1 = 0") #P-value 0.9072, dividend yield doesn't granger cause real exchange rate change
)   
linearHypothesis(
  var_model_SWE2$varresult$diff_SWEDY,
  c("diff_SWERER_Change.l1 = 0") #P-value 0.7729, diff_SWERER_Change doesn't granger cause dividend yield changes

)

linearHypothesis(
  var_model_DEN2$varresult$diff_DENRER_Change,
  c("diff_DENDY.l1 = 0") #P-value 0.3077, dividend yield doesn't granger cause real exchange rate change
)  #P-value 0.1326 => Dividend yield doesn't granger-cause DENRER_Change

linearHypothesis(
  var_model_DEN2$varresult$diff_DENDY,
  c("diff_DENRER_Change.l1 = 0") #P-value 0.07018, diff_DENRER_Change doesn't granger cause dividend yield changes

)

summary(var_model_FIN2)
summary(var_model_SWE2)
summary(var_model_DEN2)


ir_FIN2<-irf(var_model_FIN2,n.ahead = 12)
ir_SWE2<-irf(var_model_SWE2,n.ahead = 12)
ir_DEN2<-irf(var_model_DEN2,n.ahead = 12)
plot(ir_FIN2)
plot(ir_SWE2)
plot(ir_DEN2)
	
vd_fin2 = fevd(var_model_FIN2,n.ahead = 12)
vd_fin2
plot(vd_fin2)

vd_swe2 = fevd(var_model_SWE2,n.ahead = 12)
vd_swe2
plot(vd_swe2)

vd_den2 = fevd(var_model_DEN2,n.ahead = 12)
vd_den2
plot(vd_den2)


#Cointegration analysis

model_FIN<-lm(train$FINRER_Change~train$FINDY)
model_SWE<-lm(train$SWERER_Change~train$SWEDY)
model_DEN<-lm(train$DENRER_Change~train$DENDY)

residuals_FIN<-resid(model_FIN)
residuals_SWE<-resid(model_SWE)
residuals_DEN<-resid(model_DEN)


adfTest(residuals_FIN, lags = 12, type = "c") #Non-Stationary, p-value=0.09474
adfTest(residuals_SWE, lags = 12, type = "c") #Non-Stationary, p-value=0.08518
adfTest(residuals_DEN, lags = 12, type = "c") #Non-Stationary, p-value=0.06947

summary(y)
adfTest(residuals_FIN, lags = 6, type = "c") #Stationary, p-value=0.01
adfTest(residuals_SWE, lags = 6, type = "c") #Stationary, p-value=0.01
adfTest(residuals_DEN, lags = 6, type = "c") #Stationary, p-value=0.01


adfTest(residuals_FIN, lags = 8, type = "c") #Stationary, p-value <0.01
adfTest(residuals_SWE, lags = 8, type = "c") #Stationary, p-value <0.01
adfTest(residuals_DEN, lags = 8, type = "c") #Stationary, p-value <0.01

ect_FIN<-lag(residuals_FIN, 1)
ect_SWE<-lag(residuals_SWE, 1)
ect_DEN<-lag(residuals_DEN, 1)

ect_FIN

ecm_FIN <- lm(
  train$diff_FINRER_Change[-(1:13)] ~ train$diff_FINDY[-(1:13)] + ect_FIN[-1]  # lagatun residuaalin ensimmäinen NA poistettu
)


ecm_SWE <- lm(
  train$diff_SWERER_Change[-(1:13)] ~ train$diff_SWEDY[-(1:13)] + ect_SWE[-1]
)

ecm_DEN <- lm(
  train$diff_DENRER_Change[-(1:13)] ~ train$diff_DENDY[-(1:13)] + ect_DEN[-1]
)

summary(ecm_FIN)
summary(ecm_SWE)
summary(ecm_DEN)

#Plot FIN

time_index <- train$time[-(1:12)]
plot(time_index, train$FINRER_Change[-(1:12)], type="l", col="black")
lines(time_index, model_FIN$fitted.values, col="red")
lines(time_index, model_FIN$residuals, col="blue")
axis(side=4, at = pretty(range(model_FIN$residuals)))
legend("bottomleft", legend=c("Actual", "Fitted"),col=c("black","red"),lty=1)
legend("bottomright", legend=c("Resid"),col=c("blue"),lty= 1)

#Plot SWE

plot(time_index, train$SWERER_Change[-(1:12)], type="l", col="black")
lines(time_index, model_SWE$fitted.values, col="red")
lines(time_index, model_SWE$residuals, col="blue")
axis(side=4, at = pretty(range(model_SWE$residuals)))
legend("bottomleft", legend=c("Actual", "Fitted"),col=c("black","red"),lty=1)
legend("bottomright", legend=c("Resid"),col=c("blue"),lty= 1)

#Plot DEN

plot(time_index, train$DENRER_Change[-(1:12)], type="l", col="black")
lines(time_index, model_DEN$fitted.values, col="red")
lines(time_index, model_DEN$residuals, col="blue")
axis(side=4, at = pretty(range(model_DEN$residuals)))
legend("bottomleft", legend=c("Actual", "Fitted"),col=c("black","red"),lty=1)
legend("bottomright", legend=c("Resid"),col=c("blue"),lty= 1)


#m GARCH-analysis

plot(train$time,ts(train$diff_FINDY), type="l")
plot(train$time,ts(train$diff_SWEDY), type="l")
plot(train$time,ts(train$diff_DENDY), type="l")

par(mfrow=c(3,1))
plot(train$time,ts(train$diff_FINRER_Change), type="l")
plot(train$time,ts(train$diff_SWERER_Change), type="l")
plot(train$time,ts(train$diff_DENRER_Change), type="l")


#ARCH tests
ArchTest(residuals(fit_FINDY), lags = 12) #ARCH-effects exist
Box.test(fit_FINDY$resid^2,lag=12)

ArchTest(residuals(fit_SWEDY), lags = 12) #ARCH-effects exist
Box.test(fit_SWEDY$resid^2,lag=12)

ArchTest(residuals(fit_DENDY),lags = 12) #ARCH-effects exist
Box.test(fit_DENDY$resid^2,lag=12)

ArchTest(residuals(fit_FIN_RER), lags = 12) #ARCH-effects exist
ArchTest(residuals(fit_SWE_RER), lags = 12) #ARCH-effects exist
ArchTest(residuals(fit_DEN_RER), lags = 12) #ARCH-effects exist


#Testing different Garch-representations


spec_FIN = ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=FALSE),variance.model = list(
garchOrder=c(1,1),model="sGARCH"))

spec_FIN2 = ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=FALSE),variance.model = list(
garchOrder=c(2,1),model="sGARCH"))

spec_FIN3 = ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=FALSE),variance.model = list(
garchOrder=c(3,1),model="sGARCH"))

spec_FIN4 = ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=FALSE),variance.model = list(
garchOrder=c(1,2),model="sGARCH"))

spec_FIN5 = ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=FALSE),variance.model = list(
garchOrder=c(2,2),model="sGARCH"))

spec_FIN6 = ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=FALSE),variance.model = list(
garchOrder=c(2,3),model="sGARCH"))

spec_FIN7 = ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=FALSE),variance.model = list(
garchOrder=c(1,3),model="sGARCH"))

spec_FIN8 = ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=FALSE),variance.model = list(
garchOrder=c(2,3),model="sGARCH"))

spec_FIN9 = ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=FALSE),variance.model = list(
garchOrder=c(3,3),model="sGARCH"))

fit_FINDY   #ARMA(0,0)
fit_FIN_RER #ARMA(0,1)
fit_SWEDY   #ARMA(0,0)
fit_SWE_RER #ARMA(3,0)
fit_DENDY   #ARMA(0,0)
fit_DEN_RER #ARMA(0,1)

length(train$FINRER)

GARCH_FIN<-ugarchfit(spec_FIN,data=na.omit(train$diff_FINDY))  #AIC = 0.44313 #BIC=0.48839  #GARCH(1,1) * #No remaining autocorrelation
GARCH_FIN

GARCH_FIN2<-ugarchfit(spec_FIN2,data=na.omit(train$diff_FINDY)) #AIC = 0.45458 #BIC = 0.51494
GARCH_FIN2

GARCH_FIN3<-ugarchfit(spec_FIN3,data=na.omit(train$diff_FINDY)) #AIC = 0.46519 #BIC = 0.54063
GARCH_FIN3

GARCH_FIN4<-ugarchfit(spec_FIN4,data=na.omit(train$diff_FINDY)) #AIC = 0.44680 #BIC = 0.50716  #GARCH(1,2)
GARCH_FIN4

GARCH_FIN5<-ugarchfit(spec_FIN5,data=na.omit(train$diff_FINDY)) #AIC = 0.45561 #BIC = 0.53105
GARCH_FIN5

GARCH_FIN6<-ugarchfit(spec_FIN6,data=na.omit(train$diff_FINDY)) #AIC = 0.45098 #Bayes = 0.54150
GARCH_FIN6

GARCH_FIN7<-ugarchfit(spec_FIN7,data=na.omit(train$diff_FINDY)) #AIC = 0.44330 #Bayes = 0.51874
GARCH_FIN7

GARCH_FIN8<-ugarchfit(spec_FIN8,data=na.omit(train$diff_FINDY)) #AIC = 0.45098 #Bayes = 0.54150
GARCH_FIN8

GARCH_FIN9<-ugarchfit(spec_FIN9,data=na.omit(train$diff_FINDY)) #AIC= 0.45979 #Bayes = 0.56540
GARCH_FIN9


#Sweden

spec_SWE = ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=FALSE),variance.model = list(
garchOrder=c(1,1),model="sGARCH"))

spec_SWE2 = ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=FALSE),variance.model = list(
garchOrder=c(2,1),model="sGARCH"))

spec_SWE3 = ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=FALSE),variance.model = list(
garchOrder=c(3,1),model="sGARCH"))

spec_SWE4 = ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=FALSE),variance.model = list(
garchOrder=c(1,2),model="sGARCH"))

spec_SWE5 = ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=FALSE),variance.model = list(
garchOrder=c(2,2),model="sGARCH"))

spec_SWE6 = ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=FALSE),variance.model = list(
garchOrder=c(2,3),model="sGARCH"))

spec_SWE7 = ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=FALSE),variance.model = list(
garchOrder=c(1,3),model="sGARCH"))

spec_SWE8 = ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=FALSE),variance.model = list(
garchOrder=c(3,2),model="sGARCH"))

spec_SWE9 = ugarchspec(mean.model = list(armaOrder=c(1,0),include.mean=TRUE),variance.model = list(
garchOrder=c(1,1),model="sGARCH"))


GARCH_SWE<-ugarchfit(spec_SWE,data=na.omit(train$diff_SWEDY)) #AIC = -0.87381 #BIC = -0.82855 #GARCH(1,1)
GARCH_SWE

GARCH_SWE2<-ugarchfit(spec_SWE2,data=na.omit(train$diff_SWEDY)) #AIC = -0.87749  #BIC = -0.81714 
GARCH_SWE2 #GARCH(2,1)

GARCH_SWE3<-ugarchfit(spec_SWE3,data=na.omit(train$diff_SWEDY)) #AIC = -0.88873 #BIC = -0.81329
GARCH_SWE3 #GARCH(3,1)

GARCH_SWE4<-ugarchfit(spec_SWE4,data=na.omit(train$diff_SWEDY)) #AIC =-0.87749 #BIC = -0.81714
GARCH_SWE4 #GARCH(1,2)

GARCH_SWE5<-ugarchfit(spec_SWE5,data=na.omit(train$diff_SWEDY)) #AIC= -0.86868 #BIC = -0.79324
GARCH_SWE5

GARCH_SWE6<-ugarchfit(spec_SWE6,data=na.omit(train$diff_SWEDY)) #AIC= -0.88560 #Bayes = -0.79507
GARCH_SWE6

GARCH_SWE7<-ugarchfit(spec_SWE7,data=na.omit(train$diff_SWEDY)) #AIC = -0.88822 #Bayes = -0.81278
GARCH_SWE7

checkresiduals(residuals(GARCH_SWE7, standardize = TRUE))
checkresiduals(residuals(GARCH_SWE7, standardize = TRUE)^2)
ArchTest(residuals(GARCH_SWE7, standardize = TRUE), lags = 12) #No ARCH effect left


GARCH_SWE8<-ugarchfit(spec_SWE8,data=na.omit(train$diff_SWEDY)) #AIC= -0.87153  #Bayes = -0.76591 
GARCH_SWE8

GARCH_SWE9<-ugarchfit(spec_SWE9,data=na.omit(train$diff_SWEDY)) #AIC=-0.87424 #Bayes = -0.75354
GARCH_SWE9


#Denmark 

spec_DEN = ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=FALSE),variance.model = list(
garchOrder=c(1,1),model="sGARCH"))

spec_DEN2 = ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=FALSE),variance.model = list(
garchOrder=c(2,1),model="sGARCH"))

spec_DEN3 = ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=FALSE),variance.model = list(
garchOrder=c(3,1),model="sGARCH"))

spec_DEN4 = ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=FALSE),variance.model = list(
garchOrder=c(1,2),model="sGARCH"))

spec_DEN5 = ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=FALSE),variance.model = list(
garchOrder=c(2,2),model="sGARCH"))

spec_DEN6 = ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=FALSE),variance.model = list(
garchOrder=c(2,3),model="sGARCH"))

spec_DEN7 = ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=FALSE),variance.model = list(
garchOrder=c(1,3),model="sGARCH"))

spec_DEN8 = ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=FALSE),variance.model = list(
garchOrder=c(3,2),model="sGARCH"))

spec_DEN9 = ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=FALSE),variance.model = list(
garchOrder=c(3,3),model="sGARCH"))


GARCH_DEN<-ugarchfit(spec_DEN,data=na.omit(train$diff_DENDY)) #AIC = -0.36726 #BIC= -0.32200 *
GARCH_DEN
ArchTest(residuals(GARCH_DEN, standardize = TRUE), lags = 12) #ARCH effect left


GARCH_DEN2<-ugarchfit(spec_DEN2,data=na.omit(train$diff_DENDY)) #AIC =-0.35852  #BIC = -0.29817
GARCH_DEN2

GARCH_DEN3<-ugarchfit(spec_DEN3,data=na.omit(train$diff_DENDY)) #AIC = -0.34979 #BIC = -0.27435
GARCH_DEN3

GARCH_DEN4<-ugarchfit(spec_DEN4,data=na.omit(train$diff_DENDY)) #AIC = -0.35852 #BIC = -0.29817
GARCH_DEN4

GARCH_DEN5<-ugarchfit(spec_DEN5,data=na.omit(train$diff_DENDY)) #AIC= -0.34969 #BIC = -0.27425
GARCH_DEN5

GARCH_DEN6<-ugarchfit(spec_DEN6,data=na.omit(train$diff_DENDY)) #AIC= -0.34095 #Bayes = -0.25042
GARCH_DEN6

GARCH_DEN7<-ugarchfit(spec_DEN7,data=na.omit(train$diff_DENDY)) #AIC = -0.34978 #Bayes = -0.27434
GARCH_DEN7

GARCH_DEN8<-ugarchfit(spec_DEN8,data=na.omit(train$diff_DENDY)) #AIC= -0.34097  #Bayes = -0.25045
GARCH_DEN8

GARCH_DEN9<-ugarchfit(spec_DEN9,data=na.omit(train$diff_DENDY)) #AIC= -0.31832 #Bayes = -0.18253
GARCH_DEN9


spec_DENDY = ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=FALSE),variance.model = list(
garchOrder=c(1,0),model="sGARCH"))
GARCH_DENDY<-ugarchfit(spec_DENDY,data=na.omit(train$diff_DENDY)) 
GARCH_DENDY


#Real exchange rates

spec_FINRER = ugarchspec(mean.model = list(armaOrder=c(0,1),include.mean=FALSE),variance.model = list(
garchOrder=c(1,1),model="sGARCH"))
spec_FINRER2 = ugarchspec(mean.model = list(armaOrder=c(0,1,include.mean=FALSE)),variance.model = list(
garchOrder=c(1,2),model="sGARCH"))
spec_FINRER3 = ugarchspec(mean.model = list(armaOrder=c(0,1),include.mean=FALSE),variance.model = list(
garchOrder=c(2,1),model="sGARCH"))
spec_FINRER4 = ugarchspec(mean.model = list(armaOrder=c(0,1),include.mean=FALSE),variance.model = list(
garchOrder=c(2,2),model="sGARCH"))


GARCH_FINRER<-ugarchfit(spec_FINRER,data=na.omit(train$diff_FINRER_Change)) 
GARCH_FINRER2<-ugarchfit(spec_FINRER2,data=na.omit(train$diff_FINRER_Change)) 
GARCH_FINRER3<-ugarchfit(spec_FINRER3,data=na.omit(train$diff_FINRER_Change)) 
GARCH_FINRER4<-ugarchfit(spec_FINRER4,data=na.omit(train$diff_FINRER_Change)) 

GARCH_FINRER #AIC=3.0921 #BIC=3.1548 *
GARCH_FINRER2 #AIC=3.1100 #BIC = 3.2041
GARCH_FINRER3 #AIC=3.1013 #BIC = 3.1797
GARCH_FINRER4 #AIC=3.1107 #BIC =3.2047

spec_FINRER5 = ugarchspec(mean.model = list(armaOrder=c(3,3),include.mean=FALSE),variance.model = list(
garchOrder=c(0,1),model="sGARCH"))
GARCH_FINRER5<-ugarchfit(spec_FINRER5,data=na.omit(train$diff_FINRER_Change)) 
GARCH_FINRER5



#Sweden

spec_SWERER = ugarchspec(mean.model = list(armaOrder=c(3,0),include.mean=FALSE),variance.model = list(
garchOrder=c(1,1),model="sGARCH"))
spec_SWERER2 = ugarchspec(mean.model = list(armaOrder=c(3,0),include.mean=FALSE),variance.model = list(
garchOrder=c(1,2),model="sGARCH"))
spec_SWERER3 = ugarchspec(mean.model = list(armaOrder=c(3,0),include.mean=FALSE),variance.model = list(
garchOrder=c(2,1),model="sGARCH"))
spec_SWERER4 = ugarchspec(mean.model = list(armaOrder=c(3,0),include.mean=FALSE),variance.model = list(
garchOrder=c(2,2),model="sGARCH"))

spec_SWERER5 = ugarchspec(mean.model = list(armaOrder=c(1,2),include.mean=FALSE),variance.model = list(
garchOrder=c(1,1),model="sGARCH"))
GARCH_SWERER5<-ugarchfit(spec_SWERER5,data=na.omit(train$diff_SWERER_Change)) 
GARCH_SWERER5


GARCH_SWERER<-ugarchfit(spec_SWERER,data=na.omit(train$diff_SWERER_Change)) 
GARCH_SWERER2<-ugarchfit(spec_SWERER2,data=na.omit(train$diff_SWERER_Change)) 
GARCH_SWERER3<-ugarchfit(spec_SWERER3,data=na.omit(train$diff_SWERER_Change)) 
GARCH_SWERER4<-ugarchfit(spec_SWERER4,data=na.omit(train$diff_SWERER_Change)) 

GARCH_SWERER #AIC = 4.0416 #BIC = 4.1357 *
GARCH_SWERER2 #AIC = 4.0492 #BIC = 4.1592
GARCH_SWERER3 #AIC = 4.0475 #BIC = 4.1572
GARCH_SWERER4 #AIC=4.0568 #BIC = 4.1822


plot(GARCH_SWERER)
plot(GARCH_SWE)
plot(GARCH_FIN)
plot(GARCH_DEN)
plot(GARCH_FINRER)
plot(GARCH_DENRER)
11212
#Denmark

spec_DENRER5 = ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=FALSE),variance.model = list(
garchOrder=c(1,1),model="gjrGARCH"))

spec_DENRER = ugarchspec(mean.model = list(armaOrder=c(0,1),include.mean=FALSE),variance.model = list(
garchOrder=c(1,1),model="sGARCH"))
spec_DENRER2 = ugarchspec(mean.model = list(armaOrder=c(0,1),include.mean=FALSE),variance.model = list(
garchOrder=c(1,2),model="sGARCH"))
spec_DENRER3 = ugarchspec(mean.model = list(armaOrder=c(0,1),include.mean=FALSE),variance.model = list(
garchOrder=c(2,1),model="sGARCH"))
spec_DENRER4 = ugarchspec(mean.model = list(armaOrder=c(0,1),include.mean=FALSE),variance.model = list(
garchOrder=c(2,2),model="sGARCH"))

mspec_DENRER<-multispec(c(spec_DENRER,spec_DENRER2,spec_DENRER3,spec_DENRER4))
mspec_DENRER
multfit_DENRER<-multifit(mspec_DENRER,na.omit(train$diff_DENRER_Change))

GARCH_DENRER<-ugarchfit(spec_DENRER,data=na.omit(train$diff_DENRER_Change)) 
GARCH_DENRER2<-ugarchfit(spec_DENRER2,data=na.omit(train$diff_DENRER_Change)) 
GARCH_DENRER3<-ugarchfit(spec_DENRER3,data=na.omit(train$diff_DENRER_Change)) 
GARCH_DENRER4<-ugarchfit(spec_DENRER4,data=na.omit(train$diff_DENRER_Change)) 
GARCH_DENRER5<-ugarchfit(spec_DENRER5,data=na.omit(train$diff_DENRER_Change)) 

GARCH_DENRER #AIC = 2.8451 #BIC = 2.9078 *
GARCH_DENRER2 #AIC = 2.8535 #BIC = 2.9319
GARCH_DENRER3 #AIC = 2.8518 #BIC = 2.9302
GARCH_DENRER4 #AIC = 2.8599 #BIC = 2.9540
GARCH_DENRER5#AIC = 2.8621

meanmodel_DEN = list(armaOrder=c(0,0),archm=T,archpow=2)
varmodel_DEN = list(garchOrder=c(1,1),model="sGARCH")
spec_DEN_ = ugarchspec(mean.model = meanmodel_DEN,variance.model = varmodel_DEN)
ugarchfit(spec_DEN_,data=na.omit(train$diff_DENDY))

meanmodel_DENRER_ = list(armaOrder=c(0,1))
varmodel_DENRER = list(garchOrder=c(1,1),model="gjrGARCH")
spec = ugarchspec(mean.model = meanmodel_DENRER_,variance.model = varmodel_DENRER)
ugarchfit(spec,data=na.omit(train$diff_DENRER_Change))

#Forecasts for optimal models

#Static and Dynamic forecasts FINDY
fit_FINDY_ = ugarchfit(spec_FIN,data = na.omit(train$diff_FINDY),out.sample = 36)

static_fc = ugarchforecast(fit_FINDY_,n.ahead=1,n.roll = 35)
dynamic_fc = ugarchforecast(fit_FINDY_,n.ahead = 36)
actual_FINDY = na.omit(test$diff_FINDY)

ac
static_pred_FINDY  <- as.numeric(static_fc@forecast$seriesFor)
dynamic_pred_FINDY <- as.numeric(dynamic_fc@forecast$seriesFor)
err_static_FINDY  <- actual_FINDY - static_pred_FINDY
err_dynamic_FINDY <- actual_FINDY - dynamic_pred_FINDY
rmse_static  <- sqrt(mean(err_static_FINDY^2))
rmse_dynamic <- sqrt(mean(err_dynamic_FINDY^2))
mae_static  <- mean(abs(err_static_FINDY))
mae_dynamic <- mean(abs(err_dynamic_FINDY))
mse_static  <- mean(err_static_FINDY^2)
mse_dynamic <- mean(err_dynamic_FINDY^2)
mape_static  <- mean(abs(na.omit(err_static_FINDY /actual_FINDY))) * 100
mape_dynamic <- mean(abs(na.omit(err_dynamic_FINDY / actual_FINDY))) * 100

results <- data.frame(
  Model=c("Static","Dynamic"),
  RMSE=c(rmse_static, rmse_dynamic),
  MAE=c(mae_static, mae_dynamic),
  MSE=c(mse_static, mse_dynamic),
  MAPE=c(mape_static,mape_dynamic)
)

print(results)

par(mfrow=c(2,1))

par(lwd=2,cex.axis = 2)
x_axis = data$time[data$time >= "2018-01-01"]
plot(x_axis,static_fc@forecast$sigmaFor,type="l",xlab="Time",ylab="Variance",col="blue3", main="Forecast of variance (FIN)"
)
lines(x_axis,dynamic_fc@forecast$sigmaFor,col="brown1")
legend("topright", legend=c("Static", "Dynamic"),col=c("blue3","brown1"),
lty= 1)

par(lwd=2,cex.axis = 2)
x_axis = data$time[data$time >= "2018-01-01"]
plot(x_axis,static_fc@forecast$seriesFor,type="l",xlab="",ylab="",col="blue3",main="Forecast of mean (FIN)"
)

lines(x_axis,dynamic_fc@forecast$seriesFor,col="brown1")
legend("topright", legend=c("Static", "Dynamic"),col=c("blue3","brown1"),
lty= 1)

#Static and Dynamic forecasts SWEDY
fit_SWEDY_ = ugarchfit(spec_SWE,data = na.omit(train$diff_SWEDY),out.sample = 36)

static_fc_SWEDY = ugarchforecast(fit_SWEDY_,n.ahead=1,n.roll = 35)
dynamic_fc_SWEDY = ugarchforecast(fit_SWEDY_,n.ahead = 36)

par(lwd=2,cex.axis = 2)
x_axis = data$time[data$time >= "2018-01-01"]
plot(x_axis,static_fc_SWEDY@forecast$sigmaFor,type="l",xlab="Time",ylab="Variance",col="blue3", main="Forecast of variance (SWE)"
)

lines(x_axis,dynamic_fc_SWEDY@forecast$sigmaFor,col="brown1")
legend("topright", legend=c("Static", "Dynamic"),col=c("blue3","brown1"),
lty= 1)

par(lwd=2,cex.axis = 2)
x_axis = data$time[data$time >= "2018-01-01"]
plot(x_axis,static_fc_SWEDY@forecast$seriesFor,type="l",xlab="",ylab="",col="blue3",main="Forecast of mean (SWE)"
)

lines(x_axis,dynamic_fc_SWEDY@forecast$seriesFor,col="brown1")
legend("topright", legend=c("Static", "Dynamic"),col=c("blue3","brown1"),
lty= 1)

Static and Dynamic forecasts DENDY

fit_DENDY_ = ugarchfit(spec_DEN,data = na.omit(train$diff_DENDY),out.sample = 36)
static_fc_DENDY = ugarchforecast(fit_DENDY_,n.ahead=1,n.roll = 35)
dynamic_fc_DENDY = ugarchforecast(fit_DENDY_,n.ahead = 36)

par(lwd=2,cex.axis = 2)
x_axis = data$time[data$time >= "2018-01-01"]
plot(x_axis,static_fc_DENDY@forecast$sigmaFor,type="l",xlab="Time",ylab="Variance",col="blue3", main="Forecast of variance (DEN)"
)

lines(x_axis,dynamic_fc_DENDY@forecast$sigmaFor,col="brown1")
legend("topright", legend=c("Static", "Dynamic"),col=c("blue3","brown1"),
lty= 1)

par(lwd=2,cex.axis = 2)
x_axis = data$time[data$time >= "2018-01-01"]
plot(x_axis,static_fc_DENDY@forecast$seriesFor,type="l",xlab="",ylab="",col="blue3",main="Forecast of mean (DEN)"
)

lines(x_axis,dynamic_fc_DENDY@forecast$seriesFor,col="brown1")
legend("topright", legend=c("Static", "Dynamic"),col=c("blue3","brown1"),
lty= 1)

#Real exchange rates

fit_DENRER_ = ugarchfit(spec_DENRER,data = na.omit(train$diff_DENRER_Change),out.sample = 36)
static_fc_DENRER = ugarchforecast(fit_DENRER_,n.ahead=1,n.roll = 35)
dynamic_fc_DENRER = ugarchforecast(fit_DENRER_,n.ahead = 36)

par(lwd=2,cex.axis = 2)
x_axis = data$time[data$time >= "2018-01-01"]
plot(x_axis,static_fc_DENRER@forecast$sigmaFor,type="l",xlab="Time",ylab="Variance",col="blue3", main="Forecast of variance (diff_DENRER_Change)"
)

lines(x_axis,dynamic_fc_DENRER@forecast$sigmaFor,col="brown1")
legend("topright", legend=c("Static", "Dynamic"),col=c("blue3","brown1"),
lty= 1)


par(lwd=2,cex.axis = 2)
x_axis = data$time[data$time >= "2018-01-01"]
plot(x_axis,static_fc_DENRER@forecast$seriesFor,type="l",xlab="",ylab="",col="blue3",main="Forecast of mean (DEN)"
)

lines(x_axis,dynamic_fc_DENRER@forecast$seriesFor,col="brown1")
legend("topright", legend=c("Static", "Dynamic"),col=c("blue3","brown1"),
lty= 1)


fit_SWERER = ugarchfit(spec_SWERER,data = na.omit(train$diff_SWERER_Change),out.sample = 36)
static_fc_SWERER = ugarchforecast(fit_SWERER_,n.ahead=1,n.roll = 35)
dynamic_fc_SWERER = ugarchforecast(fit_SWERER_,n.ahead = 36)

par(lwd=2,cex.axis = 2)
x_axis = data$time[data$time >= "2018-01-01"]
plot(x_axis,static_fc_SWERER@forecast$sigmaFor,type="l",xlab="Time",ylab="Variance",col="blue3", main="Forecast of variance (diff_SWERER_Change)"
)

lines(x_axis,dynamic_fc_SWERER@forecast$sigmaFor,col="brown1")
legend("topright", legend=c("Static", "Dynamic"),col=c("blue3","brown1"),
lty= 1)


par(lwd=2,cex.axis = 2)
x_axis = data$time[data$time >= "2018-01-01"]
plot(x_axis,static_fc_SWERER@forecast$seriesFor,type="l",xlab="",ylab="Mean",col="blue3",main="Forecast of mean (SWE)"
)

lines(x_axis,dynamic_fc_SWERER@forecast$seriesFor,col="brown1")
legend("topright", legend=c("Static", "Dynamic"),col=c("blue3","brown1"),
lty= 1)

par(mfrow=c(3,1))
plot(test$time,test$diff_SWERER_Change, type="l")


fit_FINRER_ = ugarchfit(spec_FINRER,data = na.omit(train$diff_FINRER_Change),out.sample = 36)
static_fc_FINRER = ugarchforecast(fit_FINRER_,n.ahead=1,n.roll = 35)
dynamic_fc_FINRER = ugarchforecast(fit_FINRER_,n.ahead = 36)

par(lwd=2,cex.axis = 2)
x_axis = data$time[data$time >= "2018-01-01"]
plot(x_axis,static_fc_FINRER@forecast$sigmaFor,type="l",xlab="Time",ylab="Variance",col="blue3", main="Forecast of variance (diff_FINRER_Change)"
)

lines(x_axis,dynamic_fc_FINRER@forecast$sigmaFor,col="brown1")
legend("topright", legend=c("Static", "Dynamic"),col=c("blue3","brown1"),
lty= 1)


par(lwd=2,cex.axis = 2)
x_axis = data$time[data$time >= "2018-01-01"]
plot(x_axis,static_fc_FINRER@forecast$seriesFor,type="l",xlab="",ylab="Mean",col="blue3",main="Forecast of mean (FIN)"
)

lines(x_axis,dynamic_fc_FINRER@forecast$seriesFor,col="brown1")
legend("topright", legend=c("Static", "Dynamic"),col=c("blue3","brown1"),
lty= 1)

par(mfrow=c(3,1))
plot(test$time,test$diff_FINRER_Change, type="l")


aic_table = array(NA,c(6,6,2))
for (ar in 0:5) {
	for (ma in 0:5) {
		arma = arima(train$diff_FINDY,order = c(ar,0,ma))
		aic_table[ar+1,ma+1,1] = AIC(arma) #AIC
		aic_table[ar+1,ma+1,2] = AIC(arma, k = log(nrow(train))) #SBIC
	}
}

aic_table


aic_table2 = array(NA,c(6,6,2))
for (ar in 0:5) {
	for (ma in 0:5) {
		arma = arima(train$diff_SWERER_Change,order = c(ar,0,ma))
		aic_table2[ar+1,ma+1,1] = AIC(arma) #AIC
		aic_table2[ar+1,ma+1,2] = AIC(arma, k = log(nrow(train))) #SBIC
	}
}

aic_table2

aic_table3 = array(NA,c(6,6,2))
for (ar in 0:5) {
	for (ma in 0:5) {
		arma = arima(train$diff_DENRER_Change,order = c(ar,0,ma))
		aic_table3[ar+1,ma+1,1] = AIC(arma) #AIC
		aic_table3[ar+1,ma+1,2] = AIC(arma, k = log(nrow(train))) #SBIC
	}
}

aic_table3



    
