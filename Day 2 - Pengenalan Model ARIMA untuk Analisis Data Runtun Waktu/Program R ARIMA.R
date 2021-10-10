#Install library yg dibutuhkan
install.packages(c( "lmtest" , "forecast" , "tseries" ))
#Library
library(lmtest)
library(forecast)
library(tseries)

#Baca data
nile <- datasets::Nile

#Bagi data menjadi train sama test
train_df <- nile[ 1:80 ]
test_df <- nile[ 81 : 100 ]

#Eksplorasi data train
train_df <- ts(train_df, start = 1871 )
plot(train_df,
     col = "navyblue" ,
     main = "Annual flow of the River Nile" )

#ACF-PACF
par(mfrow = c( 1 , 2 ))
acf(train_df, lag.max = 10 , main = "ACF" )
axis( 1 , at = 1 : 15 , labels = 1 : 15 )
pacf(train_df, lag.max = 10 , main = "PACF" )
axis( 1 , at = 1 : 15 , labels = 1 : 15 )

#Fitting Model ARIMA
#AR(1)
arima_model <- arima(train_df, order = c( 1 , 0 , 0 ), method = c( "ML" ))

arima_pred <- arima_model $ fitted
plot(train_df,
     col = "navyblue" ,
     type = "l" ,
     main = "Plot Aktual vs Prediksi" )
lines(arima_pred,
      col = "red" )

summary(arima_model)
coeftest(arima_model)

#Analisis Sisaan
#1. Sisaan Menyebar Normal
sisaan <- arima_model $ residuals
#Secara Eksploratif
par(mfrow = c( 1 , 2 ))
hist(sisaan, col = "grey" )
qqnorm(sisaan, col = "navyblue" )
qqline(sisaan, col = "red" )

#Uji Formal Jarque Bera
jarque.bera.test(sisaan)
#2. Ragam Sisaan Homogen
plot(x = as .numeric(arima_pred), y = as .numeric(sisaan),
     col = "blue" ,
     main = "Fitted vs Residual" ,
     xlab = "Fitted Value" ,
     ylab = "Residual" )
abline(h = 0 ,
       col = "red" ,
       lty = 2 )

#3. Antar Sisaan Saling Bebas
plot(sisaan,
     col = "navyblue" ,
     main = "Residual vs Order" )
abline(h = 0 ,
       col = "red" ,
       lty = 2 )

#Overfitting
#AR(2)
arima_model_2 <- Arima(train_df, order = c( 2 , 0 , 0 ), method = "ML" )
summary(arima_model_2)
coeftest(arima_model_2)

#Forecasting
forecast_arima <- forecast(arima_model, 20 )
accuracy(forecast_arima, test_df)
forecast_arima_2 <- forecast(arima_model_2, 20 )
accuracy(forecast_arima_2, test_df)


#Penerapan pada Semua Data ARIMA(1,0,0)
nile_arima <- Arima(nile, order = c( 1 , 0 , 0 ), method = "ML" )
forecast_nile <- forecast(nile_arima, 20 )
plot(forecast_nile)

