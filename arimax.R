#Import Library 
library(readxl)
library(TTR)
library(forecast)
library(tseries)
library(TSA)
library(dynlm)
library(lmtest)
library(imputeTS)
library(stats)
library(MASS)
library(kableExtra)
library(padr)
library(astsa)
library(tfarima)
library(FinTS)
library(tidyverse)


# Model ARIMA
dataset <- read_excel("D:/Skripsi/data.xlsx")
df <- dataset$`Jumlah Penempatan`
dataPengaduan <- dataset$`Jumlah Pengaduan`
dataInflasi <- dataset$Inflasi


## Eksplorasi Data

#Mengubah data ke dalam bentuk time series
#data.ts<-ts(data,frequency=12, start=2018)
df.ts<-ts(df)

#Format Time Series untuk peubah x
dataPengaduan <- ts(dataPengaduan,frequency=12,start=2018)
dataInflasi <- ts(dataInflasi, frequency = 12, start = 2018)

#Plot data penempatan
plot(df.ts,xlab ="Periode", ylab = "Data Jumlah Penempatan", col="black", main = "Plot Deret Waktu Data Penempatan")
points(df.ts)


#Plot Data Inflasi
plot(dataInflasi,xlab ="Periode", ylab = "Data Inflasi (Persen)", col="black", main = "Plot Data Inflasi (Persen)")
points(dataInflasi)

#Plot Data Pengaduan
plot(dataPengaduan,xlab ="Periode", ylab = "Data Pengaduan", col="black", main = "Plot Data Pengaduan")
points(dataPengaduan)


## Splitting Data (Data Training dan Testing) untuk ARIMA
Data periode Januari 2018 sampai Januari 2023 digunakan sebagai data training untuk membangun model dan data Februari 2023 sampai Desember 2023 digunakan sebagai data testing untuk memeriksa keakuratan model dalam memprediksi inflasi.

```{r}
#Splitting Data
df.train <- ts(df[1:58])
df.test <- ts(df[59:72])

#Time Series Data
training.ts<-ts(df.train,start=1)
testing.ts<-ts(df.test,start=59)

#Splitting Data
dataPengaduan.train <- ts(dataPengaduan[1:58])
dataPengaduan.test <- ts(dataPengaduan[59:72])

dataInflasi.train <- ts(dataInflasi[1:58])
dataInflasi.test <- ts(dataInflasi[59:72])

#Time Series Data
Pengaduantraining.ts<-ts(dataPengaduan.train,start=1)
Pengaduantesting.ts<-ts(dataPengaduan.test,start=59)

Inflasitraining.ts <- ts(dataInflasi.train, start=1)
Inflasitesting.ts <-ts(dataInflasi.test,start=59)


## Plot Data Training dan testing data penempatan
ts.plot(df.ts, xlab = "Periode", ylab ="Data Penempatan", 
        main = "Plot Deret Waktu Data Penempatan")
lines(training.ts, col = "blue")
lines(testing.ts, col="Red")
legend(30,30000,c("Data Training","Data Testing"), 
       lty=c(1,1), col=c("blue","red"), cex=0.8)
abline(v=59, col=c("black"), lty=1, lwd=1)


#ARIMA
## Cek Kestasioneran Data 
### 1. ACF & PACF Plot data 

acf(df.train, lag.max = 24, main = "Plot ACF Data Penempatan")

### 2. Uji Formal (ADF-Test)
adf.test(df.train)


### Differencing 1 karena data tidak stasioner
df.dif1<-diff(df.train,differences = 1) 
plot.ts(ts(df.dif1,start = 2018, frequency = 12),lty=1,xlab = "Periode", ylab= "Data Penempatan Pembedaan 1", main="Plot Differencing Data Penempatan")

### Cek Kestasioneran Data Setelah Differencing 1
adf.test(df.dif1)

### ACF dan PACF Plot, EACF Matrix
acf(df.dif1, lag.max = 24, main = "Plot ACF Data Penempatan Setelah Differencing satu kali")
pacf(df.dif1, lag.max = 24, main = "Plot PACF Data Penempatan Setelah Differencing satu kali")
eacf(df.dif1)

### Pemodelan ARIMA Data Penempatan
model1 <- Arima(df.dif1, order=c(0,1,1), method="ML")   
model2 <- Arima(df.dif1, order=c(1,1,0), method="ML") 
model3 <- Arima(df.dif1, order=c(1,1,1), method="ML") 
model4 <- Arima(df.dif1, order=c(2,1,0), method="ML") 
model5 <- Arima(df.dif1, order=c(2,1,1), method="ML")

### Pendugaan Parameter Model
#### Arima (0,1,1)
summary(model1)
lmtest::coeftest((model1)) 

#### Arima (1,1,0)
summary(model2)
lmtest::coeftest((model2)) 

#### Arima (1,1,1)
summary(model3)
lmtest::coeftest((model3)) 

#### Arima (2,1,0)
summary(model4)
lmtest::coeftest((model4)) 

#### Arima (2,1,1)
summary(model5)
lmtest::coeftest((model5)) 

#AIC ARIMA dan Signifikansi Parameter
modelaccuracy<-data.frame(
  "Model"=c("ARIMA(0,1,1)", "ARIMA(1,1,0)", "ARIMA(1,1,1)", "ARIMA(2,1,0)", "ARIMA(2,1,1)" ),
  "AIC"=c(model1$aic, model2$aic, model3$aic,model4$aic, model5$aic),
  "BIC"=c(model1$bic, model2$bic, model3$bic,model4$bic, model5$bic) ,
  "Signifikansi"=c("Signifikan","Signifikan", "Signifikan", "Signifikan","Tidak Signifikan"))

modelaccuracy

# Model ARIMAX
model3arimax <- Arima(df.train, order = c(1,1,1), xreg = cbind(dataPengaduan.train,dataInflasi.train), method = "ML")

summary(model3arimax)
lmtest::coeftest((model3arimax))


### Overfitting
Overfitting yang digunakan adalah model ARIMA(1,1,2). Model ARIMA(1,1,1) akan dibandingkan dengan model ARIMA(1,1,2)
# (1,1,2)

model3arimax1 <- Arima(df.train, order = c(1,1,2), xreg = cbind(dataPengaduan.train,dataInflasi.train), method = "ML")  

summary(model3arimax1)
lmtest::coeftest((model3arimax1))

# (1,1,2)

model3arimax2 <- Arima(df.train, order = c(2,1,1), xreg = cbind(dataPengaduan.train,dataInflasi.train), method = "ML")  

summary(model3arimax2)
lmtest::coeftest((model3arimax2))

#AIC ARIMA dan Signifikansi Parameter Model Overfitting
modelaccuracy<-data.frame(
  "Model"=c("ARIMAX(1,1,1)", "ARIMAX(1,1,2)", "ARIMAX(2,1,1)"),
  "AIC"=c(model3arimax$aic, model3arimax1$aic, model3arimax2$aic),
  "BIC"=c(model3arimax$bic, model3arimax1$bic, model3arimax2$bic),
  "Signifikansi"=c("Signifikan","Tidak Signifikan","Tidak Signifikan"))

modelaccuracy


# Uji Diagnostik Sisaan
sisaanx <- model3arimax$residuals
par(mfrow=c(2,2))
qqnorm(sisaanx)
box(col="black",lwd=2)
qqline(sisaanx, col = "red", lwd =1, col.lab="black",
       col.axis="black",col.sub = "black")
box(col="black",lwd=2)
plot(c(1:length(sisaanx)),sisaanx,col="black",col.lab="black",col.axis="black")
box(col="black",lwd=2)
acf(sisaanx,col="black",col.sub = "black",col.axis="black", col.lab="black")
box(col="black",lwd=2)
pacf(sisaanx,col="black",col.sub = "black",col.axis="black", col.lab="black",col.main="black")
box(col="black",lwd=2)

## Pengujian White Noise Regressi
Box.test(sisaanx, lag = 24)

##Sisaan Menyebar Normal
shapiro.test(sisaanx)

##Uji Kehomogenan Ragam Sisaan
ArchTest(sisaanx)

## Forecast Model ARIMAX(1,1,1) data testing
library(forecast)
ramalanx <- forecast((model3arimax), xreg = cbind(dataPengaduan.test,dataInflasi.test))
ramalanx

data.ramalanx <- ramalanx$mean
data.ramalan.tsx <- ts(data.ramalanx, start = 2023, frequency = 12)
plot(ramalanx,col="black",col.sub ="black",col.axis="black",
     col.lab="black",col.main="black",lwd=2)
box(col="black",lwd=2)

perbandinganx <- matrix(c(df.test[1:14], data.ramalanx[1:14]),nrow=14, ncol = 2, byrow = FALSE)
colnames(perbandinganx) <- c("Aktual", "Ramalan")
perbandinganx

error <- data.frame(df.test[1:14])-data.frame(data.ramalanx[1:14]) 

## SSE (Sum Square Error)
SSE <- sum(error^2, na.rm = T)

## MSE (Mean Squared Error)
MSE<- sapply(error^2, mean, na.rm = T)

## RMSE (Root Mean Square Error)
RMSE <- sqrt(MSE)

## MAD (Mean Absolute Deviation)
MAD <- sapply(abs(error), mean, na.rm = T)

## MAPE (Mean Absolute Percentage Error)
r.error <- (error/data.frame(df.test)) # Relative Error
MAPE <- sapply(abs(r.error), mean, na.rm = T)*100

akurasiarimax <- data.frame(
  "Ukuran Keakuratan" = c("SSE", "MSE", "MAPE", "RMSE", "MAD"), 
  "Forecasting" = c(SSE, MSE, MAPE, RMSE, MAD))
akurasiarimax


## Forecast Model ARIMAX(1,1,1) data training
library(forecast)
ramalanx.train <- forecast((model3arimax), xreg = cbind(dataPengaduan.train,dataInflasi.train))
ramalanx.train

data.ramalanx.train <- ramalanx.train$mean
data.ramalan.tsx <- ts(data.ramalanx.train, start = 2023, frequency = 12)
plot(ramalanx.train,col="black",col.sub ="black",col.axis="black",
     col.lab="black",col.main="black",lwd=2)
box(col="black",lwd=2)

perbandinganx.train <- matrix(c(df.train[1:58], data.ramalanx.train[1:58]), nrow = 58, ncol = 2, byrow = FALSE)
colnames(perbandinganx) <- c("Aktual", "Ramalan")
perbandinganx.train

error.train <- (data.frame(df.train[1:58]))-(data.frame(data.ramalanx.train[1:58])) 

## SSE (Sum Square Error)
SSE.train <- sum(error.train^2, na.rm = T)

## MSE (Mean Squared Error)
MSE.train<- sapply(error.train^2, mean, na.rm = T)

## RMSE (Root Mean Square Error)
RMSE.train <- sqrt(MSE.train)

## MAD (Mean Absolute Deviation)
MAD.train <- sapply(abs(error.train), mean, na.rm = T)

## MAPE (Mean Absolute Percentage Error)
r.error.train <- (error.train/data.frame(df.train)) # Relative Error
MAPE.train <- sapply(abs(r.error.train), mean, na.rm = T)*100

akurasiarimax.train <- data.frame(
  "Ukuran Keakuratan" = c("SSE.train", "MSE.train", "MAPE.train", "RMSE.train", "MAD.train"), 
  "Forecasting" = c(SSE.train, MSE.train, MAPE.train, RMSE.train, MAD.train))
akurasiarimax.train