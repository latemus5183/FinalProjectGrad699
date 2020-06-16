#GetData
df <- EURUSDExcel
head(df)
#Reorganize data
myvars<- c("Date", "Price")
UsdEu<- df[myvars]
summary(UsdEu)
str(UsdEu)

#Data Visualization
hist(df$Price, xlab = "Price", main = "Price Distribution")
boxplot(df$Price, df$High )

my_data <- EURUSDExcel[, c(2,3,4,5,6)]

corMatrix <- cor(my_data)

library(corrplot)
corrplot(corMatrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

library(scatterplot3d)
scatterplot3d(
  x = df$High, y = df$Price, z = df$Date,
  grid = TRUE, box = FALSE,
  xlab = "Close", ylab = "High", zlab = "Time", main = "Character of Time with Close and High"   
)



#Create Time Series
tsModel <- ts(UsdEu$Price, start = c(2002,1,1), end = c(2019,12,30), frequency = 7)

plot.ts(tsModel)

tsComp<- decompose(tsModel)
plot.ts(tsComp$seasonal)
plot.ts(tsComp$trend)

tsAdjusted <- tsModel - tsComp$seasonal

plot.ts(tsAdjusted)

#HoltWinters Model
library(forecast)
UsdHW <- HoltWinters(tsAdjusted ,beta=FALSE,gamma=FALSE)
plot(UsdHW)

UsdHWforecast <- forecast(UsdHW, 30) 
plot(UsdHWforecast)

hist(UsdHWforecast$residuals, col="yellow")
dnormResiduals <- dnorm(ompleterecords,mean=mean(ompleterecords), sd=sd(ompleterecords) )
accuracy(UsdHWforecast)
ompleterecords <- na.omit(UsdHWforecast$residuals) 
mean(ompleterecords)
sd(ompleterecords)

plot(dnormResiduals)
plot(UsdHWforecast$residuals,dnormResiduals,col="blue")

#Arima Model
arima <- auto.arima(tsModel)
arimaForecast <- forecast(arima, h=1000)
plot(arimaForecast)
hist(arimaForecast$residuals, col="yellow")
dnormResidualsArima <- dnorm(arimaForecast$residuals,mean=mean(arimaForecast$residuals), sd=sd(arimaForecast$residuals) )
plot(arimaForecast$residuals,dnormResidualsArima,col="blue")

mean(arimaForecast$residuals)

acf(arimaForecast$residuals)
pacf(arimaForecast$residuals)
summary(arima)
arimaForecast

#Linear Regression
tslm<-lm(UsdEu$Price ~ UsdEu$Date, data = UsdEu)

summary(tslm)
library(ggplot2)
res <- residuals(tslm)
res <- as.data.frame(res)
ggplot(res,aes(res)) +  geom_histogram(fill='blue',alpha=0.5)

lmForecast <- predict(tslm, h = 1000)

G3.predictions <- predict(lmForecast,test)

results <- cbind(G3.predictions,test$Price) 
colnames(results) <- c('pred','real')
results <- as.data.frame(results)
plot(results)
mse <- mean((results$real-results$pred)^2)
print(mse)
attributes(lmForecast)
