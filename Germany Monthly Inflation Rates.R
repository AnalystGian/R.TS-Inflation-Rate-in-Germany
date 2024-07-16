https://www.statbureau.org/en/germany/inflation-tables#google_vignette

# Set working directory
getwd()
setwd('D:\Desktop\DATA ANALYSIS\R\TIME SERIES ANALYSIS AND FORECASTING IN R\PROJECT 2 - SEASONAL DATA')

# install and load readxl package to import the data in xlsx format
install.packages("readxl")
library(readxl)

# Read the excel file
ger_inflation <- read_excel("ger_inflation.xlsx", col_names = FALSE)

# Display the data to understand its structure
print(ger_inflation)
str(ger_inflation)

# First column contains the years
years <- ger_inflation[[1]]
inflation_data <- ger_inflation[, -1]
inflation_data

# Convert the character data to numeric
inflation_data <- apply(inflation_data, 2, as.numeric)
inflation_data

# Reshape the data to a vector for time series conversion
inflation_vector <- as.vector(t(inflation_data))
inflation_vector

plot.ts(inflation_vector)

# Create a time series object starting from January 2008
inflation_ts <- ts(inflation_vector, start = 2008, frequency = 12)

# Display the time series object
print(inflation_ts)

# Plot the time series
plot(inflation_ts)

# Seasonal decomposition
decompose(inflation_ts)

plot(decompose(inflation_ts))

# Remove NA values
inflation_ts.nona <- na.omit(inflation_ts)

# Using the stl method
library(forecast)
plot(stl(inflation_ts.nona, s.window = 7))

# stl forecasting
plot(stlf(inflation_ts.nona, method = 'ets'))

# Comparison with a standard ets forecast
plot(forecast(ets(inflation_ts), h = 24))

# Using autoplot
library(ggplot2)
autoplot(stlf(inflation_ts, method = 'ets'))

# Seasonal Arima (package forecast)
auto.arima(inflation_ts, stepwise = T,
           approximation = F, trace = T)

# Assign it to an object
inflation_ts.arima <- auto.arima(inflation_ts,
                                 stepwise = T,
                                 approximation = F,
                                 trace = T)

forec <- forecast(inflation_ts.arima)
plot(forec)

# Exponential smoothing with ets
# Auto generated
ets(inflation_ts)

# Forecast plot
inflation_ts.ets <- ets(inflation_ts)

plot(forecast(inflation_ts.ets, h = 60))

# Comparison with seasonal Holt Winters model
plot(hw(inflation_ts, h = 60))

## Cross Validation of 2 models
inflation_ts.ets = ets(inflation_ts)
inflation_ts.arima = auto.arima(inflation_ts, 
                             stepwise = T, 
                             approximation = F, 
                             trace = T)

forecastets = function(x, h) {
  forecast(ets(x), h = h)
}

forecastarima = function(x, h) {
  forecast(auto.arima(x), stepwise = T, approximation = F, h=h)
}

etserror = tsCV(inflation_ts, forecastets, h=1)
arimaerror = tsCV(inflation_ts, forecastarima, h=1)

mean(etserror^2, na.rm=TRUE)
mean(arimaerror^2, na.rm=TRUE)









# Handle NA values using linear interpolation
library(zoo)
inflation_ts.nona <- na.approx(inflation_ts)
inflation_ts.nona