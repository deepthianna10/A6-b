# Load necessary libraries
library(quantmod)
library(rugarch)

# Get the data for tatamotors
ticker <- "TATAMOTORS.NS"  

# Download the data
data <- getSymbols(ticker, src = 'yahoo', from = '2021-04-01', to = '2024-03-31', auto.assign = FALSE)
# Create the differenced variable (log returns)
data$Returns <- diff(log(Cl(data)))

# Drop NaN values resulted from differencing
data <- na.omit(data)

# Display the first few rows
head(data)
# Plot the returns
plot(data$Returns, main = "Log Returns", type = 'l')

# Fit an ARCH model
spec_arch <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 0)),
                        mean.model = list(armaOrder = c(0, 0)))
fit_arch <- ugarchfit(spec = spec_arch, data = data$Returns)
show(fit_arch)

# Plot the conditional volatility
plot(fit_arch, which = 2)

# Fit a GARCH model
spec_garch <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(0, 0)))
fit_garch <- ugarchfit(spec = spec_garch, data = data$Returns)
show(fit_garch)

# Plot the conditional volatility
plot(fit_garch, which = 2)

# Forecast using the GARCH model
forecasts <- ugarchforecast(fit_garch, n.ahead = 90)
forecasts



# Extract the forecasted sigma (volatility)
sigma_forecasts <- sigma(forecasts)

# Plot the forecasted sigma (volatility)
plot(sigma_forecasts, type = 'l', main = "Forecasted Sigma (Volatility)", ylab = "Sigma", xlab = "Days Ahead")