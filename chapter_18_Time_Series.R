# Chapter 18 - Time Series Analysis
library(scales)

plot_dir <- "Plots/"

temp <- read.table("Datasets/temp.txt", header = TRUE)
head(temp)

pdf(paste0(plot_dir, "Temperature_TS_Raw_Plot.pdf"), width = 6, height = 5)
plot(temp$temps, col = hue_pal()(4)[1], pch = 16,
     xlab = "Index",
     ylab = "Temperature")
lines(temp$temps, col = hue_pal()(4)[2], lwd=2)
dev.off()

# Define a function for Moving Averages
ma <- function(y, n){
  l <- length(y)
  y_new <- numeric(l - n + 1)
  for (i in 1:length(y_new)) {
    y_new[i] <- mean(y[i:(i + n - 1)])
  }
  y_new
}

# Moving average of 3 points
pdf(paste0(plot_dir, "Temperature_TS_MA_Plot.pdf"), width = 6, height = 5)
plot(temp$temps, col = hue_pal()(4)[1], pch = 16,
     xlab = "Index",
     ylab = "Temperature")
lines(x = 2:155, y = ma(temp$temps, 3), 
      col = hue_pal()(4)[3], lwd=2)
dev.off()

# Moving average of 12 points
pdf(paste0(plot_dir, "Temperature_TS_MA_12_Plot.pdf"), width = 6, height = 5)
plot(temp$temps, col = hue_pal()(4)[1], pch = 16,
     xlab = "Index",
     ylab = "Temperature")
lines(x = seq(6.5, 150.5, 1), y = ma(temp$temps, 12), 
      col = hue_pal()(4)[4], lwd=3)
dev.off()

# Blowflies Data: A bit of a nuissance here
blowfly <- read.table("Datasets/blowfly.txt" , header = TRUE)
head(blowfly)
blowfly <- blowfly[grepl("^[0-9.]+$", trimws(blowfly$flies)), , drop = FALSE]
blowfly$flies <- as.numeric(as.character(blowfly$flies))

# Convert it into time series object
flies <- ts(blowfly$flies)
length(flies)
flies

pdf(paste0(plot_dir, "Blowflies_TS_Plot.pdf"), width = 6, height = 5)
plot(flies, lwd=2, col=hue_pal()(1))
dev.off()

pdf(paste0(plot_dir, "Blowflies_TS_2.pdf"), width = 6, height = 5)
plot.ts(flies, lwd=2, col=hue_pal()(1))
dev.off()

# Autocorrelation and Partial Autocorrelation
# lag 1-4
pdf(paste0(plot_dir, "Blowflies_lag_1_4.pdf"), width = 8, height = 8)
par(mfrow = c(2, 2))
sapply(1:4, function (x) plot(flies[-(361: (361 - x + 1))], flies[-(1:x)],
                              xlab = "", ylab = "", col = hue_pal()(8)[x],
                              pch = 16))
dev.off()
# par(mfrow = c(1, 1))

# lags 7-10
pdf(paste0(plot_dir, "Blowflies_lag_7_10.pdf"), width = 8, height = 8)
par(mfrow = c(2, 2))
sapply (7:10, function (x) plot(flies[-(361: (361 - x + 1))], flies[-(1:x)],
                                xlab = "", ylab = "", col = hue_pal()(8)[x - 2],
                                pch=16))
# par(mfrow = c(1, 1))
dev.off()

# Autocorrelation Function
pdf(paste0(plot_dir, "Blowflies_AutoCorrelation_Partial_Autocorrelation.pdf"), 
    width = 8, height = 4)
par(mfrow = c(1, 2))
acf(flies, col = hue_pal()(2)[1], main = "", lwd=2)
acf(flies, type = "p", col = hue_pal()(2)[2], main = "", lwd=2)
dev.off()

# Examine the data from week 200 to the end
flies_2 <- flies[201:length(flies)]

# Look for linear trend
blowlfies_mod <- lm(flies_2 ~ I(1:length(flies_2)))
summary(blowlfies_mod)
# 22 extra flies for each week

# de-trend the data
flies_detrended <- flies_2 - predict(lm (flies_2 ~ I (1:length (flies_2))))

# Now plot the thing
pdf(paste0(plot_dir, "Blowflies_second_half.pdf"), width = 6, height = 4)
plot.ts(flies_detrended, col = hue_pal()(3)[1], 
        ylab = "flies (detrended)", xaxt = "n", lwd = 2)
axis(1, at = seq(0, 150, 50), labels = seq(201, 361, 50))
dev.off()

pdf(paste0(plot_dir, "Blowflies_correlations_part2.pdf"), 
    width = 8, height = 4)
par(mfrow = c(1, 2))
acf(flies_detrended, col = hue_pal()(3)[2], main = "", lwd=2)
acf(flies_detrended, type = "p", col = hue_pal()(3)[3], main = "", lwd=2)
dev.off()

# Do the same for the more regular first half of the data
flies_1 <- flies[1:200]

flies_detrended_1 <- flies_1 - predict(lm (flies_1 ~ I (1:length (flies_1))))

pdf(paste0(plot_dir, "Blowflies_first_half.pdf"), width = 6, height = 4)
plot.ts(flies_detrended_1, col = hue_pal()(3)[1], 
        ylab = "flies (detrended)", xaxt = "n", lwd = 2)
dev.off()

pdf(paste0(plot_dir, "Blowflies_correlations_part1.pdf"), 
    width = 8, height = 4)
par(mfrow = c(1, 2))
acf(flies_1, col = hue_pal()(2)[1], main = "", lwd=2)
acf(flies_1, type = "p", col = hue_pal()(2)[2], main = "", lwd=2)
dev.off()

# Seasonal Data: rainfall and temperature over 19 years in england
silwood <- read.table("Datasets/SilwoodWeather.txt", header = TRUE)
head(silwood)
tail(silwood)

# delete the leap year
silwood <- silwood[-seq(365 + 31 + 29, nrow(silwood), 365 * 4 + 1),]

# Autocorrelation by month
# First average by month: Get a matrix 12x9
tapply(silwood$upper, list(silwood$month, silwood$yr), mean)

# Flatten the matrix
as.vector(tapply(silwood$upper, list(silwood$month, silwood$yr), mean))

# Now convert it to time series: Monthly averages for 19 years
month_ts <- ts(as.vector(tapply(silwood$upper, list(silwood$month, silwood$yr), mean)))
head(month_ts)
month_ts

pdf(paste0(plot_dir, "Silwood_Monthly_Autocorrelation.pdf"), 
    width = 6, height = 4)
acf(month_ts, main="", col=hue_pal()(2)[1], lwd=2)
dev.off()

# Now take yearly averages: A vector
tapply(silwood$upper, list(silwood$yr), mean)

# Convert to time series
years_ts <- ts(as.vector(tapply(silwood$upper, list(silwood$yr), mean)))
years_ts

pdf(paste0(plot_dir, "Silwood_Yearly_Autocorrelation.pdf"), 
    width = 6, height = 4)
acf(years_ts, main="", col=hue_pal()(2)[1], lwd=2)
dev.off()

# Use ts default functions
## Daily (365)
daily_ts <- ts(silwood$upper, start = c(1987, 1), frequency = 365)

pdf(paste0(plot_dir, "Silwood_TS_All.pdf"), width = 6, height = 4)
plot(daily_ts, ylab="degrees", col=hue_pal()(1))
dev.off()

## Monthly: 12
month_ts <- ts(as.vector(tapply(silwood$upper, list(silwood$month, silwood$yr), mean)),
               start = c(1987, 1), frequency = 12)

pdf(paste0(plot_dir, "Silwood_Monthly_TS.pdf"), width = 6, height = 4)
plot(month_ts, ylab="degrees", col=hue_pal()(1), lwd=2)
dev.off()

## Yearly: 1
years_ts <- ts(as.vector(tapply(silwood$upper, silwood$yr, mean)),
               start = 1987, frequency = 1)

pdf(paste0(plot_dir, "Silwood_Yearly_TS.pdf"), width = 6, height = 4)
plot(years_ts, ylab="degrees", col=hue_pal()(1), lwd=2)
dev.off()

# Decompose and plot all in one
upper_decomp <- stl(daily_ts, "periodic")

pdf(paste0(plot_dir, "Silwood_Decomposed.pdf"), width = 8, height = 6)
plot(upper_decomp, col=hue_pal()(2)[1], col.range = hue_pal()(2)[2])
dev.off()

# Cycles
# Equation of a cycle is defined by means of sine and cosine functions
# y_t = \alpha + \beta*sin(2*\pi*t) + \gamma*cos(2*\pi*t) + \epsilon_t
## Need to estimate three parameters: alpha. beta and gamma
# yearly cycle: gotta be 19
time <- (1:nrow(silwood)/365)

silwood_mod1 <- lm(upper ~ sin(2 * pi * time) + cos(2 * pi * time), 
                   data = silwood)
summary(silwood_mod1)

# Plot the model against the real data
pdf(paste0(plot_dir, "Model_vs_Real_Data.pdf"), width = 6, height = 4)
plot(silwood$upper, ylab = "upper", pch=".", col=hue_pal()(2)[1])
lines(1:(nrow(silwood)),predict(silwood_mod1), col=hue_pal()(2)[2], lwd=2)
dev.off()

# Plot the residuals
pdf(paste0(plot_dir, "Model_Residuals.pdf"), width = 6, height = 4)
plot(silwood_mod1$residuals, main="", pch=".", col=hue_pal()(2)[1])
dev.off()

# Check for serial correlation in residuals
pdf(paste0(plot_dir, "Residual_Series_Correlation.pdf"), width = 6, height = 4)
par(mfrow = c(1, 2))
acf(silwood_mod1$residuals, main="", col=hue_pal()(2)[1], lwd=2)
acf(silwood_mod1$residuals, type = "p", main="", col=hue_pal()(2)[2], lwd=2)
dev.off()

# Testing for Possible Trend
head(upper_decomp$time.series)
trend_rem <- upper_decomp$time.series[,"trend"] + upper_decomp$time.series[,"remainder"]
head(trend_rem)

# Stick it in linear model
silwood_mod2 <- lm(trend_rem ~ I(1:length(trend_rem)))
summary(silwood_mod2)
# 1.888e-04 degrees per day, 0.07 degrees per year increase

# Part 4: Multiple Time Series
twoseries <- read.table("Datasets/twoseries.txt", header = TRUE)
head(twoseries)

twoseries <- ts(twoseries)
twoseries

pdf(paste0(plot_dir, "Multiple_Series.pdf"), width = 6, height = 4)
ts.plot(twoseries, col=hue_pal()(2), lwd=2)
legend(10, 500, legend = c ("x", "y"), lwd = 1, col = hue_pal()(2))
dev.off()

pdf(paste0(plot_dir, "Two_Series_Corr_Autococrr.pdf"), width = 6, height = 6)
acf(twoseries, type = "p", col=hue_pal()(10))
dev.off()

# ARIMA Analysis
lynx <- read.table("Datasets/lynx.txt", header = TRUE)
head(lynx)

pdf(paste0(plot_dir, "Lynx_TS.pdf"), width = 6, height = 4)
plot.ts(lynx$Lynx, col = hue_pal()(3)[1], ylab = "lynx skins", xaxt = "n", lwd=2)
dev.off()

pdf(paste0(plot_dir, "Lynx_Co_and_Autocorrelation.pdf"), width = 6, height = 4)
par(mfrow = c(1, 2))
acf(lynx$Lynx, col = hue_pal()(3)[2], main = "", lwd=2)
acf(lynx$Lynx, type = "p", col = hue_pal()(3)[3], main = "", lwd=2)
dev.off()

# Differencing
lynx_aics <- numeric(6)
names(lynx_aics) <- 0:5

for (q in 0:5) {
  lynx_aics[q + 1] <- arima(lynx$Lynx, order = c(2, 1, q))$aic
}

lynx_aics

# Smalles is for q4
which.min(lynx_aics)
lynx_aics[5]

# Now fit a model
lynx_mod <- arima(lynx$Lynx, order = c(2, 1, 4))
lynx_mod

# Time Series Simulation
set.seed(271828)

white_noise <- arima.sim(list(order = c(0, 0, 0)), n = 1000)

pdf(paste0(plot_dir, "White_Noise_TS.pdf"), width = 6, height = 4)
plot(white_noise, ylab = "", col = hue_pal()(15)[1], lwd=2)
dev.off()

pdf(paste0(plot_dir, "White_Noise_Co_and_Autocorrelation.pdf"), width = 6, height = 4)
par(mfrow = c(1, 2))
acf(white_noise, main = "", col = hue_pal()(15)[2], lwd=2)
acf(white_noise, type = "p", main = "", col = hue_pal()(15)[3], lwd=2)
dev.off()

# Simulate a new one: a1 = 1, a2 = -0.7
set.seed(182845)

ar2 <- arima.sim(list(order = c (2, 0, 0), ar = c(1, -0.7)), n = 1000)

plot(ar2, ylab = "", col = hue_pal()(15)[4], lwd=2)
acf(ar2, main = "", col = hue_pal()(15)[5], lwd=2)
acf(ar2, type = "p", main = "", col = hue_pal()(15)[6], lwd=2)

# Another simulation: b1=2, b2=-1, b3=1.3
set.seed(904523)

ma3 <- arima.sim(list(order = c(0, 0, 3), ma = c(2, -1, 1.3)), n = 1000)

plot(ma3, ylab = "", col = hue_pal()(15)[7], lwd=2)
acf(ma3, main = "", col = hue_pal()(15)[8], lwd=2)
acf(ma3, type = "p", main = "", col = hue_pal()(15)[9], lwd=2)

# Simulate a new one integrrating both TSs above, plus differencing order 1
set.seed(536028)

ar2d1ma3 <- arima.sim(list(order = c(2, 1, 3), ar = c(1, -0.7), 
                           ma = c(2, -1, 1.3)), n = 1000)

pdf(paste0(plot_dir, "Simulated_Complex_TimeSeries.pdf"), width = 6, height = 4)
plot(ar2d1ma3, ylab = "", col = hue_pal()(15)[10], lwd=2)
dev.off()

pdf(paste0(plot_dir, "Simulated_TS_Co_and_Autocorrelation.pdf"), width = 6, height = 4)
par(mfrow = c(1, 2))
acf(ar2d1ma3, main = "", col = hue_pal()(15)[11], lwd=2)
acf(ar2d1ma3, type = "p", main = "", col = hue_pal()(15)[12], lwd=2)
dev.off()

set.seed (747135)
ar2ma3 <- arima.sim(list(order = c(2, 0, 3), ar = c(1, -0.7), 
                         ma = c(2, -1, 1.3)), n = 1000)

plot(ar2ma3, ylab = "", col = hue_pal()(15)[13], lwd = 2)
acf(ar2ma3, main = "", col = hue_pal()(15)[14], lwd = 2)
acf(ar2ma3, type = "p", main = "", col = hue_pal()(15)[15], lwd = 2)

sim_mod1 <- arima(ar2d1ma3, order = c(3, 0, 5))
summary(sim_mod1)
sim_mod1$aic

sim_mod2 <- arima(ar2d1ma3, order = c(2, 0, 3))
sim_mod2$aic

# EOF