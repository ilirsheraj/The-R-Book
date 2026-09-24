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
month_ts <- ts(as.vector(tapply(silwood$upper, list(silwood$month, silwood$yr), mean)))
head(month_ts)
month_ts

pdf(paste0(plot_dir, "Silwood_Autocorrelation.pdf"), 
    width = 6, height = 4)
acf(month_ts, main="", col=hue_pal()(2)[1], lwd=2)
dev.off()


years_ts <- ts(as.vector(tapply(silwood$upper, list(silwood$yr), mean)))
years_ts
acf(years_ts, main="", col=hue_pal()(2)[1], lwd=2)
