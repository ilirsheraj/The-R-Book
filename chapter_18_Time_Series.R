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

# Blowflies Data
blowfly <- read.table("Datasets/blowfly.txt" , header = TRUE)
head(blowfly)
head.matrix(blowfly)

# Convert it into time series object
flies <- ts(blowfly$flies)
flies

pdf(paste0(plot_dir, "Blowflies_TS_Plot.pdf"), width = 6, height = 5)
plot(flies, lwd=2, col=hue_pal()(1))
dev.off()

pdf(paste0(plot_dir, "Blowflies_TS_2.pdf"), width = 6, height = 5)
plot.ts(flies, lwd=2, col=hue_pal()(1))
dev.off()
