# Chapter 18 - Time Series Analysis
library(scales)

plot_dir <- "Plots/"

temp <- read.table("Datasets/temp.txt", header = TRUE)
head(temp)

plot(temp$temps, col = hue_pal()(4)[1], pch = 16,
     xlab = "Index",
     ylab = "Temperature")
lines(temp$temps, col = hue_pal()(4)[2], lwd=2)

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
plot(temp$temps, col = hue_pal()(4)[1], pch = 16,
     xlab = "Index",
     ylab = "Temperature")
lines(x = 2:155, y = ma(temp$temps, 3), 
      col = hue_pal()(4)[3], lwd=2)

# Moving average of 12 points
plot(temp$temps, col = hue_pal()(4)[1], pch = 16,
     xlab = "Index",
     ylab = "Temperature")
lines(x = seq(6.5, 150.5, 1), y = ma(temp$temps, 12), 
      col = hue_pal()(4)[4], lwd=2)

# Blowflies Data
blowfly <- read.table ("blowfly.txt" , header = T)
head.matrix(blowfly)
