# Plotting with Base R
scatter <- read.table("Datasets/scatter1.txt", header = T)
head(scatter)

plot_dir <- "Plots/"

attach(scatter)
plot(xv, ys)

# Add Labels
plot(xv, ys, xlab = "Explanatory variable", ylab = "Response variable",
     main = "A plot of the response variable against the explanatory variable")

# Put the title in multiple lines
plot(xv, ys, xlab = "Explanatory variable", ylab = "Response variable",
     main = "A plot of the response variable \nagainst the explanatory variable")

# Change dot style and size with cex
plot(xv, ys, xlab = "Explanatory variable", ylab = "Response variable",
     main = "A plot of the response variable \nagainst the explanatory variable",
     pch = 16, cex = 0.6)

# Add dot color
plot(xv, ys, xlab = "Explanatory variable", ylab = "Response variable",
     pch=16, cex=0.6, col="blue")

# Add background color for dots with circumference and backgroun: pch=21-25
plot(xv, ys, xlab = "Explanatory variable", ylab = "Response variable",
     pch=21, cex=0.6, col="blue", bg="orange")

# Use scales library
library(scales)
plot(xv, ys, xlab = "Explanatory variable", ylab = "Response variable",
     pch=21, cex=0.7, col=hue_pal()(2)[1], bg=hue_pal()(2)[2])

# Save it as pdf
pdf(paste0(plot_dir, "scatter.pdf"), width = 7, height = 4)
plot(xv, ys, xlab = "Explanatory variable", ylab = "Response variable",
     pch=21, cex=0.7, col=hue_pal()(2)[1], bg=hue_pal()(2)[2])
dev.off()

postscript(paste0(plot_dir, "scatter.ps"))
plot(xv, ys, xlab = "Explanatory variable", ylab = "Response variable",
     pch=21, cex=0.7, col=hue_pal()(2)[1], bg=hue_pal()(2)[2])
dev.off()

tiff(paste0(plot_dir, "scatter.tiff"), width = 700, height = 500)
plot(xv, ys, xlab = "Explanatory variable", ylab = "Response variable",
     pch=21, cex=0.7, col=hue_pal()(2)[1], bg=hue_pal()(2)[2])
dev.off()

# Detach it after finishing
detach(scatter)
################################################################################
# Plots for single variable
# Histograms
daph <- read.table("Datasets/daphnia.txt", header = T)
head(daph)

# Attach it to make plotting easy
attach(daph)

# Basic Histogram
hist(Growth.rate, col = hue_pal()(4)[1], main = "")

hist(Growth.rate, col = hue_pal()(4)[1], main = "", 
     breaks = seq(0, 8, 0.25))

# Turn into density
hist(Growth.rate, col = hue_pal()(4)[1], main = "", freq = FALSE)

# Add density Line
hist(Growth.rate, col = hue_pal()(4)[1], main = "", freq = FALSE)
lines(density(Growth.rate))

# Make a separate density
plot(density(Growth.rate), col=hue_pal()(4)[1], xlab="Daphnia growth rate", main="")
# Fill it
polygon(density(Growth.rate), col=hue_pal()(4)[1], xlab="Daphnia growth rate", main="")

# Generate random data from a poisson distribution
values <- rpois(1000, 1.7)
hist(values, col = hue_pal()(4)[1], main="Random Poisson")
#Improve it
hist(values, breaks=(-0.5 : 8.5), col = hue_pal()(4)[1], main = "",
     xlab = "random numbers from a Poisson with mean 1.7")

plot(density(values), col=hue_pal()(4)[1], xlab="Random Numbers", main="")
# Fill it
polygon(density(values), col=hue_pal()(4)[1], xlab="Random Numbers", main="")

# Boxplots
# Simple, default
boxplot(Growth.rate)

# Color
boxplot(Growth.rate, col=hue_pal()(4)[1], ylab="Daphnia Growth Rate")

# The 6-point Summary
summary(Growth.rate)

# Add some extreme values for visualization of outliers
Growth.rate <- c(Growth.rate, 10, 11)
boxplot(Growth.rate, col=hue_pal()(4)[1], ylab="Daphnia Growth Rate", pch=19)
detach(daph)

# Dotplots: Cool for few samples
caterpillar <- read.table("Datasets/caterpillar.txt", header = T)
caterpillar

stripchart(caterpillar$growth, xlab = "Caterpillar growth", col = hue_pal()(2)[1])
# There is some overlap
stripchart(caterpillar$growth, pch = 16, cex=2,  
           xlab = "Caterpillar growth", col = hue_pal()(2)[1])

# Add jitter
stripchart(caterpillar$growth, xlab = "Caterpillar growth", col = hue_pal()(2)[2],
           method = 'jitter', jitter = 1, pch = 16, cex = 2)

# Use stack, cleaner in htis case
stripchart(caterpillar$growth, xlab = "Caterpillar growth", col = hue_pal()(2)[2],
           method = 'stack', pch = 16, cex = 2)

# Bar Charts
hair_eye <- read.table("Datasets/hair_eye.txt", header = T)
head(hair_eye)

# Create a table for hair color
table_hair <- table(hair_eye$Hair)
table_hair
table_order <- order(-table_hair)
barplot(table_hair, col = hue_pal()(4), ylab="Frequency", main = "Hair Color")

# Order from most to least frequent
barplot(table_hair[table_order], col = hue_pal()(4), ylab="Frequency", main = "Hair Color")

# Pie Chart
piedata <- read.csv("Datasets/piedata.csv")
piedata
pie(piedata$amounts, labels = piedata$names, col=hue_pal()(5))
################################################################################
# Plots for two Numeric Variables
attach(scatter)
head(scatter)

# Back to scatterplot
plot(xv, ys, col = hue_pal()(3)[1], pch = 16, cex = 0.6)

plot(xv, ys, col = hue_pal()(3)[1], cex = 0.6, 
      xlab = "Explanatory variable", ylab = "Response variable")
# Add Vertical Line at x=40
abline(v = 40, col = hue_pal()(3)[2], lwd = 3)

# Plot again and add regression line
plot(xv, ys, col = hue_pal()(3)[1], cex = 0.6,
     xlab = "Explanatory variable", ylab = "Response variable")
abline(lm (ys ~ xv), col = hue_pal()(3)[2], lwd = 3)

# Add an additional dataset to the same plot
scatter2 <- read.table("Datasets/scatter2.txt", header = T)
head(scatter2)
attach(scatter2)

# Normal plot of the first dataset
plot(xv, ys, col = hue_pal()(3)[1], cex = 0.6,
     xlab = "Explanatory variable", ylab = "Response variable")
abline(lm (ys ~ xv), lty = 1, lwd = 2)
# Add the second dataset and its regression line
points(xv2, ys2, col = hue_pal()(3)[2], pch = 16, cex = 0.6)
abline(lm (ys2 ~ xv2), lty = 2, lwd = 2)

# Make it better (Avoid removing data points)
# Create a blank plot encompassing all ranges
plot(c (xv, xv2), c(ys, ys2), xlab = "Explanatory variable",
     ylab = "Response variable", type = "n")
# Now add
points(xv, ys, col = hue_pal()(3)[1], pch = 16, cex = 0.6)
points(xv2, ys2, col = hue_pal()(3)[2], pch = 16, cex = 0.6)
abline(lm (ys ~ xv), lty = 1, lwd = 2)
abline(lm (ys2 ~ xv2), lty = 2, lwd = 2)

# Change the ranges
range(c(xv, xv2))
range(c(ys, ys2))
plot(c (xv, xv2), c(ys, ys2), xlab = "Explanatory variable",
     ylab = "Response variable", xlim=c(0, 100), ylim = c(10, 65), type = "n")
# Now add
points(xv, ys, col = hue_pal()(3)[1], pch = 16, cex = 0.6)
points(xv2, ys2, col = hue_pal()(3)[2], pch = 16, cex = 0.6)
abline(lm (ys ~ xv), lty = 1, lwd = 2)
abline(lm (ys2 ~ xv2), lty = 2, lwd = 2)

# Add a legend
plot(c (xv, xv2), c(ys, ys2), xlab = "Explanatory variable",
     ylab = "Response variable", xlim=c(0, 100), ylim = c(10, 65), type = "n")
# Now add
points(xv, ys, col = hue_pal()(3)[1], pch = 16, cex = 0.6)
points(xv2, ys2, col = hue_pal()(3)[2], pch = 16, cex = 0.6)
abline(lm (ys ~ xv), lty = 1, lwd = 2)
abline(lm (ys2 ~ xv2), lty = 2, lwd = 2)
# Gives you the option of clicking on the plot to locate the legend
legend(locator (1), c("Dataset 1", "Dataset 2"), pch = c (16, 16),
       col = hue_pal()(3)[1:2])

# Determine the legend by coordinates
pdf(paste0(plot_dir, "scatter2.pdf"), width = 7, height = 4)
plot(c (xv, xv2), c(ys, ys2), xlab = "Explanatory variable",
     ylab = "Response variable", xlim=c(0, 100), ylim = c(10, 65), type = "n")
# Now add
points(xv, ys, col = hue_pal()(3)[1], pch = 16, cex = 0.6)
points(xv2, ys2, col = hue_pal()(3)[2], pch = 16, cex = 0.6)
abline(lm (ys ~ xv), lty = 1, lwd = 2)
abline(lm (ys2 ~ xv2), lty = 2, lwd = 2)
legend(0, 60, c("Dataset 1", "Dataset 2"), pch = c (16, 16),
        col = hue_pal()(3)[1:2])
dev.off()
detach(scatter)
detach(scatter2)

# Plots with many identical values
longdata <- read.table("Datasets/longdata.txt", header = T)
head(longdata)
attach(longdata)
# Many values are repeated, so they get hidden away
plot(xlong, ylong, pch=16, col=hue_pal()(3)[1], xlab = "Input", ylab = "Count")

# Use Jitter
plot(jitter(xlong, amount = 1), jitter(ylong, amount = 1), col=hue_pal()(4)[1], 
     xlab = "Input", ylab = "Count")

# Sunflower: This is cool
sunflowerplot(xlong, ylong, col=hue_pal()(4)[1], xlab = "Input", ylab = "Count")

# Bubble plot: The bigger the bubble, the more data points clustered there
tab_longdata <- table(longdata$ylong, longdata$xlong)
tab_longdata
tab_longdata_df <- as.data.frame(tab_longdata)
head(tab_longdata_df)
# Remove zero frequency
tab_longdata_df <- tab_longdata_df[!(tab_longdata_df$Freq == 0),]

# Setting the radius of circles so that area of bubble represents frequency
radius_area <- sqrt(tab_longdata_df$Freq / pi)
symbols(tab_longdata_df$Var2, tab_longdata_df$Var1, circles = radius_area,
        xlab = "Input", ylab = "Count", 
        # Downscale with inches to make them fit the plot
        inches = 0.3, bg = hue_pal()(4)[1])
detach(longdata)
################################################################################
# Plots for Numerical Variables by Groups
weather <- read.table("Datasets/SilwoodWeather.txt", header = T)
head(weather)

# Convert month into factor for boxplot
weather$month <- as.factor(weather$month)
attach(weather)

# Boxplot by Group
boxplot(upper ~ month, ylab = "Max Daily Temperature", xlab = "Months",
        col=hue_pal()(1)[1])
# Add notches
boxplot(upper ~ month, ylab = "Max Daily Temperature", xlab = "Months",
        col=hue_pal()(1)[1], notch=TRUE)
detach(weather)

# Dotplots by Group
compexpt <- read.table("Datasets/compexpt.txt", header = T)
head(compexpt)
attach(compexpt)
length(unique(clipping))
stripchart(biomass ~ clipping, col=hue_pal()(5), pch=16)
detach(compexpt)

# Barplot
head(weather)
attach(weather)
means <- tapply(upper, month, mean)
means
barplot(means, xlab = "Month", ylab = "Mean daily maximum temperature",
        col = hue_pal()(4)[1])
# Put error bars
sem <- tapply(upper, month, function(x) sqrt(var(x)/length(x)))
mybarplot <- barplot(means, xlab = "Month", ylab = "Mean daily maximum temperature", 
                     col = hue_pal()(4)[1])
# Add arrows: code=3 for head, not pointy
arrows(x0 = mybarplot, y0 = means - sem, x1 = mybarplot, y1 = means + sem,
       code = 3, angle = 90, length = 0.1)
detach(weather)
################################################################################
# Plots for two categorical variables
table_hair_eye <- table(hair_eye$Hair, hair_eye$Eye)
table_hair_eye

barplot(table_hair_eye, col = c(hue_pal()(4)[1:4]), ylab = "Frequency", 
        xlab = "Eye colour", legend = rownames(table_hair_eye))

# Normalize
normalized_table <- prop.table(table_hair_eye, margin = 2)
barplot(normalized_table, col = hue_pal()(4)[1:4], ylab = "Proportion",
        xlab = "Eye colour", legend = rownames(table_hair_eye))

barplot(table_hair_eye, col = c(hue_pal()(4)[1:4]), ylab = "Frequency", 
        xlab = "Eye colour", legend = rownames(table_hair_eye), beside = TRUE)

# Mosaic Plots: Hard to read
mosaicplot(table_hair_eye, col = c(hue_pal()(4)[1:4]), ylab = "Eye colour", 
           xlab = "Hair colour", main = "")

# 3+ Variables
ozone <- read.table("Datasets/ozone_pollution.txt", header = TRUE)
head(ozone)
pairs(ozone, col = hue_pal()(1), pch=16)

# Text in plot
pgr <- read.table("Datasets/pgr.txt", header = T)
head(pgr)
attach(pgr)

plot(hay, pH, col = hue_pal()(2)[1])
text(hay, pH, labels = round (FR, 1), pos = 1, offset = 0.5, cex = 0.7)

# Color by condition
plot(hay, pH, pch = 16, xlim = c(1, 10), ylim = c(3.5, 8),
     col = ifelse(FR > median (FR), hue_pal()(2)[1], hue_pal()(2)[2]))
legend(1, 8, c("FR > median", "FR <= median"), pch = 16, col = hue_pal()(2)[1:2])
detach(pgr)
#################################################################################
# 3D Plots
library(plotly)
library(akima)
attach(pgr)

# Create grid
plot_dat <- interp(hay, pH, FR)
plot_dat

plot_ly(x = plot_dat$x, y = plot_dat$y, 
        z = matrix(plot_dat$z, nrow = length(plot_dat$y), byrow = TRUE),
        type = "contour", colorscale = hue_pal()(24))

# 3D Plot
plot_ly(x = plot_dat$x, y = plot_dat$y,
        z = matrix (plot_dat$z, nrow = length(plot_dat$y), byrow = TRUE),
        type = "surface", colorscale = hue_pal()(24))
detach(pgr)
################################################################################
# Trellis Graphics: Multiple Plots per page and Multi-Page Plots
panels <- read.table("Datasets/panels.txt", header = T)
head(panels)
table(panels$gender)
attach(panels)
library(lattice)
xyplot(weight ~ age | gender, col = hue_pal()(1)[1])
detach(panels)

head(daph)
attach(daph)
# Remove the mdified growthrate
rm(Growth.rate)
bwplot(Growth.rate ~ Water + Daphnia | Detergent,
       col = hue_pal()(1)[1], scales = list(x = list(rot = 45)))
detach(daph)

fertilizer_data <- read.table("Datasets/fertilizer.txt", header = T)
head(fertilizer_data)
summary(fertilizer_data)
attach(fertilizer_data)

xyplot(root ~ week | plant, col = hue_pal()(1)[1])

# Change dot type
xyplot(root ~ week | plant, col = hue_pal()(1)[1], pch=16)

# Fancier with regression lines inside
xyplot(root ~ week | plant, panel = function (x, y) {
  panel.xyplot(x, y, pch = 16, col = hue_pal()(2)[1])
  panel.abline(lm (y ~ x), col = hue_pal()(2)[2])
  })

# Add horixontal line on fourth data point
xyplot(root ~ week | plant, panel = function (x, y) {
  panel.xyplot(x, y, pch = 16, col = hue_pal()(3)[1])
  panel.abline(lm (y ~ x), col = hue_pal()(3)[2])
  panel.abline(h=y[4], col = hue_pal()(3)[3], lty = 3)
  })

# Add numbers to each pannel
xyplot(root ~ week | plant, panel = function (x, y) {
  panel.xyplot(x, y, pch = 16, col = hue_pal()(2)[1])
  panel.abline(lm (y ~ x), col = hue_pal()(2)[2])
  panel.text(8, 2, panel.number(), col = hue_pal()(4)[4], cex = 0.7)
  })
detach(fertilizer_data)

# Panel Barplots
data("barley")
head(barley)
barchart(yield ~ variety | site, data = barley, groups = year, 
         layout = c(2,3), stack = TRUE, col = c (hue_pal ()(2)[1:2]),
         ylab = "Barley Yield (bushels/acre)",
         scales = list(x = list (rot = 45)))

data("ethanol")
head(ethanol)
# Divide into 9 intervals with 25% overlap
EE <- equal.count(ethanol$E, number = 9, overlap = 1/4)

xyplot(NOx ~ C | EE, data = ethanol, layout = c (9, 1),
       panel = function(x, y){
         panel.xyplot (x, y, col = hue_pal()(2)[1], pch = 16)
         panel.abline (lm (y ~ x), col = hue_pal()(2)[2])
         })

# Panel Histograms
head(weather)
histogram( ~ lower | month, type = "count", data = weather,
           xlab = "mimimum temerature", ylab = "frequency",
           breaks = seq(-12, 28, 2), col = hue_pal()(1))

# Panel Lines
data("OrchardSprays")
head(OrchardSprays)

xyplot(decrease ~ treatment, data = OrchardSprays, groups = rowpos, type="a", 
       col = c(hue_pal()(8)[1:8]), 
       key = list(lines = list(col = c(hue_pal()(8)[1:8])),
                  text = list (as.character(unique(OrchardSprays$rowpos))),
                  space = "right"))
################################################################################
# Plotting Functions
# Classical way
x <- seq(-2, 2, 0.01)
y <- x^3 - 3*x
plot(x, y, type = "l", col = hue_pal()(1), xlab = "x", ylab = "y")

# Can all be done using curve() function
curve(x^3 - 3*x, -2, 2, col = hue_pal()(1), xlab = "x", ylab = "y")

# Standard Normal Distribution
curve(dnorm(x, mean = 0, sd = 1), -5, 5, col = hue_pal()(2)[1],
      xlab = "x", ylab = "Density of standard normal", lwd = 3)

# Modify
curve(dnorm(x, mean = 0, sd = 1), -5, 5, col = hue_pal()(2)[1],
      xlab = "x", ylab = "Density of standard normal")
w <- seq(-2, -1, 0.01)
polygon(c(-2, w, -1), c(0, dnorm(w), 0), col = hue_pal()(2)[2])

# More 3D Plots
x <- seq(0, 10, 0.1)
y <- seq(0, 10, 0.1)
# Define a function
func <- function(a, b) 3*a*exp (0.1*a) * sin(b*exp (-0.5*a))
z <- t(outer(x, y, func))

plot_ly(x = x, y = y, z = z, type = "contour", colorscale = hue_pal()(24))

plot_ly(x = x, y = y, z = z, type = "surface", colorscale = hue_pal()(24))

