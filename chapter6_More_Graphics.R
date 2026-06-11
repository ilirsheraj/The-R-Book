# More in depth about plotting
library(scales)
plot_dir <- "Plots/"
################################################################################
# Colors
weather <- read.table("Datasets/SilwoodWeather.txt", header = T)
head(weather)
weather$month <- factor(weather$month)

# Use heat colors
season <- heat.colors(12)
# Define temeperatures for heat
temp <- c(11, 10, 8, 5, 3, 1, 2, 3, 5, 8, 10, 11)
attach(weather)
plot(month, upper, col = season[temp])
detach(weather)

# See different colors in pie plots
par(mfrow = c(2, 2))
pie(rep (1, 7), col = rainbow (7), radius = 1)
pie(rep (1, 14), col = rainbow (14), radius = 1)
pie(rep (1, 28), col = rainbow (28), radius = 1)
pie(rep (1, 56), col = rainbow (56), radius = 1)

pie(rep (1, 14), col = heat.colors(14), radius = 0.9)
pie(rep (1, 14), col = terrain.colors(14), radius = 0.9)
pie(rep (1, 14), col = topo.colors(14), radius = 0.9)
pie(rep (1, 14), col = cm.colors(14), radius = 0.9)
par(mfrow = c(1,1))

custom <- c(rgb (0.6, 0.8,1), rgb(1, 0.8, 0.2), rgb(1, 0.8, 0.4),
            rgb(1, 0.8, 0.6), rgb(1, 0.8, 0.8), rgb(1, 0.8,1),
            rgb(0.8, 0.8,1), rgb(0.7, 0.8,1))
pie(rep (1/8, 8), col = custom)

# Explore the RcolorBrewer
library(RColorBrewer)
mypalette <- brewer.pal(8, "Reds")
pie(rep(1,8), col = mypalette)

mypalette <- brewer.pal(8, "Set1")
pie(rep(1,8), col = mypalette)

mypalette <- brewer.pal(8, "Set2")
pie(rep(1,8), col = mypalette)

# Foreground Colors (Plot Corners)
plot(1 : 10, 1 : 10, xlab = "x", ylab = "y")
plot(1 : 10, 1 : 10, xlab = "x", ylab = "y", fg = "blue")
plot(1 : 10, 1 : 10, xlab = "x", ylab = "y", fg = hue_pal ()(1))
plot(1 : 10, 1 : 10, xlab = "x", ylab = "y", fg = brewer.pal (3, "Accent")[1])

# Background Colors
jaws <- read.table("Datasets/jaws.txt", header = T)
head(jaws)
# Set background color
par(bg = "wheat2")
plot(jaws$age,jaws$bone, pch = 16, cex = 2, col = hue_pal()(1))
plot(jaws$age, jaws$bone, pch = 21, cex = 2, col = hue_pal()(2)[1], bg = hue_pal()(2)[2])
# reset to white
par(bg = "white")

# Change Legend Color
x <- seq(1, 10, length=30)
y <- rnorm(30, mean = 10, sd=2)
z <- rep(c("Control", "Heat", "Dose"), 10)
bs <- data.frame(x, y, z)
bs$z <- as.factor(bs$z)
my_colors <- hue_pal()(6)[c(2, 4, 6)]
plot(bs$x, bs$y, col = my_colors[as.numeric(bs$z)], pch = 19, xlab = "X", ylab = "Y")
legend(6, 14, legend = c ("Control", "Heat", "Dose"), pch = c (16, 16, 16),
       bg = "wheat1", col = hue_pal()(6)[c(2, 4, 6)])

# Fill and circumference coloring
plot(bs$x, bs$y, col = my_colors[as.numeric(bs$z)], pch = 19, xlab = "X", ylab = "Y")
legend (6, 14, legend = c ("Control", "Heat", "Dose"), pch = c(21, 21, 21),
        bg = "wheat1", col = hue_pal()(6)[c(2, 4, 6)],
        pt.bg = hue_pal()(6)[c (1, 3, 5)])

# Different colors for different parts of the graph
plot(1:10, 1:10, xlab = "x label", ylab = "y label", pch = 16,
     col = hue_pal()(3)[1], col.lab = hue_pal()(3)[2], col.axis = hue_pal()(3)[3])


# Cooler boxplots, fully customized
head(weather)
attach(weather)
plot(factor(month), lower, ylab = "minimum temperature", xlab = "month",
     # remove median line and replace it by dot with specific color
     medlty = "blank", medpch = 21, medbg = hue_pal()(4)[1], medcol = hue_pal()(4)[2],
     # Color the box, fill it differently and make outlied pch=21
     boxcol = hue_pal()(4)[1], boxfill = hue_pal()(4)[3], outpch = 21,
     # Color the fill and circumference of outliers
     outbg = hue_pal()(4)[2], outcol = hue_pal()(4)[1],
     # Change line color and whiskers color
     staplecol = hue_pal()(4)[4], whisklty = 1, whiskcol = hue_pal()(4)[4])
detach(weather)

box <- read.table("Datasets/box.txt", header = T)
head(box)
attach(box)
barplot(tapply(response, fact, mean), density = 3:10, 
        angle = seq(30, 60, length = 8))

# Fill it in different shades of grey
barplot(tapply(response, fact, mean), col = grey(seq(0.8, 0.2, length = 8)))
#################################################################################
# Changing the looks of Graphics
x1 <- seq(1:20)
y1 <- rnorm(length(x1), 10, 2)
x2 <- seq(1:50)
y2 <- x2 + rnorm(length(x2), 0, 0.1)
x3 <- seq(1:10)
y3 <- rpois(length(x3), 1.8)

par(mfrow=c(1,3))
plot(x1, y1)
plot(x2, y2)
plot(x3, y3)
par(mfrow = c (1, 1))

# Tick marks
xvals <- seq(0, 150, 10)
yvals <- 16 + xvals * 0.4 + rnorm (length (xvals), 0, 6)
par(mfrow = c (1, 2))
plot(xvals, yvals, pch = 16, col = hue_pal()(1), 
     xlab = "x axis label", ylab = "y axis label")
plot(xvals, yvals, pch = 16, col = hue_pal()(1),
     xlab = "x axis label", ylab = "y axis label",
     las = 1, cex.lab = 1.8, cex.axis = 1.5)
par(mfrow = c(1, 1))


# Text font
## Create empty plot
plot(1:10, 1:10, type = "n", xlab = "x", ylab = "y")
# default font
par(family = "sans")
text(5, 8, "This is the default font")
# Change to Serif
par(family = "serif")
text(5, 6, "This is the serif font")
par(family = "mono")
text(5, 4, "This is the mono font")
par(family = "HersheySymbol")
text(5, 2, "This is the symbol font")

# Back to default
par(family = "sans")
################################################################################
# Adding Items to Plots
map_places <- read.csv("Datasets/map.places.csv", header = T)
head(map_places)
attach(map_places)

# This dataset contains coordinates of places
map_data <- read.csv("Datasets/bowens.csv", header = T)
head(map_data)
attach(map_data)

# Shift the coordinates under 60 north by 100 for whatever reason :)
nn <- ifelse(north < 60, north + 100, north)

# Create an empty plot
plot(c(20, 100), c(60, 110), type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
# Now populate it with text: this can be handled like a pro by geom_text()
for (i in 1:length (wanted)){
  ii <- which(place == as.character(wanted[i]))
  text(east[ii], nn[ii], as.character(place[ii]), cex = 0.6)
}

detach(map_places)
detach(map_data)

# Adding smooth parametric curves
plotfit <- read.table("Datasets/plotfit.txt", header = T)
head(plotfit)
attach(plotfit)

# Remove not to interfere
rm(list = c("x", "y"))

plot(x, y, pch=16, cex=1.3, col=hue_pal()(3)[1])

# Fit the Rick Curve
xv <- 0:100
yA <- 482 * xv * exp (-0.045 * xv)
yB <- 518 * xv * exp (-0.055 * xv)
lines(xv, yA, lty = 2, col = hue_pal()(3)[2])
# Fix the error in the book
lines(xv, yB, lty = 1, col = hue_pal()(3)[3])
detach(plotfit)

# Fit non-parametric curves (My favorite)
jaws <- read.table("Datasets/jaws.txt", header = T)
head(jaws)
attach(jaws)

par(mfrow = c (2, 2))
plot(age, bone, pch = 16, col = hue_pal()(2)[1], main = "Lowess")
# fit lowess
lines(lowess(age, bone), col = hue_pal()(2)[2])

# Loess
plot(age, bone, pch = 16, col = hue_pal()(2)[1], main = "Loess")
model <- loess(bone ~ age)
xv <- 0:50
yv <- predict(model, data.frame(age = xv))
lines(xv, yv, col = hue_pal()(2)[2])

# GAM
library(mgcv)
plot(age, bone, pch = 16, col = hue_pal()(2)[1], main = "GAM")
model <- gam(bone ~ s(age))
xv <- 0:50
yv <- predict(model, list (age = xv))
lines(xv, yv, col = hue_pal()(2)[2])

# Polynomial
plot(age, bone, pch = 16, col = hue_pal()(2)[1], main = "Polynomial")
model <- lm(bone ~ age + I (age^2) + I (age^3))
xv <- 0:50
yv <- predict(model, list(age = xv))
lines(xv, yv, col = hue_pal()(2)[2])
par(mfrow=c(1,1))
detach(jaws)

# Connecting Observations


