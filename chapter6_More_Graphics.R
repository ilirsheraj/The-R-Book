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
smooth <- read.table("Datasets/smoothing.txt", header = TRUE)
head(smooth)
attach(smooth)

plot(x, y, pch=16, col=hue_pal()(2)[1])
lines(x, y, col=hue_pal()(2)[2])

# Order x from smallest to largest
plot(x, y, pch=16, col=hue_pal()(2)[1])
seqs <- order(x)
lines(x[seqs], y[seqs], col=hue_pal()(2)[2])

plot(x[seqs], y[seqs], pch=16, col=hue_pal()(2)[1], type = "b", cex=1.5, lty = 1)
detach(smooth)

# Stepped Lines
x <- 0:10
y <- 0:10
plot(x, y, pch = 16, col = hue_pal()(4)[1])
# Add line connecting all dots
lines(x, y, col = hue_pal()(4)[2])
# Accross first -> s
lines(x, y, col = hue_pal()(4)[3], type = "s")
# Up First
lines(x, y, col = hue_pal()(4)[3], type = "S")

# Multiple Lines for each row (patient)
sleep <- read.table("Datasets/sleep.txt", header = T)
head(sleep)
attach(sleep)
plot(Days, Reaction, pch=16, col=hue_pal()(1))

library(lattice)
length(unique(sleep$Subject))
# To join lines: type=c("p", "l")
xyplot(Reaction ~ Days, groups = Subject, type=c("p", "l"), pch=16, 
       col=hue_pal()(18)[c(1:18)])
detach(sleep)

# Adding Shapes
## Create an empty plot
plot(0:10, 0:10, xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n")
# Inset a solid rectangle
rect(6, 6, 9, 9, col = hue_pal()(5)[1])
# Insert Arrows
arrows(1, 1, 3, 8, col = hue_pal()(5)[2])
# Insert double-headed arrow (code=3)
arrows(1, 9, 5, 9, code = 3, col = hue_pal()(5)[3])
# Like error bars: angle 90 instead of default 30
arrows(4, 1, 4, 6, code = 3, angle = 90, col = hue_pal()(5)[4])
# Add a polygon too
polygon(c(5, 7, 9, 8, 7, 5), c(4, 4, 2, 0.5, 1, 2), col = hue_pal()(5)[5])

# Adding mathematical Symbols
# Create a vector
x <- seq(-4, 4, len = 101)
plot(x, sin (x), type = "l", xaxt = "n", col = hue_pal()(1), 
     xlab = expression(paste("Phase Angle ", phi)),
     ylab = expression("sin " * phi))
# define xticks
axis(1, at = c(-pi, -pi/2, 0, pi/2, pi), 
     lab = expression(-pi, -pi/2, 0, pi/2, pi))
# Add mathematical expressions inside the plot
text(-pi/2, 0.5, substitute(chi^2 == "24.5"))
# Add lattex type-expression
text(pi/2, -0.5, 
     expression(paste(frac(1, sigma * sqrt(2*pi)), " ", 
                      e^{frac (-(x - mu)^2, 2*sigma^2)})))
text(pi/2, 0, expression(hat(y) %+-% se))
cd <- 0.63
text(-pi/2, 0, substitute(r^2 == cd, list (cd = cd)))
################################################################################
# ggplot2 -> The real thing :)
library(ggplot2)
daph <- read.table("Datasets/daphnia.txt", header = T)
head(daph)
# Way easier with pipe (%>%) operator, but let me stick to the book
## default histogram
ggplot(daph, aes(x = Growth.rate)) + geom_histogram ()

## COlor histogram
ggplot(daph, aes(x = Growth.rate)) + geom_histogram(fill = hue_pal()(1))

## Histogram showing water type: global color
ggplot(daph, aes(x = Growth.rate, fill = Water)) + geom_histogram()

# Facet by water type, one per each
ggplot(daph, aes(x = Growth.rate)) + geom_histogram (fill = hue_pal()(1)) +
  facet_grid (Water ~ .)

# Boxplots
ggplot(daph, aes(x = Detergent, y = Growth.rate)) + geom_boxplot()

ggplot(daph, aes(x = Water, y = Growth.rate, fill = Detergent)) + 
  geom_boxplot()

ggplot(daph, aes(x = Water, y = Growth.rate, fill = Water)) + 
  geom_boxplot()

# Flip it
ggplot(daph, aes(x = Water, y = Growth.rate, fill = Water)) + 
  geom_boxplot() + coord_flip()

# SHow dots and avoid cluttering with jitter
ggplot(daph, aes(x = Water, y = Growth.rate, fill = Water)) + 
  geom_boxplot() +
  geom_jitter(shape=16, position = position_jitter(0.2))

fertilizer_data <- read.table("Datasets/fertilizer.txt", header = T)
head(fertilizer_data)
fertilizer_data$week <- as.factor(fertilizer_data$week)

ggplot(fertilizer_data, aes(week, root)) + geom_point()

# Color by Plant
ggplot(fertilizer_data, aes(week, root, color = plant)) + 
  geom_point()

# Lines by plant
ggplot(fertilizer_data, aes(week, root, group = plant)) + 
  geom_line(aes(colour = plant), size=1)

# Add points too
ggplot(fertilizer_data, aes(week, root, group = plant)) + 
  geom_line(aes(colour = plant), size=1) +
  geom_point(aes(color=plant), size=2)

# Add Regression for each
ggplot(fertilizer_data, aes(week, root, color=plant, group = plant)) + 
  geom_point() +
  geom_smooth(method = "lm", se=FALSE)
################################################################################
# Graphics Cheat Sheet
## Current Plotting Region
par("usr")
par("usr")[1]

# Save the default plotting parameters
default.parameters <- par(no.readonly = TRUE)

# Current margins parameters
par ("mar")

# Text justification
par(adj = 0)

plot(1:10, 10:1, type = "n", axes = FALSE, xlab = "", ylab = "")
axis(1, 1:10, LETTERS[1:10], col.axis = hue_pal()(3)[1])
axis(2, 1:10, letters[10:1], col.axis = hue_pal()(3)[2])
axis(3, lwd = 3, col.axis = hue_pal()(3)[3])
axis(4, at = c (2, 5, 8), labels = c ("one", "two", "three"))

# Background colors for plots
par(bg = "cornsilk")

# Boxes around plots
plot(1 : 10, 10 : 1, type = "n")
plot(1 : 10, 10 : 1, type = "n", bty = "n")
plot(1 : 10, 10 : 1, type = "n", bty = "]")
plot(1 : 10, 10 : 1, type = "n", bty = "c")
plot(1 : 10, 10 : 1, type = "n", bty = "u")
plot(1 : 10, 10 : 1, type = "n", bty = "7")

# Change background to default
par(bg = "white")

# Cex for bubbles of different sizes
plot(0:10, 0:10, type = "n", xlab = "", ylab = "")
for(i in 1:10) points (2, i, cex = i, col = hue_pal()(10)[i])
for(i in 1:10) points (6, i, cex = (10 + (2 * i)), col = hue_pal()(10)[i])

# Change the shape of plotting region
par(plt = c(0.15, 0.95, 0.3, 0.7))
plot(c(0, 3000), c(0, 1500), type = "n", ylab = "y", xlab = "x")


# Reset at the end
par(default.parameters)
