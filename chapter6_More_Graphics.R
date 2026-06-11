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
