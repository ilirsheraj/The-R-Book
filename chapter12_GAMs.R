# Generalized Additive Models
library(scales)
library(mgcv)
library(tree)
plot_dir <- "Plots/"

soay <- read.table("Datasets/soaysheep.txt", header = TRUE)
head(soay)

# Delta was generated as below
bs <- vector(mode = "numeric", length = nrow(soay))
for (i in 1:nrow(soay)){
  bs[i] <- round(log(soay$Population[i+1]/soay$Population[i]), 9)
}

identical(soay$Delta, bs)

attach(soay)
plot(Population, Delta, col=hue_pal()(2)[1], pch=16)
abline(h=0, lty=2)

# Fit a loess model
soay_mod1 <- loess(Delta ~ Population, data = soay)
summary(soay_mod1)
# 4.66 parameters

# Draw the model line through the data within the same range
xv <- seq(600, 2000, 1)
yv <- predict(soay_mod1, data.frame(Population = xv))
lines(xv, yv, col = hue_pal()(3)[2], lwd=2)


# Let's add confidence intervals
plot(Population, Delta, col=hue_pal()(2)[1], pch=16)
pred <- predict(soay_mod1, newdata = data.frame(Population = xv),
                se = TRUE)
# 95% confidence intervals
fit <- pred$fit
upper <- fit + 1.96 * pred$se.fit
lower <- fit - 1.96 * pred$se.fit

ok <- complete.cases(xv, fit, upper, lower)
xv2 <- xv[ok]
fit2 <- fit[ok]
upper2 <- upper[ok]
lower2 <- lower[ok]

pdf(paste0(plot_dir, "Loess_with_CI.pdf"), width = 6, height = 4)
plot(Delta ~ Population, data = soay,
     col = hue_pal()(2)[1], pch = 16)

polygon(
  x = c(xv2, rev(xv2)),
  y = c(lower2, rev(upper2)),
  col = adjustcolor(hue_pal()(3)[2], alpha.f = 0.2),
  border = NA)
lines(xv2, fit2, col = hue_pal()(3)[2], lwd = 2)
dev.off()

# Let's fit a step function
thresh <- tree(Delta ~ Population, data = soay)
print(thresh)

# Firts split at
th <- 1289.5
soay_mod2 <- aov(Delta ~ (Population > th), data = soay)
summary(soay_mod2)

# Get the averages of two population classes
tapply(soay$Delta, (soay$Population > th), mean, na.rm=TRUE)

# Add it to the plot
pdf(paste0(plot_dir, "Loess_vs_Tree.pdf"), width = 6, height = 4)
plot(Population, Delta, col=hue_pal()(2)[1], pch=16)
xv <- seq(600, 2000, 1)
yv <- predict(soay_mod1, data.frame(Population = xv))
lines(xv, yv, col = hue_pal()(3)[2], lwd=2)
lines(x = c(600, th, th, 2000), y = c(0.2265, 0.2265, -0.2837, -0.2837),
      lty = 2, col = hue_pal()(3)[3], lwd=2)
legend("topright",
       legend = c("Loess", "Tree"),
       col = c(hue_pal()(3)[2], hue_pal()(3)[3]),
       lty = c(1, 2),
       lwd = c(2, 2),
       bty = "n")
dev.off()



