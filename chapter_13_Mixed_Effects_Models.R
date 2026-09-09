# Mixed Effects Models
library(nlme)
library(scales)

plot_dir <- "Plots/"

farms <- read.table("Datasets/farms.txt", header = TRUE)
head(farms)
table(farms$farm)
# 24 by 5
attach(farms)
plotcol <- hue_pal()(24)
plot(N, size, col = plotcol[farm], pch = 16,
      ylab = "Plant size", xlab = "Soil nitrogen level")
detach(farms)

# Fit a separate linear regression for each farm
fits <- lapply(split(farms, farms$farm), function(x) {
  lm(size ~ N, data = x)
})

# fits
coef <- sapply(fits, coefficients)
coef

# Fit a linear regression, each farm with dummy variable
farms$farm <- factor(farms$farm)
fit <- lm(size ~ N + farm, data = farms)
summary(fit)


# Original scatterplot
pdf(paste0(plot_dir, "Farms_Linear_Regression.pdf"), width = 6, height = 4)
plot(farms$N, farms$size,
     col = plotcol[farms$farm],
     pch = 16,
     xlab = "Soil nitrogen level",
     ylab = "Plant size")

# Sequence of plant sizes
x <- seq(min(farms$N),
         max(farms$N),
         length.out = 100)

# Prediction for every farm at every x value
preds <- lapply(levels(farms$farm), function(f) {
  newdat <- data.frame(
    N = x,
    farm = factor(f, levels = levels(farms$farm)))
  predict(fit, newdata = newdat)
})

# Average fitted value across farms
yhat <- Reduce("+", preds) / length(preds)
lines(x, yhat, lwd = 3, col="red")
dev.off()

# Fit a mixed effects model with farms as random
farms_mod1 <- lme(size ~ 1, random = ~ 1 | farm, data = farms)
summary(farms_mod1)

# Another model with Fixed and Random
farms_mod2 <- lme(size ~ N, random = ~ 1 | farm, data = farms)
summary(farms_mod2)

# Scatterplot
pdf(paste0(plot_dir, "Farms_LME.pdf"), width = 6, height = 4)
plot(farms$N, farms$size,
     col = plotcol[farms$farm],
     pch = 16,
     xlab = "Soil nitrogen level",
     ylab = "Plant size")

# Nitrogen values
x <- seq(min(farms$N),
         max(farms$N),
         length.out = 100)

# Fixed-effect / population-level prediction
newdat <- data.frame(N = x)

pred <- predict(farms_mod2,
                newdata = newdat,
                level = 0)

# Add regression line
lines(x, pred, lwd = 3)
dev.off()

# Now with interaction: Nitrogen may change from farm to farm
# farms_mod3 <- lme(size ~ N, random = ~ N | farm, data = farms) No convergence

# Modify the above code
farms_mod4 <- lme(size ~ N, random = ~ N | farm, data = farms,
                  control = list (msMaxIter = 100 , opt = "optim" , msVerbose = TRUE))

summary(farms_mod4)

# Which is better, 4 or 2
anova(farms_mod4, farms_mod2)
# No difference, model 2 is simpler, thus preferable

library(predictmeans)
residplot(farms_mod2)
residplot(farms_mod4, level = 2)
################################################################################
# Multi-Level Data
rats <- read.table("Datasets/rats.txt" , header = TRUE)
head(rats)

# A bit complicated: 3 Treatments, 2 rats per treatment, 3 pieces of liver per rat
# 2 experimental replicates per piece of live: 3x2x3x2 = 36
rats_num <- cumsum(!duplicated(rats[2:3]))
rats$rat_num <- rats_num
attach(rats)
table(Treatment)
table(Rat)
table(rat_num)
rats[names(rats) != "Glycogen"] <- lapply(rats[names(rats) != "Glycogen"], factor)

# This time we use lme4
library(lme4)
# Treatment is fixed, rat_num and liver are random
rats_mod1 <- lmer(Glycogen ~ Treatment + (1 | rat_num / Liver), data = rats)
summary(rats_mod1)

# We see that the total variance is about 14.17 + 36.06 + 21.17 = 71.40 and so about 50%
# of the variation is between rats within treatments, 19.8% is between liver bits
# within rats and 29.6% is between readings within liver bits within rats.
pdf(paste0(plot_dir, "Rats_resid_plots_1.pdf"), width = 6, height = 4)
residplot(rats_mod1, level = 1)
dev.off()
pdf(paste0(plot_dir, "Rats_resid_plots_2.pdf"), width = 6, height = 4)
residplot(rats_mod1, level = 2)
dev.off()
