# Regression Analysis
# Different from ML, independent variable/predictor is prefferrably called 
# "COVARIATE" and dependent variable is called "OUTCOME" in Statistics
library(scales)
plot_dir <- "Plots/"

# Let's start with a simple example
caterpillardata <- read.table("Datasets/caterpillar.txt" , header = T)
head(caterpillardata)
attach(caterpillardata)

# Plot it
plot(tannin, growth, pch=16, col = hue_pal()(3)[1],
     xlab = "% of Tannin in Diet", ylab = "Growth Rate")
detach(caterpillardata)

# First simple linear model
lm(growth ~ tannin, data = caterpillardata)

# To get all info, save the model as an object first
caterpillar_model <- lm(growth ~ tannin, data = caterpillardata)
summary(caterpillar_model)

# Or even shorter
summary(lm(growth ~ tannin, data = caterpillardata))
################################################################################
# Multiple Linear Regression Model
ozone_pollution <- read.table("Datasets/ozone_pollution.txt", header = T)
head(ozone_pollution)

# Check out the scatter plot and inspect relationships between variables
pairs(ozone_pollution, pch=16, col=hue_pal()(3)[2])

# Build a linear model
ozone_mod1 <- lm(ozone ~ rad + temp + wind, data = ozone_pollution)
summary(ozone_mod1)

# Model without intercept
summary(lm(ozone ~ rad + temp + wind - 1, data = ozone_pollution))

# Or
summary(lm(ozone ~ rad + temp + wind + 0, data = ozone_pollution))

# Dealing with categorical covariates
yields <- read.table("Datasets/yields.txt", header = T)
head(yields)
yields_long <- stack(yields)
head(yields_long)
colnames(yields_long) <- c("yield", "soil")
head(yields_long)

yields_model1 <- lm(yield ~ soil, data = yields_long)
summary(yields_model1)

# Change reference from sand to clay
yields_long$soil <- factor(yields_long$soil, levels = c("clay", "loam", "sand"))
yields_model2 <- lm(yield ~ soil, data = yields_long)
summary(yields_model2)

# Change from clay to loam
yields_long$soil <- factor(yields_long$soil, levels = c("loam", "sand", "clay"))
yields_model3 <- lm(yield ~ soil, data = yields_long)
summary(yields_model3)

# Create a dummy matrix without intercept (otherwise introduces a new coefficient)
dummy_matrix <- model.matrix(~yields_long$soil - 1)
head(dummy_matrix)

# This will make clay the reference
summary(lm(yields_long$yield ~ dummy_matrix))

# Or put them all in a dataframe
new_frame <- data.frame(yields_long, model.matrix(~ yields_long$soil - 1))
head(new_frame)

# Again clay is reference so its displayed as NA
summary(lm(yield ~ yields_long.soilloam + yields_long.soilsand + 
             yields_long.soilclay, data = new_frame))

# Interactions between covariates
gain <- read.table("Datasets/Gain.txt", header = T)
gain$Genotype <- as.factor(gain$Genotype)
head(gain)
sapply(gain, class)

# Full model
gain_model1 <- lm(Weight ~ Sex + Age + Genotype, data = gain)
summary(gain_model1)

# Lets's check for interaction terms
## Age effect on weight depending on sex
pdf(paste0(plot_dir, "Interaction_Plots.pdf"), width = 6, height = 4)
par(mfrow=c(1,2))
coplot(Weight ~ Age | Sex, data = gain, pch=16)
## Effect of Genotype on weight depending on sex
coplot(Weight ~ Genotype | Sex, data = gain, pch=16)
dev.off()

pdf(paste0(plot_dir, "Interaction_Plot2.pdf"), width = 6, height = 4)
interaction.plot(gain$Genotype, gain$Sex, gain$Weight)
dev.off()

# Let's introduce interaction terms
gain_mod2 <- lm(Weight ~ Sex * Age + Genotype, data = gain)
summary(gain_mod2)

# Interaction between all covariates
gain_model3 <- lm(Weight ~ Age * Genotype * Sex, data = gain)
summary(gain_model3)

# More details on output
summary(ozone_mod1)

anova(ozone_mod1)

# We change order, no change in lm, but p-changes in ANOVA
ozone_mod1a <- lm(ozone ~ wind + rad + temp, data = ozone_pollution)
summary(ozone_mod1a)
anova(ozone_mod1a)

# Extracting Model Information
names(ozone_mod1)

# Extract coefficients
ozone_mod1$coefficients
ozone_mod1[[1]]

# Extract the second coefficient
ozone_mod1$coefficients[2]
ozone_mod1$coefficients[[2]]

# Create a vector of coefficients
coefs_ozone <- as.vector(ozone_mod1$coefficients)
coefs_ozone

# Extract residuals
resid_ozone <- summary(ozone_mod1)$residuals
head(resid_ozone)

# All in one
coef(ozone_mod1)
sigma(ozone_mod1)
residuals(ozone_mod1)
fitted(ozone_mod1)
################################################################################
# Fitting Models
## The Principle of Parsimony
summary(ozone_mod1)

cor(ozone_pollution)
# Since wind and temperature are highly correlated, lets remove temp
summary(lm(ozone ~ rad + wind, data = ozone_pollution))

## Nested Models
ozone_mod2 <- lm(ozone ~ wind, data = ozone_pollution)
anova(ozone_mod1, ozone_mod2)
# We cannot simplify this much

# Omit all variables
ozone_mod3 <- lm(ozone ~ 1, data = ozone_pollution)
anova(ozone_mod1, ozone_mod3)

ozone_mod4 <- lm(ozone ~ wind + temp, data = ozone_pollution)
summary(ozone_mod4)

anova(ozone_mod1, ozone_mod4)

ozone_mod5 <- lm(ozone ~ rad + wind, data = ozone_pollution)
anova(ozone_mod1, ozone_mod5)

# Non-Nested Models with AIC
ozone_mod5 <- lm(ozone ~ temp + wind, data = ozone_pollution)
ozone_mod6 <- lm(ozone ~ rad + wind, data = ozone_pollution)
ozone_mod7 <- lm(ozone ~ rad + temp, data = ozone_pollution)
AIC(ozone_mod5, ozone_mod6)

AIC(ozone_mod5, ozone_mod6, ozone_mod7)

# Automatic covariate selection
model_all <- lm(ozone ~ ., data = ozone_pollution)
# Forward Propagation
model_auto <- step(model_all, direction = "forward")
# Backward Elimination
model_auto <- step(model_all, direction = "backward")
# Both
model_auto <- step(model_all, direction = "both")
################################################################################
# Model Assumptions
# We have four main assumptions:
## 1 - Linearity: the response is a linear combination of the parameters and covariates
## 2 - Homoscedasticity: the variance of error term is sigma^2
## 3 - Normality: the error components 𝜖1 , … , 𝜖n are Normally distributed
## 4 - Independence of error components: the error components are mutually independent

## Linearity: Lets get standardized residuals
summary(ozone_mod1)
# Raw Residuals
ozone_res <- predict(ozone_mod1) - ozone_pollution$ozone
# Standardized Residuals: Pearson Residuals
ozone_stdres <- rstandard(ozone_mod1)
# Put them together
head(cbind(ozone_res, ozone_stdres))

# Quick way to check it
plot(ozone_mod1)

# Lets plot our customized data
attach(ozone_pollution)
plot(rad, ozone_stdres, pch = 20, col = hue_pal ()(3)[1],
     ylab = "Standardised residuals", xlab ="Solar radiation", 
     ylim = c (-2, 4.5))
abline(a = 0, b = 0, lty = 3)
# Solar radiation is cool
plot(temp, ozone_stdres, pch = 20, col = hue_pal ()(3)[1],
     ylab = "Standardised residuals", xlab = "Air temperature", 
     ylim = c (-2, 4.5))
abline(a = 0, b = 0, lty = 3)

plot(wind, ozone_stdres, pch=20, col = hue_pal ()(3)[1],
     ylab = "Standardised residuals", xlab = "Wind speed", 
     ylim = c (-2, 4.5))
abline(a = 0, b = 0, lty = 3)
# The other two are not that linear: see pairplot

## Homoscedasticity
plot(fitted(ozone_mod1), ozone_stdres, pch=16, col=hue_pal()(3)[1],
     xlab = "Fitted Values", ylab = "Standardized Residuals")
abline(a = 0, b = 0, lty = 3)

## Normality of Errors
qqnorm(ozone_stdres, col = hue_pal ()(3)[1], pch=16, main = "",
       ylab = "Standardized Residuals", xlab = "Quantiles of N(0,1)")
qqline(ozone_stdres)
detach(ozone_pollution)

## Independence of Errors: More complicated

# We can see an example of Serial Correlation and testing it
library(car)
cost_profit <- read.table("Datasets/cost_profit.txt", header = T)
head(cost_profit)
plot(cost_profit, pch=16)

model_cost1 <- lm(profit ~ cost, data = cost_profit)
summary(model_cost1)

# Check for serial correlation
durbinWatsonTest(model_cost1)

# Influential Observations
influence.measures(ozone_mod1)
influence.measures(ozone_mod1)$is.inf

# Multi-colinearity
## Use Variance Inflating Factor (VIF) from "car" package
summary(ozone_mod1)
vif(ozone_mod1)
# Min 1 (Perfect linear independence)
# If above 5, take a closer look

# Lets transform the covariates to improve the model
attach(ozone_pollution)
# Square the temperature
temp_sq <- temp^2
ozone_mod8 <- lm(ozone ~ wind + rad + temp_sq)
summary(ozone_mod8)

ozone_res <- predict(ozone_mod8) - ozone_pollution$ozone
# Standardized Residuals: Pearson Residuals
ozone_stdres <- rstandard(ozone_mod8)

plot(rad, ozone_stdres, pch = 20, col = hue_pal ()(3)[1],
     ylab = "Standardised residuals", xlab ="Solar radiation", 
     ylim = c (-2, 4.5))
abline(a = 0, b = 0, lty = 3)
# Solar radiation is cool
plot(temp_sq, ozone_stdres, pch = 20, col = hue_pal ()(3)[1],
     ylab = "Standardised residuals", xlab = "Air temperature", 
     ylim = c (-2, 4.5))
abline(a = 0, b = 0, lty = 3)

plot(wind, ozone_stdres, pch=20, col = hue_pal ()(3)[1],
     ylab = "Standardised residuals", xlab = "Wind speed", 
     ylim = c (-2, 4.5))
abline(a = 0, b = 0, lty = 3)

library(MASS)
bc <- boxcox(ozone ~ rad + temp + wind)
which.max(bc$y)
bc$x[56]
# We can take y^(1/4) or y^0 (log transformation)
ozone_mod9 <- lm(log(ozone) ~ wind + rad + temp)
summary(ozone_mod9)

# Standardized Residuals: Pearson Residuals
ozone_stdres <- rstandard(ozone_mod9)

plot(rad, ozone_stdres, pch = 20, col = hue_pal ()(3)[1],
     ylab = "Standardised residuals", xlab ="Solar radiation", 
     ylim = c (-2, 4.5))
abline(a = 0, b = 0, lty = 3)
# Solar radiation is cool
plot(temp, ozone_stdres, pch = 20, col = hue_pal ()(3)[1],
     ylab = "Standardised residuals", xlab = "Air temperature", 
     ylim = c (-2, 4.5))
abline(a = 0, b = 0, lty = 3)

plot(wind, ozone_stdres, pch=20, col = hue_pal ()(3)[1],
     ylab = "Standardised residuals", xlab = "Wind speed", 
     ylim = c (-2, 4.5))
abline(a = 0, b = 0, lty = 3)

# Finally, with power 1/4
ozone_mod10 <- lm(ozone^(0.22) ~ wind + rad + temp)
summary(ozone_mod10)

# Standardized Residuals: Pearson Residuals
ozone_stdres <- rstandard(ozone_mod10)

plot(rad, ozone_stdres, pch = 20, col = hue_pal ()(3)[1],
     ylab = "Standardised residuals", xlab ="Solar radiation", 
     ylim = c (-2, 4.5))
abline(a = 0, b = 0, lty = 3)
# Solar radiation is cool
plot(temp, ozone_stdres, pch = 20, col = hue_pal ()(3)[1],
     ylab = "Standardised residuals", xlab = "Air temperature", 
     ylim = c (-2, 4.5))
abline(a = 0, b = 0, lty = 3)

plot(wind, ozone_stdres, pch=20, col = hue_pal ()(3)[1],
     ylab = "Standardised residuals", xlab = "Wind speed", 
     ylim = c (-2, 4.5))
abline(a = 0, b = 0, lty = 3)

# Weighted Least Squares
ipomopsis <- read.table("Datasets/ipomopsis.txt", header = T)
head(ipomopsis)
table(ipomopsis$Grazing)

ipomopsis_mod1 <- lm(Fruit ~ Grazing + Root, data = ipomopsis)
summary(ipomopsis_mod1)

plot(ipomopsis_mod1)

# Custom
ipo_stdres <- rstandard(ipomopsis_mod1)

pdf(paste0(plot_dir, "Ipomopsis_Residuals.pdf"), width = 6, height = 4)
plot(ipomopsis$Root, ipo_stdres, pch = 20, col = hue_pal ()(3)[1],
     ylab = "Standardised residuals", xlab ="Solar radiation", 
     ylim = c (-2, 4))
abline(a = 0, b = 0, lty = 3)

# Same for grazing, but this time it is categorical
stripchart(ipo_stdres ~ Grazing, data = ipomopsis, method = "jitter",
           pch = 20, vertical = TRUE)
abline(h = 0, lty = 3)

# Or do it in a different way too
boxplot(rstandard(ipomopsis_mod1) ~ Grazing, data = ipomopsis, 
        ylab = "Standardised residuals")
dev.off()

# Weighted model
ipomopsis_mod2 <- lm(Fruit ~ Grazing, data = ipomopsis, weights = Root)
summary(ipomopsis_mod2)
# Complete collapse

grazing_cols <- as.numeric(as.factor(ipomopsis$Grazing))
pdf(paste0(plot_dir, "Grazing_Plot.pdf"), width = 6, height = 4)
plot(ipomopsis$Root, ipomopsis$Fruit, col = as.factor(ipomopsis$Grazing),
     pch=16)
legend("topleft",
       legend = levels(as.factor(ipomopsis$Grazing)),
       col = seq_along(levels(as.factor(ipomopsis$Grazing))),
       pch = 16,
       bty = "n")
dev.off()

# Prediction at population and individual level
data.frame(rad = c(110, 110), temp = c(60, 80),
           wind = c(15.3, 9.5))
predict(ozone_mod1, data.frame(rad = c(110, 110), temp = c(60, 80),
                                 wind = c(15.3, 9.5)))

# Confidence interval for population: mean
## Narrower CI
predict(ozone_mod1, data.frame(rad = 110, temp = 80, wind = 9.5),
        interval = "confidence", level = 0.95)

# Interval = prediction for an individual data point
## Wider CI
predict(ozone_mod1, data.frame (rad = 110, temp = 80, wind = 9.5),
        interval = "prediction", level = 0.95)
