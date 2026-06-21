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
