# Non-Linear Regression
library(scales)
library(nlstools) # for nonlinear models

plot_dir <- "Plots/"

jaws <- read.table("Datasets/jaws.txt", header = TRUE)
head(jaws)

pdf(paste0(plot_dir, "Deer_Jaws_Dataset.pdf"), width = 6, height = 4)
plot(jaws$age, jaws$bone,
     xlab = "Age of Deer",
     ylab = "Jaw Bone Length",
     col = hue_pal()(3)[1],
     pch = 16)
dev.off()

# y(x) = a - bexp(-cx)
# x = 0 => y = a - b
# x = Inf => y = a
# Provide it with initial conditions
preview(bone ~ a - b * exp(-c * age), data = jaws,
        list (a = 120, b = 110, c = 0.064))
# This works nicely
jaws_mod1 <- nls(bone ~ a - b * exp (-c * age), data = jaws,
                 start = list (a = 120, b = 110, c = 0.064))
summary(jaws_mod1)

# Try simpler, 2-parameter model
jaws_mod2 <- nls(bone ~ a * (1 - exp(-c * age)), data = jaws,
                 start = list (a = 120, c = 0.064))
summary(jaws_mod2)

# Is there any significant difference between them
anova(jaws_mod1, jaws_mod2)

# Plot the thing
pdf(paste0(plot_dir, "Deer_Jaws_two_Param_Model.pdf"), width = 6, height = 4)
plotfit(jaws_mod2, smooth = TRUE, ylab = "Jaw bone length", 
        xlab = "Age of deer",
        col.obs = hue_pal()(3)[1], 
        col.fit = hue_pal()(3)[2], 
        pch.obs = 16, lwd=2)
dev.off()

# overview more detailed than summary
overview(jaws_mod2)
# Final model is: Yi = 115.58(1-exp(-0.12xi)) + ei

# Michaelis-Menten Model: I changed the parameters here, more accurate
preview(bone ~ a * age / (1 + b * age), data = jaws, list(a = 9, b = 0.05))

# Fit the model
jaws_mod3 <- nls(bone ~ a * age / (1 + b * age), data = jaws, list(a = 9, b = 0.05))
summary(jaws_mod3)

jaws_mod3_resids <- nlsResiduals(jaws_mod3)

# These plots are boring, cant modify them
plot(jaws_mod3_resids, which = 2)
plot(jaws_mod3_resids, which = 6)

# Make it Manual
plot(jaws_mod3_resids$resi2[, 1],
     jaws_mod3_resids$resi2[, 2],
     pch = 16,
     xlab = "Fitted values",
     ylab = "Standardized residuals")
abline(h = 0, lty = 2)

# Normal Q-Q plot
qqnorm(jaws_mod3_resids$resi2[, 2],
       pch = 16,
       main = "Normal Q-Q Plot of Standardized Residuals")
qqline(jaws_mod3_resids$resi2[, 2])

# Book's Plots
jaws_mod3_resids <- nlsResiduals (jaws_mod3)
plot(nlsResiduals(jaws_mod3)[[2]][,1], 
     nlsResiduals (jaws_mod3)[[2]][,2],
     col = hue_pal()(3)[1], pch = 16,
     ylab = "Standardized residuals", 
     xlab = "Fitted values")
abline(a=0, b=0, lty=3)

qqnorm(nlsResiduals(jaws_mod3)[[2]][,2], 
       col = hue_pal()(3)[1], 
       main="", pch = 16,
       ylab = "Standardized Residuals", 
       xlab = "Quantiles of N(0,1)")
qqline(nlsResiduals (jaws_mod3)[[2]][,2])

pdf(paste0(plot_dir, "Deer_Jaws_Michaelis_Menten_Model.pdf"), width = 6, height = 4)
plotfit(jaws_mod3, smooth = TRUE, ylab = "Jaw bone length", 
        xlab = "Age of deer",
        col.obs = hue_pal()(3)[1], 
        col.fit = hue_pal()(3)[2], 
        pch.obs = 16, lwd=2)
dev.off()

# Asymptotic exponential model reaches asymptote quickly, MM still increases
#############################################################################
# Grouped Data: Enzyme kinetics of different bacterial strains
reaction <- read.table("Datasets/reaction.txt", header=TRUE)
head(reaction)

table(reaction$strain)

# Plot the data
pdf(paste0(plot_dir, "Bacterial_Strains_Enzymes.pdf"), width = 6, height = 4)
plot(reaction$enzyme, 
     reaction$rate, 
     pch=16, 
     col = hue_pal()(5)[as.factor(reaction$strain)],
     xlab = "Enzyme Concentration",
     ylab = "Reaction Rate")
dev.off()


library(nlme)
reaction <- groupedData(rate ~enzyme | strain, data = reaction)

# Keep the order
reaction$strain <- factor(reaction$strain, levels = c(LETTERS[1:5]))

pdf(paste0(plot_dir, "Bacterial_Strains_Grouped.pdf"), width = 6, height = 4)
plot(reaction, pch=16, col = hue_pal()(1)[1])
dev.off()

# c rate without enzyme (y-intercept)
par(mfrow = c(2, 3))
for (s in levels(reaction$strain)) {
  d <- subset(reaction, strain == s)
  preview(
    rate ~ c + a * enzyme / (1 + b * enzyme),
    data = d,
    start = list(a = 20, b = 0.1, c = 10),
    variable = which(names(d) == "enzyme")
  )
}
par(mfrow = c(1, 1))

# To fit a model for each strain, we use nlsList
react_mod1 <- nlsList(rate ~ c + a * enzyme / (1 + b * enzyme) | strain,
                      data = reaction, start = c (a = 20, b = 0.25, c = 10))
summary(react_mod1)

# Define colors 
strain_cols <- rainbow(length(levels(reaction$strain)))
names(strain_cols) <- levels(reaction$strain)

pdf(paste0(plot_dir, "Bacterial_Strains_nls_Fitted.pdf"), width = 6, height = 5)
plot(reaction$enzyme, reaction$rate,
     col = strain_cols[reaction$strain],
     pch = 16,
     xlab = "Enzyme Concentration",
     ylab = "Reaction Rate")

# Add the fitted lines to each strain
for (s in levels(reaction$strain)) {
  d <- reaction[reaction$strain == s, ]
  x <- seq(min(d$enzyme),
           max(d$enzyme),
           length.out = 100)
  
  # Convert row to a numeric vector
  cf <- unlist(coef(react_mod1)[s, ])
  
  # Fitted nonlinear curve
  y <- cf["c"] + cf["a"] * x / (1 + cf["b"] * x)
  
  lines(x, y,
        col = strain_cols[s],
        lwd = 2)
}

legend("topleft",
       legend = levels(reaction$strain),
       col = strain_cols,
       pch = 16,
       lwd = 2,
       bty = "n")

dev.off()

# Include Random Effects
react_mod2 <- nlme(rate ~ c + a * enzyme / (1 + b * enzyme), 
                   fixed = a + b + c ~ 1, random = a ~ 1 | strain, 
                   data = reaction,
                   start = c (a = 20, b = 0.25, c = 10))
summary(react_mod2)
coef(react_mod2)

# Scatterplot
pdf(paste0(plot_dir, "Bacterial_Strains_nls_random.pdf"), width = 6, height = 5)
plot(reaction$enzyme, reaction$rate,
     col = strain_cols[reaction$strain],
     pch = 16,
     xlab = "Enzyme Concentration",
     ylab = "Reaction Rate")

# Fitted curve for each strain
for (s in levels(reaction$strain)) {
  d <- reaction[reaction$strain == s, ]
  x <- seq(min(d$enzyme),
           max(d$enzyme),
           length.out = 100)
  
  newdat <- data.frame(
    enzyme = x,
    strain = factor(s, levels = levels(reaction$strain))
  )
  
  # Includes strain-specific random effect
  y <- predict(react_mod2,
               newdata = newdat,
               level = 1)
  
  lines(x, y,
        col = strain_cols[s],
        lwd = 2)
}

legend("topleft",
       legend = levels(reaction$strain),
       col = strain_cols,
       pch = 16,
       lwd = 2,
       bty = "n")
dev.off()

# Remove strain effect
pdf(paste0(plot_dir, "Bacterial_Strains_nls_random_nostrain.pdf"), width = 6, height = 5)
plot(reaction$enzyme, reaction$rate,
     col = strain_cols[reaction$strain],
     pch = 16,
     xlab = "Enzyme Concentration",
     ylab = "Reaction Rate")

# Fitted curve for each strain
for (s in levels(reaction$strain)) {
  
  d <- reaction[reaction$strain == s, ]
  
  x <- seq(min(reaction$enzyme),
           max(reaction$enzyme),
           length.out = 100)
  
  newdat <- data.frame(
    enzyme = x,
    strain = factor(levels(reaction$strain)[1],
                    levels = levels(reaction$strain))
  )
  
  y_pop <- predict(react_mod2,
                   newdata = newdat,
                   level = 0)
  
  lines(x, y_pop,
        lwd = 3,
        lty = 2)
}

legend("topleft",
       legend = levels(reaction$strain),
       col = strain_cols,
       pch = 16,
       lwd = 2,
       bty = "n")
dev.off()

################################################################################
# Self-Starting functions: determine starting values automatically
# SSmicmen() for Michaelis-Menten
mm <- read.table("Datasets/mm.txt", header=TRUE)
head(mm)

plot(mm$rate ~ mm$conc, col = hue_pal()(1)[1],
     xlab = "Concentration", 
     ylab = "Reaction rate",
     pch=16)

mm_mod1 <- nls(rate ~ SSmicmen(conc, a, b), data = mm)
summary(mm_mod1)

# Equation: y = 212.7x/(0.064 + x) for y = ax/(b+x)
plotfit(mm_mod1, smooth = TRUE, 
        ylab = "Reaction rate" , 
        xlab = "Concentration",
        col.obs = hue_pal()(3)[1], 
        col.fit = hue_pal()(3)[2], 
        pch.obs = 16,
        lwd = 2)

# Self-starting asymptotic exponential model: SSasymp()
# y = a - be^(-cx)
# Back to jaws data
head(jaws)
jaws_ss_mod1 <- nls(bone ~ SSasymp(age, a, b, c), data = jaws)
summary(jaws_ss_mod1)

# Visualize
plotfit(jaws_ss_mod1, smooth = TRUE, 
        xlab = "Age of Deer",
        ylab = "Jaw Bone Length",
        col.obs = hue_pal()(3)[1], 
        col.fit = hue_pal()(3)[2], 
        pch.obs = 16,
        lwd = 2)

# Two-parameters passing through the origin: SSasympOrig()
# y = a(1-e^(-bx))
jaws_ss_mod2 <- nls(bone ~ SSasympOrig(age, a, b), data = jaws)
summary(jaws_ss_mod2)

# Visualize again
plotfit(jaws_ss_mod2, smooth = TRUE, 
        xlab = "Age of Deer",
        ylab = "Jaw Bone Length",
        col.obs = hue_pal()(3)[1], 
        col.fit = hue_pal()(3)[2], 
        pch.obs = 16,
        lwd = 2)

# Self-Starting Logistic: 3-parameter growth models (SSlogis())
# y = a / (1 + be^(-cx))
sslogistic <- read.table("Datasets/sslogistic.txt", header = TRUE)
head(sslogistic)

plot(sslogistic$concentration,
     sslogistic$density,
     col = hue_pal()(1)[1],
     xlab = "Concentration", 
     ylab = "Density",
     pch=16)

sslogis_mod1 <- nls(density ~ SSlogis(log(concentration), a, b, c),
                    data = sslogistic)
summary(sslogis_mod1)

# Plot the fitted line
plotfit(sslogis_mod1, smooth = TRUE, 
        xlab = "Concentration", 
        ylab = "Density",
        col.obs = hue_pal()(3)[1], 
        col.fit = hue_pal()(3)[2], 
        pch.obs = 16,
        lwd = 2)

# Self-Starting 4-parameter logistic: SSfpl()
# y = a + [(b-a) / (1 + e^((d-x)/c))]
chicks <- read.table("Datasets/chicks.txt", header = TRUE)
head(chicks)

plot(chicks$Time, chicks$weight,
     xlab = "Time",
     ylab = "Weight",
     pch=16,
     col=hue_pal()(2)[1])

chicks_mod1 <- nls(weight ~ SSfpl(Time, a, b, c, d), data = chicks)
summary(chicks_mod1)

# Plot the fitted line
plotfit(chicks_mod1, smooth = TRUE, 
        xlab = "Time", 
        ylab = "Weight",
        col.obs = hue_pal()(3)[1], 
        col.fit = hue_pal()(3)[2], 
        pch.obs = 16,
        lwd = 2)
################################################################################
# Further Considerations: Evaluation, CIs and Prediction, among others
## We will use jaws_mpdel_2
jaws_mod2 <- nls(bone ~ a * (1 - exp(-c * age)), data = jaws,
                 start = list (a = 120, c = 0.064))
summary(jaws_mod2)

jaws_mod2_resids <- nlsResiduals(jaws_mod2)
# I did this plots in a more customized way above
plot(jaws_mod2_resids, which = 2)
plot(jaws_mod2_resids, which = 6)

# Highly influential observations in the data?
jaws_mod2_jack <- nlsJack(jaws_mod2)
summary(jaws_mod2_jack)

# Let's get Confidence Intervals: with bootstrap
jaws_mod2_boot <- nlsBoot(jaws_mod2)
summary(jaws_mod2_boot)

# EOF