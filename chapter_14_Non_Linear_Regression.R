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
