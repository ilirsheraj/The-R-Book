# Generalized Linear Models (GLMs)
library(scales)
plot_dir <- "Plots/"

ozonepoppution <- read.table("Datasets/ozone_pollution.txt", header=TRUE)
head(ozonepoppution)

lm_ozone <- glm(ozone ~ rad + temp + wind, family = gaussian(),
                data = ozonepoppution)
lm_ozone
# This is exactly the same as lm(), which is a member of GLM family of models
summary(lm_ozone)

# Lets check the distribution of residuals
stdres_lm <- rstandard(lm_ozone)
plot(lm_ozone$fitted.values, stdres_lm, col = hue_pal ()(2)[1], pch = 20,
     ylab = "standardised residuals", xlab = "fitted values")
# We have some outliers, especially as the values increase

# Modify the model
glm_ozone_g <- glm(ozone ~ rad + temp + wind, family = Gamma(),
                data = ozonepoppution)
summary(glm_ozone_g)
# Check residuals
stdres_glm <- rstandard(glm_ozone_g)
plot(glm_ozone_g$fitted.values, stdres_glm, col = hue_pal ()(2)[2], pch = 20,
     ylab = "standardised residuals", xlab = "fitted values")
abline(a = 0, b = 0, lty = 3)

# Fit log-link model to count for heteroscedasticity
glm_ozone_gl <- glm(ozone ~ rad + temp + wind, family = Gamma(link = "log"),
                   data = ozonepoppution)
summary(glm_ozone_gl)

stdres_glm <- rstandard(glm_ozone_gl)
plot(glm_ozone_gl$fitted.values, stdres_glm, col = hue_pal ()(2)[2], pch = 20,
     ylab = "standardised residuals", xlab = "fitted values")
abline(a = 0, b = 0, lty = 3)

# Finally, use power y^(1/4) similar to previous chapter
glm_ozone_p <- glm(ozone ~ rad + temp + wind, family = Gamma(link = power(0.25)),
                    data = ozonepoppution)
summary(glm_ozone_p)

stdres_glm <- rstandard(glm_ozone_p)
plot(glm_ozone_p$fitted.values, stdres_glm, col = hue_pal ()(2)[2], pch = 20,
     ylab = "standardised residuals", xlab = "fitted values")
abline(a = 0, b = 0, lty = 3)

# Predict: we will use log-link since it had the smallest AIC
covs <- data.frame(rad = c(250, 200), temp = c(64, 62), wind = c(10.3, 15.7))
# CI = 99
z <- qnorm(0.995)

# Use "response" to handle exponential transformation
preds <- predict(glm_ozone_gl, newdata = covs, type = "response", se.fit = TRUE)
preds
point_est <- preds$fit
point_est

# To get the lower and upper CIs: CI = x +/- z * se
ci_lower <- preds$fit - z * preds$se.fit
ci_upper <- preds$fit + z * preds$se.fit
c(ci_lower[1], ci_upper[1])
c(ci_lower[2], ci_upper[2])
###############################################################################
# Count Data and GLM
species <- read.table("Datasets/species.txt", header = T, colClasses = list(pH = "factor"))
head(species)
sapply(species, class)
table(species$pH)

cols <- data.frame(pH = levels(species$pH), col=hue_pal()(3))
pdf(paste0(plot_dir, "Species_Biomass.pdf"), width = 6, height = 4)
plot(species[,c(2,3)], col= cols[match(species$pH, cols$pH), 2], pch=19)
legend(8, 42, legend = c("high", "mid", "low"), pch=rep(19, 3), 
       col=hue_pal()(3), title = "pH Level")
dev.off()

# By default, link = "log"
species_mod1 <- glm(Species ~ Biomass + pH, family = poisson(), data = species)
summary(species_mod1)

# Let's introduce some interaction term
species_mod2 <- glm(Species ~ Biomass * pH, family = poisson(), data = species)
summary(species_mod2)

# Compare the two models
anova(species_mod1, species_mod2)
# Interaction between pH and Biomass has important effect on the number of species

# Lets plot the predictions
pdf(paste0(plot_dir, "Species_Biomass_Interaction_Model.pdf"), width = 6, height = 4)
plot(species[,2:3], col = cols[match(species$pH, cols$pH),2], pch=19)
legend(8, 42, legend = c("high", "mid", "low"), pch = rep(19, 3),
        col = hue_pal()(3), title = "pH level")
x <- seq(0, 10, 0.1)
for (levs in levels(species$pH)) {
  lines(x, exp(predict(species_mod2,list(Biomass = x, pH = rep(levs,length(x))))),
        col = cols[match(levs, cols$pH),2])
  }
dev.off()

# Infected blood cells per square millimeter
cellcounts <- read.table("Datasets/cells.txt", header = TRUE,
                         colClasses = c ("numeric", rep ("factor", 4)))
sapply(cellcounts, class)

# Check the value sof dead cells
table(cellcounts$cells)
hist(cellcounts$cells)

# Check the number of living and dead cells according to different variable
tapply(cellcounts$cells, cellcounts$smoker, mean)

tapply(cellcounts$cells, cellcounts$age, mean)

tapply(cellcounts$cells, cellcounts$sex, mean)

tapply(cellcounts$cells, cellcounts$weight, mean)

# Smoking and body mass seem to be interacting with each other
tapply(cellcounts$cells, list(cellcounts$smoker, cellcounts$weight), mean)

# Plot it
pdf(paste0(plot_dir, "CellDeath_Smoke_Weight.pdf"), width = 6, height = 4)
barplot(tapply(cellcounts$cells, list(cellcounts$smoker, cellcounts$weight), mean),
        col = hue_pal ()(2), beside = T, ylab = "damaged cells", xlab = "body mass")
legend (1.2,3.4, c("non-smoker", "smoker"), fill = hue_pal ()(2))
dev.off()

# Start with a simple model
cells_mod1 <- glm(cells ~ smoker + sex + age + weight, poisson(), data = cellcounts)
summary(cells_mod1)
# Residual deviance ~ 2 (Overdispersion)

# Try Quasi-Poisson
cells_mod2 <- glm(cells ~ smoker + sex + age + weight, quasipoisson(), data = cellcounts)
summary(cells_mod2)
# Second model is better because it deals with overdispersion

# Try quasi-poisson with interaction terms
cells_mod3 <- glm(cells ~ smoker * sex * age * weight, quasipoisson, data = cellcounts)
summary(cells_mod3)

# Let's use ANOVA to see the effect of adding terms
anova(cells_mod3, test = "F")

# Since there seems to be strong interaction between smoking and weight, we include it
cells_mod4 <- glm(cells ~ smoker * weight, quasipoisson(), data = cellcounts)
summary(cells_mod4)

anova(cells_mod4, test = "F")

# A more complicated dataset
library(MASS)
data("quine")
head(quine)
sapply(quine, class)

# Sex: male (M) and female (F), 
# Eth: Aboriginal (A) and not (N)
# Age: F0 (primary), F1, F2, and F3
# Lrn: learner status average (AL) and slow (SL)

table(quine$Eth, quine$Sex, quine$Age, quine$Lrn)
# Flatten the table
ftable(table(quine$Eth, quine$Sex, quine$Age, quine$Lrn))

# Start with whole model
quine_mod1 <- glm(Days ~ Eth + Age + Sex + Lrn, data = quine, family = poisson)
summary(quine_mod1)
# Residual deviance ~ 15 (Huge)

quine_mod2 <- glm(Days ~ Eth + Age + Sex + Lrn, data = quine, family = quasipoisson())
summary(quine_mod2)

quine_mod3 <- glm(Days ~ Eth * Age * Sex * Lrn, data = quine, family = quasipoisson())
summary(quine_mod3)
plot(quine_mod3, which = 1, col = hue_pal()(1), pch=16)

# Look for interactions
anova(quine_mod3, test = "F")

quine_mod4 <- glm(Days ~ Eth * Age + Sex * Age + Eth * Sex * Lrn, data = quine, 
                  family = quasipoisson())
summary(quine_mod4)
plot(quine_mod4, which = 1, col = hue_pal()(1), pch=16)

# Let's use Negative Binomial
quine_mod5 <- glm.nb(Days ~ Eth + Sex + Age + Lrn, data = quine)
summary(quine_mod5)
plot(quine_mod5, which = 1, col = hue_pal()(1), pch=16)

# Modified
quine_mod6 <- glm.nb(Days ~ Eth * Age + Sex * Age + Eth * Sex * Lrn, data = quine)
summary(quine_mod6)
# See the improvement over model 5
plot(quine_mod6, which = 1, col = hue_pal()(1), pch=16)
################################################################################
# Count Table Data and GLMs











