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
pdf(paste0(plot_dir, "MosaicPlot.pdf"), width = 9, height = 8)
mosaicplot(HairEyeColor, shade = TRUE, type = "deviance")
dev.off()

# Suppose we are carrying out a study of induced defences in trees
induced <- read.table("Datasets/induced.txt", header = TRUE,
                      colClasses = c(rep("factor", 3), "numeric"))
head(induced)
sapply(induced, class)

# Begin with a saturated model
induced_mod1 <- glm(Count ~ Tree * Aphid * Caterpillar, family = poisson,
                    data = induced)
summary(induced_mod1)

# Remove the three-way interaction and compare
induced_mod2 <- update(induced_mod1, ~ . - Tree:Aphid:Caterpillar)
summary(induced_mod2)

# Compare the two nested models
anova(induced_mod1, induced_mod2, test= "Chi")

# Remove Aphid:Caterpillar interaction
induced_mod3 <- update(induced_mod2, ~ . - Aphid:Caterpillar)
summary(induced_mod3)

# Again compare the last two models
anova(induced_mod2, induced_mod3, test= "Chi")
# No interaction between Aphid and caterpillar!

# Wrong start screws up everything: exclude trees
induced_mod1a <- glm(Count ~ Aphid * Caterpillar, family = poisson, 
                     data = induced)
summary(induced_mod1a)

# Then remove the interaction
induced_mod2a <- update(induced_mod1a, ~ . - Aphid:Caterpillar)
summary(induced_mod2a)

anova(induced_mod1a, induced_mod2a, test = "Chi")

# Why the difference: Tree effect
as.table(by(induced$Count, INDICES = list(induced$Tree, induced$Caterpillar),
            FUN = sum))

# Another more complicated case
lizards <- read.table("Datasets/lizards.txt",header = TRUE,
                      colClasses = c ("numeric", rep ("factor", 5)))
head(lizards)
# But them all in a summary table
tapply(lizards$n, list(lizards$species, lizards$sun, lizards$height,
                       lizards$perch, lizards$time), sum)

# Flatten the table
ftable(tapply(lizards$n, list(lizards$species, lizards$sun, lizards$height,
                              lizards$perch, lizards$time), sum))
# Start with saturated model
lizards_mod1 <- glm(n ~ sun * height * perch * time * species, poisson,
                    data = lizards)
summary(lizards_mod1)
# par(mfrow=c(2,2))
# plot(lizards_mod1)
par(mfrow=c(1,1))
lizards_mod1$aic

# Use the simples model without interactions
lizards_mod2 <- glm(n ~ sun + height + perch + time + species, poisson,
                    data = lizards)
summary(lizards_mod2)

lizards_mod2$aic
# Missing a lot

# Introduce interactions of all variables with species
lizards_mod3 <- glm(n ~ (sun + height + perch + time) * species, poisson,
                    data = lizards)
summary(lizards_mod3)

# AIC in between the simpest and the saturated
lizards_mod3$aic

# Since it is nested we can now compare to model 1
anova(lizards_mod3, lizards_mod1, test = "Chi")

# Start removing interactions of 4 or more variables
lizards_mod4 <- update(lizards_mod1, ~ . - sun:height:perch:time:species -
                         height:perch:time:species -
                         sun:perch:time:species -
                         sun:height:time:species -
                         sun:height:perch:species -
                         sun:height:perch:time)
summary(lizards_mod4)
lizards_mod4$aic

# Better than 1, but still problematic
plot(lizards_mod4)

anova (lizards_mod4, lizards_mod1, test = "Chi")

# Add interactions only with species
lizards_mod5 <- glm(n ~ (sun * height + perch * time + sun * perch +
                        sun * time + height * perch + height * time) * species,
                    poisson, data = lizards)

summary(lizards_mod5)
# More improvement
lizards_mod5$aic
anova(lizards_mod5, lizards_mod4, test = "Chi")

# Further reduction with stepwise between model 3 and model 5
# To avoid printing the output use trace=0
lizards_mod6 <- step(lizards_mod5, lower = lizards_mod3, upper = lizards_mod5,
                     trace = 0)

summary(lizards_mod6)
lizards_mod6$aic

anova(lizards_mod6, lizards_mod5, test = "Chi")

# Quite an improvement between 3 and 6
anova(lizards_mod6, lizards_mod3, test = "Chi")

# Plot the residuals for 6
pdf(paste0(plot_dir, "Lizards_final_Model.pdf"), width = 9, height = 8)
par(mfrow=c(2,2))
plot(lizards_mod6)
dev.off()

# Combine morning and afternoon
levels(lizards$time)
levels(lizards$time)[c(1, 3)] <- "Not.mid.day"

lizards_mod5a <- glm(n ~ (sun * height + perch * time + sun * perch +
                      sun * time + height * perch + height * time) * species,
                     poisson, data = lizards)
summary(lizards_mod5a)

lizards_mod6a <- step(lizards_mod5, lower = lizards_mod3, upper = lizards_mod5a,
                      trace = 0)
summary(lizards_mod6a)
# Overdispersion! Residual deviance:  51.699  on 27

# One more case with few datapoints
spino <- read.table("Datasets/spino.txt", header = TRUE, 
                    colClasses = rep("factor", 2))
head(spino)

spino$condition <- factor(spino$condition, 
                          c("much.worse", "worse", "no.change","better", "much.better"))
spino$treatment <- factor(spino$treatment, c("placebo", "drug.A", "drug.B"))

# Similar to stacked barplot
spineplot(condition ~ treatment, data = spino, col = hue_pal()(5))

# Check the number of cases
table(spino$condition, spino$treatment)

# Convert it into a table
spino_df <- as.data.frame.table(table(spino$condition, spino$treatment))
head(spino_df)
# Rename them
colnames(spino_df) <- c("condition", "treatment", "count")
spino_df

# Now we can model it: Very low dataset for interactions
## First with saturated model
spino_mod1 <- glm(count ~ condition * treatment, poisson, data = spino_df)
summary(spino_mod1)
# plot(spino_mod1)

# Simple Model
spino_mod2 <- glm(count ~ condition + treatment, poisson, data = spino_df)
summary(spino_mod2)
plot(spino_mod2)

anova(spino_mod1, spino_mod2, test = "Chi")
################################################################################

