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
# Proportion Data and GLM
x <- seq(-60, 60, 0.1)
a <- 1
b <- 0.1
p <- exp(a + b * x) / (1 + exp (a + b * x))
par(mfrow=c(1,2))
plot(x, p, xlab = "x", ylab = "p", type = "l", col = hue_pal ()(2)[1],
     main = "Linear")
plot(x, log (p / (1 - p)), xlab = "x", ylab = "logit = log (p / q)",
     type = "l", col = hue_pal ()(2)[2],
     main = "Logit")
par(mfrow=c(1,1))

# Logistic Regression with Binomial Errors
sexratio <- read.table("Datasets/sexratio.txt", header = TRUE)
sexratio

# Get the probability of males (p)
pdf(paste0(plot_dir, "Insect_Sex.pdf"), width = 8, height = 4)
par(mfrow=c(1,2))
p <- sexratio$males / (sexratio$males + sexratio$females)
plot(sexratio$density, p, ylab = "proportion male", xlab = "density",
     pch = 19, col = hue_pal()(2)[1])
plot(log(sexratio$density), p, ylab = "proportion male",
     xlab = "log (denisty)", pch = 16, col = hue_pal()(2)[2])
dev.off()

# Fit in a model
y <- cbind(sexratio$males, sexratio$females)
sex_mod1 <- glm(y ~ density, binomial, data = sexratio)
summary(sex_mod1)
# Overdispersion: residual deviance = 22.09, degrees of freedom = 6
plot(sex_mod1)

# See how it fits
xv <- seq(0, 400, 0.01)
xv <- data.frame(density = xv)
yv <- predict(sex_mod1, newdata = xv, type = "response")
plot(sexratio$density, p, ylab = "Proportion male", 
     xlab = "log (density)", col = hue_pal()(2)[1], pch = 16)
lines(xv$density, yv, col = hue_pal()(2)[2])

# Log-transform the covarite
sex_mod2 <- glm(y ~ log(density), binomial, data = sexratio)
summary(sex_mod2)
# No overdispersion
plot(sex_mod2)

# Fit the model
xv <- seq(0, 6, 0.01)
ev <- data.frame(density = exp(xv))
yv <- predict(sex_mod2, newdata = ev, type = "response")
pdf(paste0(plot_dir, "Fitted_Model_2.pdf"), width = 6, height = 4)
plot(log(sexratio$density), p, ylab = "Proportion male", 
     xlab = "log (density)", col = hue_pal()(2)[1], pch = 16)
lines(xv, yv, col = hue_pal()(2)[2])
dev.off()

# Predicting x from y
bioassay <- read.table("Datasets/bioassay.txt", header = TRUE)
bioassay

# Convert the response in probability of death
y <- cbind(bioassay$dead, bioassay$batch - bioassay$dead)

bioassay_mod1 <- glm(y ~ dose, binomial, data = bioassay)
summary(bioassay_mod1)

xv <- seq(0, 100, 1)
ev <- data.frame(dose = xv)
yv <- predict(bioassay_mod1, newdata = ev, type = "response")
p <- bioassay$dead / bioassay$batch
pdf(paste0(plot_dir, "Bioassay.pdf"), width = 6, height = 4)
plot(bioassay$dose, p, pch = 16,
     col = hue_pal()(2)[1],
     xlab = "log(dose)",
     ylab = "Proportion dead")
lines(xv, yv, lwd = 2, col = hue_pal()(2)[2])
dev.off()

bioassay_mod2 <- glm(y ~ log(dose), binomial, data = bioassay)
summary(bioassay_mod2)
# Improvement

# Still the model is not that good
xv <- seq(0, 5, 0.01)
ev <- data.frame(dose = exp(xv))
yv <- predict(bioassay_mod2, newdata = ev, type = "response")
p <- bioassay$dead / bioassay$batch
pdf(paste0(plot_dir, "Bioassay_2.pdf"), width = 6, height = 4)
plot(log(bioassay$dose), p, pch = 16,
     col = hue_pal()(2)[1],
     xlab = "log(dose)",
     ylab = "Proportion dead")
lines(xv, yv, lwd = 2, col = hue_pal()(2)[2])
dev.off()

# Predict doses to kill 50, 90 and 95% of insects
dose_pred <- dose.p(bioassay_mod2, p = c (0.5,0.9,0.95))
dose_df <- as.data.frame(dose_pred)
dose_df$dose <- exp(dose_df$x)
dose_df

# Proportion data with categorical explanatory variables
## germination of seeds of two genotypes of the parasitic plant Orobanche and 
# two extracts from host plants (bean and cucumber) that were used to stimulate
# germination. Count = # germinated out of Sample
germination <- read.table("Datasets/germination.txt", header = TRUE,
                colClasses = c(rep("numeric", 2), rep("factor", 2)))
head(germination)
table(germination$extract)

tapply(germination$count, list(germination$Orobanche, germination$extract), sum)
tapply(germination$sample, list(germination$Orobanche, germination$extract), sum)

# There is no interaction between Genotype and Plant extract in germination
y <- cbind(germination$count, germination$sample - germination$count)
head(y)

germ_mod1 <- glm(y ~ Orobanche * extract, binomial, data = germination)
summary(germ_mod1)
# Overdispersion: ~ 2

# Use quasi-binomial
germ_mod2 <- glm(y ~ Orobanche * extract, quasibinomial, data = germination)
summary(germ_mod2)

# And now we remove the itneraction term
germ_mod3 <- update(germ_mod2, ~ . - Orobanche:extract)
summary(germ_mod3)

anova(germ_mod3, germ_mod2, test = "F")

# Use ANOVA once again on model 3
anova(germ_mod3, test = "F")

# Remove genotype too
germ_mod4 <- update(germ_mod3, ~ . - Orobanche)
summary(germ_mod4)

anova(germ_mod4, germ_mod3, test = "F")

# Convert values from logit to coefficients
tapply(predict(germ_mod4, type="response"), germination$extract, mean)

# Check the means of proportions from raw data
p <- germination$count / germination$sample
tapply(p, germination$extract, mean)

# Compare to this
tapply(germination$count, germination$extract, sum)
tapply(germination$sample, germination$extract, sum)

# Get the ratios
as.vector(tapply(germination$count, germination$extract, sum))/
  as.vector(tapply(germination$sample, germination$extract, sum))

# Binomial GLM with Ordered Categorical Covariates
## Covariates are ordinal, their order has meaning like scale 1-5
data("esoph")
head(esoph)
sapply(esoph, class)
y <- cbind(esoph$ncases, esoph$ncontrols)

# Start with a simple model, assume no interactions
esoph_mod1 <- glm(y ~ agegp + alcgp + tobgp, binomial, data = esoph)
summary(esoph_mod1)

# Check for interaction between alcohol and tobacco
esoph_mod2 <- glm(y ~ agegp + alcgp * tobgp, binomial, data = esoph)
summary(esoph_mod2)

# Check using ANOVA: HO-> Omit deleted variables without losing anything
anova(esoph_mod1, esoph_mod2, test = "Chisq")

# Plot each covariate separately
p <- esoph$ncases / (esoph$ncases + esoph$ncontrols)
pdf(paste0(plot_dir, "Esophahgus.pdf"), width = 10, height = 5)
par(mfrow=c(1,3))
plot(p ~ alcgp, col = hue_pal()(3)[1], data = esoph, xlab = "",
     cex.lab = 1.5, cex.axis = 1, las=2)
plot(p ~ tobgp, col = hue_pal()(3)[2], data = esoph, xlab = "",
     cex.lab = 1.5, cex.axis = 1, las=2)
plot(p ~ agegp, col = hue_pal()(3)[3], data = esoph, xlab = "",
     cex.lab = 1.5, cex.axis = 1, las=2)
dev.off()

# Reduce the number of groups in covariates
## Alcohol: 40-79 and 80-119 -> 40-119
esoph$alcgp2 <- esoph$alcgp
levels(esoph$alcgp2)[2:3] <- "40-119"
levels(esoph$alcgp2)

## Tobacco: 10-19 & 20-29 -> 10-29
esoph$tobgp2 <- esoph$tobgp
levels(esoph$tobgp2)[2:3] <- "10-30"
levels(esoph$tobgp2)

## Age Groups: 25-34 & 35-44 -> under 45
## 45-54 -> same
## 55-64 - end -> 55+
esoph$agegp2 <- esoph$agegp
levels(esoph$agegp2)[4:6] <- "55+"
levels(esoph$agegp2)[1:2] <- "under45"
levels(esoph$agegp2)

# Fit new model
esoph_mod3 <- glm(y ~ agegp2 + alcgp2 + tobgp2, binomial, data = esoph)
summary(esoph_mod3)

# Remove the orders
esoph$alcgp3 <- factor(esoph$alcgp, ordered = FALSE)
esoph$agegp3 <- factor(esoph$agegp2, ordered = FALSE)
esoph$tobgp3 <- factor(esoph$tobgp2, ordered = FALSE)

esoph_mod4 <- glm(y ~ agegp3 + alcgp3 + tobgp3, binomial, data = esoph)
summary(esoph_mod4)

# Binomial GLM with categorical and continuous covariates
flowering <- read.table("Datasets/flowering.txt", header = TRUE,
                        colClasses = list(variety = "factor"))
head(flowering)

# Plot
## flower - non-flower groups
y <- cbind(flowering$flowered, flowering$number - flowering$flowered)
head(y)

# Probability of flowered
pf <- flowering$flowered / flowering$number
pdf(paste0(plot_dir, "Flowering_Plants.pdf"), width = 6, height = 4)
plot(flowering$dose, jitter(pf), xlab = "dose", ylab = "proportion flowered",
     col = hue_pal()(5)[as.vector(as.numeric(factor(flowering$variety)))],
     pch=16)
legend(1, 1, legend = levels(flowering$variety),
       pch = rep(19, 5), title = "variety",
       col = hue_pal()(5))
dev.off()

# Maximal Model with interaction
flow_mod1 <- glm(y ~ dose * variety, binomial, data = flowering)
summary(flow_mod1)
# Overdispersion: ~2.5

# Plot to see the fit
pdf(paste0(plot_dir, "Flowering_Plants_Model1.pdf"), width = 6, height = 4)
plot(flowering$dose, jitter(pf), xlab = "dose", ylab = "proportion flowered",
      col = hue_pal()(5)[as.vector(as.numeric(factor(flowering$variety)))],
     pch=19)
legend(1, 1, legend = levels (flowering$variety), pch = rep (19, 5), 
       title = "variety", col = hue_pal()(5))

xv <- seq(0, 35, 0.1)
for (i in 1:5) {
  vn <- rep(levels(flowering$variety)[i],length(xv))
  yv <- predict(flow_mod1, list(variety = factor(vn), dose = xv),
                type = "response")
  lines (xv, yv, col = hue_pal()(5)[i], lwd=2)
}
dev.off()

tapply(pf, list(flowering$dose, flowering$variety), mean)

# Back to lizards
lizards <- read.table("Datasets/lizards.txt",header = TRUE,
                      colClasses = c ("numeric", rep ("factor", 5)))
head(lizards)

lizards_sort <- lizards[order(lizards$species,
                              lizards$sun,
                              lizards$height,
                              lizards$perch,
                              lizards$time),]
lizards_sort_top <- lizards_sort[1:24,]
head(lizards_sort_top)
names(lizards_sort_top)[1] <- "Ag"
lizards_sort_top <- lizards_sort_top[,-6]

lizards_new <- data.frame(lizards_sort$n[25:48], lizards_sort_top)
names(lizards_new)[1] <- "Ao"
head(lizards_new)

y <- cbind(lizards_new$Ao, lizards_new$Ag)
head(y)

# Probability of flowered
pf <- lizards_new$Ao / (lizards_new$Ao + lizards_new$Ag)

# Stick it in the model
lizards_model1 <- glm(y ~ sun * height * perch * time, binomial, 
                      data = lizards_new)
summary(lizards_model1)
################################################################################
# Binary Response Variables and GLMs
isolation <- read.table("Datasets/isolation.txt", header = TRUE)
head(isolation)

# Model with interactions
iso_mod1 <- glm(incidence ~ area * isolation, binomial, data = isolation)
summary(iso_mod1)

# Simple Model
iso_mod2 <- glm(incidence ~ area + isolation, binomial, data = isolation)
summary(iso_mod2)

# Compare the two
anova(iso_mod2, iso_mod1, test = "Chi")
# Keep simpler

# Plot the Univariate logistic regressions
iso_moda <- glm(incidence ~ area, binomial, data = isolation)
iso_modi <- glm(incidence ~ isolation, binomial, data = isolation)
xva <- seq(0, 9, 0.01)
yva <- predict(iso_moda, list(area = xva), type = "response")
plot(isolation$area, isolation$incidence, xlab = "area", 
     pch = 16, ylab = "incidence", col = "red")
lines(xva, yva, col = hue_pal()(2)[1], lwd=2)

xvi <- seq(0, 10, 0.01)
yvi <- predict(iso_modi, list (isolation = xvi), type = "response")
plot(isolation$isolation, isolation$incidence, xlab = "isolation",
      ylab = "incidence", pch = 16, col = "blue")
lines(xvi, yvi, col = hue_pal()(2)[2])

# Graphical Tests (Very cool)
occupation <- read.table("Datasets/occupation.txt", header = TRUE)
head(occupation)

# Fit the model
occ_mod <- glm(occupied ~ resources, binomial, data = occupation)
summary(occ_mod)

# Create a rug plot
pdf(paste0(plot_dir, "RugPlot.pdf"), width = 6, height = 4)
plot(occupation$resources, occupation$occupied, type = "n",
     xlab = "resources", ylab = "occupied or not")
rug(occupation$resources[occupation$occupied == 0])
rug(occupation$resources[occupation$occupied == 1], side = 3)
xv <- 0:1000
yv <- predict(occ_mod, list(resources = xv), type = "response")
lines(xv, yv, col = hue_pal()(2)[1], lwd=2)
dev.off()

# Cut the data by ranges and plot
occ_cut <- cut(occupation$resources, 5)

# Total per cut
tapply(occupation$occupied, occ_cut, sum)
# Number of cases per bin
table(occ_cut)

# Empirical Probabilities
occ_probs <- tapply(occupation$occupied, occ_cut, sum) / table(occ_cut)
occ_probs

occ_probs <- as.vector(occ_probs)
resmeans <- as.vector(tapply(occupation$resources, occ_cut, mean))
se <- as.vector(sqrt(occ_probs * (1 - occ_probs) / table(occ_cut)))
up <- occ_probs + se
down <- occ_probs - se

# Plot the whole thing
pdf(paste0(plot_dir, "RugPlot_Classes.pdf"), width = 6, height = 4)
plot(occupation$resources, occupation$occupied, type = "n",
     xlab = "resources", ylab = "occupied or not")
rug(occupation$resources[occupation$occupied == 0])
rug(occupation$resources[occupation$occupied == 1], side = 3)
xv <- 0:1000
yv <- predict(occ_mod, list(resources = xv), type = "response")
lines(xv, yv, col = hue_pal()(2)[1], lwd=2)
points(resmeans, occ_probs, cex=2, col = hue_pal()(2)[2], pch=19)
for (i in 1:5) {
  lines(rep(resmeans[i], 2), c(up[i], down[i]), col = hue_pal()(2)[2])
}
dev.off()

# Mixed covariate types with a binary response
infection <- read.table("Datasets/infection.txt", header = TRUE,
                        colClasses = c("factor", rep("numeric", 2), "factor"))
head(infection)

boxplot(weight ~ infected, data = infection, col=hue_pal()(2)[1], pch=16)
boxplot(age ~ infected, data = infection, col=hue_pal()(2)[2], pch=16)

# Quick Summary
table(infection$sex, infection$infected)

# Fit the full model
inf_mod1 <- glm(infected ~ age * weight * sex, family = binomial, data = infection)
summary(inf_mod1)

# Fit Minimal Model
inf_mod2 <- glm(infected ~ age + weight + sex, family = binomial, data = infection)
summary(inf_mod2)

inf_mod3 <- glm(infected ~ age + weight + sex + I(weight^2) + I(age^2),
                family = binomial, data = infection)
summary(inf_mod3)

# Spine plot and logistic regression
wasps <- read.table("Datasets/wasps.txt", header = TRUE, 
                    colClasses = list (fate = "factor"))
head(wasps)

table(wasps$density, wasps$fate)
tapply(wasps$density, wasps$fate, sum)

# SpinePlot
spineplot(fate ~ density, data = wasps, col = hue_pal()(2))

# Smoother
cdplot(fate ~ density, data = wasps, col = hue_pal()(2))

# log-transform density
cdplot (fate ~ log(density), data = wasps, col = hue_pal()(2))

# Create models for both
wasps_mod1 <- glm(fate ~ density, binomial, data = wasps)
summary(wasps_mod1)

# log-transform the covariate
wasps_mod2 <- glm(fate ~ log(density), binomial, data = wasps)
summary(wasps_mod2)

anova(wasps_mod1, wasps_mod2, test = "Chisq")

pdf(paste0(plot_dir, "Wasps.pdf"), width = 6, height = 4)
plot(jitter(log(wasps$density)), as.numeric(wasps$fate) - 1, 
     col = hue_pal()(3)[1], xlim = c(0, 5), xlab = "jittered ln (density)",
     ylab = "proportion parasitised")
xv <- seq(0, 5, 0.01)
yv <- 1 / (1 + 1 / exp(coef(wasps_mod2)[1] + coef(wasps_mod2)[2] * xv))
lines(xv, yv, col = hue_pal ()(3)[2], lwd=2)

den <- c(3.75, 16, 32, 64)
pd <- c(3/15, 5/16, 20/32, 52/64)
points(log(den), pd, cex = 2,, col = hue_pal()(4)[3], pch=19)
eb <- sqrt(pd * (1 - pd) / den)
for (i in 1:4) {
  lines(rep (log (den[i]), 2), c (pd[i] - eb[i], pd[i] + eb[i]),
        col = hue_pal ()(4)[4])
  }
dev.off()
################################################################################
# Bootstrapping a GLM
timber <- read.table("Datasets/timber.txt", header = TRUE)
head(timber)

boxplot(log(timber))

# Fit model
timber_model <- glm(log(volume) ~ log(girth) + log(height), data = timber)
summary(timber_model)

library(boot)
# Fit the model 2000 times
model.boot1 <- function (data, indices){
  sub_data <- data[indices,]
  model <- glm(log(volume) ~ log(girth) + log(height), data = sub_data)
  coef(model)
}

timber_boot <- boot(timber, model.boot1, R = 2000)
timber_boot

# Permute residuals
yhat <- fitted(timber_model)
resids <- resid(timber_model)
res_data <- data.frame(resids, timber$girth, timber$height)

model.boot2 <- function (res_data, i) {
  y <- yhat + res_data[i,1]
  nd <- data.frame(y, timber$girth, timber$height)
  model <- glm(y ~ log(timber$girth) + log(timber$height), data = nd)
  coef(model)
}

perms <- boot(res_data, model.boot2, R = 2000, sim = "permutation")
perms

# Build Confidence Intervals
boot.ci(perms, index = 1, conf = 0.99)$bca[c (1, 4, 5)]
boot.ci(perms, index = 2, conf = 0.99)$bca[c (1, 4, 5)]
boot.ci(perms, index = 3, conf = 0.99)$bca[c (1, 4, 5)]
