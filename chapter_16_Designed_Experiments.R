# Designed Experiments
library(scales)

plot_dir <- "Plots/"

# Part 1: Factorial Experiments
growth <- read.table("Datasets/growth.txt", header = TRUE,
                     colClasses = list(diet = "factor", supplement = "factor"))
head(growth)

summary(growth)

growth$supplement <- factor(growth$supplement, 
                            levels = c("control", "agrimore", "supergain", "supersupp"))

pdf(paste0(plot_dir, "Growth_Barplot.pdf"), width = 6, height = 5.5)
barplot(tapply(growth$gain, list(growth$diet, growth$supplement), mean),
        beside = TRUE, col = hue_pal()(3), ylim = c(0, 30))
legend(1.5, 30, legend=c("barley", "oats", "wheat"), fill = hue_pal()(3))
dev.off()

# We can see the means for all of them
tapply(growth$gain, list(growth$diet, growth$supplement), mean)

# Then Stick it in the model
## Use Linear Model with interaction
growth_mod1 <- lm(gain ~ diet * supplement, data = growth)
summary(aov(growth_mod1))

## Or use the Anova Function
model_aov <- aov(gain ~ diet * supplement, data = growth)
summary(model_aov)

# Intercept is the overall mean with both factor levels: diet=barley, supplement=control!
summary(growth_mod1)

# Remove the interaction term
model_aov2 <- aov(gain ~ diet + supplement, data = growth)
summary(model_aov2)

growth_mod2 <- lm(gain ~ diet + supplement, data = growth)
summary(growth_mod2)

# supergain + control -> worst
# agrimore + supersup -> best
supp_new <- growth$supplement
levels(supp_new)
levels(supp_new)[c(2,4)] <- "best"
levels(supp_new)[c(1,3)] <- "worst"
levels(supp_new)
levels(supp_new)

growth <- cbind(growth, supp_new)
head(growth)
tapply(growth$gain, list(growth$diet, growth$supp_new), mean)

growth_mod3 <- lm(gain ~ diet + supp_new, data = growth)
summary(growth_mod3)

# Compare to model 2
anova(growth_mod3, growth_mod2)

# Data Expansion for factor levels: 5 x 5 x 2
expanded_factors <- expand.grid(height = seq(60, 80, 5),
                                weight = seq(100, 300, 50),
                                sex = c ("Male", "Female"))
head(expanded_factors)
tail(expanded_factors)

################################################################################
# Part 2: Pseudo-Replication
