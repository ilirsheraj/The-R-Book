# Designed Experiments
library(scales)
library(tidyverse)

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

# use ggplot
growth %>%
  group_by(supplement, diet) %>%
  summarise(mean_gain = mean(gain),
            .groups = "drop")

growth %>% group_by(supplement, diet) %>%
  summarise(mean_gain = mean(gain), .groups = "drop") %>%
  ggplot(aes(x = supplement, y = mean_gain, fill = diet)) +
  geom_col(position = "dodge") +
  scale_fill_hue() +
  scale_y_continuous(limits = c(0, 30)) +
  labs(
    x = "Supplement",
    y = "Mean gain",
    fill = "Diet") +
  theme_classic()

# Or even simpler
ggplot(growth, aes(x = supplement, y = gain, fill = diet)) +
  stat_summary(fun = mean,
               geom = "col",
               position = "dodge") +
  scale_fill_hue() +
  scale_y_continuous(limits = c(0, 30)) +
  labs(x = "Supplement",
       y = "Mean gain",
       fill = "Diet") +
  theme_classic()

# Add error bars
growth_summary <- growth %>% group_by(supplement, diet) %>%
  summarise(
    mean_gain = mean(gain),
    se = sd(gain) / sqrt(n()),
    .groups = "drop")

ggplot(growth_summary, aes(x = supplement, y = mean_gain, fill = diet)) +
  geom_col(position = position_dodge(width = 0.9),
           width = 0.8) +
  geom_errorbar(
    aes(ymin = mean_gain - se,
        ymax = mean_gain + se),
    position = position_dodge(width = 0.9),
    width = 0.2) +
  scale_fill_hue() +
  scale_y_continuous(limits = c(0, 30)) +
  labs(x = "Supplement",
       y = "Mean gain ± SE",
       fill = "Diet") +
  theme_classic()


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
splityield <- read.table("Datasets/splityield.txt", header = TRUE)
head(splityield)

# Check the class of each
sapply(splityield, class)

# Error is from largest to smallest
splityield_mod1 <- aov(yield ~ irrigation * density * fertilizer +
                         Error(block/irrigation/density), data = splityield)
summary(splityield_mod1)

interaction.plot(splityield$fertilizer, splityield$irrigation, splityield$yield, 
                 col = hue_pal()(4)[1:2], lwd = 2,
                 trace.label = "Irrigation",
                 xlab = "Fertilizer",
                 ylab = "Mean of Yield")

interaction.plot(splityield$density, splityield$irrigation, splityield$yield, 
                 col = hue_pal()(4)[3:4], lwd = 2,
                 trace.label = "Irrigation",
                 xlab = "Fertilizer",
                 ylab = "Density")
# If there are NAs, use lme() or lmer() instead of aov()
################################################################################
# Part 3: Contrasts
comp <- read.table("Datasets/competition.txt", header = TRUE,
                   colClasses = list (clipping = "factor"))
head(comp)
table(comp$clipping)

# Calculate the means for all classes
tapply(comp$biomass, comp$clipping, mean)

# Visualize it
pdf(paste0(plot_dir, "Biomass_by_Clipping.pdf"), width = 5, height = 5)
comp %>% group_by(clipping) %>% 
  summarise(mean_group = mean(biomass), .groups = "drop") %>% 
  ggplot(aes(clipping, mean_group, fill = clipping)) + 
  geom_col() +
  labs(title = "Biomass by Clipping",
       x = "Clipping Method",
       y = "Mean Biomass") +
  theme_classic() +
  theme(legend.position = "none")
dev.off()

# Fit a classical ANOVA
comp_mod1 <- aov(biomass ~ clipping, data = comp)
summary(comp_mod1)
# There is significance, but where exactly is it?
## Control seems the lowest, n25 and n50 seems similar, r10 and r5 are also similar

# Use linear model for more details
comp_mod1 <- lm(biomass ~ clipping, data = comp)
summary(comp_mod1)
# Control is the base, all are higher than base
## Now use contrasts to see each on its own
contrasts(comp$clipping) <- cbind(c(4, -1, -1, -1, -1), # control vs rest
                                  c(0, 1, 1, -1, -1), # n25/50 vs r10/5
                                  c(0, 0, 0, 1, -1), # r10 vs r5
                                  c(0, 1, -1, 0, 0)) # n25 vs n50

# See how it looks like: check column-wise
comp$clipping[[2]]
head(comp)

comp_mod2 <- lm(biomass ~ clipping, data = comp)
summary(comp_mod2)

# Check specific contrasts
mean(comp$biomass)

# Control vs rest
c1 <- factor(1 + (comp$clipping != "control"))
tapply(comp$biomass, c1, mean)

mean(comp$biomass) - tapply(comp$biomass,c1,mean)[2]

# Second contrast
c2 <- factor(2 * (comp$clipping == "n25") + 2 * (comp$clipping == "n50") + 
               (comp$clipping == "r10") + (comp$clipping == "r5"))
(tapply(comp$biomass, c2, mean)[3] - tapply(comp$biomass, c2, mean)[2]) / 2

