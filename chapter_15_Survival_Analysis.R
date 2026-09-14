# Survival Analysis: One of my favorite topics
library(scales)
library(survival)
plot_dir <- "Plots/"

cancer <- read.table("Datasets/cancer.txt", header = TRUE)
head(cancer)

# Fit the Kaplan-Meier estimate without any class
km_fit <- survfit(Surv(death, status) ~ 1, data = cancer)

# Plot
pdf(paste0(plot_dir, "Kaplan_Meier_Plot.pdf"), width = 6, height = 4)
par(mfrow=c(1,2))
hist(cancer$death, main = "Cancer Death",
     xlab = "Survival Time",
     ylab = "Frequency")

plot(km_fit,
     xlab = "Survival Time",
     ylab = "Probability of survival",
     main = "Kaplan-Meier Curve",
     conf.int = FALSE,
     mark.time = FALSE)
dev.off()

# Fit another model, this time comparing the treatments
km_treatment <- survfit(Surv(death, status) ~ treatment, data = cancer)

pdf(paste0(plot_dir, "Kaplan_Meier_Classes.pdf"), width = 6, height = 4)
par(mfrow=c(1,2))
plot(km_treatment,
     main = "KM no Censor",
     xlab = "Survival Time",
     ylab = "Probability of survival",
     lwd = 2,
     mark.time = FALSE,
     col=hue_pal()(4)[1:4])

legend("topright",
       legend = c("Drug A", "Drug B", "Drug C", "Placebo"),
       fill = hue_pal()(4)[1:4],
       bty = "n",
       cex = 0.5)

plot(km_treatment,
     main = "KM Censored",
     xlab = "Survival Time",
     ylab = "Probability of survival",
     lwd = 2,
     mark.time = TRUE,
     col=hue_pal()(4)[1:4])

legend("topright",
       legend = c("Drug A", "Drug B", "Drug C", "Placebo"),
       fill = hue_pal()(4)[1:4],
       bty = "n",
       cex = 0.5)
dev.off()

# The logrank test
compare_treat <- survdiff(Surv(death, status) ~ treatment, data = cancer)
compare_treat

roaches <- read.table("Datasets/roaches.txt", header = TRUE)
head(roaches)
summary(roaches)

hist(roaches$death)
hist(roaches$weight)

# Cox-Proportional Hazard (CoxPH)
roach_model_ph1 <- coxph(Surv(death, status) ~ weight + group, data = roaches)
summary(roach_model_ph1)

# Since weight is not significant, rmeove it
roach_model_ph2 <- coxph(Surv(death, status) ~ group, data = roaches)
summary(roach_model_ph2)

# Plot the thing
km_roaches <- survfit(Surv(death, status) ~ group, data = roaches)

pdf(paste0(plot_dir, "Kaplan_Meier_Roaches.pdf"), width = 5, height = 5)
plot(km_roaches,
     main = "KM Roaches",
     xlab = "Survival Time",
     ylab = "Probability of survival",
     lwd = 2,
     mark.time = TRUE,
     col=hue_pal()(3)[1:3])

legend("topright",
       legend = c("Group A", "Group B", "Group C"),
       fill = hue_pal()(3)[1:3],
       bty = "n",
       cex = 0.5)
dev.off()

