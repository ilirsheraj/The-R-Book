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
