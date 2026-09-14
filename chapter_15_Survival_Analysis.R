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

################################################################################
# 4 parametric Survival functions shown in the book but not plotted
# First lets define the survival time from 0 to 100 units
t <- seq(0, 100, length.out = 1000)

# Define 3 Colours
cols <- c("#F8766D", "#00BA38", "#619CFF")

pdf(paste0(plot_dir, "Parametric_Functions_Simulation.pdf"), width = 8, height = 8)
# Fix the 2 x 2 layout
par(mfrow = c(2, 2), mar = c(4.5, 4.5, 2, 1))

# Exponential: S(t) = exp(-a t)
a <- c(1.00, 0.10, 0.01)
S1 <- exp(-a[1] * t)
S2 <- exp(-a[2] * t)
S3 <- exp(-a[3] * t)

plot(t, S1,
     type = "l",
     col = cols[1],
     lwd = 1.5,
     ylim = c(0, 1),
     main = "Exponential",
     xlab = "Time (t)",
     ylab = "Probability of survival, S(t)")

lines(t, S2, col = cols[2], lwd = 1.5)
lines(t, S3, col = cols[3], lwd = 1.5)

legend("topright",
       legend = c(
         expression(a == 1.00),
         expression(a == 0.10),
         expression(a == 0.01)),
       col = cols,
       lwd = 2,
       bty = "n")

# Weibull: S(t) = exp(-(a t)^b)
weibull_surv <- function(t, a, b) {exp(-(a * t)^b)}

S1 <- weibull_surv(t, a = 1.0, b = 2.0)
S2 <- weibull_surv(t, a = 0.1, b = 2.0)
S3 <- weibull_surv(t, a = 0.1, b = 3.0)

plot(t, S1,
     type = "l",
     col = cols[1],
     lwd = 1.5,
     ylim = c(0, 1),
     main = "Weibull",
     xlab = "Time (t)",
     ylab = "Probability of survival, S(t)")

lines(t, S2, col = cols[2], lwd = 1.5)
lines(t, S3, col = cols[3], lwd = 1.5)

legend("topright",
       legend = c(
         expression(a == 1.0 ~ "," ~ b == 2.0),
         expression(a == 0.1 ~ "," ~ b == 2.0),
         expression(a == 0.1 ~ "," ~ b == 3.0)
       ),
       col = cols,
       lwd = 2,
       bty = "n")

# Gompertz: S(t) = exp[-a/b * (exp(b t) - 1)]
gompertz_surv <- function(t, a, b) {
  exp(-(a / b) * (exp(b * t) - 1))}

S1 <- gompertz_surv(t, a = 0.010, b = 0.100)
S2 <- gompertz_surv(t, a = 0.050, b = 0.100)
S3 <- gompertz_surv(t, a = 0.005, b = 0.010)

plot(t, S1,
     type = "l",
     col = cols[1],
     lwd = 1.5,
     ylim = c(0, 1),
     main = "Gompertz",
     xlab = "Time (t)",
     ylab = "Probability of survival, S(t)")

lines(t, S2, col = cols[2], lwd = 1.5)
lines(t, S3, col = cols[3], lwd = 1.5)

legend("topright",
       legend = c(
         expression(a == 0.010 ~ "," ~ b == 0.100),
         expression(a == 0.050 ~ "," ~ b == 0.100),
         expression(a == 0.005 ~ "," ~ b == 0.010)),
       col = cols,
       lwd = 2,
       bty = "n")

# Log-logistic: S(t) = 1 / (1 + (a t)^b)
loglogistic_surv <- function(t, a, b) {
  1 / (1 + (a * t)^b)
}

S1 <- loglogistic_surv(t, a = 1.0, b = 2.0)
S2 <- loglogistic_surv(t, a = 0.1, b = 1.0)
S3 <- loglogistic_surv(t, a = 0.5, b = 0.5)

plot(t, S1,
     type = "l",
     col = cols[1],
     lwd = 1.5,
     ylim = c(0, 1),
     main = "Log-Logistic",
     xlab = "Time (t)",
     ylab = "Probability of survival, S(t)")

lines(t, S2, col = cols[2], lwd = 1.5)
lines(t, S3, col = cols[3], lwd = 1.5)

legend("topright",
       legend = c(
         expression(a == 1.0 ~ "," ~ b == 2.0),
         expression(a == 0.1 ~ "," ~ b == 1.0),
         expression(a == 0.5 ~ "," ~ b == 0.5)),
       col = cols,
       lwd = 2,
       bty = "n")
dev.off()

################################################################################
# 4 parametric Hazard functions shown in the book but not plotted
# Time grid
t <- seq(0.01, 100, length.out = 1000)

cols <- c("#F8766D", "#00BA38", "#619CFF")

pdf(paste0(plot_dir, "Parametric_Hazard_Simulation.pdf"), width = 8, height = 8)
par(mfrow = c(2, 2), mar = c(4.5, 4.5, 2, 1))

# Exponential: h(t) = a
exp_hazard <- function(t, a) {rep(a, length(t))}

h1 <- exp_hazard(t, a = 1.00)
h2 <- exp_hazard(t, a = 0.10)
h3 <- exp_hazard(t, a = 0.01)

plot(t, h1,
     type = "l",
     col = cols[1],
     lwd = 1.5,
     ylim = c(0, 1.4),
     main = "Exponential",
     xlab = "Time (t)",
     ylab = expression(paste("Hazard, ", h(t))))

lines(t, h2, col = cols[2], lwd = 1.5)
lines(t, h3, col = cols[3], lwd = 1.5)

legend("topright",
       legend = c(
         expression(a == 1.00),
         expression(a == 0.10),
         expression(a == 0.01)
       ),
       col = cols,
       lwd = 2,
       bty = "n")

# Weibull: h(t) = a*b*(a*t)^(b-1)
weibull_hazard <- function(t, a, b) {a * b * (a * t)^(b - 1)}

h1 <- weibull_hazard(t, a = 1.0, b = 2.0)
h2 <- weibull_hazard(t, a = 0.1, b = 2.0)
h3 <- weibull_hazard(t, a = 0.1, b = 3.0)

plot(t, h1,
     type = "l",
     col = cols[1],
     lwd = 1.5,
     ylim = c(0, 210),
     main = "Weibull",
     xlab = "Time (t)",
     ylab = expression(paste("Hazard, ", h(t))))

lines(t, h2, col = cols[2], lwd = 1.5)
lines(t, h3, col = cols[3], lwd = 1.5)

legend("topleft",
       legend = c(
         expression(a == 1.0 ~ "," ~ b == 2.0),
         expression(a == 0.1 ~ "," ~ b == 2.0),
         expression(a == 0.1 ~ "," ~ b == 3.0)),
       col = cols,
       lwd = 2,
       bty = "n")

# Gompertz: h(t) = a * exp(b*t)
gompertz_hazard <- function(t, a, b) {a * exp(b * t)}

h1 <- gompertz_hazard(t, a = 0.010, b = 0.100)
h2 <- gompertz_hazard(t, a = 0.050, b = 0.100)
h3 <- gompertz_hazard(t, a = 0.005, b = 0.010)

plot(t, h1,
     type = "l",
     col = cols[1],
     lwd = 1.5,
     ylim = c(0, 225),
     main = "Gompertz",
     xlab = "Time (t)",
     ylab = expression(paste("Hazard, ", h(t))))

lines(t, h2, col = cols[2], lwd = 1.5)
lines(t, h3, col = cols[3], lwd = 1.5)

legend("topleft",
       legend = c(
         expression(a == 0.010 ~ "," ~ b == 0.100),
         expression(a == 0.050 ~ "," ~ b == 0.100),
         expression(a == 0.005 ~ "," ~ b == 0.010)),
       col = cols,
       lwd = 2,
       bty = "n")

# Log-Logistic:  h(t) = (a*b*t^(b-1)) / (1 + a*t^b)
loglogistic_hazard <- function(t, a, b) {(a * b * t^(b - 1)) / (1 + a * t^b)}

h1 <- loglogistic_hazard(t, a = 1.0, b = 2.0)
h2 <- loglogistic_hazard(t, a = 0.1, b = 1.0)
h3 <- loglogistic_hazard(t, a = 0.5, b = 0.5)

plot(t, h1,
     type = "l",
     col = cols[1],
     lwd = 1.5,
     ylim = c(0, 1),
     main = "Log-Logistic",
     xlab = "Time (t)",
     ylab = expression(paste("Hazard, ", h(t))))

lines(t, h2, col = cols[2], lwd = 1.5)
lines(t, h3, col = cols[3], lwd = 1.5)

legend("topright",
       legend = c(
         expression(a == 1.0 ~ "," ~ b == 2.0),
         expression(a == 0.1 ~ "," ~ b == 1.0),
         expression(a == 0.5 ~ "," ~ b == 0.5)),
       col = cols,
       lwd = 2,
       bty = "n")
dev.off()

################################################################################
# Cox-Proportional Hazard Models (CoxPH): semi-parametric
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

roach_ph <- cox.zph(roach_model_ph2)
roach_ph

plot(roach_ph)

pdf(paste0(plot_dir, "The_PH_Assumptions_Plot.pdf"), width = 6, height = 4)
plot(roach_ph,
     var = "group",
     xlab = "Time",
     ylab = "Beta(t) for group",
     resid = TRUE,
     se = TRUE,
     pch = 16)
dev.off()

################################################################################
# Accelerated Failure Time (AFT) Models: Parametric
# Exponential
roach_model_exp1 <- survreg(Surv (death, status) ~ weight + group, 
                            data = roaches, dist = "exponential")
summary(roach_model_exp1)

# Weibull
roach_model_wei1 <- survreg(Surv (death, status) ~ weight + group,
                            data = roaches)
summary(roach_model_wei1)

# Remeove the weight completely
roach_model_wei2 <- survreg(Surv (death, status) ~ group,
                            data = roaches)
summary(roach_model_wei2)

# Prediction of mean age at death, roach_model_wei2, for each group
tapply(predict(roach_model_wei2), roaches$group, mean)

# Mean age at death for each group, using only death (non-censored) times
tapply(roaches$death[roaches$status == 1], roaches$group[roaches$status == 1], mean)

# Mean age at death/censoring for each group
tapply(roaches$death, roaches$group, mean)

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

attach(roaches)
lines(predict(roach_model_wei2, 
              newdata = list(group = "A"), 
              type="quantile",
              p = seq(0.01, 0.99, by = 0.01)), 
      seq(0.99, 0.01, by = -0.01),
      col = hue_pal()(3)[1])
lines(predict(roach_model_wei2, 
              newdata = list(group = "B"), 
              type = "quantile",
              p = seq(0.01, 0.99, by = 0.01)), 
      seq(0.99, 0.01, by = -0.01),
      col = hue_pal()(3)[2])
lines(predict(roach_model_wei2, 
              newdata = list(group = "C"), 
              type = "quantile",
              p = seq(0.01, 0.99, by = 0.01)), 
      seq(0.99, 0.01, by = -0.01),
      col = hue_pal ()(3)[3])
detach(roaches)

# EOF