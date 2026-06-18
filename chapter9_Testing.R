# Simple Statistics
library(scales)
plot_dir <- "Plots/"
################################################################################
# Principles
exams <- read.csv("Datasets/exams.csv")
head(exams)

# Plot all at once
plot(exams, pch=16)

# Look at the correlations
cor(exams)

# H0: The population mean exam 1 mark is 60%;
# H1: The population mean exam 1 mark is != 60%.
t.test(exams$Exam1, mu=60, conf.level = 0.92)
################################################################################
# Continuous Data: Averages
## Single Population Average
light <- read.table("Datasets/light.txt", header = TRUE)
head(light)

# Visualize
hist(light$speed, main="", xlab = "", col = hue_pal()(1))

# Summarize
summary(light$speed)

# H0 : estimates of the speed of light are symmetric about 299,990;
# H1 : estimates of the speed of light are not symmetric about 299,990.
wilcox.test(light$speed, mu=990)

## Two population Averages
load("Datasets/tulips.RData")
list(GardenA, GardenB)

# Visualize
boxplot(list(GardenA, GardenB), xlab="", ylab="Height (cm)", varwidth = TRUE,
        names = c("Garden A", "Garden B"), col = hue_pal()(2), notch = TRUE,
        pch=16)

# Check for normality
par(mfrow = c(1,2))
qqnorm(GardenA, main = "Garden A", col = hue_pal()(2)[1], pch=16)
qqline(GardenA)
qqnorm(GardenB, main = "Garden B", col = hue_pal()(2)[2], pch=16)
qqline(GardenB)
par(mfrow = c(1,1))

# Two-sample t-test
# H0 : the populations in GardenA and GardenB have the same mean
# H1 : the populations in GardenA and GardenB do not have the same mean.
t.test(GardenA, GardenB, conf.level = 0.9, var.equal = TRUE)

# Back to exam data
boxplot(exams, main = "Exam Scores", ylab = "Score", col = hue_pal()(3),
        pch=16)

# Run a paired test for exams: Check normality first
par(mfrow = c(1,3))
qqnorm(exams$Exam1, main = "Exam 1", col = hue_pal()(3)[1], pch=16)
qqline(exams$Exam1)
qqnorm(exams$Exam2, main = "Exam 2", col = hue_pal()(3)[2], pch=16)
qqline(exams$Exam2)
qqnorm(exams$Exam3, main = "Exam 3", col = hue_pal()(3)[3], pch=16)
qqline(exams$Exam3)
par(mfrow = c(1,1))

# Exam 2 does not appear to be normal
# Let's use non-parametric
wilcox.test(exams$Exam1, exams$Exam2, paired = TRUE, conf.int = TRUE)

# Let's test for equal variance and run t-test between 1 and 3 which are normal
if (var.test(x = exams$Exam1, y = exams$Exam3)$p.value < 0.01) {
  exams13_var_equal <- "T"
} else {
  exams13_var_equal <- "F"
}

exams13_var_equal

t.test(x = exams$Exam1, y = exams$Exam3, conf.level = 0.9, 
       paired = TRUE, var.equal = exams13_var_equal)

## Multiple Population Averages
# Exams are not normally distributed -> KW non-parametric test
# H0 The medians of the 3 populations of marks are the same.
# H1 The medians of the 3 populations of marks are not the same.
kruskal.test(exams)

# Check whether sample distributions are the same: Empirical Distribution Function
A <- length(GardenA)
B <- length(GardenB)
plot(seq(0, 1, length = A), cumsum(sort(GardenA) / sum(GardenA)), type="l",
     ylab = "cumulative probability", xlab = "percentage of samples considered",
      col = hue_pal ()(4)[1], lwd=2)
lines(seq(0, 1, length = B), cumsum(sort (GardenB) / sum(GardenB)),
      col = hue_pal ()(4)[2], lwd=2)
legend(x = 0.7, y = 0.4, legend = c("GardenA", "GardenB"),
       col = hue_pal ()(4)[1:2], lty = 1)

par(mfrow = c(1,2))
boxplot(GardenA, col = hue_pal ()(4)[1], ylim = c (0, 12))
boxplot(GardenB, col = hue_pal ()(4)[2], ylim = c (0, 12))
par(mfrow = c(1,1))

# Do data come from the same distribution
ks.test(GardenA, GardenB)

# Check if data of Garden A comes from t-distribution
ks.test(GardenA, "pt", ncp = mean(GardenA), df = length(GardenA) - 1)

# Z-Test for non-normally distributed data
library(BSDA)
z.test(x = exams$Exam1, y = exams$Exam2,
       sigma.x = sd (exams$Exam1),
       sigma.y = sd (exams$Exam2))

## Comparing Variances with Fisher's F-Test
# H0 The population variances are the same.
# H1 The population variances are not the same.
var.test(GardenA, GardenB, conf.level = 0.99)

# Variance for multiple tests
refs <- read.table("Datasets/refuge.txt", header = TRUE)
head(refs)
table(refs$T)

# Calculate variances for all classes
tapply(refs$B, refs$T, var)

# remove class 9
nine <- which(refs$T == 9)

# Bartlett is parametric
bartlett.test(refs$B[-nine], refs$T[-nine])

# Fligner is non-parametric
fligner.test(refs$B[-nine], refs$T[-nine])
################################################################################
# Discrete and Categorical Data
## Sign Test: Similar to binomial
# H0 : there is no difference in performance between the new and conventional training regimes.
# H1 : there is a difference in performance between the new and conventional training regimes.
binom.test(x=1, n=9, p=0.5)

# Another example: Success is positive, failure negative
data <- c(-1, 2, -3, 4, -5, 6, -7, 8)

binom.test(sum(data > 0), length(data), p=0.5, alternative="two.sided")

# We can write a function of our own
sign.test <- function(x, y){
  if (length(x) != length(y)) stop("The two variables must be of equal length")
  d <- x - y
  binom.test(sum(d > 0), length(d))
}

sign.test(exams$Exam1, exams$Exam2)

# We can also use BSDA Package for two sided
SIGN.test(exams$Exam1, exams$Exam2)

## Goodness of fit (Proportions)
# H0 : the number of staff in categories A, B, and C are representative of the population as a whole.
# H1 : the number of staff in categories A, B, and C are not representative of the population as a whole.
library(EMT)
multinomial.test(observed = c(4, 47, 12),
                 prob = c(0.111, 0.712, 0.177))

library(DescTools)
# Show confidence intervals
MultinomCI(x=c(4, 47, 12), conf.level = 0.95)
# All proportions fall within the CI, thus nothing special is happening

# We can use GTest too, especially for large datasets
GTest(x=c(4, 47, 12),y=c(0.111, 0.712, 0.177))

# Let's take another case of number of bankruptcies by district
banks <- read.table("Datasets/cases.txt", header = TRUE)
head(banks)
banks_table <- table(banks)
banks_table

mean_banks <- mean(banks$cases)
mean_banks

var(banks$cases)/mean(banks$cases)

pdf(paste0(plot_dir, "Poisson_Model.pdf"), width = 6, height = 4)
par(mfrow = c(1,2))
barplot(banks_table, xlab = "Cases", ylab = "Frequency", col = hue_pal()(2)[1],
        ylim = c(0,35), main = "")

barplot(dpois(0:10, mean_banks) * 80, names = as.character(0:10), 
        xlab = "Cases", ylab = "Frequency", col = hue_pal()(2)[2],
        ylim = c(0, 35), main = "")
dev.off()
par(mfrow=c(1,1))

# Data are not poisson distributed
## To get the final probability as 1, we need this trick!
GTest(x=banks_table, p=c(dpois(0:9, mean_banks), 1 - ppois(9, mean_banks)))

# Try negative binomial: we know mean, now we use size
size <- mean(banks$cases)^2 / (var(banks$cases) - mean(banks$cases))
size

pdf(paste0(plot_dir, "NegBinom_Model.pdf"), width = 6, height = 4)
par(mfrow = c(1,2))
barplot(banks_table, xlab = "Cases", ylab = "Frequency", col = hue_pal()(2)[1],
        ylim = c(0,35), main = "Data")

barplot(dnbinom(0:10, mu=mean_banks, size = size) * 80, names = as.character(0:10), 
        xlab = "Cases", ylab = "Frequency", col = hue_pal()(2)[2],
        ylim = c(0, 35), main = "Model")
dev.off()
par(mfrow=c(1,1))

# Now try G-test and see how well it fits
GTest(x=banks_table, p=c(dnbinom(0:9, size = size, mu = mean_banks), 
                         1 - pnbinom(9, size=size, mu=mean_banks)))

## Contingency Tables
# Contingency -> All Events that could possibly happen
eye_hair <- data.frame(BlueEyes = c(38, 14), BrownEyes = c(11, 51), 
                       row.names = c("FairHair", "DarkHair"))
ColTotal <- colSums(eye_hair)
rownames(ColTotal) <- "ColTotal"

eye_hair$RowTotal <- rowSums(eye_hair)
eye_hair <- rbind(eye_hair, ColTotal = colSums(eye_hair))
eye_hair

# Create a table of expected values
row_totals <- eye_hair[-3, "RowTotal"]
row_totals

col_totals <- as.numeric(eye_hair["ColTotal", -3])
col_totals

grand_total <- eye_hair["ColTotal", "RowTotal"]
grand_total

# expected values: use outer product (row_toals * col_totals^T)
expected <- outer(row_totals, col_totals) / grand_total
expected
# add names
dimnames(expected) <- list(
  rownames(eye_hair)[-3],
  colnames(eye_hair)[-3]
)

expected

# Test it
GTest(x = matrix(c(38, 14, 11, 51), nrow = 2))

fisher.test(x = matrix(c(38, 14, 11, 51), nrow = 2))

chisq.test(x = matrix(c(38, 14, 11, 51), nrow = 2))

# Use bigger data and make it fancier
data("HairEyeColor")
head(HairEyeColor)
# Flatten the table
ftable(HairEyeColor)

# Create a margin table ignoring sex
margin.table(HairEyeColor, c(1,2))

# Test it
GTest(margin.table(HairEyeColor, c(1,2)))

# Now visualize it
pdf(paste0(plot_dir, "Association_Plot.pdf"), width = 6, height = 4)
assocplot(margin.table(HairEyeColor, c(1,2)))
dev.off()
################################################################################
# Bootstrapping
# We will use the speed of light data again
## First create a zero-vector
boot_means <- numeric(10000)

for(i in 1:10000) {
  boot_means[i] <- mean(sample(light$speed, replace = T))
}

summary(boot_means)

pdf(paste0(plot_dir, "Bootstrapping_1.pdf"), width = 6, height = 4)
par(mfrow=c(1,2))
hist(light$speed, main = "Original", col= hue_pal()(2)[1], 
     xlab = "Speed of Light", ylab = "Frequency")
hist(boot_means, main = "Bootstrapped", xlab = "sample means", 
     col = hue_pal()(2)[2], ylab = "Frequency")
dev.off()
par(mfrow=c(1,1))

quantile(boot_means, probs = c(0.025, 0.975))

# Use boot library
library(boot)
mymean <- function(light, i) {
  mean (light$speed[i])
  }

boot_means_package <- boot(light, statistic = mymean, R = 10000)
boot.ci(boot_means_package, conf = 0.95)

# Plot it
hist(boot_means_package$t, main = "boot Bootstrapped", xlab = "sample means", 
     col = hue_pal()(2)[2], ylab = "Frequency")
################################################################################
# Multiple Test Correction (FWER)
hypothesis <- c(0.04, 0.03, 0.17, 0.12, 0.01, 0.18, 0.02, 0.04, 0.21, 0.08)
p.adjust(p=hypothesis, method = "holm")

################################################################################
# Power and Sample Size Calculation
# H0 : the new and old drugs have the same effect on blood pressure;
# H1 : the new drug reduces blood pressure more than the old drug.

## delta -> difference
## sd -> standard deviation: calculate from pilot
## Power: 1-betta (1 - Type II error)
## significance level: 0.01 and smaller
## This function will give us the necessary sample size to reach this power
power.t.test(delta = 2, sd = 3.5, power = 0.8, sig.level = 0.01)

# Power vs Sample Size Plot
powers <- seq(0.5, 0.95, 0.01)
sample_size <- numeric(length(powers))

for (i in 1:length(powers)) {
  sample_size[i] <- power.t.test(delta = 2, sd = 3.5, power = powers[i],
                                 sig.level = 0.01)$n
}

pdf(paste0(plot_dir, "Power_Analysis.pdf"), width = 6, height = 4)
plot(powers, sample_size, type = "l", xlab = "power", ylab = "sample size",
     col = hue_pal ()(1), lwd=3)
text(x = 0.6, y = 100, adj = c (0, 0), 
     labels = substitute (paste (delta, "= 2")))
text(x = 0.6, y = 95, adj = c (0, 0),
     labels = substitute (paste ("sd = 3.5")))
text(x = 0.6, y = 90, adj = c (0, 0),
     labels = substitute (paste (alpha, "= 0.01")))
dev.off()
