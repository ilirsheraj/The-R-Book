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

