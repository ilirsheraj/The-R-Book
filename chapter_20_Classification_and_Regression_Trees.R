# Chapter 20 - Classification and Regression Trees (CARTs)
library(scales)
library(tree)
library(rpart)

plot_dir <- "Plots/"

# Trees overview
pollute <- read.table("Datasets/Pollute.txt", header = TRUE)
head(pollute)

# Fit model: by default tree() takes the first column as response variable
pollute_mod1 <- tree(pollute)
summary(pollute_mod1)

pdf(paste0(plot_dir, "Pollution_tree.pdf"), width = 6, height = 6)
plot(pollute_mod1)
text(pollute_mod1)
dev.off()

# We can also print the values for the entire tree
print(pollute_mod1)

# From the tree, the most important variable is Industry

# Let's further explore it with simple linear regression first
pollute_mod2 <- lm(Pollution ~ ., data = pollute)
summary(pollute_mod2)

# Even in lm(), Industry has the smallest p-value, thus the most influential variable

# The first split is Industry < 748: Create a logical vector
low_ind <- (pollute$Industry < 748)

# Now check for the means of pollution after splitting by industry
ind_means <- tapply(pollute$Pollution, low_ind, mean)
ind_means

pdf(paste0(plot_dir, "Pollution_Tree_First_Split.pdf"), width = 6, height = 4)
plot(pollute$Industry, 
     pollute$Pollution, 
     col = hue_pal()(3)[1],
     xlab = "Industry",
     ylab = "Pollution",
     pch=16)
abline(v = 748, lty = 2, col = hue_pal()(3)[2], lwd=2)
lines(c(0, 748), rep(ind_means[2], 2), col = hue_pal()(3)[3], lwd=2)
lines(c(748, max(pollute$Industry)), rep(ind_means[1], 2), 
      col = hue_pal()(3)[3], lwd=2)
dev.off()

################################################################################
# Part 2: Regression Trees
## Comparison between tree(), rpart() and linear regression: lm()
## refit model 1, now being more explicit
pollute_mod1 <- tree(Pollution ~ ., data = pollute)
summary(pollute_mod1)
print(pollute_mod1)
# terminal nodes designated with *
# deviance is the penultimate number: gets smaller with further splits

# use a better library: rpart (recursive partition)
# library(rpart)
pollute_mod3 <- rpart(Pollution ~ ., data = pollute)
# This gives lots of information in summary
summary(pollute_mod3)
# rpart considers interactions between variables

pdf(paste0(plot_dir, "Pollution_Tree_rpart.pdf"), width = 6, height = 4)
plot(pollute_mod3, margin = 0.1)
text(pollute_mod3, use.n = TRUE, cex = 0.9)
dev.off()

# Lets do some more deeper analysis based on rpart output
temp_big <- factor(pollute$Temp >= 56.25)
ind_small <- factor(pollute$Industry < 597)

# Now look at the interaction between them
pollute_mod4 <- lm(pollute$Pollution ~ temp_big * ind_small)
summary(pollute_mod4)

pollute_mod5 <- lm(pollute$Pollution ~ temp_big * ind_small + pollute$Population)
summary(pollute_mod5)

# In summary, the tree() function seems to be stronger for data inspection, 
# because it shows more detail about the potential interaction structure in the dataframe.
# On the other hand, rpart() is much better at anticipating the results of model simplification.

# Comparison with linear regression

