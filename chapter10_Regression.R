# Regression Analysis
# Different from ML, independent variable/predictor is prefferrably called 
# "COVARIATE" and dependent variable is called "OUTCOME" in Statistics
library(scales)

# Let's start with a simple example
caterpillardata <- read.table("Datasets/caterpillar.txt" , header = T)
head(caterpillardata)
attach(caterpillardata)

# Plot it
plot(tannin, growth, pch=16, col = hue_pal()(3)[1],
     xlab = "% of Tannin in Diet", ylab = "Growth Rate")
detach(caterpillardata)

# First simple linear model
lm(growth ~ tannin, data = caterpillardata)

# To get all info, save the model as an object first
caterpillar_model <- lm(growth ~ tannin, data = caterpillardata)
summary(caterpillar_model)

# Or even shorter
summary(lm(growth ~ tannin, data = caterpillardata))
