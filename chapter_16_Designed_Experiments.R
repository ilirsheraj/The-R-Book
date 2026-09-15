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
