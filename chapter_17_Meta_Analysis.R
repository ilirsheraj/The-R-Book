# Chapter 17 - Meta Analysis
library(scales)
# install.packages("meta")
# library(meta)
# install.packages("metafor")
library(metafor)
plot_dir <- "Plots/"

# Analysis below will be performed using metafor functions
# Part 1: Scaled Differences
metadata <- read.table("Datasets/metadata.txt", header = TRUE)
metadata

# Calculate effect size: Use weight by sample size (n)
dat_boren <- escalc(measure = "SMD", 
                    m1i = meanT, 
                    m2i = meanC,
                    sd1i = sdT, 
                    sd2i = sdC, 
                    n1i = nT, 
                    n2i = nC,
                    data = metadata)

dat_boren
# yi: effect size
# vi: variance

# Run the meta-analysis using rma() function on the object created above
## First model on fixed effects: "FE"
fixed_boren <- rma(yi = yi, vi = vi, data = dat_boren, method = "FE")
fixed_boren

pdf(paste0(plot_dir, "Fixed_Effects_Meta.pdf"), width = 6, height = 5)
forest(fixed_boren, header = TRUE, slab = paste(dat_boren$study))
dev.off()

## Random Effects Model: DL (DerSimonian and Laird’s Method)
random_boren <- rma(yi = yi, vi = vi, data = dat_boren, method = "DL")
random_boren

pdf(paste0(plot_dir, "Random_Effects_Meta.pdf"), width = 6, height = 5)
forest(random_boren, header = TRUE, slab = paste (dat_boren$study))
dev.off()

# Funnel Plot: 
pdf(paste0(plot_dir, "Random_Effects_Funnel_Plot.pdf"), width = 4, height = 6)
funnel(random_boren, xlab = "Standardized Mean Difference")
dev.off()

# Part 2: Categorical Data
metadata2 <- read.table("Datasets/metadata2.txt", header = TRUE)
metadata2
# Odds Ratio: "OR"
dat_categ <- escalc(measure = "OR", 
                    ai = successT, 
                    bi = failureT,
                    ci = successC, 
                    di = failureC,
                    data = metadata2)
dat_categ

random_categ <- rma(yi = yi, vi = vi, data = dat_categ, method = "DL")
random_categ

pdf(paste0(plot_dir, "Random_Effects_Meta_Categorical_logodd.pdf"), width = 7, height = 5)
forest(random_categ, header = TRUE, slab = paste(dat_categ$study))
dev.off()

pdf(paste0(plot_dir, "Random_Effects_Meta_Categorical_odd.pdf"), width = 7, height = 5)
forest(random_categ, header = TRUE, slab = paste(dat_categ$study), atransf = exp)
dev.off()

# This is a huge field on it's own, but this much exploration was cool
# EOF