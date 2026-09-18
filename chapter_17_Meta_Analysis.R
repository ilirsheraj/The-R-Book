# Chapter 17 - Meta Analysis
library(scales)
# install.packages("meta")
# library(meta)
# install.packages("metafor")
library(metafor)
plot_dir <- "Plots/"

# Analysis below will be performed using metafor functions
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

