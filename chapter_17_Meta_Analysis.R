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

pdf(paste0(plot_dir, "Growth_Barplot.pdf"), width = 6, height = 5.5)