# Chapter 22 - Bayesian Statistics
library(scales)
# install.packages("rjags")
# install.packages("R2jags")
# library(rjags)
library(R2jags)
library(coda)
library(lattice)

plot_dir <- "Plots/"

growth_data <- read.table("Datasets/regression.txt", header = TRUE)
head(growth_data)

# Linear Regression
summary(lm(growth ~ tannin, data = growth_data))

growth_mod1 <- jags(data = growth_data,
                    parameters.to.save = c ("a", "b", "tau"),
                    n.iter = 10000, 
                    n.burnin = 3000, 
                    n.thin = 1, 
                    n.chains = 3,
                    model.file = "regressionbugs.txt")

growth_mod1
# Parameters (a, b) are quite close to lm() above

col_trace <- hue_pal()(3)

pdf(paste0(plot_dir, "Bayesian_Traceplot.pdf"), width = 8, height = 4)
traceplot(growth_mod1, 
          ask = FALSE, 
          varname = c ("a", "b", "tau"),
          mfrow = c(1,3), 
          col = col_trace)
dev.off()

# We can extract posterior distribution of the three parameters
growth_mod1_mcmc <- as.mcmc(
  growth_mod1$BUGSoutput$sims.matrix[,c ("a", "b", "tau")])

pdf(paste0(plot_dir, "Bayesian_Posterior_Plot.pdf"), width = 6, height = 4)
densityplot(growth_mod1_mcmc)
dev.off()

################################################################################
# Markov-Chain MonteCarlo (MCMC) for Longitudinal Data
fertelizer_data <- read.table("Datasets/fertilizer.txt", header = TRUE)
head(fertelizer_data)
