# Chapter 23 - Simulation Models
library(scales)

plot_dir <- "Plots/"

# Density-dependent processes: Try different lambda
# This is Bifurcation process :)
quad_fn <- function(x1, lambda, n) {
  x <- numeric(n)
  x[1] <- x1
  
  for (t in 2:n) {
    x[t] <- lambda * x[t-1] * (1 - x[t - 1])
  }
  
  plot(1:n, x, 
       type = "l", 
       ylim = c(0, 1),
       xlab = "Time",
       ylab = "Population",
       main = substitute(paste(lambda, " = ", a), list(a = lambda)),
       cex.main =2,
       lwd = 2,
       col = hue_pal()(100)[sample(1:100, 1)])
}

pdf(paste0(plot_dir, "Density_Dependent_Processes.pdf"), width = 6, height = 4)
# lambda = 1: population collapse (extinction)
quad_fn(0.6, 1, 40)
# No change after initial drop: Stable Point Equilibrium
quad_fn(0.6, 2, 40)
# Two-Point Cycles
quad_fn(0.6, 3.3, 40)
# 4-Point Cycles
quad_fn(0.6, 3.5, 40)
# Critical Value at lambda = 3.6
quad_fn(0.6, 3.6, 100)
# Chaotic Behavior
quad_fn(0.6, 4, 100)

dev.off()

# Function to investigate the route to chaos
chaos_fn <- function(x1, lambda, n1, n2) {
  x <- numeric(n2)
  x[1] <- x1
  
  for (t in 2:n2) {
    x[t] <- lambda * x[t-1] * (1 - x[t - 1])
  }
  
  x[n1:n2]
}

pdf(paste0(plot_dir, "Bifurcation_Process.pdf"), width = 6, height = 4)
plot(c(2,4), c(0,1), 
     type="n", 
     xlab = substitute(paste(lambda)),
     ylab = "Population")

for (i in seq (2, 4, 0.01)) {
  outs <- chaos_fn(0.6, i, 380, 400)
  points(rep(i, length(outs)), outs, col = hue_pal()(201)[i * 100 - 199], 
         pch=16, cex = 0.5)
}
dev.off()


