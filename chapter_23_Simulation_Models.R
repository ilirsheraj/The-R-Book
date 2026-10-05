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

################################################################################
# Spatial Simulation Models
## Set parameters that will keep the simulation running
m <- 0.15
e <- 0.1
s <- (1 - e)

N <- matrix(rep(0, 10000), nrow = 100)
xs <- sample(1:100, replace = TRUE)
ys <- sample(1:100, replace = TRUE)

# Randomly pick 100 patches out of 10000 and populate them
for (i in 1:100){
  N[xs[i], ys[i]] <- 1
}

pdf(paste0(plot_dir, "Meta_population_dynamics.pdf"), width = 6, height = 6)
image(1:100, 1:100, N)
box(col = "black")
dev.off()

# Run a simulation over 1000 generations
for (t in 1:1000) {
  S <- matrix(runif(10000), nrow = 100)
  # If S > s -> die, else survive
  N <- N * (S < s)
  # migration
  im <- floor(sum(N * m))
  placed <- matrix(sample(c(rep (1, im), rep(0, 10000 - im))), nrow = 100)
  N <- N + placed
  N <- apply(N, 2, function(x) ifelse(x > 1, 1, x))
}

pdf(paste0(plot_dir, "Meta_population_1000_Gens.pdf"), width = 6, height = 6)
image(1:100, 1:100, N)
box(col = "black")
dev.off()

# Proportion of occupancy: ~ 0.3
sum(N)/length(N)

# Co-Existence Model: Set-up
pdf(paste0(plot_dir, "Co_existence_setup.pdf"), width = 6, height = 6)
plot(c(0, 1), c(0, 1),
     xaxt = "n", 
     yaxt = "n",
     type = "n", 
     xlab = "", 
     ylab = "")

abline(v = c(1 / 3, 2 / 3))
abline(h = c(1 / 3, 2 / 3))
text(x = rep(c(1 / 6, 3 / 6, 5 / 6), 3)[-5],
     y = rep(c(5 / 6, 3 / 6, 1 / 6), each = 3)[-5],
     labels = (1:9)[-9])
text(3 / 6, 3 / 6, "target cell")
dev.off()

# Set up the margins
margins <- function (N) {
  edges <- matrix (rep (0, 10404), nrow = 102)
  edges[2:101, 2:101] <- N
  edges[1, 2:101] <- N[100,]
  edges[102, 2:101] <- N[1,]
  edges[2:101, 1] <- N[,100]
  edges[2:101, 102] <- N[,1]
  edges[1, 1] <- N[100, 100]
  edges[102, 102] <- N[1, 1]
  edges[1, 102] <- N[100, 1]
  edges[102, 1] <- N[1, 100]
  edges
}

# Neighborhood function
nhood <- function (X, i, j) {
  sum(X[(i - 1):(i + 1), (j - 1):(j + 1)] == 1)
}

# the reproductive rates of species A and B, 
RA <- 3
RB <- 2.0
# the death (d) and survival (s) rates of adults 
d <- 0.25
s <- (1 - d)

# Threshold number (T) of species A
T <- 6

# Half of universe is A, other half is B
N <- matrix(c(rep (1, 5000), rep(2, 5000)), nrow = 100)
image(1:100, 1:100, N)
box(col = "black")

# Run over 1000 generations
for (t in 1:1000) {
  S <- 1 * (matrix (runif(10000), nrow = 100) < s)
  N <- N * S
  space <- 10000 - sum (S)
  nt <- margins(N)
  tots <- matrix(rep (0, 10000), nrow = 100)
  for (a in 2:101) {
    for (b in 2:101) {
      tots[a - 1, b - 1] <- nhood(nt, a, b)
    }
  }
  seedsA <- sum(N == 1) * RA
  seedsB <- sum(N == 2) * RB
  all_seeds <- seedsA + seedsB
  fA <- seedsA / all_seeds
  fB <- 1 - fA
  setA <- ceiling(10000 * fA)
  placed <- matrix(sample (c(rep (1, setA), rep (2, 10000 - setA))), nrow = 100)
  for (i in 1:100) {
    for (j in 1:100) {
      if (N[i,j] == 0) {
        if (placed[i,j] == 2) {
          N[i,j] <- 2
        } else {
          if (tots[i,j] >= T) {
            N[i,j] <- 2
          } else {
            N[i,j] <- 1
          }
        }
      }
    }
  }
}

pdf(paste0(plot_dir, "Co_existence_1000_Gens.pdf"), width = 6, height = 6)
image(1:100, 1:100, N)
box(col = "black")
dev.off()

# Dynamic Interactions: Species and Parasite
# dynamics of the host (r) and the parasite (a) 
r <- 0.4
a <- 0.1
# and the migration rates of the host (Hmr) and parasite (Pmr).
Hmr <- 0.1
Pmr <- 0.9

# Set up the matrices of host and parasite
N <- matrix(rep(0, 10000), nrow = 100)
P <- matrix(rep(0, 10000), nrow = 100)

# Seed with 200 hosts and 100 parasites
N[33,33] <- 200
P[33,33] <- 100

# Nicholson-Bailey Model
host <- function(N, P) {
  N * exp(r - a * P)
}

parasite <- function(N, P) {
  N * (1 - exp (- a * P))
}

# Define margins
margins <- function (N) {
  edges <- matrix (rep (0, 10404), nrow = 102)
  edges[2:101, 2:101] <- N
  edges[1, 2:101] <- N[100,]
  edges[102, 2:101] <- N[1,]
  edges[2:101, 1] <- N[,100]
  edges[2:101, 102] <- N[,1]
  edges[1, 1] <- N[100, 100]
  edges[102, 102] <- N[1, 1]
  edges[1, 102] <- N[100, 1]
  edges[102, 1] <- N[1, 100]
  edges
}

nhood <- function (X, i, j) {
  sum (X[(i - 1):(i + 1), (j - 1):(j + 1)])
}

# Calculate the number of migrants arriving 
migration <- function (edges) {
  migs <- matrix(rep(0, 10000), nrow = 100)
  for (a in 2:101) {
    for (b in 2:101) {
      migs[a - 1, b - 1] <- nhood(edges, a, b)
    }
  }
  migs
}

# Run a simulation for 600 generations
for (t in 1:600) {
  he <- margins(N)
  pe <- margins(P)
  Hmigs <- migration(he)
  Pmigs <- migration(pe)
  N <- N - Hmr * N + Hmr * Hmigs / 9
  P <- P - Pmr * P + Pmr * Pmigs / 9
  Ni <- host(N,P)
  P <- parasite(N,P)
  N <- Ni
}

pdf(paste0(plot_dir, "Host_Parasite_600_Gens.pdf"), width = 6, height = 6)
image(1:100, 1:100, N)
box(col = "black")
dev.off()


