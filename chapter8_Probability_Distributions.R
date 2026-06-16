# Probability Distributions
library(scales)
plot_dir <- "Plots/"
################################################################################
## Part 1: Continuous Distributions
# Normal Distribution
# dnorm(), pnorm(), qnorm(), rnorm()

# Plot a probability PDF for a random normal sample
mu <- 170
sigma <- 8
pdf(paste0(plot_dir, "normal_dist.pdf"), width = 6, height = 4)
curve(dnorm(x, mu, sigma), 140, 200, xlab = "x",
      ylab = "probability density", col = hue_pal ()(1)[1])

# x values to shade: P(X < 160)
x_fill <- seq(140, 160, length.out = 300)
y_fill <- dnorm(x_fill, mu, sigma)

polygon(c(140, x_fill, 160),
        c(0, y_fill, 0),
        col = hue_pal ()(1)[1])
dev.off()

# Calculate the probability
pnorm(160, mu, sigma)

# Do the same for P(X >185)
mu <- 170
sigma <- 8
curve(dnorm(x, mu, sigma), 140, 200, xlab = "x",
      ylab = "probability density", col = hue_pal ()(1)[1])

# x values to shade: P(X < 160)
x_fill <- seq(185, 200, length.out = 300)
y_fill <- dnorm(x_fill, mu, sigma)

polygon(c(185, x_fill, 200),
        c(0, y_fill, 0),
        col = hue_pal ()(1)[1])

# Calculate the probability of shaded area
1 - pnorm(185, mu, sigma)

# Finally, P(160 < X < 185)
mu <- 170
sigma <- 8
curve(dnorm(x, mu, sigma), 140, 200, xlab = "x",
      ylab = "probability density", col = hue_pal ()(1)[1])

# x values to shade: P(X < 160)
x_fill <- seq(160, 185, length.out = 300)
y_fill <- dnorm(x_fill, mu, sigma)

polygon(c(160, x_fill, 185),
        c(0, y_fill, 0),
        col = hue_pal ()(1)[1])

# Calculate the probability of shaded area
pnorm(185, mu, sigma) - pnorm(160, mu, sigma)

# Calculate probabilities of the three areas above
pnorm(160, mu, sigma)
1 - pnorm(185, mu, sigma)
pnorm(185, mu, sigma) - pnorm(160, mu, sigma)

# Calculate the cut-off values for probability 80%
qnorm(0.8, mu, sigma)

# Generate a random number
rnorm(1, mu, sigma)

# Generate 10 random numbers
rnorm(10, mu, sigma)
#################################################################################
# Uniform Distribution: dunif(x, min, max)
# dunif (), punif (), qunif (), and runif ()
min_val = 5
max_val = 25
pdf(paste0(plot_dir, "uniform_dist.pdf"), width = 6, height = 4)
curve(dunif(x, min=min_val, max=max_val), from=0, to=30, xlab = "x", 
      ylab = "Probability Density",
      main = "Uniform Distribution", 
      col = hue_pal()(1)[1], 
      lwd=2)
dev.off()
dunif(10, 5, 25)

# Shadow parts of the curve
pdf(paste0(plot_dir, "unif_dist2.pdf"), width = 6, height = 4)
curve(dunif(x, min=min_val, max=max_val), from=0, to=30, xlab = "x", 
      ylab = "Probability Density",
      main = "Uniform Distribution", 
      col = hue_pal()(1)[1], lwd=2)

# Area to shade: between 5 and 14.33
x_fill <- seq(5, 14.33, length.out = 200)
y_fill <- dunif(x_fill, min = min_val, max = max_val)

polygon(c(5, x_fill, 14.33),
        c(0, y_fill, 0),
        col = hue_pal()(1)[1])
dev.off()

punif(14.33, 5, 25)

# Calculate the value for probability 0.7
qunif(0.6, min_val, max_val)

# Generate 100 numbers with the same parameters
runif(100, min_val, max_val)
################################################################################
# Chi-Square Distribution: dchisq(DFs)
# dchisq (), pchisq (), qchisq (), and rchisq ()
dfs <- c(3, 10, 20)
cols <- hue_pal()(3)

pdf(paste0(plot_dir, "chisq_dist.pdf"), width = 6, height = 4)
curve(dchisq(x, df = dfs[1]),
      from = 0, to = 40,
      ylim = c(0, 0.26),
      xlab = "x",
      ylab = "Probability density",
      main = "",
      col = cols[1],
      lwd = 2)

for (i in 2:length(dfs)) {
  curve(dchisq(x, df = dfs[i]),
        from = 0, to = 40,
        add = TRUE,
        col = cols[i],
        lwd = 2)
}

legend("topright",
       legend = paste(dfs, "degrees of freedom"),
       fill = cols,
       border = "black",
       bty = "n")
dev.off()
# Density at point 3, 5 df
dchisq(3, 3)
dchisq(3, 10)
dchisq(10, 20)
# Probability for x=3
pchisq(4, 3)
1 - pchisq(10, 3)
qchisq(0.6, 3)

# Generate 100 random chi-square distributed values with 3 df
rchisq(100, 3)
################################################################################
# The F-Distribution: f(df1, df2)
# df (), pf (), qf (), and rf ()
dfs <- list(c(2, 2), c(3, 10), c(5, 50), c(50, 5), c(30, 50))
cols <- hue_pal()(5)

pdf(paste0(plot_dir, "f_dist.pdf"), width = 6, height = 4)
curve(df(x, df1 = dfs[[1]][1], df2 = dfs[[1]][2]),
      from = 0, to = 4,
      ylim = c(0, 1.4),
      xlab = "x",
      ylab = "Probability density",
      main = "",
      col = cols[1],
      lwd = 2)

for (i in 2:length(dfs)) {
  curve(df(x, df1=dfs[[i]][1], df2=dfs[[i]][2]),
        from = 0, to = 4,
        add = TRUE,
        col = cols[i],
        lwd = 2)
}

legend_labels <- sapply(dfs, function(d) {
  paste(d[1], "and", d[2], "degrees of freedom")
})

legend("topright",
       legend = legend_labels,
       fill = cols,
       border = "black",
       bty = "n")
dev.off()
df(3, 3, 10)
pf(3, 3, 10)
qf(0.6, 3, 10)
# Generate 100 random numbers
rf(100, 3, 10)
################################################################################
# Students t-test: df(df)
# dt (), pt (), qt (), and rt ()
r <- c(2, 5, 20, 30)
cols <- hue_pal()(5)

pdf(paste0(plot_dir, "t_dist.pdf"), width = 6, height = 4)
curve(dt(x, r[1]), 
      from =-4, to=4,
      ylim=c(0, 0.42),
      xlab = "x",
      ylab = "Probability Density",
      col=cols[1],
      lwd=2)
# Add the rest
for (i in 2:length(r)){
  curve(dt(x, r[i]),
        from=-4, to=4,
        add=TRUE,
        col=cols[i],
        lwd=2)
}

legend("topright",
       legend = paste("r =", r),
       fill = cols,
       border = "black",
       bty = "n")
dev.off()

dt(0, 30)
pt(0, 30)
qt(0.6, 2)
# Generate 100 random t-distributed numbers
rt(100, 2)
################################################################################
# Gamma Distribution (alpha, betta)
# dgamma (), pgamma (), qgamma (), and rgamma ()
params <- list(c(1, 1), c(2, 2), c(5, 5), c(10, 5))
cols <- hue_pal()(4)

pdf(paste0(plot_dir, "gamma_dist.pdf"), width = 6, height = 4)
curve(dgamma(x, shape = params[[1]][1], rate = params[[1]][2]),
      from = 0, to = 4,
      ylim = c(0, 1),
      xlab = "x",
      ylab = "Probability density",
      main = "",
      col = cols[1],
      lwd = 2)

for (i in 2:length(params)) {
  curve(dgamma(x, shape = params[[i]][1], rate = params[[i]][2]),
        from = 0, to = 4,
        add = TRUE,
        col = cols[i],
        lwd = 2)
}

legend_labels <- sapply(params, function(d) {
  paste("shape =", d[1], "rate =", d[2])
})

legend("topright",
       legend = legend_labels,
       fill = cols,
       border = "black",
       bty = "n")
dev.off()

dgamma(1, 10, 5)
pgamma(1, 10, 5)
qgamma(0.5, 10, 5)
rgamma(100, 10, 5)
################################################################################
# Exponential Distribution: dexp(lambda)
# dexp (), pexp (), qexp (), and rexp ()
rate = c(2, 1, 0.5)
cols = hue_pal()(3)

pdf(paste0(plot_dir, "exponential_dist.pdf"), width = 6, height = 4)
curve(dexp(x, rate = rate[1]), from = 0, to=5, 
      xlab = "x",
      ylab = "Probability Density",
      main = "",
      col = cols[1],
      lwd=2)

for (i in 2:length(rate)){
  curve(dexp(x, rate = rate[i]), from=0, to=5, 
        xlab = "x",
        ylab = "Probability Density",
        main = "",
        add = TRUE,
        col = cols[i],
        lwd=2)
}

legend("topright",
       legend = paste("Rate =", rate),
       fill = cols,
       border = "black",
       bty = "n")
dev.off()

dexp(2, 1)
pexp(2, 1)
qexp(0.5, 2)
rexp(100, 2)
################################################################################
# Beta Distribution: dbeta(a,b)
# dbeta (), pbeta (), qbeta (), and rbeta ()
params <- list(c(0.1, 0.1), c(5, 1), c(1,5), c(10, 10), c(1,1))
cols = hue_pal()(length(params))

pdf(paste0(plot_dir, "beta_dist.pdf"), width = 6, height = 4)
curve(dbeta(x, shape1 = params[[1]][1], shape2 = params[[1]][2]),
      from = 0, to=1,
      ylim = c(0,5),
      main = "",
      xlab = "x",
      ylab = "Probability Density",
      col=cols[1],
      lwd=2)

for (i in 2:length(params)){
  curve(dbeta(x, shape1 = params[[i]][1], shape2 = params[[i]][2]),
        from = 0, to=1,
        add = TRUE,
        main = "",
        xlab = "x",
        ylab = "Probability Density",
        col=cols[i],
        lwd=2)
}

legend_labels <- sapply(params, function(d) {
  paste("a =", d[1], "b =", d[2])
})

legend("topright",
       legend = legend_labels,
       fill = cols,
       border = "black",
       bty="n")
dev.off()

dbeta(0.1, 1, 5)
pbeta(0.1, 1, 5)
qbeta(0.5, 1, 5)
rbeta(100, 1, 5)
################################################################################
# Lognormal Distribution: dlnorm(mean,sd)
# dlnorm (), plnorm (), qlnorm (), and rlnorm ()
params <- list(c(1, 2), c(0, 0.5), c(-0.5, 0.5))
cols <- hue_pal()(length(params))

pdf(paste0(plot_dir, "log_normal_dist.pdf"), width = 6, height = 4)
curve(dlnorm(x, meanlog = params[[1]][1], sdlog = params[[1]][2]),
      from = 0, to = 3,
      ylim = c(0, 1.5),
      main = "",
      xlab = "x",
      ylab = "Probability Density",
      col = cols[1],
      lwd = 2)

for(i in 2:length(params)){
  curve(dlnorm(x, meanlog = params[[i]][1], sdlog = params[[i]][2]),
        from = 0, to = 3,
        ylim = c(0, 1.5),
        add = TRUE,
        main = "",
        xlab = "x",
        ylab = "Probability Density",
        col = cols[i],
        lwd = 2)
}

legend_labels <- sapply(params, function(x){
  paste("Mu = ", x[1], "Sigma =", x[2])})


legend("topright",
       legend = legend_labels,
       fill = cols,
       border = "black",
       bty = "n")

dev.off()

dlnorm(1, -0.5, 0.5)
plnorm(1, -0.5, 0.5)
qlnorm(0.5, -0.5, 0.5)
rlnorm(100, -0.5, 0.5)
################################################################################
# Logistic Distribution: dlogis(mu, scale>0)
# dlogis (), plogis (), qlogis (), and rlogis ()
params <- list(c(-5, 0.5), c(0, 0.8), c(3, 2))
cols <- hue_pal()(length(params))

pdf(paste0(plot_dir, "logistic_dist.pdf"), width = 6, height = 4)
curve(dlogis(x, location = params[[1]][1], scale = params[[1]][2]),
      from = -10, to = 12,
      main = "",
      xlab = "x",
      ylab = "Probability Density",
      col = cols[1],
      lwd = 2)

for (i in 2:length(params)){
  curve(dlogis(x, location = params[[i]][1], scale = params[[i]][2]),
        from = -10, to = 12,
        add = TRUE,
        main = "",
        xlab = "x",
        ylab = "Probability Density",
        col = cols[i],
        lwd = 2)
}

legend_labels <- sapply(params, function(x){
  paste("Mu =", x[1], "Sigma =", x[2])
})

legend("topright",
       legend = legend_labels,
       fill = cols,
       border = "black",
       bty = "n")
dev.off()

dlogis(1, 0, 0.8)
plogis(1, 0, 0.8)
qlogis(0.4, 0, 0.8)
rlogis(100, 0, 0.8)
################################################################################
# Weibull Distribution: dweibull(shape, scale>0)
params <- list(c(1, 0.5), c(2,3), c(2,10), c(10,10))
cols <- hue_pal()(length(params))

pdf(paste0(plot_dir, "weibull_dist.pdf"), width = 6, height = 4)
curve(dweibull(x, shape = params[[1]][1], scale = params[[1]][2]),
      from = 0, to = 15,
      ylim = c(0, 1.2),
      main = "",
      xlab = "x",
      ylab = "Probability Density",
      col = cols[1],
      lwd = 2)

for(i in 2:length(params)){
  curve(dweibull(x, shape = params[[i]][1], scale = params[[i]][2]),
        from = 0, to = 15,
        ylim = c(0, 1.2),
        add = TRUE,
        main = "",
        xlab = "x",
        ylab = "Probability Density",
        col = cols[i],
        lwd = 2)
}

legend_labels <- sapply(params, function(x){
  paste("Alpha =", x[1], "Beta =", x[2])
})

legend("topright",
       legend = legend_labels,
       fill = cols,
       border = "black",
       bty = "n")
dev.off()

dweibull(2, 2, 3)
pweibull(2, 2, 3)
qweibull(0.5, 2, 3)
rweibull(100, 2, 3)
################################################################################
## Part 2: Discrete Distributions
# Bernoulli Distribution: p = success, 1-p = failure
# Flip a coin 100 times
coin <- rbinom(100, size = 1, prob = 0.5)
mean(coin)
var(coin)
################################################################################
# Binomial Distribution: dbinom(x, n, p)
# dbinom (), pbinom (), qbinom (), and rbinom ()
library(scales)

# Getting 1-4 successes in four trials with p=0.3
n <- 4
p <- 0.3
x <- 0:n
prob <- dbinom(x, size = n, prob = p)

pdf(paste0(plot_dir, "binomial_dist.pdf"), width = 6, height = 4)
barplot(prob,
        names.arg = x,
        ylim = c(0, 0.42),
        xlab = "Number of successes",
        ylab = "Probability",
        main = paste("Binom: p =",p, "n =", n),
        col = hue_pal()(1)[1],
        border = "black",
        las = 1)
dev.off()

dbinom(1, 4, 0.3)
pbinom(1, 4, 0.3)
qbinom(0.5, 4, 0.3)
rbinom(100, 10, 0.6)
################################################################################
# Geometric Distribution: Number of trials before we see success
# dgeom (), pgeom (), qgeom (), and rgeom ()
n <- 10
p <- 0.3
x <- 0:n
prob <- dgeom(x, prob = p)

pdf(paste0(plot_dir, "Geometric_dist.pdf"), width = 6, height = 4)
barplot(prob,
        names.arg = x,
        ylim = c(0, 0.3),
        xlab = "Number of successes",
        ylab = "Probability",
        main = paste("Geom: p =",p, "n =", n),
        col = hue_pal()(1)[1],
        border = "black",
        las = 1)
dev.off()
dgeom(1, 0.3)
pgeom(1, 0.3)
qgeom(0.3, 0.3)
rgeom(100, 0.3)
################################################################################
# Hypergeometric Distribution: dhyper(N, m, n)
# Number in population, N, number of specified type, m, number chosen, n.
# dhyper (), phyper (), qhyper (), and rhyper ()
# We have 20 balls in an urn with the following colors
blue <- 16 # Blue balls in the urn
non_blue <- 4 # Non-blue balls in the urn
sample_size <- 5 # Sample we choose
x <- 0:sample_size

prob <- dhyper(x,
               m = blue,
               n = non_blue,
               k = sample_size)

pdf(paste0(plot_dir, "Hypergeometric_dist.pdf"), width = 6, height = 4)
barplot(prob,
        names.arg = x,
        ylim = c(0, 0.5),
        xlab = "Number of blue balls in sample",
        ylab = "Probability",
        main = "",
        col = hue_pal()(1)[1],
        border = "black",
        las = 1,
        cex.lab = 1.3,
        cex.axis = 1.1,
        cex.names = 1.1)
dev.off()

dhyper(5, 16, 4, 5)
phyper(2, 16, 4, 5)
qhyper(0.5, 16, 4, 5)
rhyper(100, 16, 4, 5)
################################################################################
# Poisson Distribution: dpois(lambda)
# dpois (), ppois (), qpois (), and rpois ()
n <- 120
p <- 0.05
lambda <- n * p
k <- 0:15
prob <- dpois(k, lambda)

pdf(paste0(plot_dir, "Poisson_dist.pdf"), width = 6, height = 4)
barplot(prob,
        names.arg = k,
        ylim = c(0, 0.17),
        xlab = "Number of Observed Events",
        ylab = "Probability",
        main = "",
        col = hue_pal()(1)[1],
        border = "black",
        las = 1,
        cex.names = 0.7)
dev.off()

dpois(5, lambda)
ppois(5, lambda)
qpois(0.5, lambda)
rpois(100, lambda)
################################################################################
# Negative Binomial Distribution: dnbinom()
# dnbinom (), pnbinom (), qnbinom (), and rnbinom ()
dbinom(x, 5, 0.4)
x <- 0:25
prob <- dnbinom(x, size = 5, prob = 0.4)

pdf(paste0(plot_dir, "Neg_Binom_dist.pdf"), width = 6, height = 4)
barplot(prob,
        names.arg = x,
        ylim = c(0, 0.11),
        xlab = "Number of Observed Events",
        ylab = "Probability",
        main = "",
        col = hue_pal()(1)[1],
        border = "black",
        las = 1)
dev.off()

dnbinom(5, 5, 0.4)
pnbinom(3, 5, 0.4)
qnbinom(0.5, 5, 0.4)
rnbinom(100, 5, 0.4)
