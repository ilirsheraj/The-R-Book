# Chapter 2: Technical Background

## Logarithms and Exponentials
library(scales)
x <- seq(0, 10, 0.1)

# hue_pal(h)(c)[l]
plot(x, exp(x), ylab="y", type="l", col= hue_pal()(2)[1])
plot(x, log(x), ylab="z", type="l", col= hue_pal()(2)[2])

## Trigonometric Functions
x <- seq(0, 2*pi, 0.01)
sinx <- sin(x)
cosx <- cos(x)
tanx <- tan(x)

plot(x, sinx, ylim=c(-3, 3), ylab="", type="l", col=hue_pal()(3)[1])
lines(x, cosx, col=hue_pal()(3)[2])
lines(x, tanx, col=hue_pal()(3)[3])
legend(2, 3, legend = c("sin(x)", "cos(x)", "tan(x)"), lwd=rep(1, 3), 
       col=hue_pal()(3), bty="n")

## Power Laws
x <- seq(0, 2, 0.01)
plot(x, 1/(x^2), type="l", ylab=expression(x^b), col=hue_pal()(6)[1], ylim=c(0,4))
lines(x, 1/x, col=hue_pal()(6)[2])
abline(h=1, col=hue_pal()(6)[3])
lines(x, x^(0.5), col=hue_pal()(6)[4])
lines(x, x, col=hue_pal()(6)[5])
lines(x, x^2, col=hue_pal()(6)[6])
legend(0.9, 4, bty="n", legend=c("b=-2", "b=-1", "b=0", "b=0.5", "b=1", "b=2"),
       col = hue_pal()(6), lwd=1)

## Polynomial Functions
x <- seq (0, 10, 0.01)

# Split the plotting area into 4
par(mfrow = c(2, 2))
## define the first function
y = 5*x - 0.2*x^2
# Hide the axis: xaxt = "n" and yaxt = "n"
plot(x, y, type = "l", col = hue_pal ()(4)[1], xaxt = "n", yaxt = "n",
     xlab = "", ylab = "", main = expression(y = 5*x - 0.2*x^2))

## Define the second function
y = 5*x - 0.4*x^2
plot(x, y, type = "l", col = hue_pal ()(4)[2], xaxt = "n", yaxt = "n",
     xlab = "", ylab = "", main = expression(y = 5*x - 0.4*x^2))

## Define the Third Function
y = 2 + 4*x - 0.6*x^2 + 0.04*x^3
plot(x, y, type = "l", col = hue_pal ()(4)[3], xaxt = "n", yaxt = "n",
     xlab = "", ylab = "", main = expression(y = 2 + 4*x - 0.6*x^2 + 0.04*x^3))

## Define the Fourth Function
y = 2 + 4*x + 2*x^2 - 0.6*x^3 + 0.04*x^4
plot(x, y, type = "l", col = hue_pal ()(4)[4], xaxt = "n", yaxt = "n",
     xlab = "", ylab = "", main = expression(y = 2 + 4*x + 2*x^2 - 0.6*x^3 + 0.04*x^4))

par(mfrow = c(1, 1))


# More Plotting
x <- seq (0, 10, 0.01)
y = x / (2 + 5 * x)
plot (x, y, type = "l", col = hue_pal ()(3)[1], xaxt = "n", yaxt = "n")
y = 1 / (x - 2 + 4 / x)
plot (x, y, type = "l", col = hue_pal ()(3)[2], xaxt = "n", yaxt = "n")
y = 1 / (x^2 - 2 + 4 / x)
plot (x, y, type = "l", col = hue_pal ()(3)[3], xaxt = "n", yaxt = "n")

# Gamma Function
factorial(6)

# gamma(t+1) = factorial(t)
gamma(7)

# Gamma works on R, Not only integers
gamma(7.5)
# Factorial too
factorial(6.5)

## Plot Gamma
t <- seq (0.2 ,4, 0.01)
plot (t, gamma (t),type = "l", col = hue_pal ()(2)[1])
abline (h = 1, lty = 2, col = hue_pal ()(2)[2])

# Sigmoid Functions
par(mfrow = c(2, 2))
x <- seq (0, 10, 0.01)
y <- 100 / (1 + 90 * exp(-1 * x))
plot (x, y, type = "l", col = hue_pal ()(4)[1], main = "Three-parameter logistic")
y <- 20 +100 / (1 + exp (0.8 * (3 - x)))
plot (x, y, ylim = c (0,140), type = "l", col = hue_pal ()(4)[2], main = "Four-parameter logistic")
x <- seq (0, 100, 0.1)
y <- 50 * exp (-5 * exp (-0.08 * x))
plot (x, y, type = "l", col = hue_pal ()(4)[3], main = "Positive Gompertz")
x <- seq (-200, 100, 0.1)
y <- 100 * exp (-exp (0.02 * x))
plot (x, y, type = "l", col = hue_pal ()(4)[4], main = "Negative Gompertz")
par(mfrow = c(1, 1))

# Biexponential Functions
x <- seq (0, 10, 0.01)
# Define a function
biexp_plot <- function (a, b, c, d, i) {
  y <- a * exp (b * x) + c *exp (d * x)
  plot (x, y, type = "l", col = hue_pal ()(4)[i], lwd = 3)
}

par(mfrow = c(2, 2))
biexp_plot(10, -0.8, 10, -0.05, 1)
biexp_plot(10, -0.8, 10, 0.05, 2)
biexp_plot(200, 0.2, -1, 0.7, 3)
biexp_plot(200, 0.05, 300, -0.5, 4)
par(mfrow = c(1, 1))

###############################################################################
# Matrices
A = matrix(c(1, 0, 4, 2, -1, 1), nrow = 3)
A

B = matrix(c(1, -1, 2, 1, 1, 0), nrow = 2)
B

# Matrix Multiplication
A %*% B # 3x2 by 2x3 = 3x3

B %*% A # 2x3 by 3x2 = 2x2

# Define a diagonal matrix
C <- diag(1, nrow = 3, ncol = 3)
C

diag(C) <- 1:3
C
diag(C)

# More matrices
M <- cbind(x = 1:5, y = rnorm(5))
M
cov(M)
diag(cov(M))

# Determinants
A <- matrix(c(1, 2, 4, 2, 1, 1, 3, 1, 2), nrow = 3)
A
det(A)

# Multiply one row or column and determinant changes by the same factor
B <- A
B[3,] <- 3 * B[3,]
B
det(B)

# Create a Singular Matrix
C <- A
C[,2] <- 4 * C[,1]
C
det(C)

# Inverse Mattrix: solve
A
solve(A)
# Inverse of Inverse -> Original
solve(solve(A))

# Eiganvalues and Eiganvectors
## Leslie Matrix
L <- matrix(c(0, 0.7, 0, 0, 6, 0, 0.5, 0, 3, 0, 0, 0.3, 1, 0, 0, 0), nrow = 4)
L

# Population size vector
v <- matrix(c(45, 20, 17, 3), ncol = 1)
v

# Firt year
L %*% v

# Markov Matrix for 40 generations
age_profile <- matrix (nrow = 40, ncol = 4)
for (i in 1:40) {
  v <- L %*% v
  age_profile[i,] <- v
}
matplot(1:40, log (age_profile), type = "l", xlab = "age", 
        ylab = "ln (population)", col = hue_pal ()(4), lwd = 2)
legend(5, 35, legend = c ("juveniles", "1-year olds", "2-year olds", "3-year olds"),
       col = hue_pal ()(4),lty = 1:4)

# Decompose L
eigen(L)

# First eiganvalue
eigen(L)$values[1]

# Solving Systems of Linear Equations
A <- matrix(c(3, 1, 4, 2), nrow = 2)
A
k <- matrix(c(12, 8), nrow = 2)
k
# Same as Gauss-Jordan Elimination
solve (A, k)

################################################################################
# Calculus
## Differentiation
D(expression(2*x^3), "x")
dxy <- deriv(~ 2 * x^3, "x")
eval(dxy, envir = list(x = 1:3))

D(expression (log(x)), "x")

D(expression (a * exp (-b * x)), "x")

D(expression (a/(1 + b * exp (-c * x))), "x")

trig_exp <- expression(sin(x + 2 * y))
D(trig_exp, "x")
D(trig_exp, "y")

## Integration
integrate(dnorm, lower = -1.96, upper = 1.96)
pnorm(1.96) - pnorm(-1.96)

# Estimate the area under a function
our_fun <- function(x){
  exp(-x)
}

integrate(our_fun, lower = 0, upper = Inf)

x <- seq (0, 20, 0.01)
y <- exp(-x)
plot(x, y, type = "l", col = hue_pal ()(2)[1])
polygon(x = c (0, x, 20), y = c (0, y, 0), col = hue_pal ()(2)[2], border = NA)

## Differential Equations
library(deSolve)

# Define the equation
phmodel <- function (t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dv <- r * v * (K - v) / K - b * v * n
    dn <- c * v * n - d * n
    result <- c (dv, dn)
    list (result)
  }
  )
}

times <- seq(0, 500)
parameters <- c(r = 0.4, K = 1000, b = 0.02, c = 0.01, d = 0.3)
initial <- c(v = 50, n = 10)
phm_output <- ode(y = initial, time = times, func = phmodel, parms = parameters)
head(phm_output)

plot(phm_output[,1], phm_output[,2], ylim = c (0, max (phm_output[,2:3])),
      type = "l", ylab = "abundance", xlab = "time", col = hue_pal ()(2)[1])
lines(phm_output[,1], phm_output[,3], col = hue_pal ()(2)[2])
legend(300, 60, legend = c ("plant", "herbivore"), lty = 1, col = hue_pal ()(2))

# A Different kind of plot
plot(phm_output[,2], phm_output[,3], xlim = c (0, max (phm_output[,2:3])),
     ylim = c (0, max (phm_output[,2:3])), type = "l",
     ylab = "plant", xlab = "herbivore", col = hue_pal ()(1))

################################################################################
# Probability
## CLT
# Create a uniform distribution from 0 to 10
unif_data <- runif(6000, min = 0, max = 10)
# Convert it to a matrix with 6 columns
unif_samples <- matrix(unif_data, ncol = 6)
hist(unif_data, main = "Uniform Distribution", xlab = "", col = hue_pal ()(3)[1], breaks = 11)

# Check the means of all 1000 samples (1000 rows x 6 columns)
hist(rowMeans(unif_samples), main = "", xlab = "", col = hue_pal ()(3)[2],
      breaks = 21, freq = F)

# Calculate the parameters
mean(rowMeans(unif_samples))
sd(rowMeans(unif_samples))

# Add Normal curve on histogram
x <- seq(0, 10, 0.01)
lines(x, dnorm(x, mean = 5, sd = sqrt (100 / 72)), col = hue_pal ()(3)[3],
       lwd = 2)

# Samples size 40
unif_data <- runif(200000, min = 0, max = 10)
unif_samples <- matrix(unif_data, ncol = 40)
hist(unif_data, main = "", xlab = "", col = hue_pal ()(3)[1], breaks = 11)
y <- hist(rowMeans (unif_samples), plot = F)
x <- seq(0, 10, 0.01)
hist(rowMeans (unif_samples), main = "", xlab = "", col = hue_pal ()(3)[2],
     breaks = 41, freq = F, 
     ylim = c(0, max (dnorm (x, mean = 5,sd = sqrt (100 / (12 * 40))), y$density)))
lines(x, dnorm (x, mean = 5, sd = sqrt (100 / (12 * 40))),
      col = hue_pal ()(3)[3], lwd = 2)

## Gamma Distribution
gamma_data <- rgamma(200000, shape = 12, rate = 4)
gamma_samples <- matrix(gamma_data, ncol = 40)
hist(gamma_data, main = "", xlab = "", col = hue_pal ()(3)[1], breaks = 20)
x <- seq (2.4, 4, 0.01)
y <- hist(rowMeans (gamma_samples), plot = F)
hist(rowMeans (gamma_samples), main = "", xlab = "", col = hue_pal ()(3)[2],
     breaks = 41, freq = F, 
     ylim = c (0, max (dnorm (x, mean = 3, sd = sqrt (12 / (4 * 4 * 40))), y$density)))
lines(x, dnorm (x, mean = 3, sd = sqrt (12 / (4 * 4 * 40))),
      col = hue_pal ()(3)[3], lwd = 2)
mean(rowMeans(gamma_samples))
sd(rowMeans(gamma_samples))
