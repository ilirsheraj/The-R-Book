# Part 1: Writing R Functions
library(scales)

# define a function that calculates the arithemtic mean of a vector
arithmetic_mean <- function(x) {
  sum(x) / length(x)
}

y <- c(3, 3, 4, 5, 5)
arithmetic_mean(y)
mean(y)

# Median of a vector
## Break it into steps
sort(y)
length(y)/2
# Take the ceiling
ceiling(length(y) / 2)

# This will pick the third element
sort(y)[ceiling(length(y) / 2)]

# Put it in a function
med <- function(x) {
  odd_even <- length (x) %% 2
  if (odd_even == 0) {
    # If the vector has an even number of elements
    med_x <- (sort(x)[length(x) / 2] + sort(x)[1 + length (x) / 2]) / 2
  } else {
    # If the vector has an odd number of elements
    med_x <- sort(x)[ceiling (length(x) / 2)]
  }
  med_x
}

med(y)
med(y[-1])

# Geometric Mean
insects <- c(1, 10, 1000, 10, 1)
mean(insects) # extremely skewed to the right

# Geometric mean: x1*x2*...*xn = log(x1) + log(x2) + ... + log(xn)
# mean(log(x1) + log(x2) + ... + log(xn)) -> return to original: exp()
exp(mean(log(insects)))

g_mean <- function(x) {
  exp(mean(log(x)))
}

g_mean(insects)

# Harmonic Mean
harmonic <- function(x) {
  1/mean(1/x)
}

harmonic(c(1, 2, 4, 1))

# Variance
y <- c(13, 7, 5, 12, 9, 15, 6, 11, 9, 7, 12)

samp_var <- function(x) {
  sum((x - mean (x))^2) / (length(x) - 1)
}

samp_var(y)

# Use base function
var(y)

# Ratio test (Fischer's Test)
vrt <- function(x,y) {
  # Give two vectors, don't have to be the same length
  # Calculate the variance of vector 1
  v1 <- var(x)
  # Calculate the variance of vector 2
  v2 <- var(y)
  if (var(x) > var(y)) {
    # If varianc eof x is larger, get the ratio
    vr <- var(x) / var(y)
    df1 <- length(x)-1
    df2 <- length(y)-1
  } else {
    # If variance of y is larger, get their ratio
    vr <- var(y) / var(x)
    df1 <- length(y) - 1
    df2 <- length(x) - 1
  }
  # Calculate the p-value
  p_val <- 2 * (1 - pf(vr, df1, df2))
  p_val
}

a <- rnorm(20, 15, 2)
b <- rnorm(10, 15, 4)

vrt(a,b)

# With Base R
var.test(a,b)

# Standard Error of the Mean
se <- function(x) {
  sqrt(var (x) / length(x))
}

# Confidence Interval
ci95 <- function(x) {
  t_value <- qt(0.975, length(x) - 1)
  ci <- t_value * se (x)
  return (list(lower_CI = mean (x) - ci,
               upper_CI = mean (x) + ci))
}

x <- rnorm(150, 25, 3)
se(x)
ci95(x)


x <- 5:60
y <- numeric(length(x))
for (i in 5:60){
  vals <- rnorm(i, 25, 3)
  y[i-4] <- se(vals)
}

plot(x, y)
scatter.smooth(x, y , span = 1/3, pch=19, col = "red", lpars = list(col = "blue", lwd = 2))

# Deparsing Function
error_bars <- function (yv, z, nn){
  xv <- barplot(yv, ylim = c(0, max(yv) + max(z)), col = hue_pal()(2)[1],
                names = nn, ylab = deparse(substitute (yv)))
  g = (max (xv) - min(xv)) / 50 # 2g is width of error bar
  for (i in 1:length(xv)) {
    lines(c(xv[i], xv[i]), c(yv[i] + z[i], yv[i] - z[i]),
           col = hue_pal()(2)[2], lwd = 2)
    lines(c(xv[i] - g, xv[i] + g), c(yv[i]+z[i], yv[i] + z[i]),
           col = hue_pal()(2)[2], lwd = 2)
    lines(c(xv[i] - g, xv[i] + g), c(yv[i]-z[i], yv[i] - z[i]),
           col = hue_pal()(2)[2], lwd = 2)
  }
}

comp <- read.table("Datasets/competition.txt", header = T, colClasses = list (clipping = "factor"))
head(comp)

se <- tapply(comp$biomass, comp$clipping, function (x) sd(x)/sqrt(length(x)))
labels <- as.character(levels(comp$clipping))
biomass <- tapply(comp$biomass, comp$clipping, mean)
error_bars(biomass, se, labels)

# Switch Function
central_tend <- function(y, measure) {
  switch(measure,
         Mean = arithmetic_mean(y),
         Median = med(y),
         Geometric = g_mean(y),
         Harmonic = harmonic(y),
         stop ("Measure not included"))
}

our_data <- rnorm(100, 10, 2)
central_tend(our_data, "Median")
central_tend(our_data, 2)

# Make Median defaults
central_tend <- function(y, measure = "Median") {
  switch(measure,
         Mean = arithmetic_mean(y),
         Median = med(y),
         Geometric = g_mean(y),
         Harmonic = harmonic(y),
         stop ("Measure not included"))
}

central_tend(y)
central_tend(y, measure = "Geometric")

# Functions with multiple arguments
many_means_vars <- function(...) {
  data <- list(...)
  n <- length(data)
  means <- numeric(n)
  vars <- numeric(n)
  for (i in 1:n) {
    means[i] <- mean(data[[i]])
    vars[i] <- var(data[[i]])
  }
  print(means)
  print(vars)
}

x <- rnorm(100)
y <- rnorm(200)
z <- rnorm(300)

many_means_vars(x, y, z)
many_means_vars(x, z)
many_means_vars(y)

# Error Handling
try_dataset <- function(x) {
  options(show.error.messages = FALSE)
  options(warn = -1)
  try_read <- try(read.table(x, header = T))
  if (class(try_read) == "try-error") {
    cat("The file named", x, "does not appear to exist in the working directory\n")
  } else {
    data_in <- read.table(x, header = T)
    cat("The first records in", x, "are\n")
    print(head (data_in))
  }
  options(show.error.messages = TRUE)
  options(warn = 0)
}

our_data <- try_dataset("compitition.txt")

our_data <- try_dataset ("Datasets/competition.txt")

# Function Output
simple.function <- function(a, b) {
  a2 <- a^2
  b2 <- b^2
  return(list(asquared = a2, bsquared = b2))
}

run1 <- simple.function(6, 13)
run1

run1[[1]]
run1[1]
run1$asquared

# More text inside
simple.function <- function(a, b) {
  a2 <- a^2
  b2 <- b^2
  cat("The value of", a, "squared is", a2,
      "\nand the value of", b, "squared is", b2, "\n\n")
  return (list(asquared = a2, bsquared = b2))
}
run2 <- simple.function(6.3, pi)

# Simple linear model for demo
tannin <- read.table("Datasets/tannin.txt", header = T)
head(tannin)

tannin_model <- lm(growth ~ tannin, data = tannin)
summary.aov(tannin_model)

df1 <-unlist(summary.aov(tannin_model)[[1]] [1])[1]
df1
df2 <-unlist(summary.aov(tannin_model)[[1]] [1])[2]
df2
ss1 <-unlist(summary.aov(tannin_model)[[1]] [2])[1]
ss1
ss2 <-unlist(summary.aov(tannin_model)[[1]] [2])[2]
ss2

#################################################################################
# Part 2: Structure of R Objects
y <- seq(0.9 ,0.3, -0.1)
str(y)

spino <- read.table("Datasets/spino.txt", header = T, colClasses = rep("factor", 2))
head(spino)
str(spino)

levels(spino$condition)
levels(spino$treatment)

# Back to tanin model
tannin_model <- lm(growth ~ tannin, data = tannin)
str(tannin_model)
summary(tannin_model)
################################################################################
# Part 3: Saving
# import data
filename <- readline("Enter csv file name without the extension, and press Enter: ")
input <- read.csv(paste("Datasets/", filename, ".csv", sep = ""), header = F)
# create a vector from the imported dataframe
input <- unlist(input)
# produce summary statistics
inp_sum <- summary(input)
inp_var <- var(input)
# create output
cat("Summary statistics for data in ", filename, ".csv\n", sep = "")
cat(rep("=", 35 + nchar (filename)), sep = "")
cat("\nThe usual summary statistics are:\n")
print(inp_sum)
cat("\nThe variance of the data is", round (inp_var, 2), "\n\n")
print(Sys.Date ())


