# Part 1: Vector Functions
library(scales)
y <- c(8, 3, 5, 7, 6, 6, 8, 9, 2, 3, 9, 4, 10, 4, 11)
mean(y)
range(y)

# fivenum invented by Tukey: min, lower hinge, median, upper hinge, max
fivenum(y)

# table: Negative Binomial
counts <- rnbinom(10000, mu=0.92, size=1.1)
counts[1:10]
table(counts)

# tapply() function -> Similar to group_by() in tidyverse
temp_data <- read.table("Datasets/temp_data.txt", header = T)
head(temp_data)
tapply(temp_data$temperature, temp_data$month, mean, na.rm=TRUE)

tapply(temp_data$temperature, temp_data$month, var, na.rm=TRUE)

tapply(temp_data$temperature, temp_data$month, min, na.rm=TRUE)
tapply(temp_data$temperature, temp_data$month, max, na.rm=TRUE)

# Standard Error: sem = sqrt(var/n)
tapply(temp_data$temperature, temp_data$month,
       function(x) sqrt(var(x, na.rm = TRUE)/length(x)))

# For two variables
temp_sum <- tapply(temp_data$temperature, list(temp_data$yr, temp_data$month), 
                   mean, na.rm=TRUE)

# The trim argument allows us to specify the fraction of the data (between 0 and 0.5) 
# that we want to be omitted from the left-and right-hand tails of the sorted 
# vector of values before computing the mean of the central values.
tapply(temp_data$temperature, temp_data$yr, mean, trim = 0.2)[1:10]
## Original
tapply(temp_data$temperature, temp_data$yr, mean, na.rm=TRUE)[1:10]

# sapply() function for vectors
sapply(3:7, seq)

# Decay: y = e^(−ax)
sapdecay <- read.table("Datasets/sapdecay.txt", header = T)
head(sapdecay)

# sum of the squares of the differences between the observed (y) and 
# predicted (yf) values of y, when provided with a specific value of the parameter a
sumsq <- function(a, xv = sapdecay$x, yv = sapdecay$y) {
  yf <- exp(-a * xv)
  sum((yv - yf)^2)
}

# Estimate "a" using linear regression
a_est <- lm(log(y) ~ x, data = sapdecay)
a_est
coef(a_est)[2]

# Generate values close to 0.058
a <- seq(0.01, 0.2, 0.005)
# Now get the values for different a
plot(a, sapply(a, sumsq), type = "l", col=hue_pal()(1), lwd=3)

# aggregate for grouped summary statistics
## one to one: aggregate (y ∼ x, mean)
## one to many: aggregate (y ∼ x + w, mean)
## many to one: aggregate (cbind (y, z) ∼ x, mean)
## many to many: aggregate (cbind (y, z) ∼ x + w, mean)
phdaphnia <- read.table("Datasets/phdaphnia.txt", header = T)
head(phdaphnia)

# Find the mean of growth rate by water type
aggregate(Growth.rate ~ Water, mean, data = phdaphnia)

# Same as:
tapply(phdaphnia$Growth.rate, phdaphnia$Water, mean)

# More Complex
aggregate(Growth.rate ~ Water + Detergent, mean, data = phdaphnia)

# Similar, although uglier than
tapply(phdaphnia$Growth.rate, list(phdaphnia$Water, phdaphnia$Detergent), mean)

# For two variables by two groups
aggregate(cbind(Growth.rate, pH) ~ Water + Detergent, mean, data = phdaphnia)

# Here tapply breaks down :)
lapply(phdaphnia[c("Growth.rate", "pH")], function(x) {
  tapply(x, list(phdaphnia$Water, phdaphnia$Detergent), mean)
})

by(phdaphnia[c("Growth.rate", "pH")],
   list(Water = phdaphnia$Water, Detergent = phdaphnia$Detergent),
   colMeans)

# Parallel minima and maxima: pmin and pmax
x <- 1:10
y <- 10:1
z <- seq (0, 18, 2)
# Compares vectors side by side and finds the minimum of each for each index
pmin(x, y, z)

# Finding the closest value
xv <- rnorm(1000, 100, 10)
# closest number to 108
which(abs(xv - 108) == min(abs(xv - 108)))
xv[which(abs(xv - 108) == min(abs(xv - 108)))]

# The distance
abs(xv[which (abs (xv - 108) == min (abs (xv - 108)))] - 108)

# Put it in a function
closest <- function (xv, sv) {
  xv[which (abs (xv - sv) == min (abs (xv - sv)))]
}
closest(xv, 108)

# Sorting, Ranking and Ordering
houses <- read.table("Datasets/houses.txt", header = T)
head(houses)

# Lowest to Highest
ranked <- rank(houses$Price)
sorted <- sort(houses$Price)
# Creates a vector of indices (very cool)
ordered <- order(houses$Price)

view_houses <- data.frame(houses$Price, ranked, sorted, ordered)
view_houses
houses$Location[order(houses$Price)]

houses[order(houses$Price, decreasing = TRUE),]
houses[order(houses$Price),]

# unique() and duplicated() functions
# Create a vector with some duplications
names <- c("Williams", "Patel", "Smith", "Williams", "Patel", "Williams")
names
table(names)
unique(names)
duplicated(names)
names[!duplicated(names)]
# Create another vector for salaries
salary <- c(42, 42, 48, 42, 42, 42)
mean(salary)
mean(unique(salary))
mean(salary[!duplicated(names)])

# runs of numbers within vectors: long-repeats
set.seed(123)
(poisson_dist <- rpois(150, 0.7))
rles <- rle(poisson_dist)
rles$lengths
rles$values

max(rles[[1]])
# Find the index
which.max(rles[[1]])
# Find the value
rles$values[which.max(rles[[1]])]

# Number of runs in a given vector
length(rles[[1]])
length(rles[[2]])


n1 <- 25
n2 <- 30
y <- c (rep (1, n1),rep (0, n2))
# Create a zero vector
len <- numeric(10000)
for (i in 1:10000) {
  len[i] <- length(rle(sample(y))[[2]])
}
summary(len)

# Sets
setA <- c("a", "b", "c", "d", "e")
setB <- c("d", "e", "f", "g")

union(setA, setB)
intersect(setA, setB)

setdiff(setA, setB)
setdiff(setB, setA)

setA %in% setB
setA[setA %in% setB]
setB %in% setA
setB[setB %in% setA]
################################################################################
# Part 2: Matrices and Arrays

# Arrays
# 2 rows, 3 column, 3 matrices
(y <- array(1:24, dim = c (2, 4, 3)))

# 3 rows, 2 columns, 4 matrices
(y <- array (1:24, dim = c (3, 2, 4)))

# Row 2, column 1, matrix 3 -> 19
y[2,1,3]

# Matrices (created column-wise)
(X <- matrix(c (1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3))
class(X)
attributes(X)
dim(X)

# Make it row-wise
y <- c(1, 2, 3, 4, 4, 3, 2, 1)
(Y <- matrix(y, byrow = T, nrow = 2))

dim(y) <- c(4, 2)
y
t(y)

# row and column names
(X <- matrix(rpois (20, 1.5), nrow = 4))

# do.NULL = FALSE argument tells R not to create row names
rownames(X) <- rownames(X, do.NULL = FALSE, prefix = "Trial.")
X

# Create a vector for column names
drug_names <- c("aspirin", "paracetamol", "nurofen", "hedex", "placebo")
colnames(X) <- drug_names
X

# At once: Leave rownames empty
dimnames(X) <- list(NULL, paste0("drug.", 1:5))
X

dimnames(X) <- list(paste0("patient", 1:nrow(X)), paste0("drug.", 1:5))
X

# Calculations on rows and columns
mean(X[,4])
var(X[4,])

rowSums(X)
colSums(X)
rowMeans(X)
colMeans(X)

# Mean of all columns: row called margin 1 and column margin2, thus the numbers
apply(X, 2, mean)
apply(X, 1, mean)

group <- c("A", "B", "B", "A")
rowsum(X, group = group)

aggregate(X, list(group), sum)

# Shuffle in each column
apply(X, 2, sample)
apply(X, 1, sample)

# Adding rows and columns to matrices
X <- rbind(X, apply(X, 2, mean))
X <- cbind(X, apply(X, 1, mean))
colnames(X)[6] <- "RowMean"
rownames(X) <- c(1:4, "mean")
X

# Keep matrix as so even in 1 dim (prevent from being converted to vector)
a <- matrix(1:4, nrow = 2)
(rowmatrix <- a[2, , drop = FALSE])

a[2,]
(colmatrix <- a[, 1, drop = FALSE])
a[,1]

# Same for Arrays
b <- array(1:18, dim = c (2, 3, 3))
b
# select only first row from all
(still_4_dims <- b[1, , , drop = F])

# Otherwise it becomes matrix
b[1, , ]

# The sweep() function (magical ones :))
sweepdata <- read.table("Datasets/sweepdata.txt")
head(sweepdata)

# Get the parameters: column mean in this case
colmeans <- apply(sweepdata, 2, mean)
colmeans

# Now remove the column mean from each column in the dataframe
sweep(sweepdata, 2, colmeans)

# Do the same manually
(col.means <- matrix (rep(colmeans, rep (10, 4)), nrow = 10))
sweepdata - col.means

# Another useful case
sweep(sweepdata, 1, 1:10, function (a, b) b)

# column-wise
sweep(sweepdata, 2, 1:4, function(a, b) b)

# apply() function on matrices
(X <- matrix (1:24, nrow = 4))

apply(X, MARGIN = 1, FUN = sum)
apply(X, 2, sum)
apply(X, 1, sqrt)
apply(X, 2, sqrt)
apply(X, 2, sample)

# Define own function
apply(X, 2, function(x) x^2 + x)

# Matrix Scaling
mat_a <- matrix(c(1, -2, 5, 4, 15, -8, 1, 10, 19), ncol = 3)
mat_a

# Remove the mean
scale(mat_a, scale = FALSE)

# The same as this one
colmean <- apply(mat_a, 2, mean)
sweep(mat_a, 2, colmean)

# Now do z-scaling
scale(mat_a)

# The max.col() function
pgfull <- read.table("Datasets/pgfull.txt", header = T)
head(pgfull)
names(pgfull)

# Select only numerical values for species
species <- pgfull[,1:54]

max.col(species)
names(species)[max.col(species)]

table(names(species)[max.col(species)])

# Restructure multidimensional arrays with aperm()
toy_data <- array(1:24, 2:4)
toy_data
# add names to each dimension
dimnames(toy_data)[[1]] <- list("male", "female")
dimnames(toy_data)[[2]] <- list("young", "mid", "old")
dimnames(toy_data)[[3]] <- list("A", "B", "C", "D")
dimnames(toy_data)
toy_data

# Now switch
aperm(toy_data, c(2, 3, 1))
################################################################################
# Part 3: Random Numbers
set.seed(375)
runif(3)

.Random.seed[1:4]
length(.Random.seed)

# sampling
y <- c(8, 3, 5, 7, 6, 6, 8, 9, 2, 3, 9, 4, 10, 4, 11)
y

sample(y)

# chose only 5 random numbers without replacement
sample(y, 5)

set.seed(888)
sample(y, replace = TRUE)

# Use probabilities (weighted)
p <- c(1, 2, 3, 4, 5, 5, 4, 3, 2, 1)
x <- 1:10
sapply(1:5, function(i) sample(x, 4, prob = p))
################################################################################
# Part 4: Loops and Repeats
# for-loop
for (i in 1:5){
  print(i^2)
}

# One-liner
for (i in 1:5) print(i^2)

# More complicated
j <- 0
k <- 0
for (i in 1:5){
  j <- j + 1
  k <- k + i * j
  print(i + j + k)
}

# For loop for factorial
fac1 <- function(x){
  f <- 1
  if (x < 2) return(1)
  for (i in 2:x){
    f <- f * i
  }
  f
}

fac1(12)

# Create a vector of catorials from 0 - 5
sapply(0:5, fac1)

# Compare with factorial base function
factorial(0:5)

# while-loop
fac2 <- function(x){
  f <- 1
  t <- x
  while (t > 1){
    f <- f * t
    t <- t - 1
  }
  f
}

sapply(0:5, fac2)

# repeat: don't use it
fac3 <- function(x){
  f <- 1
  t <- x
  repeat {
    if (t < 2) break
    f <- f * t
    t <- t - 1
  }
  f
}

sapply(0:5, fac3)

# We can get the same using cumprod
cumprod(1:5)

# Put it in a function to count for 0
fac4 <- function(x){
  max(cumprod(1:x))
}

sapply(0:5, fac4)

# Fibonacci
fibonacci <- function(x){
  a <- 1
  b <- 0
  while (x > 0){
    swap <- a
    a <- a + b
    b <- swap
    x <- x - 1
  }
  b
}

sapply(1:10, fibonacci)

# Overdoing loops
y <- c(1, 3, -2, 0, -6, 17)
for (i in 1:length(y)){
  if (y[i] < 0){
    y[i] <- 0
  }
}
y

# simple vector operations
y <- c(1, 3, -2, 0, -6, 17)
y[y < 0] <- 0
y

# if-else
y <- c(1, 3, -2, 0, -6, 17)
y <- ifelse(y < 0, 0, y)
y

worms <- read.table("Datasets/worms.txt", header = TRUE)
head(worms)

# Binarise area by median
ifelse(worms$Area > median(worms$Area), "Big", "Small")

cut(worms$Area, breaks = 5)

y <- log(rpois(20, 1.5))
y
ifelse(y < 0, NA, y)

# Loops are slower as compared to vectorized operations
x <- runif(10000000)

system.time(max(x))

pc <- proc.time()
cmax <- x[1]
for (i in 2:10000000){
  if (x[i] > cmax){
    cmax <- x[i]
  }
}
proc.time() - pc

# Loops for time series
next_year <- function(x){
  lambda * x * (1 - x)
}

lambda <- 3.7
next_year(0.6)

# next year
next_year(0.888)

# Lets put it on a loop for the next 20 years
N <- numeric(20)
N[1] <- 0.6 # Initial

for (t in 2:20){
  N[t] <- next_year(N[t-1])
}

plot(N, type = "l", col = hue_pal()(1), lwd = 3)
