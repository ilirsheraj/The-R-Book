# Part 1: Calculations
# Complex Numbers
z <- 3.5 - 8i
Re(z)
Im(z)
Mod(z)
Arg(z)
Conj(z)
z * Conj(z)

is.complex(z)
## Convert a Real number to a complex number
as.complex(3.8)

# Rounding Up
x <- 3.14156
round(x, 3)
round(x)
ceiling(x)
floor(x)

y <- -x
round(y, 3)
round(y)
ceiling(y)
floor(y)

trunc(5.72)
trunc(5.72, 1)
trunc(-5.72)

signif(2718281, 3)
signif(-2718281, 3)
signif(2718281, 2)

# Modular Arithmetic
## Get full divisor without decimal
119 %/% 13
## Get the reminder
119 %% 13
15421 %% 7

# Integers
x <- c(5, 3, 7, 8)
is.integer(x)
is.numeric(x)

x <- as.integer(x)
x
is.numeric(x)
as.integer(5.7)
as.integer(-5.7)
as.integer(z)

################################################################################
# Part 2 - Factors
gender <- factor(c("female", "male", "female", "male", "female"))
gender
class(gender)

# Factors stored as integers
mode(gender)

daphnia <- read.table("Datasets/daphnia.txt", header = T)
head (daphnia)
class(daphnia$Water)

# Automatically convert into factor
daphnia <- read.table("Datasets/daphnia.txt", header = T, 
                      colClasses = c ("numeric", rep ("factor", 3)))

# See the classes of all columns in one shot
sapply(daphnia, class)

# See levels
levels(daphnia$Water)
nlevels(daphnia$Water)

levels(daphnia$Detergent)
nlevels(daphnia$Detergent)

# Change order
daphnia$Water <- factor(daphnia$Water, levels = c("Wear", "Tyne"))
levels(daphnia$Water)

# Basis R for group_by() is tapply()
tapply(daphnia$Growth.rate, daphnia$Detergent, mean)
tapply(daphnia$Growth.rate, daphnia$Detergent, sd)

# Convert factors into integers
as.vector(unclass(daphnia$Detergent))

# Equality
y = 0.3 - 0.2
y
z = 0.1

# Will give false
y == z

# See why
y - z

all.equal(y, z)

# Add tolerance
all.equal(pi, 3.141, tolerance = 0.0001)

# Equality of non-numeric objects
a <- c("cat", "dog", "goldfish")
a
b <- factor(a)
b

# This gives cool explanations
all.equal(a, b)

mode(a)
mode(b)
attributes(a)
attributes(b)
class(a)
class(b)

# Even cooler
n1 <- c(1, 2, 3)
n2 <- c(1, 2, 3, 4)
all.equal(n1, n2)
n3 <- as.character(n2)
n3
all.equal(n2, n3)
all.equal(n1, n3)

# TRUE FALSE Combinations
x <- c(NA, FALSE, TRUE)

# For &
outer(x, x, "&")

# For Or
outer(x, x, "|")

# Logical Artithmetic
x <- 0:6
x

x < 4
all(x < 4)
all(x < 0)
any(x < 4)
any(x < 0)

(x < 4) * runif(length(x))

treatment <- letters[1:5]
treatment

# Assign and Print it instantly
(treatment <- letters[1:5])

t2 <- factor(1 + (treatment == "b") + 2 * (treatment == "c") + 2 * (treatment == "d"))
t2
# a=1(base), 2=b->TRUE+1=2; 3=c->2*TRUE+1=3, 4=d+2*TRUE=3,e=1(base)
################################################################################
# Part 3: Sequences
# Simple way to generate sequences
1:10
# reverse
15:5

# More formal
seq(from=0, to=1, by=0.1)
seq(0, 1, 0.1)

seq(6, 4, -0.2)

# Determine length and let R sort out the distance
seq(from=0.04, by=0.01, length=11)

N <- c(55, 76, 92, 103, 84, 88, 121, 91, 65, 77, 99)
N
seq(from=0.04, by=0.01, along=N)
seq(from=0.04, to=0.14, along=N)

# For different lengths
## 1:4, 1:3, 1:4, 1:4, 1:4, 1:5
sequence(c(4, 3, 4, 4, 4, 5))

# Repeats
## Repeat 9 5 times
rep(9, 5)

## repeat 1:4 2 times
# 1 2 3 4 1 2 3 4
rep(1:4, 2)
## repeat 1:4 each number twice
# 1 1 2 2 3 3 4 4
rep(1:4, each=2)
## repeat each 2 (11) in toal 4
# 1 1 2 2 3 3 4 4 1 1 2 2 3 3 4 4 1 1 2 2 3 3 4 4
rep(1:4, each=2, times=3)

# Fucking cool
# 1 2 2 3 3 3 4 4 4 4
rep(1:4, 1:4)

# Even Cooler
rep(1:4, c(4, 1, 4, 2))

# Same but with text
rep(c("cat", "dog", "gerbil", "goldfish", "rat"), c (2, 3, 2, 1, 3))

# Do the same by generating factor levels: Very useful for Linear Models
gl(4, 3)

gl(4, 3, 24)

# Create a data frame of factors
gloss <- gl(2, 2, 24, labels = c ("Low", "High"))
give <- gl(3, 8, 24, labels = c("Hard", "Medium", "Soft"))
flammable <- gl(2, 4, 24, labels = c ("N", "Y"))
brand <- gl(2, 1, 24, labels = c ("X", "M"))

bs <- data.frame(gloss, give, flammable, brand)
bs
################################################################################
# Part 4: Class Memebership
lv <- c(T, F, T)
is.logical(lv)

levels(lv)

fv <- as.factor(lv)
fv
is.factor(fv)
levels(fv)

nv <- as.numeric(fv)
nv

as.numeric(factor(c("a", "b", "c", "a")))

as.numeric(c("1", "2", "4"))

# Define a function to calculate the geometric mean
geometric <- function (x) {
  if(!is.numeric (x)) stop("Input must be numeric")
  exp(mean(log(x)))
}
geometric(c(2, 4, 8))

# Make it more robust
geometric <- function (x) {
  if(!is.numeric (x)) stop ("Input must be numeric")
  if(min(x) <= 0) stop("Input must be greater than Zero")
  exp(mean(log(x)))
}

# geometric(c(2, 5, 8, 0))
geometric(c(2, 5, 8, 10))
################################################################################
# Part 5: Missing Values and Infinity
3/0
-12/0
exp(Inf)
exp(-Inf)
0/0
Inf/Inf
is.finite(10)
is.infinite(10)

# Dealing with Missing Values
y <- c(4, NA, 7)
is.na(y)
is.nan(y)

a <- na.omit(y)
a

# Removing NA from all table vs specific columns
y1 <- c (1, 2, 3, 6)
y2 <- c (5, 6, NA, 8)
y3 <- c (9, 10, 11, 12)
y4 <- c (NA, 14, 15, 16)

(full_frame <- data.frame(y1, y2, y3, y4))

(reduced_frame1 <- na.omit(full_frame))

(reduced_frame2 <- full_frame[!is.na(full_frame$y4),])

# Finding missing values
(vmv <- c(1:6, NA, NA, 9:12))

seq(along=vmv)[is.na(vmv)]
seq(along=vmv)[!is.na(vmv)]

which(is.na(vmv))
vmv[which(!is.na(vmv))]

# Replace NA with 0
vmv[is.na(vmv)] <- 0
vmv

vmv <- c(1:6, NA, NA, 9:12)
ifelse(is.na(vmv), 0, vmv)
################################################################################
# Part 6: Vectors and Subscripts (Indices)
peas <- c(4, 7, 6, 5, 6, 7)
class(peas)
length(peas)
mean(peas)
sd(peas)
max(peas)
min(peas)
quantile(peas)
quantile(peas)[3]
median(peas)

peas[4]
pods <- c(2, 3, 6)
peas[pods]
peas[-1]

# define a function that removes the first and last 2 elements
trim <- function(x){
  if(length(x) < 4) stop("Vector is too short")
  sorted_x <- sort(x)
  sorted_x[-c(1,2, length(x)-1, length(x))]
}

trim(peas)

# Select only even indices
peas[1:length(peas) %% 2 == 0]

# Naming Vectors
(counts <- c(25, 12, 7, 4, 6, 2, 1, 0, 2))
names(counts) <- 0:8
counts

# Create table
(count_table <- table(rpois (2000, 2.3)))
class(count_table)

(count_vector <- as.vector(count_table))
class(count_vector)

# Working with logical subscripts (Indices)
x <- 0:10
sum(x)

sum(x < 5)

# Select only elements smaller than 5
x[x<5]
sum(x[x<5])

# Sorting vectors
y <- c(8, 3, 5, 7, 6, 6, 8, 9, 2, 3, 9, 4, 10, 4, 11)
y
sort(y)
sort(y, decreasing = TRUE)

# Select the biggest 3
sort(y, decreasing = TRUE)[1:3]
sum(sort(y, decreasing = TRUE)[1:3])

# min and max
x <- c(2, 3, 4, 1, 5, 8, 2, 3, 7, 5, 7)
x
which(x == max(x))
which(x == min(x))

which.min(x)
which.max(x)
x[which.min(x)]
x[which.max(x)]
