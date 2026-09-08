library(scales)
# Part 1: Lists
apples <- c(4, 4.5, 4.2, 5.1, 3.9)
oranges <- c(TRUE, TRUE, FALSE)
chalk <- c("limestone", "marl","oolite", "CaC03")
cheese <- c(3.2 - 4.5i, 12.8 + 2.2i)

items <- list(apples, oranges, chalk, cheese)
items

# List index has double-brackets [[]]
items[[3]]
items[[3]][3]

# items has no names
names(items)

# Lets give it names
items <- list(first=apples, second=oranges, third=chalk, fourth=cheese)
items
# Now we can use indexing tagged lists
items$first
items$second

# Some explorations
class(items)
mode(items)
is.numeric(items)
is.list(items)
length(items)

# See the length of all elements of the list
lapply(items, length)
lapply(items, class)

# This doesnt fail if its not numeric
lapply(items, mean)

# One of the best options
summary(items)

str(items)

# A bit higher level
pa <- read.csv("Datasets/pa.csv", row.names = 1)
head(pa)

# List of sites at which each species is found (1)
# There are 10 species
lapply(1:10, function(i) which(pa[i,] > 0))

# Get the loci name
lapply(1:10, function(i) colnames(pa)[pa[i,] >0])

# Species list for each location (9 columns)
sapply(1:9, function(i) rownames(pa)[pa[i,] > 0])

# A better representation attempt
spplists <- sapply(1:9, function(i) rownames(pa)[pa[i,] > 0])
for (i in 1:length(spplists)) {
  slist <- data.frame(spplists[[i]])
  names(slist) <- names(pa)[i]
  file_name <- paste(names (pa)[i], ".txt", sep = "")
  write.table (slist, file_name)
}

# Use stacking: similar to pandas
newpa <- stack(pa)
newpa <- data.frame(newpa, rep(rownames(pa, 9)))
head(newpa)
names(newpa) <- c("Present", "Location", "Species")
head(newpa)

# Get the location (2) where "Bartsia alpina" is found
newpa[newpa$Species == "Bartsia alpina" & newpa$Present == 1, 2]
################################################################################
# Part 2: String Manipulation
a <- "abc"
b <- "123"

class(a)
class(b)

as.numeric(a)
as.numeric(b)

pets <- c("cat", "dog", "gerbil", "terrapin")
length(pets)
class(pets)

# Length of each element of the vector
nchar(pets)

# Even cleaner
sapply(pets, nchar)

letters
LETTERS

# Find the index of letter n
which(letters == "n")

# Remove the quotes from the letters
noquote(letters)
noquote(LETTERS)

# Join strings together
c(a, b)
paste(a, b, sep = "")
paste(a, b)
paste0(a, b)

paste(a, b, " a longer phrase containing blanks", sep = "")

# With vectors: The phrase is pasted for each element separately
d <- c(a, b, "new")
d
e <- paste(d,"a longer phrase containing blanks")
e

# Extracting parts of strings
phrase <- "the quick brown fox jumps over the lazy dog"
phrase

# Create an empty string
q <- character(20)
class(q)
for (i in 1:20){
  # substr(x, start, end)
  q[i] <- substr(phrase, 1, i)
}
q

# Counting things within strings
nchar(phrase)

strsplit(phrase, split = character(0))
table(strsplit(phrase, split = character(0)))

words <- table (strsplit (phrase, split = character (0)))[1]
words

strsplit(phrase, " ")
lapply(strsplit(phrase, " "), nchar)
table(lapply(strsplit(phrase, " "), nchar))

# reverse
## Split by character
strsplit(phrase, NULL)
lapply(strsplit(phrase, NULL), rev)
# now join
sapply(lapply(strsplit(phrase, NULL), rev), paste, collapse="")

# split by "the"
strsplit(phrase, "the")
## select first part
strsplit(phrase, "the")[[1]][2]
nchar(strsplit(phrase, "the")[[1]][2])

toupper(phrase)
tolower(toupper(phrase))

# match() function: Interesting
## Where (if at all) do the values in the second vector appear in the first vector?
first <- c(5, 8, 3, 5, 3, 6, 4, 4, 2, 8, 8, 8, 4, 4, 6)
second <- c(8, 6, 4, 2)
match(first, second)
## Gives the indices of the elements of second vector matching first vector

# Let's make it a bit cooler
## A vector of patients
subjects <- c("A", "B", "G", "M", "N", "S", "T", "V", "Z")
## A vector of patients suitable for new therapy
suitable_patients <- c("E", "G", "S", "U", "Z")
match(subjects, suitable_patients)
subjects[!is.na(match(subjects, suitable_patients))]

drug <- c("new", "conventional")
# Binarzie subjects into new and conventional suitable
drug[ifelse(is.na(match(subjects, suitable_patients)), 2, 1)]

which(is.na(match (suitable_patients, subjects)) == F)

# Pattern Matching
wf <- read.table("Datasets/worldfloras.txt", header=TRUE)
head(wf)

country <- wf$Country
length(country)

# Any country that has "R" in its name
country[grep("R", country)]

# Any Country whose first name begins with R
country[grep("^R", country)]

# Any Country whose Second or more starts with R
country[grep(" R", country)]

# Countries with 2 or more names
country[grep(" ", country)]

# Countries whose names end in y
country[grep("y$", country)]

# Countries with upper case C-E letters anywhere in their names
country[grep("[C-E]", country)]

# Start with C-E
country[grep("^[C-E]", country)]

# Doesn't finish in a-t
country[-grep("[a-t]$", country)]

# Capital included
country[-grep("[a-t A-T]$", country)]

# Contain y as second letter
country[grep(".y", country)]

# Contains y as third letter
country[grep("..y", country)]

# Contains y on 6th position
country[grep("^.{5}y", country)]

# 4 or fewer letters in their names
country[grep("^.{,3}$", country)]

# 15+ letters
country[grep(".{15,}$", country)]

# Substitution
limbs <- c("arm", "leg", "head", "foot", "hand", "hindleg", "elbow")
limbs

gsub("h", "H", limbs)

sub("h", "H", limbs)

sub("o", "O", limbs)

gsub("o", "O", limbs)

# All first letters with O
gsub("^.", "O", limbs)

library(stringr)
str_to_title(limbs)

str_to_upper(limbs)

# Pattern Locations within Vectors
limbs

# Capital O is in none
regexpr("O", limbs)

# small o is in some
regexpr("o", limbs)

grep("o", limbs)
limbs[grep("o", limbs)]

# charmatch() for single matches, 0 for multiple, NA otherwise
charmatch("m", c("mean", "median", "mode"))

charmatch("o", c("mean", "median", "mode"))

charmatch("med", c("mean", "median", "mode"))

# Min one "o", retuen the element, not the index
grep("o{1}", limbs, value = T)

# Min 2 o
grep("o{2}", limbs, value = T)

# Min 3 o
grep("o{3}", limbs, value = T)

# Returns the index
grep("o{1}", limbs)

# Length of the words: 4
grep("[[:alnum:]]{4, }", limbs, value = T)

grep("[[:alnum:]]{5, }", limbs, value = T)

grep("[[:alnum:]]{6, }", limbs, value = T)

grep("[[:alnum:]]{7, }", limbs, value = T)

# Vector comparison with %in% and which()
stock <- c("car", "van")
requests <- c("truck", "suv", "van", "sports", "car", "wagon", "car")

which(requests %in% stock)
requests[which(requests %in% stock)]

stock[match(requests, stock)][!is.na(match(requests, stock))]

# Simply with sapply()
sapply(requests, "%in%", stock)

which(sapply(requests, "%in%", stock))

# More complicated Patterns
entries <- c("Trial 1 58 cervicornis (52 match)",
             "Trial 2 60 terrestris (51 matched)",
             "Trial 8 109 flavicollis (101 matches)")
entries

# Remove whatever is inside brackets and any blank space
gsub(" *$", "", gsub("\\(.*\\)$", "", entries))

pos <- regexpr("\\(", entries)
pos

substring(entries, first = pos + 1, last = nchar (entries) - 1)
################################################################################
# Part 3: Date and Time in R
Sys.time()

# from January 1, 1970
as.numeric(Sys.time())

# POSIX System for time and date
class(Sys.time())

# "POSIXct" -> POSIX Continuous Time
# "POSIXt" -> POSIX List Time
time.list <- as.POSIXlt(Sys.time ())
time.list
unlist(time.list)

# Reading times and dates from files
dates <- read.table("Datasets/dates.txt", header = T)
head(dates)

# by default its character
class(dates$date)

# convert to datetime
Rdate <- strptime(as.character(dates$date), "%d/%m/%Y")
Rdate[1:5]
class(Rdate)

# Attach it back
dates <- data.frame(dates, Rdate)
head(dates)
tail(dates)

# calculate the mean for each day in the entire table
tapply(dates$x, Rdate$wday , mean)

# Numbers into days
y <- strptime("01/01/2021", format = "%d/%m/%Y")
y
weekdays(y)
y$wday

# More messy
other_dates <- c("1jan99", "2jan05", "31mar04", "30jul05")
other_dates
strptime(other_dates, "%d%b%y")

yet_more_dates <- c("2016 2 Mon", "2017 6 Fri", "2018 10 Tue")
strptime(yet_more_dates, "%Y %W %a")

# Calculations with Dates and Times
# adding any integer to a datetime object adds seconds (it sucks)
(now <- Sys.time())

now + 1
now - 2

y
y+1

# Take some dates and convert them to datetitme
y1 <- "2015-10-22"
y1 <- as.POSIXct(y1)

y2 <- "2018-10-22"
y2 <- as.POSIXct(y2)

y1 - y2

# Make it numeric
as.numeric(y1 -y2)

# use difftime too
t1 <- as.difftime ("6:14:21")
t1
t2 <- as.difftime ("5:12:32")
t2
t1 - t2
as.numeric(t1 - t2)

times <- read.table("Datasets/times.txt", header = TRUE)
head(times)

# Create difftime to carry out calculations
duration <- as.difftime(paste(times$hrs, times$min, times$sec, sep=":"))
duration

tapply(duration, times$experiment, mean)

# Generating sequences of dates
## From 4 November to 15 November, every day
seq(as.POSIXlt ("2015-11-04"), as.POSIXlt("2015-11-15"), "1 day")

# Every two weeks
seq(as.POSIXlt("2015-11-04"), as.POSIXlt("2016-04-05"), "2 weeks")

# Every three months
seq(as.POSIXlt("2015-11-04"), as.POSIXlt("2018-10-04"), "3 months")

# Every 6 years
seq(as.POSIXlt("2015-11-04"), as.POSIXlt("2045-02-04"), by = "6 years")

# Every 8955 Seconds
seq(as.POSIXlt("2015-11-04"), as.POSIXlt("2015-11-05"), 8955)

# Start and add 10 points every month
seq(as.POSIXlt("2015-11-04"), by = "month", length = 10)

# Use along
results <- 1:16
seq(as.POSIXlt("2015-11-04"), by = "month", along = results)

# Get the weekdays for the dates above
weekdays(seq(as.POSIXlt("2015-11-04"), by = "month", along = results))

# All Mondays on a sequence of dates
first_100 <- as.Date(0:99, origin = "2016-01-01")
first_100[1:5]
first_100 <- as.POSIXlt(first_100)
first_100[1:10]

first_100[first_100$wday == 1]

# First Monday of each month
first_week <- seq(as.POSIXlt("2016-01-01"), as.POSIXlt("2016-01-07"), "days")
first_mon <- first_week[weekdays(first_week) == "Monday"]
all_mons <- seq(first_mon, as.POSIXlt ("2016-12-31"), "7 days")
all_mons_months <- data.frame(all_mons, month = months(all_mons))
head(all_mons_months)

wanted <- !duplicated(all_mons_months$month)
wanted
all_mons_months[wanted,]

# Time differences between rows of a dataframe
duration
class(duration)

diffs <- duration[1:15] - duration[2:16]

diffs <- c(diffs, as.difftime("00:00:00"))

times$diffs <- diffs
times

# Simple regression using time
timereg <- read.table("Datasets/timereg.txt", header = T)
head(timereg)
timereg$date <- strptime(timereg$date,"%d/%m/%Y")

class(timereg$date)
mode(timereg$date)

plot(timereg$date, timereg$survivors, xlab = "month", pch=19, col = hue_pal()(3)[1])
plot(timereg$date, log(timereg$survivors), xlab = "month", pch=19, col = hue_pal()(3)[2])

timereg_mod2 <- lm(log(survivors) ~ as.POSIXct(date), data = subset(timereg, survivors > 0))
# Add the abline to the second plot
abline(timereg_mod2, col = hue_pal()(3)[3])

summary (timereg_mod2)
# The coefficient here is per second, so lets convert it per month
month_rate <- (coef(timereg_mod2)[2] * 60 * 60 * 24 * 30)
month_rate

# If we start with 100 insects, how many will be there after 2 months?
100 * exp(month_rate * 2)

# Environments: use with() instead of attach/detach
head(OrchardSprays)
with(OrchardSprays, boxplot(decrease ~ treatment))

library(MASS)
head(bacteria)
with(bacteria, tapply((y=="n"), trt, sum))

head(mammals)
with(mammals, plot(body, brain, log = "xy"))

regression <- read.table("Datasets/regression.txt", header = T)
head(regression)

with(regression, 
     {model <- lm(growth ~ tannin)
     summary(model)
     }
     )

# Classical method (I use)
summary(lm(growth ~ tannin, data=regression))

# All available data
data()

# All available datasets from all packages installed
data(package = .packages(all.available = TRUE))

