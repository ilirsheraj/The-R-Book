library(scales)

yields <- read.table("Datasets/yields.txt", header = TRUE)
head(yields)

yields <- read.delim("Datasets/yields.txt")
head(yields)

map <- read.csv("Datasets/bowens.csv")
head(map)

# Use scan when not sure
scan("Datasets/rt.txt")

scan("Datasets/rt.txt", sep = "\t")

scan("Datasets/rt.txt", sep = "\n")

# Let's do it step by step (Totally unnecessary)
no_lines <- length(scan("Datasets/rt.txt", sep = "\n"))
no_lines

len <- length(scan("Datasets/rt.txt", sep = "\t")) / no_lines
len

scan("Datasets/rt.txt", sep = "\t")[1:len]

as.numeric(na.omit(scan("Datasets/rt.txt", sep = "\t", quiet = T)[1:len]))

# Automate it
sapply(1:no_lines, function(i) 
  as.numeric(na.omit(scan("Datasets/rt.txt", sep = "\t", quiet = T)[(4*i-3):(4*i)])))

# readLines() function
readLines("Datasets/rt.txt")

strsplit(readLines("Datasets/rt.txt"), "\t")

strsplit(readLines("Datasets/rt.txt"), "\n")

# Introduce NAs
neighbour_rows <- lapply(strsplit(readLines("Datasets/rt.txt"), "\t"), as.numeric)
neighbour_rows

sapply(1:no_lines, function(i) as.numeric(na.omit(neighbour_rows[[i]])))

# Data from the web
url <- "http://www.bio.ic.ac.uk/research/mjcraw/therbook/data/cancer.txt"
canc <- read.table(url, header = T)
head(canc)

# Available datasets
data()

# All the datasets from all libraries installed in the machine (takes a while here)
# data(package = .packages(all.available = TRUE))
data(package = "MASS")

?lynx

library(spatstat)
data(package = "spatstat.data")

################################################################################
# DataFrames
worms <- read.table("Datasets/worms.txt", header = T, colClasses = list(Vegetation = "factor"))
head(worms)
tail(worms)

names(worms)
sapply(worms, class)

# Get a quick summary, NAs included
summary(worms)

# Summarize using by
by(worms$Area, worms$Vegetation, mean)

aggregate(worms$Area, by = list(worms$Vegetation), mean)

aggregate(worms$Area, by = list(worms$Vegetation, worms$Damp), mean)

# Indexing
worms[3, 5]
worms[14:19, 7]
worms[1:5, 2:3]

worms[3,]
worms[, 3]
# get it as df
worms[, 3, drop = FALSE]
bxs <- worms[, 3, drop = FALSE]
class(bxs)

# All rows and selected columns by index
worms[, c (1, 5)]

# Sample without replacement
worms[sample(1:20, 10),]
worms[sample(1:20, 10, replace = TRUE),]

# Sorting DataFrames
worms[order(worms$Slope),]

# Order in increasing order
worms[order(worms$Slope, decreasing = TRUE),]

# By two variables
worms[order(worms$Vegetation, worms$Worm.density),]

worms[order(worms$Vegetation, worms$Worm.density, worms$Soil.pH),]

# Order by rows and picl columns you like by index
worms[order(worms$Vegetation, worms$Worm.density, worms$Soil.pH), c(4, 7, 5, 3)]

# By column names (long, better in tidyverse)
worms[order(worms$Vegetation, worms$Worm.density, worms$Soil.pH), 
      c("Vegetation", "Worm.density", "Soil.pH", "Slope")]

# One increasing, the other decreasing
worms[order(worms$Vegetation, -worms$Worm.density),]

# Use rank for factors
worms[order(-rank(worms$Vegetation), -worms$Worm.density),]

# Pick columns that contain "S"
worms[,grep("S", names(worms))]

# Same, start with S
worms[,grep("^S", names(worms))]

# Select by logical expressions
worms[worms$Damp == T,]

worms[worms$Worm.density > median(worms$Worm.density) & worms$Soil.pH < 5.2, ]

# Select only numeric columns
sapply(worms, is.numeric)

# Stick it in brackets
worms[, sapply(worms, is.numeric)]

# Select factors only
worms[, sapply(worms, is.factor)]

worms[, sapply(worms, is.factor), drop = FALSE]

# Pick certain indices only
worms[(6:15),]
# Its complement
worms[-(6:15),]

worms[worms$Vegetation == "Grassland",]

worms[!worms$Vegetation == "Grassland",]

# Even cooler 
worms[worms$Vegetation != "Grassland",]

# Use which()
which(worms$Vegetation == "Grassland")

worms[which(worms$Vegetation == "Grassland"), ]

# Remove duplicates: Highest density per Vegetation class
worms_new <- worms[order(worms$Worm.density),]
worms_new[!duplicated(worms_new$Vegetation),]

# Dealing with NAs
worms_short <- read.table("Datasets/worms.missing.txt", header = T)
summary(worms_short)

na.omit(worms_short)

new_worms_short <- na.exclude(worms_short)
complete.cases(worms_short)
complete.cases(new_worms_short)

# Keep only complete cases
worms_short[complete.cases(worms_short),]

# Replace NAs with 0
worms_short[is.na(worms_short)] <- 0
worms_short

# First columns as rownames
worms2 <- read.table("Datasets/worms.txt", header = T, row.names = 1)
head(worms2)

worms2[1, 1]
# use rowname instead of index: This is cool in RNASeq :)
worms2["Nashs.Field", 1]

# Create dataframes from other data structures
x <- runif(10)
y <- letters[1:10]
z <- sample(c(rep(T, 5), rep(F, 5)))

new_df <- data.frame(x, y, z)
new_df

# From table
x <- rpois(1500, 1.5)
y <- table(x)
y

y_df <- data.frame(y)
y_df

y_long <- rep(1:nrow(y_df), y_df$Freq)
hist(y_long, breaks = 1:(max (x) + 1), main = "", xlab = "x", col = hue_pal()(1))

y_long_df <- y_df[y_long,]
head(y_long_df)
tail (y_long_df)

# Dealing with duplicates
dups <- read.table("Datasets/dups.txt", header = T)
dups

unique(dups)
duplicated(dups)
dups[duplicated(dups),]

# Dealing with dates
pat_resp <- read.table("Datasets/sortdata.txt", header = T)
head(pat_resp)

sapply(pat_resp, class)

# Wrong ordering by dtae because its a character
pat_resp[order(pat_resp$date), ]

# Parse the dates
pat_resp_dates <- strptime(pat_resp$date, format = "%d/%m/%Y")
pat_resp_dates

# Stick it back on dataframe
pat_resp <- cbind(pat_resp, pat_resp_dates)
head(pat_resp)
pat_resp[order(pat_resp$pat_resp_dates), ]
###############################################################################
# Matching dataframes
herbicides <- read.table("Datasets/herbicides.txt", header = T)
head(herbicides)

# Match them
hb <- herbicides$Herbicide[match(worms$Vegetation, herbicides$Type)]
hb

# Stick back to the table
worms$hb <- hb
head(worms)

# Merge
lifeforms <- read.table("Datasets/lifeforms.txt", header = T)
head(lifeforms)

flowering <- read.table("Datasets/fltimes.txt", header = T)
head(flowering)

# Leave default
merge(flowering, lifeforms)

# Keep non-matching too
merge(flowering, lifeforms, all = TRUE)
plants_all <- merge (flowering, lifeforms, all = T)

# Merge when column names are different (Like in Pandas)
seeds <- read.table("Datasets/seedwts.txt", header = T)
head(seeds)

# Merge
merge(plants_all, seeds, by.x = c("Genus", "species"), by.y = c ("name1", "name2"))
################################################################################
# Adding Margins
sales <- read.table("Datasets/sales.txt", header = T)
sales

# Get the means for each row
indiv_means <- rowMeans(sales[, 2:5])
overall_mean <- mean(indiv_means)
people <- indiv_means - overall_mean
people
round(mean(people))

# Add to the dataframe
new_sales <- cbind(sales, people)
new_sales

# Do the same for seasons
indiv_seasons <- colMeans(sales[,2:5])
overall_seasons <- mean(indiv_seasons)
seasons <- indiv_seasons - overall_seasons
seasons
round(mean(seasons))

new.row <- new_sales[1,]
new.row[1] <- "Seasonal effects"
new.row[2:5] <- seasons
new.row[6] <- 0
new.row

# Add it to the dataframe
new_sales <- rbind(new_sales, new.row)
new_sales

# Last Bits
gm <- rep(overall_mean, 4)
gm
new_sales[1:5, 2:5] <- sweep(new_sales[1:5, 2:5], 2, gm)
new_sales

new_sales[6,6] <- overall_mean
new_sales

# Back to aggregate: rename Vegetation to veg
aggregate(worms[,c (2 ,3, 5, 7)], by = list(veg = worms$Vegetation), mean)

aggregate(worms[, c(2, 3, 5, 7)], 
          by = list (veg = worms$Vegetation, d = worms$Damp), mean)

# Better readability than
aggregate(worms$Area, by = list(worms$Vegetation, worms$Damp), mean)

