# Tables of counts: Categorical Data
counts <- rpois(1000, 0.6)
table(counts)

disease <- read.table("Datasets/disease.txt", header = TRUE)
head(disease)
attach(disease)

table(gender, status)

table(status, gender)

# Tables of proportions: Based on margins
disease_tab <- table(gender, status)
disease_tab

# Row-wise normalization
prop.table(disease_tab, margin = 1)

# Column-wise normalization
prop.table(disease_tab, margin = 2)

# Grand total normalization
prop.table(disease_tab)
################################################################################
# Numerical Data
daphnia <- read.table("Datasets/daphnia.txt", header = TRUE)
head(daphnia)
attach(daphnia)

# Use Psychology package
library(psych)
# Take one variable at a time
describe(Growth.rate)

# Remove skew
describe(Growth.rate, skew = FALSE)

# Two variables
describeBy(Growth.rate, Water)

# Again shorter
describeBy(Growth.rate, Water, skew=FALSE)

# By two categorical variables
describeBy(Growth.rate, list(Water, Daphnia))

# Shorter
describeBy(Growth.rate, list(Water, Daphnia), skew=FALSE)

# Cleaner Summaries by tapply
tapply(Growth.rate, Water, mean)

tapply(Growth.rate, Detergent, mean)

tapply(Growth.rate, Daphnia, mean)

# Using two categorical variables
tapply(Growth.rate, list(Daphnia, Detergent), mean)

tapply(Growth.rate, list(Daphnia, Detergent), median)

# Standard error of the mean: define yourself
tapply(Growth.rate, list(Daphnia, Detergent), function(x) sqrt( var(x)/length(x)))

# Using three categorical variables: 3d array
tapply(Growth.rate, list(Daphnia, Detergent, Water), mean)

# Flatten the results above
ftable(tapply(Growth.rate, list(Daphnia, Detergent, Water), mean))

# Change order by providing factors with levels
water <- factor(Water, levels = c("Wear", "Tyne"))
ftable(tapply(Growth.rate, list(Daphnia, Detergent, water), mean))

# Trim from ends
tapply(Growth.rate, Detergent, mean, trim=0.1)

tapply(Growth.rate, Detergent, mean)

tapply(Growth.rate, Detergent, mean, na.rm=TRUE)
################################################################################
# Table to DF and Vice Versa
daph_table <- tapply(Growth.rate, list(Detergent, Daphnia), mean)
daph_table

# This is awesome
daph_table_data <- as.data.frame.table(daph_table)
daph_table_data

# Rename
names(daph_table_data) <- c("detergents", "daphnia", "mean")
daph_table_data

tabledata <- read.table("Datasets/tabledata.txt", header = T)
tabledata

# Expand by the number in count column: Returns a list
lapply(tabledata, function(x) rep(x, tabledata$count))

# Convert the list into data frame
expanded_table <- lapply(tabledata, function(x) rep(x, tabledata$count))
expanded_table <- as.data.frame(expanded_table)
head(expanded_table)

# Remove the first column
expanded_table <- expanded_table[,-1]
head(expanded_table)

# Another way: Who TF thought of this?
expanded_table2 <- tabledata[rep(1:nrow(tabledata), tabledata [["count"]]),]
head(expanded_table2)

# From dataframe back to table
table(expanded_table)

# Full cycle back to original dataframe
as.data.frame(table(expanded_table))

contract_table <- as.data.frame(table(expanded_table))
contract_table
names(contract_table)[4] <- "count"
contract_table

# Cool chapter, especially ftable is better than tidyverse :)
