library(scales)
library(PerformanceAnalytics)
library(ggplot2)
library(tidyr)

plot_dir <- "Plots/"

# Moving away from classical statistics: Looking for Structure in the data
## Unsupervised Learning

# 7 Plant characteristics
taxa <- read.table("Datasets/taxonomy.txt", header = T, 
                   colClasses = list(Taxon = "factor"))
head(taxa)

table(taxa$Taxon)

# Remove the automatic title
pdf(paste0(plot_dir, "Taxa_Corrplot.pdf"), width = 6, height = 6)
chart.Correlation(taxa[, 2:8], histogram = TRUE, pch = 20, main = "")
dev.off()

# Radar Plot: Track individual observations along any variable
pdf(paste0(plot_dir, "Taxa_Radar_Plot.pdf"), width = 5, height = 5)
stars(taxa[,2:8], locations = c(0, 0), key.loc = c(0, 0), radius = F,
      col.lines = hue_pal()(120))
dev.off()

# Lets make it more visible
taxa_summary <- taxa %>% 
  group_by(Taxon) %>% 
  summarize(across(everything(), mean)) %>% 
  as.data.frame()

pdf(paste0(plot_dir, "Taxa_Mean_Radar_Plot.pdf"), width = 5, height = 5)
stars(taxa_summary[, -1],
      locations = c(0, 0),
      key.loc = c(0, 0),
      radius = TRUE,
      col.lines = hue_pal()(4),
      lwd = 2,
      main = "Mean traits by taxon",
      xlim = c(-1.5, 2), ylim = c(-1.3, 1.3))
dev.off()

################################################################################
# Multivariate Analysis of Variance (MANOVA)
plastic <- read.table("Datasets/manova.txt", header = TRUE)
head(plastic)

table(plastic$additive, plastic$rate)

# Three outcomes combined together: Creates a numerical matrix
plastic_out <- cbind(plastic$tear, plastic$gloss, plastic$opacity)
head(plastic_out)

# Give it column names
colnames(plastic_out) <- colnames(plastic)[1:3]
head(plastic_out)

# Fit the model with interactions between respeonse variables
plastic_mod1 <- manova(plastic_out ~ plastic$rate * plastic$additive)
summary(plastic_mod1)
# Interaction not significant

# Without interaction
plastic_mod2 <- manova(plastic_out ~ plastic$rate + plastic$additive)
summary(plastic_mod2)

summary.aov(plastic_mod2)

# Pivot the data into a long format so R treats the measurements as one column
plastic_long <- pivot_longer(plastic, 
                             cols = c(tear, gloss, opacity), 
                             names_to = "Measurement", 
                             values_to = "Value")
head(plastic_long)

# Create a grouped boxplot
pdf(paste0(plot_dir, "Plastic_Additive_Level_Plot.pdf"), width = 5, height = 5)
ggplot(plastic_long, aes(x = Measurement, y = Value, fill = additive)) +
  geom_boxplot() +
  labs(title = "Plastic Properties by Additive Level",
       x = "Property Type",
       y = "Value") +
  theme_classic()
dev.off()

pdf(paste0(plot_dir, "Plastic_Rate_Level_Plot.pdf"), width = 5, height = 5)
ggplot(plastic_long, aes(x = Measurement, y = Value, fill = rate)) +
  geom_boxplot() +
  labs(title = "Plastic Properties by Rate Level",
       x = "Property Type",
       y = "Value") +
  theme_classic()
dev.off()

################################################################################
# Principle Component Analysis (PCA)
# 54 (col 1:54, AC - VK) plant species grown in 89 plots for 10 years
pgdata <- read.table("Datasets/pgfull.txt", header = TRUE)
head(pgdata)

names(pgdata)
# Princinple species and variables that explain output variation
hist(pgdata$richness)

pgfull <- pgdata[, 1:54]
pg_pca10 <- prcomp(pgfull, scale. = TRUE, rank. = 10)
pg_pca10$rotation[,1]

# Compare PCA1 and PCA2
biplot(pg_pca10)
summary(pg_pca10)

# Variance Explained
barplot(pg_pca10$sdev[1:10]^2, main = "", col = hue_pal()(2)[1],
        cex.axis = 1, ylab = "")


# Calculate variance explained by each principal component
pca_var <- pg_pca10$sdev^2
pca_ve  <- pca_var / sum(pca_var) * 100

# Draw the scree plot
barplot(pca_ve[1:10], 
        col = "royalblue",
        xlab = "Principal Component", 
        ylab = "Percentage of Variance Explained (%)",
        main = "Scree Plot",
        # names.arg = paste0("PC", 1:10),
        ylim = c(0, max(pca_ve[1:10]) + 5))

barplot(pca_var[1:10], 
        col = "royalblue",
        xlab = "Principal Component", 
        ylab = "Variance Explained",
        main = "Scree Plot",
        ylim = c(0, max(pca_var[1:10]) + 1))

yv <- predict(pg_pca10)[,1]
plot(pgdata$hay, yv, xlab = "", ylab = "PC1", col= hue_pal()(2)[1], pch=16)

yv2 <- predict(pg_pca10)[,2]
plot(pgdata$pH, yv2, xlab = "", ylab = "PC2", col = hue_pal()(2)[2], pch=16)


# Factor Analysis
pg_fact8 <- factanal(pgfull, 8)
loadings(pg_fact8)

# Cluster Analysis
