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

# prcomp() is for better numerical accuracy
pg_pca10 <- prcomp(pgfull, scale. = TRUE, rank. = 10)

# PC1 (loading) of the species
pg_pca10$rotation[,1]
sort(pg_pca10$rotation[,1])

# Compare PCA1 and PCA2
pg_pca10$rotation[,c(1,2)]["AP",]

pdf(paste0(plot_dir, "PCA_biplot.pdf"), width = 5, height = 5)
biplot(pg_pca10)
dev.off()

plot(pg_pca10$rotation[,1],
     pg_pca10$rotation[,2],
     xlab = "PC1",
     ylab = "PC2",
     pch = 16)

summary(pg_pca10)

# Variance Explained
barplot(pg_pca10$sdev[1:10]^2, main = "", col = hue_pal()(2)[1],
        cex.axis = 1, ylab = "")

# Calculate variance explained by each principal component
pca_var <- pg_pca10$sdev^2
pca_ve  <- pca_var / sum(pca_var) * 100

# Draw the scree plot
pdf(paste0(plot_dir, "PCA_Variance_Percent_Scree_Plot.pdf"), width = 5, height = 4)
barplot(pca_ve[1:10], 
        col =  hue_pal()(2)[1],
        xlab = "Principal Component", 
        ylab = "Percentage of Variance Explained (%)",
        main = "Scree Plot",
        # names.arg = paste0("PC", 1:10),
        ylim = c(0, max(pca_ve[1:10]) + 5))
dev.off()

pdf(paste0(plot_dir, "PCA_Variance_Scree_Plot.pdf"), width = 5, height = 4)
barplot(pca_var[1:10], 
        col = hue_pal()(2)[1],
        xlab = "Principal Component", 
        ylab = "Variance Explained",
        main = "Scree Plot",
        ylim = c(0, max(pca_var[1:10]) + 1))
dev.off()

# Prediction
yv <- predict(pg_pca10)[,1]
fit <- lm(yv ~ pgdata$hay)

pdf(paste0(plot_dir, "PCA1_vs_hay.pdf"), width = 5, height = 4)
plot(pgdata$hay, yv, xlab = "", ylab = "PC1", col= hue_pal()(2)[1], pch=16)
abline(fit, col = hue_pal()(2)[1], lwd = 2)
dev.off()

yv2 <- predict(pg_pca10)[,2]
fit <- lm(yv2 ~ pgdata$pH)

pdf(paste0(plot_dir, "PCA2_vs_ph.pdf"), width = 5, height = 4)
plot(pgdata$pH, yv2, xlab = "", ylab = "PC2", col = hue_pal()(2)[2], pch=16)
abline(fit, col = hue_pal()(2)[2], lwd = 2)
dev.off()

################################################################################
# Factor Analysis
head(pgfull)

pg_fact8 <- factanal(pgfull, 8)
loadings(pg_fact8)

# Lets make it more visible/explicit
L <- unclass(loadings(pg_fact8))
library(dplyr)
library(tidyr)
library(ggplot2)

# Order variables by the factor they load most strongly on
main_factor <- apply(abs(L), 1, which.max)
var_order <- rownames(L)[order(main_factor, -apply(abs(L), 1, max))]

pdf(paste0(plot_dir, "Factor_Analysis.pdf"), width = 6, height = 6)
as.data.frame(L) %>%
  tibble::rownames_to_column("Variable") %>%
  pivot_longer(-Variable, names_to = "Factor", values_to = "Loading") %>%
  mutate(Variable = factor(Variable, levels = rev(var_order))) %>%
  ggplot(aes(Factor, Variable, fill = Loading)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(abs(Loading) >= 0.3, round(Loading, 2), "")), size = 3) +
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "firebrick",
                       limits = c(-1, 1)) +
  labs(x = NULL, y = NULL) +
  theme_minimal()
dev.off()

################################################################################
# Cluster Analysis
# 1 - K-means
kmd <- read.table("Datasets/kmeansdata.txt", header = TRUE)
head(kmd)
table(kmd$group)

pdf(paste0(plot_dir, "KM_Cluster_Plot.pdf"), width = 6, height = 6)
plot(kmd$x, kmd$y, col=hue_pal()(6)[kmd$group],
     pch=16,
     xlab = "X",
     ylab = "Y")
dev.off()

# 4-6 clusters
model4 <- kmeans(kmd[, 1:2], 4)
model5 <- kmeans(kmd[, 1:2], 5)
model6 <- kmeans(kmd[, 1:2], 6)

pdf(paste0(plot_dir, "KM_4_6_Cluster_Plot.pdf"), width = 8, height = 8)
par(mfrow = c(2, 2))
plot(kmd$x, kmd$y, col=hue_pal()(4)[model4[[1]]],
     pch=16, xlab = "", ylab = "", main = "k=4")

plot(kmd$x, kmd$y, col=hue_pal()(5)[model5[[1]]],
     pch=16, xlab = "", ylab = "", main = "k=5")

plot(kmd$x, kmd$y, col=hue_pal()(6)[model6[[1]]],
     pch=16, xlab = "", ylab = "", main = "k=6")

plot(kmd$x, kmd$y, col=hue_pal()(6)[kmd$group],
     pch=16, xlab = "", ylab = "", main = "Original")
dev.off()

# Back to taxonomy dataset
head(taxa)

# kmeans with k=4
taxa_kn <- kmeans(taxa[,-1], 4)
taxa_kn$centers
taxa_kn$cluster

# See how well it has clustered them
table(taxa$Taxon, taxa_kn$cluster)

# 2 - Hierarchical Clustering
## back to pgdata
head(pgdata[1:54])

# Add labels
labels <- paste(pgdata$plot, letters[pgdata$lime], sep = "")

# Check whether the lables are unqiue
sort(table(labels))

# Calculate the distance: here we use plot distance (89x89 matrix)
pgdist <- dist(pgdata[,1:54])

# Check it
dim(as.matrix(pgdist))

hpg <- hclust(pgdist)

pdf(paste0(plot_dir, "pg_data_hierarchical_clustering.pdf"), width = 8, height = 6)
plot(hpg, labels = labels, main = "", xlab = "", ylab = "", axes = FALSE, 
     sub = "", cex = 0.6)
dev.off()

# Do the same on taxonomic data
head(taxa)

pdf(paste0(plot_dir, "taxa_hierarchical_clustering.pdf"), width = 8, height = 6)
plot(hclust(dist(taxa[,-1])), main = "", xlab = "", ylab = "", axes = FALSE, 
     sub = "", cex = 0.6)
dev.off()

################################################################################
# Discriminant Analysis
library(MASS)

table(taxa$Taxon)

lda_model <- lda(Taxon ~ ., data = taxa)
# Summary gives not much important stuff
summary(lda_model)

# Get the entire output here
lda_model

# 4 taxa classes -> 4 colors: This is ugly AF
plot(lda_model, col=rep(hue_pal()(4), each=30))

# Make nice PCA-Style plot using ggplot
## Extract the LD scores by predicting on the original data
lda_pred <- predict(lda_model, taxa)
plot_data <- data.frame(Taxon = taxa$Taxon,
                        LD1 = lda_pred$x[, 1],
                        LD2 = lda_pred$x[, 2])

head(plot_data)

pdf(paste0(plot_dir, "taxa_LDA_plot.pdf"), width = 6, height = 4)
ggplot(plot_data, aes(x = LD1, y = LD2, color = Taxon, fill = Taxon)) +
  geom_point(size = 2.5, alpha = 0.8) +
  # Add 95% confidence zones
  stat_ellipse(geom = "polygon", alpha = 0.1, level = 0.95) +
  scale_color_manual(values = hue_pal()(4)) +
  scale_fill_manual(values = hue_pal()(4)) +
  theme_classic() +
  labs(title = "LDA Separation",
       x = "LD1 (72.7%)",
       y = "LD2 (14.2%)") +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold"))
dev.off()

# Train a model on half of the data and then use the other half for testing
train <- sort(sample(1:120, 60))
table(taxa$Taxon[train])

lda_mode2 <- lda(Taxon ~., data = taxa, subset = train)
test <- taxa[-train,]
not_train <- predict(lda_mode2, test)
not_train$class

lda_cm <- table(taxa$Taxon[-train], not_train$class)
lda_cm

# Nice separation

################################################################################
# Neural Networks
library(nnet)
# size = number of hidden units
nn_model <- nnet(Taxon ~ ., data = taxa, subset = train, size=4, 
                 decay=1.0e-5, maxit=200)

nn_cm <- table(taxa$Taxon[-train], predict(nn_model, test, type = "class"))
nn_cm

# EOF