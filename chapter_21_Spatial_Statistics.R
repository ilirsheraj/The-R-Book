# Chapter 21 - Spatial Statistics
library(scales)
# install.packages("spatstat")
library(spatstat)
# install.packages("geoR")
library(geoR)

plot_dir <- "Plots/"

# Create the spatstat object
set.seed(42)
xcoord <- runif(100)
ycoord <- runif(100)

# create the ppp object recognized by the package
ran_pts <- ppp(x = xcoord, y = ycoord, window = square(r = 1))
summary(ran_pts)

pdf(paste0(plot_dir, "Simulated_Spatial_Dist.pdf"), width = 6, height = 6)
plot.ppp(ran_pts, cols=hue_pal()(3)[1], main = "", pch=20)
dev.off()

# To explore these techniques, we will use the ponderosa data set from spatstat
data("ponderosa")
?ponderosa
# 108 Ponderosa Pine (Pinus ponderosa) trees in a 120 metre square region in the
# Klamath National Forest in northern California
# It is already a spatial point pattern (ppp object)

# We cn get a quick summary of the object
summary(ponderosa)
# 108 points, divided into 9 squares means 9 points per square by chance

# We can test for random distribution with Quadrat Test as follows:
pond_quad <- quadrat.test(ponderosa, nx = 3, ny = 3)
# Divide the square into 3 subsquares: Uses chi-Square test to check deviance 
# from expected distribution

pdf(paste0(plot_dir, "Ponderosa_Quadrat_Test.pdf"), width = 6, height = 6)
plot(ponderosa, main = "", cols = hue_pal()(1), pch = 20)
plot(pond_quad, add = TRUE)
dev.off()

# We can also check the statistics: In this case its not significant at 5%
pond_quad

# The second approach to exploring randomness is to select a (mathematical) 
# function that takes values at every point in the window (perhaps a covariate 
# such as direction or slope) and then compare its empirical cumulative distribution 
# function (CDF) acting on the data with the CDF that would have arisen from CSR, 
# using a Kolmogorov–Smirnov test

# Kolmogorov-Sminrov Test: Compare empirical vs CSR CDF along x-axis
cdf.test(ponderosa, covariate = "x", test = "ks")
# significant

# Check along the y-axis
cdf.test(ponderosa, covariate = "y", test = "ks")
# not significant

# Next Battery of tests
pdf(paste0(plot_dir, "Ponderosa_Distance_Test.pdf"), width = 6, height = 5)
plot(envelope(ponderosa, Gest, nsim = 19, verbose = F, global = T), main = "")
dev.off()

# Heatmap showing the intensity of the points
pdf(paste0(plot_dir, "Ponderosa_Intensity_Heatmap.pdf"), width = 6, height = 5)
plot(density(ponderosa), main = "")
dev.off()

# Model the intensity: lambda = e^(b0 + b1X)
# along the x-axis: similar to lm() model
ponderosa_model1 <- ppm(ponderosa, ~x)
ponderosa_model1

# Poisson without a covariate
ponderosa_model10 <- ppm(ponderosa)
ponderosa_model10

# Compare both models to see if there is significant improvement
anova(ponderosa_model1, ponderosa_model10)

# More explicit
anova(ponderosa_model10, ponderosa_model1, test = "Chisq")


# Fit a quadratic Model
ponderosa_model2 <- ppm(ponderosa, ~polynom(x, 2))
ponderosa_model2

# Compare model 1 and 2
anova(ponderosa_model1, ponderosa_model2, test = "Chisq")
# No significant difference: keep simple one

# Are the x-coordinates of the trees distributed the way my null model says they should be?
# Take the covariate at each data point.
# Work out what distribution those values should have under the null model.
# Compare observed to expected with KS.
cdf.test(ponderosa_model1, covariate = "x", test = "ks")
# H0: the trees' x-coordinates follow the distribution predicted by ponderosa_model1, 
# meaning intensity exp(β₀ + β₁x).-> Fail to reject it

# Check residuals
pdf(paste0(plot_dir, "Ponderosa_Residuals.pdf"), width = 7, height = 7)
diagnose.ppm(ponderosa_model1, main = "")
dev.off()

# Strauss Model: regularity up to 6 meters (r = 6)
ponderosa_model3 <- ppm(ponderosa, ~x, interaction = Strauss(r = 6))
ponderosa_model3
# Interaction Parameter (gamma): 0.5990555
# gamma < 1: regularity
# gamma > 1: Clustering
# gamma ~ 1: Neither

# Check the fitness of model 3
pdf(paste0(plot_dir, "Ponderosa_GRes.pdf"), width = 6, height = 4)
plot(Gres(ponderosa_model3), main="", legend = FALSE)
dev.off()

pdf(paste0(plot_dir, "Ponderosa_Kres.pdf"), width = 6, height = 4)
plot(Kres(ponderosa_model3), xlim = c(0,14), main="", legend = FALSE)
dev.off()

# Marks
ragwort_data <- read.table("Datasets/ragwortmap2.txt", header = T,
                           colClasses = c (type = "factor"))
head(ragwort_data)

# Convert to ppp object
ragwort <- ppp(x = ragwort_data$x, 
               y = ragwort_data$y,
               xrange = c(0, 3000),
               yrange = c(0, 1500), 
               marks = ragwort_data$type)
ragwort

summary(ragwort)

pdf(paste0(plot_dir, "Ragwort_Marks.pdf"), width = 6, height = 4)
plot(ragwort, main = "", cols = hue_pal()(4), pch = 15:18)
dev.off()

# Plot Each Species into Separate Heatmaps
pdf(paste0(plot_dir, "Ragwort_Heatmaps.pdf"), width = 6, height = 6)
plot(density(split(ragwort)), main = "")
dev.off()

# Plot of the relative frequency: zlim standardizes the plots
pdf(paste0(plot_dir, "Ragwort_Normalized_Heatmaps.pdf"), width = 6, height = 6)
plot(relrisk(ragwort), zlim=c(0,1), main="")
dev.off()

# Distance plots for the pair of species
pdf(paste0(plot_dir, "Ragwort_Distance_Plots.pdf"), width = 6, height = 6)
plot(alltypes(ragwort, "G"), title="")
dev.off()

# Point process models can be built which take the marks into account
ppm(ragwort, ~ marks)

################################################################################
# Geospatial Statistics

# The following example is a geographic-scale trial to compare the yields of 56 
# different varieties of wheat.
wheat <- read.table("Datasets/wheat.txt", header = TRUE)
head(wheat)
table(wheat$Block)
table(wheat$variety)

# Convert it to geoR object
wheat_geo <- as.geodata(wheat, coords.col = 5:4, data.col = 3)
wheat_geo

pdf(paste0(plot_dir, "Wheat_Geodata.pdf"), width = 6, height = 6)
plot(wheat_geo)
dev.off()

pdf(paste0(plot_dir, "Wheat_Geodata_Loess.pdf"), width = 6, height = 6)
plot(wheat_geo, trend = '2nd', lowess = T)
dev.off()

# Variogram: plot of distance against variability between points at that distance apart.
wheat_var <- variog(wheat_geo, trend = "2nd", max.dist = 20)

nug <- 18
sill <- 28
partial_sill <- sill - nug
range <- 13
exp_est <- variofit(wheat_var, cov.model = "exp", 
                    ini.cov.pars = c(partial_sill, range), nugget = nug)
sph_est <- variofit(wheat_var, cov.model = "sph",
                    ini.cov.pars = c (partial_sill, range), nugget = nug)

pdf(paste0(plot_dir, "Wheat_Variograms.pdf"), width = 8, height = 4)
par(mfrow=c(1,2))
plot(wheat_var, main = "", 
     xlab = "distance (h)", ylab = "variogram", col = "red", pch = 19)

plot(wheat_var, main = "", cex.lab = 1.5, xlab = "distance (h)", 
     ylab = "variogram", col = "red", pch = 19)

lines(exp_est, col = "brown", lty = 3, lwd = 2)
lines(sph_est, col = "blue", lty = 3, lwd = 2)
legend(5, 8, legend = c("exponential", "spherical"), lty = c (2, 3),
       lwd = rep(2, 2), bty = "n", cex = 1, col = c("blue", "brown"))
dev.off()



plot(wheat$longitude, wheat$latitude, xlab = "", ylab = "", col = "blue",
     pch = 20, cex = wheat$yield / 10)
pred_pts <- matrix (c (4, 22, 13, 23, 42, 33, 12, 6), ncol = 2)
krige_pts <- krige.conv (wheat_geo, loc = pred_pts,
                         krige = krige.control (obj.m = exp_est))
points (pred_pts, col = "red", pch = 20, cex = krige_pts$predict / 10)



