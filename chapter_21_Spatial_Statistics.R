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

# along the x-axis
ponderosa_model1 <- ppm(ponderosa, ~x)
ponderosa_model1

# Poisson without a covariate
ponderosa_model10 <- ppm(ponderosa)
ponderosa_model10
