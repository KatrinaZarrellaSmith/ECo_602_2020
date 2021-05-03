#bird habitat data

#returns the absolute path to the base directory of your RProject
require(here)

here() #[1] "C:/Users/Mr. Computer/Documents/GitHub/ECo_602_2020"

#tell whether you are looking in the right spot for your file
file.exists(here("data", "hab.sta.csv"))

#include the subdirectories (in the correct order) and filename as character values (with quotations marks)
dat_habitat = data.frame(read.csv(here("data", "hab.sta.csv")))

# ---- HISTOGRAMS ----
#save original par settings
#mypar = par()

#create a grid of multiple plots, first number is number of rows to arrange, second number is number of columns
#run immediately prior to the code you use to create your plots
par(mfrow = c(1, 3))
#annotate text over a plot 
#mext + lines - 
#  cowplot plot_grid
#base plot graphic object to ggplot to combine cowplot to ggplot
#https://stackoverflow.com/questions/19690175/title-key-on-each-panel-of-a-plot-generated-with-parmfrow-cx-y

#use the png() function to save your plot output to a png file.
#everything you run after calling png() will be directed to a file.
png(filename = "elevation_histogram.png", 
    width = 1300, #alternate units="in", 
    height = 800, #width=5, height=4, pointsize=12
    res = 150, #dpi (pixels/inch)
    bg = "transparent")
    
#expand y-axis margin
#par(mar=c(5,6,4,1)+.1)

#determine x-axis limits (not needed while saving with png)
#range(dat_habitat$elev)

#elevation histogram
hist(dat_habitat$elev,
     main = "Histogram of sampling site elevation",
     xlab = "Elevation (m)",
     xlim = c(0,1000),
     ylim = c(0,200),
     col = "cornflowerblue")
#alternate axis control
#axis(1, seq(0,950,100))
#axis(2, seq(0,200, 50))
#https://rstudio-pubs-static.s3.amazonaws.com/297778_5fce298898d64c81a4127cf811a9d486.html

# dev.off() tells R to finish the plot formatting and write the data into the file.
dev.off()

#slope
png(filename = "slope_histogram.png", 
    width = 1300, #alternate units="in", 
    height = 800, #width=5, height=4, pointsize=12
    res = 150, #dpi (pixels/inch)
    bg = "transparent")

#determine x-axis limits
range(dat_habitat$slope)

#slope histogram
hist(dat_habitat$slope,
     main = "Histogram of sampling site slope",
     xlab = "Slope (%)",
     xlim = c(0,120),
     ylim = c(0,200),
     col = "cornflowerblue")

dev.off()

#aspect
png(filename = "aspect_histogram.png", 
    width = 1300, #alternate units="in", 
    height = 800, #width=5, height=4, pointsize=12
    res = 150, #dpi (pixels/inch)
    bg = "transparent")

#determine x-axis limits
range(dat_habitat$aspect)

#determine breaks
max(dat_habitat$aspect)

#aspect histogram
hist(dat_habitat$aspect,
     main = "Histogram of sampling site aspect",
     xlab = "Aspect (degrees)",
     ylim = c(0,200),
     col = "cornflowerblue")

dev.off()


# ---- SCATTERPLOTS ----

#transparency for plot point colors
require(scales)

# Calculates the value of x for a linear function, given the coordinates
# of a known point (x1, y1) and the slope of the line.
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

#elevation
png(filename = "scatterplot.png", 
    width = 1300, #alternate units="in", 
    height = 800, #width=5, height=4, pointsize=12
    res = 150, #dpi (pixels/inch)
    bg = "transparent")
par(mfrow = c(1, 3))

#elevation scatterplot
plot(x = dat_habitat$elev,
     y = dat_habitat$ba.tot,
     xlab = "Elevation (m)",
     ylab = "Basal area of trees (m2/ha)",
     main = "Scatterplot of sampling site",
     col = alpha("blue", 0.4), #color & transparency
     pch = 16, #shape
     cex = .92) #size

#add fit lines (choose one style)
abline(lm(dat_habitat$ba.tot ~ dat_habitat$elev), #regression line (y~x)
       col="red",
       lwd = 0.5) #thickness
lines(lowess(dat_habitat$elev, dat_habitat$ba.tot), #lowess line (x,y)
      col="red",
      lty = "dashed")
curve(line_point_slope(x, #Mike's function
                       x1 = 20, y1 = 20,
                       slope = 0.003), 
                       add = TRUE,
                       col="red",
                       lwd = 2) #thickness

dev.off()

png(filename = "slope_scatterplot.png", 
    width = 1300, #alternate units="in", 
    height = 800, #width=5, height=4, pointsize=12
    res = 150, #dpi (pixels/inch)
    bg = "transparent")

#slope scatterplot
plot(x = dat_habitat$slope,
     y = dat_habitat$ba.tot,
     xlab = "Slope (%)",
     ylab = "Basal area of trees (m2/ha)",
     main = "Scatterplot of sampling site",
     col = alpha("blue", 0.4), #color & transparency
     pch = 16, #shape
     cex = .92) #size

#add fit lines (choose one style)
abline(lm(dat_habitat$ba.tot ~ dat_habitat$slope), #regression line (y~x)
       col="red", #color
       lwd = 0.5) #thickness
lines(lowess(dat_habitat$slope, dat_habitat$ba.tot), #lowess line (x,y)
      col="red",
      lty = "dashed")
curve(line_point_slope(x, #Mike's function
                       x1 = 20, y1 = 20,
                       slope = 0.04), 
                       add = TRUE,
                       col="red",
                       lwd = 2) #thickness

dev.off()


png(filename = "aspect_scatterplot.png", 
    width = 1300, #alternate units="in", 
    height = 800, #width=5, height=4, pointsize=12
    res = 150, #dpi (pixels/inch)
    bg = "transparent")

#aspect scatterplot
plot(x = dat_habitat$aspect,
     y = dat_habitat$ba.tot,
     xlab = "Aspect (degrees)",
     ylab = "Basal area of trees (m2/ha)",
     main = "Scatterplot of sampling site",
     col = alpha("blue", 0.4), #color & transparency
     pch = 16, #shape
     cex = .92) #size

#add fit lines (choose one style)
abline(lm(dat_habitat$ba.tot ~ dat_habitat$aspect), #regression line (y~x)
       col="red", #color
       lwd = 0.5) #thickness
lines(lowess(dat_habitat$aspect, dat_habitat$ba.tot), #lowess line (x,y)
      col="red",
      lty = "dashed")
curve(line_point_slope(x, #Mike's function
                       x1 = 20, y1 = 20,
                       slope = 0.015), 
                       add = TRUE,
                       col="red",
                       lwd = 2) #thickness

dev.off()

#Green and gray alien slime plot of basal area and elevation
plot(
  ba.tot ~ elev,
  data = dat_habitat, pch = 16, cex = 0.01, col = rgb(0,0,0, 0.1),
  main = "Basal area plot: green and gray cloud effect",
  xlab = "elevation",
  ylab = "basal area")
for (i in seq(4.5, 0.01, length.out = 30))
  points(
    ba.tot ~ elev, data = dat_habitat,
    pch = 16, cex = i,
    col = rgb(0,00,0, 0.008))
for (i in seq(3.5, 0.01, length.out = 30))
  points(
    ba.tot ~ elev, data = dat_habitat,
    pch = 16, cex = i,
    col = rgb(0,0.2,0, 0.008))

# ----LAB 03----
install.packages("psych")
require(psych)
pairs.panels(iris)

require(here)
dat_bird = data.frame(read.csv(("bird.sta.csv")))

#merge data frames
dat_all = merge(dat_habitat, dat_bird)

#check merge
plot(ba.tot ~ elev, data = dat_all)

#counts of Cedar Waxwings at 100 randomly sampled sites
sample(dat_all$CEWA, 100)

#Boolean vector from the column of count data for cedar waxwings; presence/absence
cewa_present_absent = as.numeric(dat_all$CEWA > 1)

plot(x = dat_all$elev, y = cewa_present_absent)

# Function to calculate the logistic parameter a given the slope and midpoint
get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

# Function to calculate the logistic parameter b given the slope
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}


# Calculate the value of the logistic function at x, given the parameters a and b.
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}

# Calculate the value of the logistic function at x, given a slopoe and midpoint.
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}

#positive slope
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)

#negative slope
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.1), add = TRUE)

#shallower negative slope
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.05), add = TRUE)

#create a pair plot of the three terrain variable and basal area
hab_pp = data.frame(dat_habitat$elev, dat_habitat$slope, dat_habitat$aspect, dat_habitat$ba.tot)

names(hab_pp)[1] <- "Elevation"
names(hab_pp)[2] <- "Slope"
names(hab_pp)[3] <- "Aspect"
names(hab_pp)[4] <- "Basal_Area"

pairs.panels(hab_pp)

#American Robin
amro_present_absent = as.numeric(dat_all$AMRO > 0)

png(filename = "robin_logistic.png", 
    width = 700, #alternate units="in", 
    height = 700, #width=5, height=4, pointsize=12
    res = 150, #dpi (pixels/inch)
    bg = "transparent")

plot(x = dat_all$ba.tot, 
     y = amro_present_absent,
     xlab = "Basal Area (m2/ha)",
     ylab = "Absence/Presence",
     main = "American Robin")
curve(logistic_midpoint_slope(x, midpoint = 29, slope = -1), 
      add = TRUE, col = "red",
      lwd = 2)

dev.off()

#O-crowned warbler
ocwa_present_absent = as.numeric(dat_all$OCWA > 0)

png(filename = "warbler_logistic.png", 
    width = 700, #alternate units="in", 
    height = 700, #width=5, height=4, pointsize=12
    res = 150, #dpi (pixels/inch)
    bg = "transparent")

plot(x = dat_all$ba.tot, 
     y = ocwa_present_absent,
     xlab = "Basal Area (m2/ha)",
     ylab = "Absence/Presence",
     main = "O-crowned Warbler")
curve(logistic_midpoint_slope(x, midpoint = 20, slope = -1.5), 
      add = TRUE, col = "red",
      lwd = 2)

dev.off()

#total number of Gray Jays observed in all of the sampling sites
sum(dat_all$GRJA)

#total number of sampling sites in which Gray Jays were observed
sum(dat_all$GRJA > 0)
