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
     xlab = "Elevation",
     xlim = c(0,1000),
     ylim = c(0,200),
     col = "cornflowerblue")
#alternate axis control
#axis(1, seq(0,950,100))
#axis(2, seq(0,200, 50))

# dev.off() tells R to finish the plot formatting and write the data into the file.
dev.off()


#determine x-axis limits
range(dat_habitat$slope)

#slope histogram
hist(dat_habitat$slope,
     main = "Histogram of sampling site slope",
     xlab = "slope",
     xlim = c(0,120),
     ylim = c(0,200),
     col = "cornflowerblue")

#determine x-axis limits
range(dat_habitat$aspect)

#aspect histogram
hist(dat_habitat$aspect,
     main = "Histogram of sampling site aspect",
     xlab = "aspect",
     ylim = c(0,200),
     col = "cornflowerblue")

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

#elevation scatterplot
plot(x = dat_habitat$elev,
     y = dat_habitat$ba.tot,
     xlab = "elevation",
     ylab = "basal area of trees",
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


#slope scatterplot
plot(x = dat_habitat$slope,
     y = dat_habitat$ba.tot,
     xlab = "slope",
     ylab = "basal area of trees",
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


#aspect scatterplot
plot(x = dat_habitat$aspect,
     y = dat_habitat$ba.tot,
     xlab = "aspect",
     ylab = "basal area of trees",
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