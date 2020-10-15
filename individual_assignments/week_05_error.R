#Lab 5 Modeling Errors

#clear workspace
rm(list=ls(all=TRUE))

# ----Ricker----
{
#a determines the initial slope
#b=x coordinate for height of curve (a=1/b)
#Ricker function f(x)=axe^???bx
ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}
#plot
curve(
  ricker_fun(x, 1, 1), 
  from = 0, to = 5, #range of x-values it should include in the plot
  add = FALSE, #creates a new plot
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")
}

# ----Exponential----
{
#exponential function f(x)=ae^bx
exp_fun = function(x, a, b) 
{
  return(a * exp(-b * x))
}

#plot
curve(
  exp_fun(x, 1, 1), 
  from = 0, to = 10, #range of x-values it should include in the plot
  add = FALSE, #creates a new plot
  main = "Exponential function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")
}

# ----Error----
{
#choose 50 uniformly-distributed random x-values within the interval 2 to 10
# Seed the RNG so we can reproduce our results
set.seed(1234567)
# Specify the x-range and number of points:
n_pts = 50
x_min = 2
x_max = 10
# Generate the x-values
x_sim = runif(n_pts, min = x_min, max = x_max)
#choose an intercept and slope for our deterministic model and generate the 'predicted' y values
param_intercept = 2.3
param_slope = 0.67
y_pred = param_intercept + x_sim * param_slope
plot(x_sim, y_pred)
#add some normally-distributed noise to generate our 'observed' y-values
error_mean = 0
error_sd = 0.25
y_observed = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd)
plot(x_sim, y_observed)
}

{
#make the variability larger with increasing values of x
error_mean = 0
error_sd = 0.1

y_observed_2 = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd * x_sim)
plot(x_sim, y_observed)

plot(x_sim, y_observed_2)
}

{
#generate exponentially-distributed errors with rate 1.2
  y_observed_3 = 
    y_pred + 
    rexp(n = n_pts, 
         rate = 1.2)
  plot(x_sim, y_observed)
  plot(x_sim, y_observed_3)
}

{
par(mfrow = c(3, 1))
  plot(x_sim, y_observed)
  plot(x_sim, y_observed_2)
  plot(x_sim, y_observed_3)
}

# ----Residuals----
{
#how would you choose deterministic and stochastic models?
#The deterministic functions look linear, so we'll focus on the stochastic models.
#A first step is to examine histograms of the residuals:
par(mfrow = c(3, 1))
hist(y_observed - y_pred, main = "sim data 1", xlab = "observed y=values")
hist(y_observed_2 - y_pred, main = "sim data 2", xlab = "observed y=values")
hist(y_observed_3 - y_pred, main = "sim data 3", xlab = "observed y=values")
}

# ----Q1: Exponential----
{
#exponential function f(x)=ae^bx
  exp_fun = function(x, a, b) 
  {
    return(a * exp(-b * x))
  }
#save image to png file
  png(filename = "lab_05_exp_fun.png", 
      width = 1000, #pixels wide
      height = 700, #pixels high
      res = 180, #dpi (pixels/inch)
      bg = "transparent")
#plot
  curve(
    exp_fun(x, .1, 0.1), 
    from = -10, to = 80, #range of x-values it should include in the plot
    add = FALSE, #creates a new plot
    main = "Exponential function: variable a and b",
    ylab = "f(x)", xlab = "x",
    lwd = 2
    )
  curve(
    exp_fun(x, 100, 0.3), 
    from = -10, to = 80, #range of x-values it should include in the plot
    add = TRUE, #creates a new plot
    main = "Exponential function: variable a and b",
    ylab = "f(x)", xlab = "x",
    lty = 3,
    lwd = 2
  )
  curve(
    exp_fun(x, 1.2, 0.2), 
    from = -10, to = 80, #range of x-values it should include in the plot
    add = TRUE, #creates a new plot
    main = "Exponential function: variable a and b",
    ylab = "f(x)", xlab = "x",
    col = "red",
    lwd = 2
  )
  curve(
    exp_fun(x, 1.2, 0.4), 
    from = -10, to = 80, #range of x-values it should include in the plot
    add = TRUE, #creates a new plot
    main = "Exponential function: variable a and b",
    ylab = "f(x)", xlab = "x",
    col = "red",
    lty = 3,
    lwd = 2
  )
  dev.off()
}

# ----Q3: Ricker----
{
#Ricker function f(x)=axe^???bx
ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}
#save image to png file
  png(filename = "lab_05_ricker_fun.png", 
      width = 1200, #pixels wide
      height = 700, #pixels high
      res = 180, #dpi (pixels/inch)
      bg = "transparent")
#plot
curve(
  ricker_fun(x, 25, 0.1), 
  from = 0, to = 100, #range of x-values it should include in the plot
  add = FALSE, #creates a new plot
  main = "Ricker function: variable a and b",
  ylab = "f(x)", xlab = "x",
  lwd = 2)
curve(
  ricker_fun(x, 25, 1), 
  from = 0, to = 100, #range of x-values it should include in the plot
  add = TRUE, #creates a new plot
  main = "Ricker function: variable a and b",
  ylab = "f(x)", xlab = "x",
  lwd = 2, lty = 3)
curve(
  ricker_fun(x, 10, 0.2), 
  from = 0, to = 100, #range of x-values it should include in the plot
  add = TRUE, #creates a new plot
  main = "Ricker function: variable a and b",
  ylab = "f(x)", xlab = "x",
  lwd = 2, lty = 3)
curve(
  ricker_fun(x, 75, 0.3), 
  from = 0, to = 100, #range of x-values it should include in the plot
  add = TRUE, #creates a new plot
  main = "Ricker function: variable a and b",
  ylab = "f(x)", xlab = "x",
  lwd = 2, col = "red")
curve(
  ricker_fun(x, 50, 0.3), 
  from = 0, to = 100, #range of x-values it should include in the plot
  add = TRUE, #creates a new plot
  main = "Ricker function: variable a and b",
  ylab = "f(x)", xlab = "x",
  lwd = 2, lty = 3, col = "red")
curve(
  ricker_fun(x, 40, 0.3), 
  from = 0, to = 100, #range of x-values it should include in the plot
  add = TRUE, #creates a new plot
  main = "Ricker function: variable a and b",
  ylab = "f(x)", xlab = "x",
  lwd = 2, lty = 3, col = "red")
dev.off()
}

# ----Q5: Scatterplot-----
dat_dispersal = read.csv("salamander_dispersal.csv")
{
#save image to png file
png(filename = "lab_05_scatterplot.png", 
    width = 1000, #pixels wide
    height = 1000, #pixels high
    res = 180, #dpi (pixels/inch)
    bg = "transparent")
#scatterplot of the dispersal data
plot(dat_dispersal$dist.class, dat_dispersal$disp.rate.ftb,
     main = "Dispersal Rate of First-Time Breeders",
     xlab = "Distance Class (m)",
     ylab = "Standardized Dispersal Rate",
     pch = 16,
     cex = .9)
dev.off()
}

# ----Q6: Linear----
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
  
{
#save image to png file
  png(filename = "lab_05_linear.png", 
      width = 1000, #pixels wide
      height = 1000, #pixels high
      res = 180, #dpi (pixels/inch)
      bg = "transparent")
#scatterplot of the dispersal data
  plot(dat_dispersal$dist.class, dat_dispersal$disp.rate.ftb,
       main = "Linear Model:\nDispersal Rate of First-Time Breeders",
       xlab = "Distance Class (m)",
       ylab = "Standardized Dispersal Rate",
       pch = 16,
       cex = .9)
#plot linear fit
#fit = lm(disp.rate.ftb~dist.class, data = dat_dispersal)
#summary(fit)
#abline((coef(fit)[1:2]), lwd =2, col = "blue")
#Mike's way
x = dat_dispersal$dist.class
x1 = 800
y1 = 0.25
slope = -0.0004
curve(line_point_slope(x, x1, y1, slope), add = T,
      lwd =2, col = "blue")
dev.off()
}

# ----Q7: Exponential----
{
#save image to png file
  png(filename = "lab_05_exponential.png", 
      width = 1000, #pixels wide
      height = 1000, #pixels high
      res = 180, #dpi (pixels/inch)
      bg = "transparent")
#scatterplot of the dispersal data
  plot(dat_dispersal$dist.class, dat_dispersal$disp.rate.ftb,
       main = "Exponential Model:\nDispersal Rate of First-Time Breeders",
       xlab = "Distance Class (m)",
       ylab = "Standardized Dispersal Rate",
       pch = 16,
       cex = .9)
#plot exponential fit
  exp_fun = function(x, a, b) 
  {
    return(a * exp(-b * x))
  }
#plot
  curve(
    exp_fun(x, 1.5, 0.003), 
    from = 0, to = 1600, #range of x-values it should include in the plot
    add = TRUE,
    lwd = 2,
    col = "red") #creates a new plot
dev.off()
}
  
# ----Q8: Ricker----
{
#save image to png file
  png(filename = "lab_05_ricker.png", 
      width = 1000, #pixels wide
      height = 1000, #pixels high
      res = 180, #dpi (pixels/inch)
      bg = "transparent")
#scatterplot of the dispersal data
  plot(dat_dispersal$dist.class, dat_dispersal$disp.rate.ftb,
       main = "Ricker Model:\nDispersal Rate of First-Time Breeders",
       xlab = "Distance Class (m)",
       ylab = "Standardized Dispersal Rate",
       pch = 16,
       cex = .9)
#Ricker function f(x)=axe^???bx
  ricker_fun = function(x, a, b) 
  {
    return(a * x * exp(-b * x))
  }
#plot
  curve(
    ricker_fun(x, 0.01, 0.0049), 
    from = 0, to = 1600, #range of x-values it should include in the plot
    add = TRUE, #creates a new plot
    lwd = 2,
    col = "chartreuse4")
dev.off()
}

# ----Q9: Linear Residuals----
#calculate residuals
  resids_linear = dat_dispersal$disp.rate.ftb - line_point_slope(x, x1, y1, slope)
{
#save image to png file
  png(filename = "lab_05_linear_res.png", 
      width = 1000, #pixels wide
      height = 1000, #pixels high
      res = 180, #dpi (pixels/inch)
      bg = "transparent")
#histogram of residuals
  hist(x = resids_linear,
       main = "Linear Model:\nResiduals",
       xlab = "Residuals",
       col = "blue")
dev.off()
}
  
# ----Q10: Ricker Residuals----
#calculate residuals
  resids_ricker = dat_dispersal$disp.rate.ftb - ricker_fun(x, 0.01, 0.0049)
{
#save image to png file
  png(filename = "lab_05_ricker_res.png", 
      width = 1000, #pixels wide
      height = 1000, #pixels high
      res = 180, #dpi (pixels/inch)
      bg = "transparent") 
#histogram of residuals
  hist(x = resids_ricker,
       main = "Ricker Model:\nResiduals",
       xlab = "Residuals",
       col = "chartreuse4")
dev.off()
}
  
# ----Q11: Exponential Residuals----
#calculate residuals
  resids_exp = dat_dispersal$disp.rate.ftb - exp_fun(x, 1.5, 0.003)
{
#save image to png file
  png(filename = "lab_05_exp_res.png", 
      width = 1000, #pixels wide
      height = 1000, #pixels high
      res = 180, #dpi (pixels/inch)
      bg = "transparent")
#histogram of residuals
  hist(x = resids_exp,
       main = "Exponential Model:\nResiduals",
       xlab = "Residuals",
       col = "red")
dev.off()
}
  