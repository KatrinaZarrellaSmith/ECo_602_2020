#Probability distributions

#the probability density, i.e. the height of the curve at x
dnorm(x, mean = 0, sd = 1, log = FALSE)
#try with x: -1.96, -1, 0, 1.96

#the cumulative probability density, i.e. the area under the curve left of x
pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)

#the quantile function
qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)

#function to generate random, normally-distributed numbers
rnorm(n, mean = 0, sd = 1)


#NORMAL CURVE
# Generate a vector of x-values
x = seq(-3, 3, length.out = 1000)
y = dnorm(x) #change mean and SD here

plot(x, y, main = "Normal PDF", type = "l")
abline(h = 0) #plot horizontal line at y=0

#RESIDUALS
#residual is the difference between a predicted value and the observed value

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

#Ex. 1
set.seed(123)
n = 17
slope = 0.7
intcp = 0.2

guess_x = 6
guess_y = 4
guess_slope = 0.72

x = runif(n = n, min = 1, max = 10)
y = rnorm(n = n, mean = slope * x + intcp)

plot(x, y, pch = 16)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

#Ex. 2
set.seed(10)
n_pts = 30
x_min = 1
x_max = 100
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot(y_observed ~ x, data = dat, pch = 8)

guess_x = 30
guess_y = -1
guess_slope = 0.1

plot(y_observed ~ x, data = dat, pch = 8)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

y_predicted = line_point_slope(dat$x, guess_x, guess_y, guess_slope)

#add to data frame
dat = merge(dat,y_predicted)
colnames(dat)[3] = "y_predicted"

#residual=measured-predicted
dat$resids = abs(dat$y_observed-dat$y_predicted)

#sum of residuals
sum(dat$resids)

#Q1
#set question parameters
my_mean = 10.4
my_sd = 2.4

#norm_17
set.seed(10) #makes random number generation reproducible
n_pts = 17 #number of elements in vector
x_min = 1 #min for vector
x_max = 100 #max for vector
x = runif(n = n_pts, min = x_min, max = x_max)#generate n number of random samples within min-max interval
norm_17 = rnorm(n_pts, mean = my_mean, sd = my_sd) #generate random, normally-distributed numbers

#norm_30
set.seed(10)
n_pts = 30
x_min = 1
x_max = 100
x = runif(n = n_pts, min = x_min, max = x_max)
norm_30 = rnorm(n_pts, mean = my_mean, sd = my_sd)

#norm_300
set.seed(10)
n_pts = 300
x_min = 1
x_max = 100
x = runif(n = n_pts, min = x_min, max = x_max)
norm_300 = rnorm(n_pts, mean = my_mean, sd = my_sd)

#Q2
#save image to png file
png(filename = "lab_04_hist_01.png", 
    width = 700, #pixels wide
    height = 1400, #pixels high
    res = 180, #dpi (pixels/inch)
    bg = "transparent")

#create 3 histograms in 1 column
par(mfrow = c(3, 1))

hist(norm_17, 
     xlim = c(0,20),
     main = "Randomly Generated Data, n=17", 
     xlab = "Random Data")
hist(norm_30, 
     xlim = c(0,20),
     main = "Randomly Generated Data, n=30", 
     xlab = "Random Data")
hist(norm_300, 
     xlim = c(0,20),
     main = "Randomly Generated Data, n=300", 
     xlab = "Random Data")

#stop png
dev.off()

#Q4
#save image to png file
png(filename = "norm_1.png", 
    width = 1400, #pixels wide
    height = 1300, #pixels high
    res = 180, #dpi (pixels/inch)
    bg = "transparent")
# Generate a vector of x-values
x = seq(0, 20, length.out = 1000)
y = dnorm(x, mean = my_mean, sd = my_sd, log = FALSE)

plot(x, y,
     main = "Density curve of a normal distribution\n(mean = 10.4, SD = 2.4)", 
     type = "l") #line type
abline(h = 0)

dev.off()

#Q5
#save image to png file
png(filename = "sim_data_scatterplots.png", 
    width = 1400, #pixels wide
    height = 1100, #pixels high
    res = 180, #dpi (pixels/inch)
    bg = "transparent")

#create 4 scatterplots in 2 columns
par(mfrow = c(2,2))

set.seed(5)
n_pts = 50
x_min = -5
x_max = 5
x = runif(n = n_pts, min = x_min, max = x_max)
dat = data.frame(x = x, y_observed = rnorm(n_pts))
plot(dat$x,dat$y_observed,
     xlab = "Random Data",
     ylab = "Density of Probability",
     pch = 5)

set.seed(444)
n_pts = 200
x_min = 1
x_max = 50
x = runif(n = n_pts, min = x_min, max = x_max)
dat = data.frame(x = x, y_observed = rnorm(n_pts))
plot(dat$x,dat$y_observed,
     xlab = "Random Data",
     ylab = "Density of Probability",
     pch = 6)

set.seed(3000)
n_pts = 500
x_min = 1
x_max = 100
x = runif(n = n_pts, min = x_min, max = x_max)
dat = data.frame(x = x, y_observed = rnorm(n_pts))
plot(dat$x,dat$y_observed,
     xlab = "Random Data",
     ylab = "Density of Probability",
     pch = 3)

set.seed(2)
n_pts = 1000
x_min = 1
x_max = 1000
x = runif(n = n_pts, min = x_min, max = x_max)
dat = data.frame(x = x, y_observed = rnorm(n_pts))
plot(dat$x,dat$y_observed,
     xlab = "Random Data",
     ylab = "Density of Probability",
     pch = 4)

dev.off()

#Q6
#save image to png file
png(filename = "lab_04_fitted_line.png", 
    width = 1400, #pixels wide
    height = 1600, #pixels high
    res = 180, #dpi (pixels/inch)
    bg = "transparent")

#generate random data
set.seed(444)
n_pts = 200
x_min = 1
x_max = 50
x = runif(n = n_pts, min = x_min, max = x_max)
dat = data.frame(x = x, y_observed = rnorm(n_pts))

#scatterplot
plot(dat$x,dat$y_observed,
     xlab = "Random Data",
     ylab = "Density of Probability",
     pch = 6)

#add linear function from visual inspection
slope = 5
intcp = 1
guess_x = 20
guess_y = -0.05
guess_slope = 0.01
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T, 
      col = "red", lwd = 2)

dev.off()

#Q7
#create a column of predicted y-values
y_predicted = line_point_slope(dat$x, guess_x, guess_y, guess_slope)

#add to data frame
dat = merge(dat,y_predicted)
#rename column header
colnames(dat)[3] = "y_predicted"

#add residuals(=measured-predicted) to data frame
dat$resids = dat$y_observed-dat$y_predicted