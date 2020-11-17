#Lab 11 Inference Frameworks

#Simulation/forward modeling:
#pick a model and parameters and work forward to predict patterns in the data

rm(list=ls(all=TRUE))
require(here)

#read the data
bird.sub = read.csv(here("data","bird.sub.csv"))
hab.sub = read.csv(here("data","hab.sub.csv"))
#merge based on the columns: basin, and sub
birdhab = merge(bird.sub, hab.sub, by = c('basin', 'sub'))
dim(birdhab) #check

# ----Linear Model----

#exploratory scatterplot
plot(birdhab$ls, birdhab$BRCR)

#fit a simple linear regression model
fit_1 = lm(birdhab$BRCR ~ birdhab$ls)

#Plot the regression line over the scatterplot
abline(fit_1)

#model coefficient table
summary(fit_1)

#data-collection effort was one realization of the stochastic process of sampling. There are many, many other possible observations that could have been sampled
#stochastic simulation=What might the data look like if we were to obtain another snapshot?

#Deterministic Model: Linear Function
#build an R function to calculate the value of a linear function given a value of x, a slope parameter, and an intercept parameter
linear = function(x, y_int, slope) (y_int + slope * x)

linear(x = 1, y_int = 1, slope = 1) #test=2
linear(x = 3:5, y_int = 1, slope = 1) #test=4,5,6
linear(x = 3:5, y_int = -1, slope = 1) #test=2,3,4
linear(x = 3:5, y_int = -1, slope = 0.01) #test=-0.97,-0.96,-0.95

#Stochastic Model: Normal Distribution
#normally-distributed errors
rnorm()

# ----Simulation Function----
#deterministic and stochastic functions into a single data simulator
linear_simulator = function(x, y_int, slope, st_dev) (y_int + slope * x + rnorm(x, 0, st_dev))

#test
{
n = 200

par(mfrow = c(2, 2))
for (i in 1:4)
{
  x = runif(n = n)
  plot(
    x, linear_simulator(x, 1, 4.5, 0.1),
    main = "", xlab = "x", ylab = "y",
    pch = 16, col = rgb(0, 0.2, 0, 0.2))
}
}

{
n = 400

par(mfrow = c(2, 2))
for (i in 1:4)
{
  x = runif(n = n)
  plot(
    x, linear_simulator(x, 10, slope = -6.5, st_dev = 1.1),
    main = "", xlab = "x", ylab = "y",
    pch = 16, col = rgb(0, 0.2, 0, 0.2))
}
}

#Retrieve the model coefficients
#extract the intercept and slope values
fit_1_coefs = coefficients(fit_1)
str(fit_1_coefs)
int_obs = fit_1_coefs[1] #intercept
slope_obs = fit_1_coefs[2] #slope

#extract standard deviation
fit_1_summary = summary(fit_1)
sd_obs = fit_1_summary$sigma #SD

#Simulate Data
#keep the original values of x and let brown creeper abundance vary among simulations
plot(
  x = birdhab$ls, 
  y = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  ),
  main = "Simulated Data",
  xlab = "late-successional forest",
  ylab = "Brown Creeper Abundance")

#plot the observed data first, then add the simulated data to the existing plot
plot(
  birdhab$ls, birdhab$BRCR, 
  xlab = "late-successional forest extent",
  ylab = "Brown Creeper abundance",
  pch = 19)

points(
  x = birdhab$ls, 
  y = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  ),
  col = adjustcolor("red", alpha = 0.3),
  pch = 16)
#One problem with the use of the normal distribution is that it is unbounded on the lower limit
#abundance cannot be negative

# ----Power Analysis----
#can we reject the null hypothesis in a single experiment
#we simulate a data set with a given intercept and slope, and number of data points
#run a linear regression
#extract the p-value (probability of observing our data if in fact it came from distribution described by the null model)
#ex. brown creeper abundance is independent of ls or has no relationship to ls
#see whether it is less than our specified alpha criterion (usually 0.05)
{
  y_sim = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  )
  
  fit_sim = lm(y_sim ~ birdhab$ls)
  summary(fit_sim)$coefficients
}

#estimate the probability of successfully rejecting the null hypothesis when it is false (the power), 
#we have to repeat this procedure many times and calculate the proportion of the time that we reject the null hypothesis

n_sims = 1000 #specify the number of simulations to run
p_vals = numeric(n_sims) #vector to hold the p-value for each simulation

#repeat what we did above (without redefining some of the objects that have not changed, such as x_vals, a, b, and y.error), each time saving the p-value in the storage vector
for(i in 1:n_sims)
{
  y_sim = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  )
  fit_sim = lm(y_sim ~ birdhab$ls)
  
  p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
}

#calculate the power by summing up how many times we rejected the null hypothesis at the specified alpha-level, and dividing by the number of simulations to convert it to a proportion
sum(p_vals < 0.05) / n_sims
#This is the power to detect a slope of roughly b = 0.006 with a sample size of N = 30, given our specified statistical model

#make our simulation loops a little simpler
linear_sim_fit = function(x, y_int, slope, st_dev)
{
  y_sim = linear_simulator(
    x = x,
    y_int = y_int,
    slope = slope,
    st_dev = st_dev
  )
  return(lm(y_sim ~ x))
}

#how the power changes as we change some aspect of the design such as the sample size or the effect size (slope, in this case)
#have to repeat the entire procedure multiple times, each time changing some parameter of the simulation such as the slope or the sample size

#examine how power changes as a function of the slope
{
  alpha = 0.05
  n_sims = 10
  p_vals = numeric(n_sims)
  effect_size = seq(-.01, .01, by = 0.001) #vector of slope values
  effect_power = numeric(length(effect_size)) #storage vector to hold the results
  
  for(j in 1:length(effect_size)) #loop over slope values
  {
    for(i in 1:n_sims) #loop over simulations
    {
      fit_sim = linear_sim_fit(
        x = birdhab$ls,
        y_int = int_obs,
        slope = effect_size[j], #power is computed for the first value of slope in effect_size
        st_dev = sd_obs
      )
      
      p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
    }
    effect_power[j] = sum(p_vals < alpha) / n_sims #result is stored in the first position of the storage vector
  }
}
#Each time through the outer loop, a new value of power is computed for the next value of slope
#result is a vector of power values for increasing values of slope

#plot the result and add a vertical line to show the slope of our original data set
plot(effect_size, effect_power, type = 'l', 
     xlab = 'Effect size', ylab = 'Power')
abline(v = coef(fit_1)[2], 
       lty = 2, col = 'red')

#same thing for a gradient in sample sizes
{
  alpha = 0.05
  n_sims = 100
  p_vals = numeric(n_sims)
  sample_sizes = seq(10, 50)
  sim_output_1 = numeric(length(sample_sizes))
  for(j in 1:length(sample_sizes))
  {
    x_vals = seq(0, 100, length.out = sample_sizes[j])
    
    for(i in 1:n_sims)
    {
      fit_sim = linear_sim_fit(
        x = x_vals,
        y_int = int_obs,
        slope = slope_obs,
        st_dev = sd_obs
      )
      p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
    }
    sim_output_1[j] = sum(p_vals < alpha) / n_sims
  }
}

plot(sample_sizes, sim_output_1,  type = 'l', 
     xlab = 'Sample size', ylab = 'Power')
abline(v = nrow(birdhab), lty = 2, col = 'red')

# ----Q1----
#Population Dispersion Analysis
#examine how power changes as a function of the population standard deviation
#Decide the range of population standard deviations to test
#What was the standard deviation of the residuals from our model
c(sd_obs, sd_obs*7)
#suggest starting at the observed standard deviation up to 3 times the observed standard deviation
  alpha = 0.05
  n_sims = 5000
  p_vals = numeric(n_sims)
  
 # What was the observed standard deviation?
  sd_obs
  
 # Specify the number of different standard deviation values to simulate:
  n_sds = 100
  pop_sds = seq(from = sd_obs, to = sd_obs*7, length.out = n_sds)  #vector of SD values
  pop_sd_power = numeric(length(pop_sds)) #storage vector to hold the results
  
  for(j in 1:length(pop_sds)) #loop over slope values
  {
    pop_sd_j = pop_sds[j]
    for(i in 1:n_sims)
    {
      fit_sim = linear_sim_fit(
        x = birdhab$ls,
        y_int = int_obs,
        slope = slope_obs, 
        st_dev = pop_sds[j] #power is computed for the first value of SD in effect_size
      )
      
      p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
    }
    pop_sd_power[j] = sum(p_vals < alpha) / n_sims #result is stored in the first position of the storage vector
  }

#Each time through the outer loop, a new value of power is computed for the next value of slope
#result is a vector of power values for increasing values of slope

#plot the result and add a vertical line to show the slope of our original data set
  plot(pop_sds, pop_sd_power, type = 'l', 
       xlab = 'Population Standard Deviation', 
       ylab = 'Power')
  abline(v = sd_obs, lty = 2, col = 'red')

  
# ----Combinations----

#vary combinations of parameters, slope and sample size
{
  alpha = 0.05
  n_sims = 10
  p_vals = numeric(n_sims)
  effect_sizes = seq(-.01, .01, by = 0.001)
  sample_sizes = seq(10, 50)
  sim_output_2 = matrix(nrow = length(effect_sizes), ncol = length(sample_sizes))
  
  for(k in 1:length(effect_sizes))
  {
    effect_size = effect_sizes[k]
    for(j in 1:length(sample_sizes))
    {
      x_vals = seq(0, 100, length.out = sample_sizes[j])
      
      for(i in 1:n_sims)
      {
        fit_sim = linear_sim_fit(
          x = x_vals,
          y_int = int_obs,
          slope = effect_size,
          st_dev = sd_obs
        )
        p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
      }
      sim_output_2[k, j] = sum(p_vals < alpha) / n_sims
    }
  }
}

#plot a matrix as if it were raster data, that is to plot a grid in which the pixel color is determined by the value of the matrix element
image(sim_output_2)


# ----Q2----
#Population Dispersion and Sample Size Analysis
#Could we improve our statistical power with larger sample sizes?
#modify simulation of population standard deviation to include sample size
alpha = 0.05
n_sims = 100
p_vals = numeric(n_sims)
n_sds = 10
# you can use the values you chose in the last simulation as a guide for what to choose here
pop_sds = seq(from = sd_obs, to = sd_obs*7, length.out = n_sds)  #vector of SD values

# These were the sample sizes in the walkthrough simulation.  you may want to try a different range.
sample_sizes = seq(10, 50)

sim_output_3 = matrix(nrow = length(pop_sds), ncol = length(sample_sizes))

for(k in 1:length(pop_sds))
{
  pop_sd_k = pop_sds[k]
  for(j in 1:length(sample_sizes))
  {
    x_vals = seq(0, 100, length.out = sample_sizes[j])
    
    for(i in 1:n_sims)
    {
      fit_sim = linear_sim_fit(x = birdhab$ls,
                               y_int = int_obs,
                               slope = slope_obs, 
                               st_dev = pop_sds[j])
      
      p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
    }
    sim_output_3[k, j] = sum(p_vals < alpha) / n_sims
  }
}
image(sim_output_3)


# ----Contour----
#[iso]lines show interpolated lines at which the value is the same
contour(x = effect_sizes, y = sample_sizes, z = sim_output_2,
        xlab = "effect size",
        ylab = "sample size",
        main = "Contour Plot of Statistical Power")

# ----3D----
#static perspective plot
persp(
  x = effect_sizes, y = sample_sizes, z = sim_output_2,
  col = 'lightblue',
  theta = 30, phi = 30, expand = .75,
  ticktype = 'detailed')

#interactive plot
install.packages("rgl")
library(rgl)
persp3d(
  x = effect_sizes, y = sample_sizes, z = sim_output_2,
  col = 'lightblue',
  theta = 30, phi = 30, expand = .75,
  ticktype = 'detailed')

# ----Save----
#save the results of my effect size and sample size simulation to a file
save(
  sim_output_2, 
  file = here::here("data", "sample_size_effect_size_power_sim.Rdata"))

#load data again
load(file = here::here("data", "sample_size_effect_size_power_sim.Rdata"))
