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
#use rnorm()

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

#alternate
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
  
  n_effect_sizes = 20
  effect_sizes_1 = seq(-.01, .01, length.out = n_effect_sizes) #vector of slope values/sequence of effect sizes to try
  
  effect_size_powers = numeric(n_effect_sizes) #storage vector to hold the results/vector of statistical powers
  
  for(j in 1:n_effect_sizes) #loop over slope values/iterates over each of the effect sizes
  {
    for(i in 1:n_sims) #loop over simulations
    {
      fit_sim = linear_sim_fit(
        x = birdhab$ls,
        y_int = int_obs,
        slope = effect_sizes_1[j], #power is computed for the first value of slope in effect_size
        st_dev = sd_obs
      )
      p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)'] #p-values calculated for a specific effect size
    }
    effect_size_powers[j] = sum(p_vals < alpha) / n_sims #result is stored in the first position of the storage vector
  }
}

#Each time through the outer loop, a new value of power is computed for the next value of slope
#result is a vector of power values for increasing values of slope

#store results in data frame
sim_effect_size = 
  data.frame(
    power = effect_size_powers,
    effect_size = effect_sizes_1) #columns for effect size and statistical power

#plot the result and add a vertical line to show the slope of our original data set
plot(power ~ effect_size, data = sim_effect_size,
  type = 'l', xlab = 'Effect size', ylab = 'Power')
abline(v = coef(fit_1)[2], lty = 2, col = 'red')

#same thing for a gradient in sample sizes
{
alpha = 0.05
n_sims = 1000
p_vals = numeric(n_sims)

sample_sizes = seq(5, 100)
sample_size_powers = numeric(length(sample_sizes))

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
  sample_size_powers[j] = sum(p_vals < alpha) / n_sims
}

sim_sample_size = 
  data.frame(
    power       = sample_size_powers,
    sample_size = sample_sizes)

plot(
  power ~ sample_size, data = sim_sample_size,
  type = 'l', xlab = 'Sample size', ylab = 'Power')
abline(v = nrow(birdhab), lty = 2, col = 'red')
}

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
  n_sds = 20
  pop_sds = seq(from = 0.01, to = 1.5, length.out = n_sds)  #vector of SD values
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

#data frame of results  
  sim_output_dispersion = data.frame(
    sd = pop_sds,
    power = pop_sd_power)
  
# save your simulation results so you don't have to run it every time.
  save(
    sim_output_dispersion, 
    file = here::here("data", "lab_ll_dat_dispersion_sim.RData")) 

  #load data again
  load(file = here::here("data", "lab_ll_dat_dispersion_sim.RData"))
  
png(filename = here::here("figures", "lab_11_lineplot.png"), 
      width = 1000, #pixels wide
      height = 800, #pixels high
      res = 180, #dpi (pixels/inch)
      bg = "transparent")
#plot the result and add a vertical line to show the SD of our original data set
  plot(pop_sds, pop_sd_power, type = 'l', 
       lwd = 2,
       xlab = 'Population Dispersion', 
       ylab = 'Statistical Power',
       main = "Line Plot of Statistical Power")
  abline(v = sd_obs, lty = 2, col = 'red', lwd = 2)
dev.off()

# ----Bivariate Power----
#Bivariate Power Analysis
#vary combinations of parameters, slope and sample size  
#set n_sims and n_effect_sizes to small values as you are experimenting with the code
  {
    alpha = 0.01
    n_sims = 50
    
    p_vals = numeric(n_sims)
    
    n_effect_sizes = 20
    effect_sizes = seq(-.01, .01, length.out = n_effect_sizes)
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
      print(paste0("computing effect size ", k," of ", length(effect_sizes)))
    }
    
    sim_n_effect_size = 
      list(
        power = sim_output_2,
        effect_size = effect_sizes,
        sample_size = sample_sizes
      )
  }

#plot a matrix as if it were raster data, that is to plot a grid in which the pixel color is determined by the value of the matrix element  
image(sim_n_effect_size$power)
#save your simulation output to a file

# ----Q2----
#Population Dispersion and Sample Size Analysis
#Could we improve our statistical power with larger sample sizes?
#modify simulation of population standard deviation to include sample size
alpha = 0.05 #rerun with 0.01, name output to overlay plots with transparency
n_sims = 1500
p_vals = numeric(n_sims)
n_sds = 60
# you can use the values you chose in the last simulation as a guide for what to choose here
pop_sds = seq(from = 0.05, to = 1.5, length.out = n_sds)  #vector of SD values

# sample sizes
sample_sizes = seq(5, 100)

sim_output_3 = matrix(nrow = length(pop_sds), ncol = length(sample_sizes))

for(k in 1:length(pop_sds))
{
  pop_sd_k = pop_sds[k]
  for(j in 1:length(sample_sizes))
  {
    x_vals = seq(0, 100, length.out = sample_sizes[j])
    
    for(i in 1:n_sims)
    {
      fit_sim = linear_sim_fit(x = x_vals,
                               y_int = int_obs,
                               slope = slope_obs, 
                               st_dev = pop_sds[k])
      
      p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
    }
    sim_output_3[k, j] = sum(p_vals < alpha) / n_sims
  }
  print(paste0("Testing standard deviation ", k, " of ", n_sds))
}

image(sim_output_3)

#save to data frame
sim_3_dat = 
  list(
    power = sim_output_3,
    sample_size = sample_sizes,
    pop_sd = pop_sds)

#save results
save(
  sim_3_dat, 
  file = here::here("data", "lab_ll_sim_output_dispersion_n_1000.RData"))


# ----Contour----
#[iso]lines show interpolated lines at which the value is the same
contour(x = effect_sizes, y = sample_sizes, 
        z = sim_output_2, #matrix in which cells represent the values for which to create the contours
        xlab = "effect size",
        ylab = "sample size",
        main = "Contour Plot of Statistical Power")

png(filename = here::here("figures", "lab_11_contourplot.png"), 
    width = 1200, #pixels wide
    height = 800, #pixels high
    res = 180, #dpi (pixels/inch)
    bg = "transparent")
#Q2 [iso]lines show interpolated lines at which the value is the same
contour(x = pop_sds, y = sample_sizes, 
        z = sim_output_3, #matrix in which cells represent the values for which to create the contours
        xlab = "Population Dispersion",
        ylab = "Sample Size",
        main = "Contour Plot of Statistical Power")
dev.off()

# ----3D----
#static perspective plot
persp(
  x = sim_n_effect_size$effect_size,
  y = sim_n_effect_size$sample_size,
  z = sim_n_effect_size$power,
  xlab = "beta", ylab = "n", zlab = "power",
  col = 'lightblue',
  theta = 30, phi = 30, expand = .75,
  ticktype = 'detailed')

#Q2 static perspective plot
persp(
  x = pop_sds, y = sample_sizes, z = sim_output_3,
  col = 'lightblue',
  theta = 30, phi = 30, expand = .75,
  ticktype = 'detailed')

#interactive plot
#install.packages("rgl")
library(rgl)
persp3d(
  x = effect_sizes, y = sample_sizes, z = sim_output_2,
  col = 'lightblue',
  theta = 30, phi = 30, expand = .75,
  ticktype = 'detailed')

#save the linked 3D plot
rgl::writeWebGL(
  dir = here::here("docs", "webGL"), 
  filename = here::here(
    "docs", "webGL",
    "n_effect_size_power_sim_plot.html"),
  width = 1200, height = 1200
)

{
#Q2
persp3d(
  x = pop_sds, y = sample_sizes, z = sim_output_3,
  col = 'lightgoldenrod3',
  xlab = 'Population Dispersion',
  ylab = 'Sample Size',
  zlab = 'Statistical Power',
  alpha = 0.6,
  theta = 30, phi = 30, expand = .75,
  ticktype = 'detailed')

#save the linked 3D plot
rgl::writeWebGL(
  dir = here::here("docs", "webGL"), 
  filename = here::here(
    "docs", "webGL",
    "n_effect_size_power_sim_plot2.html"),
  width = 1200, height = 800
)
}

# ----Save----
#save the results of my effect size and sample size simulation to a file
save(
  sim_n_effect_size,
  file = here::here("data", "lab_11_n_effect_sizes.Rdata"))

#load data again
load(file = here::here("data", "lab_11_n_effect_sizes.Rdata"))