#Week 7 Bootstrapping

#apply() works on data frames or other 2-dimensional arrays of data, such as matrices.
#It applies a function to either the rows or columns of the input data.
#X the 2-dimensional data, usually a data frame or a matrix
#MARGIN whether to apply the function to rows (MARGIN = 1) or columns (MARGIN = 2)
#FUN The function to apply to the rows or columns

# Create simulated data
dat = matrix(1:49, nrow = 7, ncol = 7)
print(dat)

#Minimum and maximum values in each row
apply(dat, MARGIN = 1, FUN = min)
apply(dat, MARGIN = 1, FUN = max)
#Mean values in each column
apply(dat, MARGIN = 2, FUN = mean)

#Moths data
require(here)
moths = read.csv(here("data", "moths.csv"))
head(moths)

#bootstrap refers to a resampling technique by with which we use sampling with replacement on a single sample of observations to simulate the process of taking many samples from a population
#You want to know something about a larger population, so you have taken a single sample of n measurements.
#Although you have only a single sample, you could resample data from the sample in many ways if you use sampling with replacement
#Sampling with replacement allows some values to appear more than once, and other samples to be left out.
#You can draw observations at random from the original sample, replacing each observation after it is selected so that it has the same chance of being drawn on the next draw.
#By doing this repeatedly, you can create a new data set by resampling the original data set.
#It's not quite as good as being able to resample the population, but it is a powerful technique nonetheless.
#One of the most important uses of the bootstrap is calculating non-parametric confidence intervals for parameter estimates

#single species
hist(moths$anst)
#The standardized abundances appear highly non-normal

#PARAMETRIC CI
#obtain a 95% confidence interval for the mean standardized abundance.

#Recognizing that the central limit theorem states that the sampling distribution of a sample mean will be normally distributed, under certain assumptions (which may not be met here), we could use a standard normal or Student's t distribution to calculate a confidence interval on the mean.
#To illustrate, we'll perform the calculating using a t-distribution, which is essentially a normal distribution when the population variance is unknown and thus must be estimated from the sample.
#The exact shape of the t-distribution depends on the sample size, but in general it has heavier tails than the normal. For sample sizes of 30 and higher, the t distribution very closely approaches a normal.
#Because of the heavy tails, you can think of the t-distribution as being more conservative than the normal

#first calculate the critical t-value, then multiply it by the sample standard error of the mean to get the radius of the CI:
alpha = 0.05
anst = moths$anst
n = sum(!is.na(anst))
t_crit = abs(qt(alpha / 2, df = n - 1))

sse = sd(anst) / sqrt(n)

sample_mean = mean(anst)
ci_parametric = sse * t_crit

confidence_intervals = 
  data.frame(
    technique = c("parametric: t-dist"),
    mean = sample_mean,
    ci_radius = sse * t_crit,
    lower = sample_mean - ci_parametric,
    upper = sample_mean + ci_parametric
  )
confidence_intervals
#This is the standard way to construct a confidence interval for the mean and test the hypothesis that the mean differs from 0.
#If the confidence interval does not contain 0, then we can say that the mean is unlikely to have come from a distribution with a mean of 0.

#NONPARAMETRIC BOOTSTRAPPING
#An alternative to the parametric approach above is to use bootstrapping to obtain a confidence interval for the mean.
#The bootstrap CI would likely be a more robust estimate since we have a small sample size (n = 24) and we do not know whether the underlying population is normally-distributed.
#We can perform a simple bootstrap simulation by calculating the mean abundance on many randomly resampled (with replacement) data sets.

#Create an empty vector to hold the bootstrap sample means
m = 10000

# numeric() creates an vector of length m with all values initialized to zero
result = numeric(m)
head(result)
#Perform the bootstrap/Create the resampled data sets and calculate the means
for(i in 1:m){
  result[i] = mean(sample(anst, replace=TRUE))
}
#The vector result now contains 10,000 bootstrap sample means
#Calculate the CI from the quantiles of the resulting bootstrap means
#We can calculate the mean of the bootstrap means and, more importantly, the 2.5% and 97.5% quantiles of the bootstrap distribution
mean(result)
quantile(result,c(0.025,0.975))
#How does this compare with parametric confidence interval?
#Close, but not identical.
#Our bootstrap confidence intervals are skewed because the data are skewed, whereas the parametric confidence interval is symmetric.

# ----Bootstrap Interval----
install.packages("boot")
require(boot)
#boot(data, statistic, R)
#data is the data object you want to resample. It can be a vector, matrix, or data.frame.
#statistic is a function that returns the statistic of interest.
#There are restrictions on the order and types of arguments that this function must take.
#We'll usually need to write a custom function to meet the restrictions.

#create a modified version of the mean() function
boot_mean = function(x, i){
  return(mean(x[i], na.rm = TRUE))
}
#The modified function is needed because:
#Per the help entry, the first argument to our statistic function has to be the input data.
#In this case, our data are a vector of numbers. I've called the argument x in the custom function.
#The second argument is an index (a vector of subscripts) that is used within boot() to select random assortments of x.

#bootstrap for 10000 iterations
myboot = 
  boot(
    data = anst,
    statistic = boot_mean,
    R = 10000)
print(myboot)
#original is the original mean of the whole sample: mean(anst).
#bias is the difference between the original mean and the mean of the bootstrapped samples.
#std.error is the standard deviation of the simulated values.
str(myboot)
mean(anst)
myboot$t0
mean(myboot$t) - myboot$t0
sd(myboot$t)
#extract bootstrap confidence interval
quantile(
  myboot$t,
  c(0.025, 0.975))

#set up data
moth_dat = moths[,-1]
head(moth_dat)
n = nrow(moth_dat) #number of rows or sample observations
m = 100 #number of bootstrap iterations
moth_result = matrix(
  nrow = m,
  ncol = n)

#organize our output as follows:
#Each iteration of the bootstrap produces one row of output
#The output columns correspond to the sampling intensity

#draw a bootstrap sample from the data set of a specified size.
#using indexing and sample() for the row index of our data frame
#data[sample(...), ]
#select the rows from data corresponding to the result of the sample() function
#Inside sample() we need to specify a vector containing a list of numbers ranging from 1 to n, the size of the bootstrap sample (i.e., number of row observations to take), and the replace=TRUE to ensure sampling with replacement.

#compute the species richness of the sample.
#using apply() function to sum by column (species) and then count how many species have sums > 0 (indicating presence)

#The inside loop will create bootstrap samples of size 1 to n; the outside loop will iterate through 10,000 bootstrap iterations.
n = nrow(moth_dat) #number of rows or sample observations

m = 100 #number of bootstrap iterations

moth_result = matrix(
  nrow = m,
  ncol = n)

# The outer loop: runs once for each bootstrap iteration.  index variable is i
for(i in 1:m){
  # The inner loop: simulates increasing sampling intensity
  # Sampling intensity ranges from 1 site to the complete count of sites (24)
  # index variable is j
  for(j in 1:n)
  {
    # sample the input data row indices, with replacement
    rows_j = sample(n, size = j, replace=TRUE)
    
    # Creates a new data matrix
    t1 = moth_dat[rows_j, ]
    
    # Calculates the column sums
    t2 = apply(t1, 2, sum)
    
    # Counts the number of columns in which any moths were observed
    moth_result[i, j] = sum(t2 > 0)
  }
}

head(moth_result)

#What do I want my function to do?
#Execute the double loop.
#Return the results in a matrix.
#What input does my function need to know about?
#The data to resample: a data.frame.
#The number of bootstrap iterations

#In my first draft, I'll just copy all of the code inside the function and propose argument names based on the function inputs I identified above
rarefaction_sampler = function(input_dat, n_iterations)
#...
rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)

#In the second draft, I'll check that the body of my function does not make reference to any variables that are not defined either by the arguments or within the function body.
#I know that m was defined already outside of the function, so I want to replace it with n_iterations.
#I know that n was defined as the number of rows in moth_dat.
#I want my function to work for any input dataframe, so I'll replace it with a new variable, n_input_rows, that I define within the function body.
#I'll get rid of moth_result, and instead create a new output matrix called results_out
rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of 
    # sites in the input data (n)
    # index variable is j
    for(j in 1:n)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)

#check in fresh environment
# This clears the current R session's environment
rm(list = ls())

# Re-read my data:
moths = read.csv(here("data", "moths.csv"))
moth_dat = moths[,-1]

rarefaction_sampler = function(input_dat, n_iterations)
{
  
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of 
    # sites in the input data (n)
    for(j in 1:n)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact = rarefaction_sampler(moth_dat, 100)
## Error in rarefaction_sampler(moth_dat, 100): object 'n' not found
head(rarefact)
## Error in head(rarefact): object 'rarefact' not found

# ----Rarefaction Curve----
# Re-read my data:
moths = read.csv(here("data", "moths.csv"))
rarefact = rarefaction_sampler(moths[,-1], 10000)
#each row is a separate bootstrap iteration and each column represents the size of the bootstrap sample
#In this case the column number represents the number of sample observations or sampling intensity ranging from 1 to 24
#calculate the mean and 2.5% and 97.5% quantiles of the bootstrapped species richness for each sampling intensity using apply() function
#bind the objects together and transpose the data frame so that the columns represent the mean, 2.5% and 97.5% quantiles, and the rows represent sampling intensity ranging from 1 to 24
rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))

#plot the rarefaction curve and the 95% confidence interval using the matplot() function, which is useful for simultaneous plotting of several columns of a data frame or matrix
matplot(
  rare,
  type='l',
  xlab='Number of sampling plots',
  ylab='Species richness', 
  main='Rarefaction Curve')

legend(
  'bottomright',
  legend=c('mean','2.5%','97.5%'),
  lty=c(1,2,3),col=c(1,2,3), inset=c(.1,.1))

# ----Q1----
#parametric CI
require(palmerpenguins)
#first calculate the critical t-value, then multiply it by the sample standard error of the mean to get the radius of the CI:
alpha = 0.05
gentoo = subset(penguins, species=="Gentoo") #subset to species
gentoo_bill = !is.na(gentoo$bill_length_mm) #remove NAs from data of interest
n = length(gentoo_bill) #number of observations
t_crit = abs(qt(alpha / 2, df = n - 1))

sample_mean = mean(gentoo_bill)
sd = sd(gentoo_bill)
sse = sd / sqrt(n)

ci_parametric = sse * t_crit

confidence_intervals = 
  data.frame(
    technique = c("parametric: t-dist"),
    mean = sample_mean,
    ci_radius = sse * t_crit,
    lower = sample_mean - ci_parametric,
    upper = sample_mean + ci_parametric
  )
confidence_intervals #print result

# ----Q2----
#bootstrap 95% CI
boot_mean = function(x, i){
  return(mean(x[i], na.rm = TRUE))
}
#bootstrap for 10000 iterations
myboot = 
  boot(
    data = gentoo_bill,
    statistic = boot_mean,
    R = 10000)
print(myboot)

#extract bootstrap confidence interval
quantile(
  myboot$t,
  c(0.025, 0.975))

# ----Q3----
#rarefaction
# This clears the current R session's environment
rm(list = ls())
# Re-read my data:
moths = read.csv(here("data", "moths.csv"))
rarefaction_sampler = function(input_dat, n_iterations)
{
  n = nrow(moths[,-1]) #number of rows or sample observations
  
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of 
    # sites in the input data (n)
    for(j in 1:n)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact = rarefaction_sampler(moths[,-1], 100)
head(rarefact)

# ----Q4----
# Re-read my data:
moths = read.csv(here("data", "moths.csv"))
rarefact = rarefaction_sampler(moths[,-1], 10000)
#each row is a separate bootstrap iteration and each column represents the size of the bootstrap sample
#In this case the column number represents the number of sample observations or sampling intensity ranging from 1 to 24
#calculate the mean and 2.5% and 97.5% quantiles of the bootstrapped species richness for each sampling intensity using apply() function
#bind the objects together and transpose the data frame so that the columns represent the mean, 2.5% and 97.5% quantiles, and the rows represent sampling intensity ranging from 1 to 24
rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))

# ----Q4----
#save image to png file
png(filename = "lab_07_rarefaction.png", 
    width = 1000, #pixels wide
    height = 800, #pixels high
    res = 180, #dpi (pixels/inch)
    bg = "transparent")
#plot the rarefaction curve and the 95% confidence interval using the matplot() function, which is useful for simultaneous plotting of several columns of a data frame or matrix
matplot(
  rare,
  type='l',
  lwd = '2',
  xlab='Number of sampling plots',
  ylab='Species richness', 
  main='Rarefaction Curve')

legend(
  'bottomright',
  legend=c('mean','2.5%','97.5%'),
  lty=c(1,2,3),col=c(1,2,3), inset=c(.1,.1), lwd = c(2,2,2))

dev.off()
