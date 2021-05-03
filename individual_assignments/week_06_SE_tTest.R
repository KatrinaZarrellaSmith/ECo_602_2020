#Lab 5 Building Inference

#clear workspace
rm(list=ls(all=TRUE))

# ----SE----

#Q1
sse_mean = function(x) {
  x = na.omit(x)
  sse = sd(x)/sqrt(length(x))
  return(sse)
}

require(palmerpenguins)
sse_mean(penguins$bill_depth_mm)
sse_mean(penguins$body_mass_g)
sse_mean(mtcars$mpg)

# ----Resampling----
boxplot(flipper_length_mm ~ species, data = penguins)

#remove one species/remove unused factor levels from a data.frame using droplevels()
dat_pen = droplevels(subset(penguins, species != "Gentoo"))
{
  par(mfrow = c(1, 2))
  boxplot(flipper_length_mm ~ species, data = penguins)
  boxplot(flipper_length_mm ~ species, data = dat_pen)
}

#randomly reshuffle data and make boxplot (nonparametric)
# for reproduceability
set.seed(123)
#resample the flipper lengths with replacement using the sample()
flipper_shuffled = sample(penguins$flipper_length_mm, replace = TRUE)
par(mfrow = c(1, 2))
boxplot(flipper_length_mm ~ species, data = penguins)
boxplot(flipper_shuffled ~ penguins$species, xlab = "species")

# ----T-test----
t_test = t.test(dat_pen$flipper_length_mm ~ dat_pen$species)
t_test
#use str() to examine what the object contains
str(t_test)

#mean statement
t_test$estimate
#difference in means
diff_observed = round(diff(t_test$estimate), digits = 3)
print(diff_observed, digits = 3)
#difference in means using the aggregate() function
agg_means = aggregate(
  flipper_length_mm ~ species, 
  data = dat_pen, 
  FUN = mean, 
  na.rm = TRUE)
diff_observed = diff(agg_means[, 2])

agg_means
diff_observed

#resample the flipper lengths with replacement
# for reproduceablility
set.seed(1)
flipper_shuffled = sample(dat_pen$flipper_length_mm)

boxplot(flipper_shuffled ~ dat_pen$species)
t_test_1 = t.test(flipper_shuffled ~ dat_pen$species)
t_test_1

# ----Sample Sizes----
table(dat_pen$species)

#Resampling with replacement is the same thing as randomly sampling 68 flipper lengths in one group and 152 in another
n_1 = 68
n_2 = 152

dat_1 = sample(dat_pen$flipper_length_mm, n_1, replace = TRUE)
dat_2 = sample(dat_pen$flipper_length_mm, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)

#difference in means for the resampled data
print(c(observed = diff_observed, simulated = diff_simulated))

#Q2/as a function
two_group_resample = function(x, n_1, n_2) {
  dat_1 = sample(x, n_1, replace = TRUE)
  dat_2 = sample(x, n_2, replace = TRUE)
  difference_in_means = (mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE))
  return(difference_in_means)
}

set.seed(54321)
two_group_resample(dat_pen$flipper_length_mm, 68, 152)
# [1] 2.123452

#run many times
n = 200
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 68, 152)
  )
}
hist(mean_differences)

sum(abs(mean_differences) >= diff_observed)
# [1] 0

#run many more times
n = 2000
mean_differences = c()
for (i in 1:n){
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 68, 152)
  )
}

#Q3/save image to png file
{
png(filename = "lab_06_mean_diff.png", 
    width = 700, #pixels wide
    height = 700, #pixels high
    res = 180, #dpi (pixels/inch)
    bg = "transparent")
hist(mean_differences,
     main = "Resampled Differences of Means:\nAdelie and Chinstrap",
     xlab = "Mean Differences",
     col ="grey",
     xlim = c(-4,4))
dev.off()
}

#Q4
sum(abs(mean_differences) > 5.8)
# [1] 0

#Q5
#How often would I expect to see a result as extreme, or more extreme, if the null hypothesis were true.
#The p-value from the t-test for the difference in mean flipper length was very very small:
#6.049e-08 That's less than once in 10 million!
#Given a p value of less than 1 per 10 million, how many simulations do you think you would have to do to see a difference in mean flipper length equal to or greater than 5.8 mm?
##10 million + ~1

#Q6
{
  png(filename = "lab_06_boxplot.png", 
      width = 700, #pixels wide
      height = 700, #pixels high
      res = 180, #dpi (pixels/inch)
      bg = "transparent")
boxplot(body_mass_g ~ species, data = dat_pen,
        ylab = "Body Mass (g)",
        xlab = "Species")
dev.off()
}

#difference in means using the aggregate() function
agg_means = aggregate(
  body_mass_g ~ species, 
  data = dat_pen, 
  FUN = mean, 
  na.rm = TRUE)
diff_crit = diff(agg_means[, 2])

agg_means #group means
diff_crit #difference in means

#t-test & p-value
t.test(dat_pen$body_mass_g ~ dat_pen$species)

#resampling test with 1000 repetitions
n = 1000
mean_differences = c()
for (i in 1:n){
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$body_mass_g , 68, 152))}

{
  png(filename = "lab_06_mean_diff2.png", 
      width = 700, #pixels wide
      height = 700, #pixels high
      res = 180, #dpi (pixels/inch)
      bg = "transparent")
  hist(mean_differences,
       main = "Resampled Differences of Means:\nAdelie and Chinstrap",
       xlab = "Mean Differences",
       col ="grey",
       xlim = c(-300,300),
       ylim = c(0, 350))
  dev.off()
}

sum(abs(mean_differences) > diff_crit)
