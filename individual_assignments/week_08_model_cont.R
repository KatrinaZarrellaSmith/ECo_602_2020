#Week 8 - Modeling Continuous Data

require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))

#t-tests are useful when we have a categorical predictor with two levels and a continuous response variable
#t-test with the alternative hypothesis that Adelie penguins have shorter flippers than Chinstrap penguins
t.test(flipper_length_mm ~ species, data = penguin_dat, alternative = "less")

#create a bootstrap confidence interval on the difference of two means. That sounds a lot like a t-test
install.packages("simpleboot")
require(simpleboot)
#Bootstrapping is done by independently resampling from sample1 and sample2

#subset
adelie_flipper = subset(penguin_dat, species == "Adelie")$flipper_length_mm
chinstrap_flipper = subset(penguin_dat, species == "Chinstrap")$flipper_length_mm


hist(two.boot(adelie_flipper, chinstrap_flipper, 
              FUN = mean, R = 10000, na.rm = T),
     main = "Histogram of 10000 bootstrap differences\nin mean Adelie and Chinstrap flipper length",
     xlab = "Difference in mean flipper length (mm)")

# ----Vegetation----
veg = read.csv("vegdata.csv")

#visualize
boxplot(pine ~ treatment, dat = veg)

#create a new data frame that contains only the observations that received the "clipped" or "control" treatments
dat_tree = droplevels(subset(veg, treatment %in% c("control", "clipped")))

#visualize
boxplot(pine ~ treatment, dat = dat_tree)

#determine how many observations are in each of the treatments
table(dat_tree$treatment)

#Conduct a Wilcoxon ranked sum test on the difference in means between the treatments
wilcox.test(pine ~ treatment, dat = dat_tree)

#Conduct a bootstrap of the tree data
tree_boot = 
  two.boot(
    subset(dat_tree, treatment == "clipped")$pine,
    subset(dat_tree, treatment == "control")$pine,
    FUN = mean,
    R = 10000,
    na.rm = TRUE
  )

# sum(tree_boot$t >= 0)
# sum(tree_boot$t < 0)
require(boot)
boot.ci(tree_boot)

hist(tree_boot$t, main = "Bootstrap sampling distribution")

quantile(tree_boot$t, 0.025)

# ----Birds----
require(here)
dat_bird = read.csv(here("data", "bird.sub.csv"))
dat_habitat = read.csv(here("data", "hab.sub.csv"))

dat_all = merge(
  dat_bird, 
  dat_habitat,
  by = c("basin", "sub"))

#visualize
plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")

#fit a simple linear regression using a Least Squares criterion
fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)

#examine the model coefficients with coef()
coef(fit_1)
#slope coefficient
slope_observed = coef(fit_1)[2]

#add a regression line from a model fitted by lm() to an existing plot using abline() and the fitted model object
plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_1)

#extracting just the two variables we need
dat_1 = 
  subset(
    dat_all,
    select = c(b.sidi, s.sidi))

#Monte Carlo randomization breaks up the associations by sampling frandom values from each column, in stead of keeping rows intact

#create two vectors of randomly generated row indices
#use these to create two new vectors of bird and vegetation diversity indices
index_1 = sample(nrow(dat_1), replace = TRUE)
index_2 = sample(nrow(dat_1), replace = TRUE)

dat_resampled_i = 
  data.frame(
    b.sidi = dat_1$b.sidi[index_1],
    s.sidi = dat_1$s.sidi[index_2]
  )

fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
slope_resampled_i = coef(fit_resampled_i)[2]

print(slope_resampled_i)

#re-create the scatterplot with regression line
plot(
  b.sidi ~ s.sidi, data = dat_resampled_i,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_resampled_i)
#We can repeat the process many times to estimate the sampling distribution of the null hypothesis
#I call it the distribution of the null because the null hypothesis states that there is no association between variables. We destroyed associations by shuffling both columns independently.

#pre-allocate a vector to hold the results using numeric()
m = 10000
result = numeric(m)

#loop that will resample the data, fit a simple linear regression, and extract the slope parameter
for(i in 1:m){
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
    )
  
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  result[i] = coef(fit_resampled_i)[2]
}
#The output of your loop is a collection of regression slope parameters that were calculated on randomized and resampled data

#save image to png file
png(filename = "MCsim.png", 
    width = 1200, #pixels wide
    height = 900, #pixels high
    res = 180, #dpi (pixels/inch)
    bg = "transparent")
#Plot a histogram of the slope values
#draw a vertical line showing the value of the slope from the regression model on the observed data
hist(result, main = "Null Distribution of Regression Slope", 
     xlab = "Slope Parameter",
     xlim = c(-0.04, 0.04),
     ylim = c(0, 2500))
abline(v = slope_observed, col = "blue", lwd = 2)
#vertical line showing where the observed slope occurred

#just like finding the critical z-value for confidence intervals, we can calculate a critical value for the slope
#Use quantile() to find the 5th percentile of the null distribution of slopes
v2 = quantile(result, c(.05))

#vertical line showing the critical value
abline(v = v2, lty = 2, col = "red", lwd = 2)
dev.off()

# ----Q1----
#subset data of interest
adelie_flipper = subset(penguin_dat, species == "Adelie")$flipper_length_mm
chinstrap_flipper = subset(penguin_dat, species == "Chinstrap")$flipper_length_mm

#Conduct a bootstrap of the penguin data
pen_boot = 
  two.boot(
    adelie_flipper,
    chinstrap_flipper,
    FUN = mean,
    R = 10000,
    na.rm = TRUE
  )

#save image to png file
png(filename = "pen_boot.png", 
    width = 1200, #pixels wide
    height = 900, #pixels high
    res = 180, #dpi (pixels/inch)
    bg = "transparent")
#histogram of bootstrapped differences
hist(pen_boot$t,
     main = "Histogram of 10000 bootstrap differences\nin mean Adelie and Chinstrap flipper length",
     xlab = "Difference in mean flipper length (mm)",
     xlim = c(-10,0),
     ylim = c(0,2000),
     col = "grey")
dev.off()

# ----Q2----
#calculate a 95% Confidence interval on the difference in mean flipper lengths
quantile(pen_boot$t, 0.025)

# ----Q3----
#Create a distribution function from pen_boot using ecdf()
pen_ecdf = ecdf(pen_boot$t) #returns a new function
#calculates the area under the density curve to the left of x

# ----Q4----
#Use pen_ecdf() to calculate the empirical probability of observing a mean difference of -4.5 or greater
1-pen_ecdf(-4.5)

#calculate the empirical probability of observing the mean difference predicted by the null hypothesis, i.e. 0 or greater
1-pen_ecdf(0)

# ----Q5----
#Conduct a Wilcoxon ranked sum test on the difference in means between the treatments
wilcox.test(pine ~ treatment, dat = dat_tree,
            alternative = "two.sided")

# ----Q6----
boot.ci(tree_boot)
#endpoints of your bootstrap CI = BCa

#observed difference in mean tree counts
mean(subset(dat_tree, treatment == "clipped")$pine)-
  mean(subset(dat_tree, treatment == "control")$pine)

#----Q8----