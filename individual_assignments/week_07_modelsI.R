# Week 8 Using Models I

catrate=read.csv("catrate.csv")
head(catrate)
summary(catrate)

hist(catrate$cat.rate)

#check whether your data are likely to have been drawn from a Normal distribution
shapiro.test(catrate$cat.rate)
#The null hypothesis for the Shapiro-Wilk test is: "The data were sampled from a normally-distributed population
#p<0.05 = not normally distributed

#package nortest contains many other functions for assessing normality including ad.test(), lillie.test(), and many others

#compare two independent means
#If the data are normally distributed, we can use the Student's t test
#the null hypothesis is: "The mean of population from which the data were collected is not different from x"

#ONE SAMPLE test
#t test assumes data is normally distributed
#test the alternative hypothesis that the observed mean cat.rate (0.54) is significantly different from an expected value of 0.28
t.test(x=catrate$cat.rate, mu=0.28)
#Note, the default t test is a two-sided alternative; i.e., the alternative hypothesis is that the observed mean is not equal to the expected mean, which can mean less than or greater than the expected mean

#If we have a good reason to believe that the observed mean should be greater than the null hypothesis mean, you can use a one-tailed test
#In the case that you propose a one-tailed hypothesis before looking at the data you can conduct a one-sided test by including the argument alternative = "greater" in the call to t.test()
t.test(x=catrate$cat.rate, mu=0.28, alternative = "greater")


#In the case of small samples from a non-normal distribution, we can use the Wilcoxon's signed rank test (also known as the Mann-Whitney test)
wilcox.test(catrate$cat.rate, mu = 0.28, alternative = "greater")

#TWO SAMPLE test
#The null hypothesis in a two-sample test is: "There is no difference in mean values between the two groups."
#The values in the two groups were drawn from the same population.

require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))
summary(penguin_dat)
boxplot(flipper_length_mm ~ species, data = penguin_dat)

# Extract the Adelie penguin data
dat_adelie = subset(penguin_dat, species == "Adelie")
# Extract the Chinstrap penguin data
dat_chinstrap = subset(penguin_dat, species == "Chinstrap")

#normality test, use this to decide if t test is appropriate
#null=data is normally distributed
shapiro.test(dat_adelie$flipper_length_mm)
shapiro.test(dat_chinstrap$flipper_length_mm)
#normal=p>0.05

#all penguins
#Bill lengths appear very non-normal.
#A the very low p-value in the Shapiro test of normality provides strong evidence against the null hypothesis that bill lengths are normally-distributed.
shapiro.test(penguins$bill_length_mm)
#non-normal=p<0.05
#a general linear model of bill lengths as that includes species and body mass as predictors passes a test of the normality assumption
fit_1 = lm(bill_length_mm ~ body_mass_g + species, data = penguins)
shapiro.test(residuals(fit_1))

#two sample t-test
#null hypothesis "There is no difference in mean values between the two groups."
#The values in the two groups were drawn from the same population
t.test(flipper_length_mm ~ species, data = penguin_dat)

#Wilcoxon rank
wilcox.test(flipper_length_mm ~ species, data = penguin_dat)

#You can test 1-tailed hypotheses in the two-sample tests as described above for the one-sample tests
#The trick is that you have to figure out which group R considers to be the the 'base level'
levels(penguin_dat$species) #Adelie base level
wilcox.test(flipper_length_mm ~ species, data = penguin_dat, alternative = "less")

#----Q1----
#histogram of the salamander reproduction catastrophic rates
#save image to png file
{
png(filename = "sal_catrate.png", 
    width = 1000, #pixels wide
    height = 1000, #pixels high
    res = 180, #dpi (pixels/inch)
    bg = "transparent")
hist(catrate$cat.rate,
     main = "Salamander Reproduction Catastrophe Rates",
     xlab = "Catastrophe Rate",
     col = "grey")
dev.off()
}

#----Q2----
#Shapiro-Wilk test of normality of the salamander catastrophic rates
shapiro.test(catrate$cat.rate)

#----Q3----
#one-sample t-test of the alternative hypothesis that the catastrophic rate is different from the pond late-filling rate
#Null=The catastrophic rate is not different from the pond late-filling rate, i.e. the proportion of years in which less than one juvenile was produced per female in the salamander population is the same as the proportion of years in which late pond-filling occurs.
t.test(x=catrate$cat.rate, mu=0.28)

#----Q4----
#p-value = 0.01054
#95% CI = (0.3526250, 0.7261295), does not include 0
#Yes, there is strong evidence to reject the null that the catastrophic rate is not different from the pond late-filling rate (p = 0.01054).

#----Q5----
#one-sample Wilcoxon rank sum test of the alternative hypothesis that the catastrophic rate is different from the pond late-filling rate
wilcox.test(catrate$cat.rate, mu = 0.28)

#----Q6----
#p-value = 0.006275
#Yes, there is strong evidence to reject the null that the catastrophic rate is not different from the pond late-filling rate (p = 0.006275).

#----Q7----
#The histogram of catastrophe rates and the Shapiro-Wilk test demonstrate that the catastrophe rate data were sampled from a non-normally-distributed population. Therefore, the Wilcoxon's signed rank test is more appropriate to compare the difference between the measures of central tendency of catastrophe rates and pond late-filling rates.

#The same conclusion can be drawn from both tests, rejection of the null hypothesis. However, the methods vary greatly as the center of the population that is being statistically compared differs. For parametric t-tests, the parameter is the population mean whereas in nonparametric Wilcoxon rank sum, the shape of the distributions are compared to see if one has more systematically different values than the other (and if they are the same distribution shape, this equates to comparing the median). Given that comparing means is not ideal for this non-normal data, the conclusion is more appropriately drawn from the nonparametric test.

#----Q10----
#ingle figure consisting of histograms of flipper lengths of Adelie and Chinstrap penguins
{
  png(filename = "pen_hist.png", 
      width = 1600, #pixels wide
      height = 700, #pixels high
      res = 180, #dpi (pixels/inch)
      bg = "transparent")
  par(mfrow = c(1, 2))
  hist(dat_adelie$flipper_length_mm,
       main = "Adelie Penguin",
       xlab = "Flipper Length (mm)",
       col = "grey",
       xlim = c(170,220),
       ylim = c(0,50))
  hist(dat_chinstrap$flipper_length_mm,
       main = "Chinstrap Penguin",
       xlab = "Flipper Length (mm)",
       col = "grey",
       xlim = c(170,220),
       ylim = c(0,20))
  mtext("Note: Y-axis limits differ between plots.", side = 1, line = -1, outer = TRUE, cex = .8)
  dev.off()
}

#----Q11----
#alternative hypothesis
#The average flipper length of Adelie penguins is shorter than the average flipper length of Chinstrap penguins.

levels(penguin_dat$species) #Adelie base level
#conduct the t-test one-tailed
t.test(flipper_length_mm ~ species,
       data = penguin_dat, 
       alternative = "less")
