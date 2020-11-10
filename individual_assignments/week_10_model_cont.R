#Week 10 - Using Models II

#----T-test----
#T-tests are univariate tests that we can use to determine whether we have good evidence that:
#The mean of one sample is different from a fixed value.
#The means of two samples are different from each other.

require(palmerpenguins)
#1-sample t-test on the Gentoo penguin flipper lengths
t.test(subset(penguins, species == "Gentoo")$flipper_length_mm)
#null=there is no difference in Gentoo penguin flipper lengths compared to zero

#equal to 218 mm
t.test(
  x = subset(penguins, species == "Gentoo")$flipper_length_mm,
  mu = 218
)
#null=there is no difference in Gentoo penguin flipper lengths compared to 218 mm

#one-tailed alternative hypothesis: Gentoo penguin flippers are smaller than 218 mm
t.test(
  x = subset(penguins, species == "Gentoo")$flipper_length_mm,
  mu = 218,
  alternative = "less"
)

#compare the flipper lengths of two species
t.test(flipper_length_mm ~ species, data = subset(penguins, species != "Chinstrap"))

#alternative hypothesis that Adelie penguins have shorter than Gentoo penguins
levels(penguins$species) #Adelie base level
t.test(flipper_length_mm ~ species, data = subset(penguins, species != "Chinstrap"), alternative = "less")

#----One-way ANOVA----
#Model 1
#Response variable: body mass
#continuous variable, ratio scale

#Predictor variable: species
#categorical variable, nominal scale

#Perform graphical and numerical data exploration

#explore normality using histograms and density plots
par(mfrow = c(1, 2))
hist(penguins$body_mass_g, breaks = 80, main = "histogram of body mass", xlab = "body mass (g)")
plot(density(penguins$body_mass_g, na.rm = TRUE), main = "density plot of body mass")
dev.off()

#Conditional boxplots are great for categorical variables
boxplot(body_mass_g ~ species, data = penguins)

#test whether within-group data are normally-distributed
#Extract the measurements for each species.
dat_chinstrap = subset(penguins, species == "Chinstrap")

#Calculate the mean body mass for each species.
mean(dat_chinstrap$body_mass_g, na.rm = TRUE)
#shortcut
aggregate(body_mass_g ~ species, data = penguins, FUN = mean)

#Conduct Shapiro tests on each species' body mass.
shapiro.test(dat_chinstrap$body_mass_g)
#Shapiro test null hypothesis: "The data are drawn from a normally-distributed population."
#shortcut
aggregate(body_mass_g ~ species, data = penguins, function (x) shapiro.test(x)$p.value)

#Fit a linear model using lm()
fit_species = lm(body_mass_g ~ species, data = penguins)

#Examine the model coefficient table using summary()
summary(fit_species)
#Model Coefficients table
#The base case is the intercept
#the 'slope' coefficients are the adjustments you need to make to the base case to arrive at the means for the other levels of the factor.
#the p-value in each row of the table is a significance test for whether the coefficient in that row is different from zero.
#p-values do not tell us whether any of the coefficients are significantly different from each other

#Conduct the Analysis of Variance using anova()
anova(fit_species)
#ANOVA table: Predictor variable p-values
#Degrees of freedom represent the number of levels within in a categorical variable (-1)

#sum of squares: how much of the total data variability is explained by each of the predictor variables
#residuals sum of squares contains information about the variation that our model couldn't explain
#total sum of squares (equal to the sum of all the numbers in the Sum sq. column) is a measure of the total variability in the data

#Mean Sq column is the Sum Sq adjusted by the degrees of freedom associated with each predictor variable
#When you have more than one predictor variable, you can think of the Mean Sq column as an estimate of the relative importance of each predictor

#Sum Sq and Mean Sq columns give us information about how much variability the predictor variable is able to explain.
#Sum Sq column tells us about the variability explained by each factor, 
#but the Mean sq column allows us to compare the relative amount of information that each factor explains


#F-statistics as a measure of how much a adding a variable to the model improves the model fit. There will be an F-statistic for each predictor variable.
#null hypothesis is: "The within-group variance is equal to the between-group variance".
#null="Adding predictor x to the model does not improve the model fit."
#Pr(>F) column gives us a rough idea of whether the predictor significantly improves the model's prediction or not
#low p-value here (approximately) means that adding the predictor creates a significantly better model than leaving it out

#----Two-way ANOVA----
#factorial design means that all combinations of categorical variables appear in the data

#conditional boxplot for penguin body mass conditioned on both species and sex
boxplot(body_mass_g ~ sex * species, data = penguins)

fit_both = lm(body_mass_g ~ sex * species, data = penguins)
summary(fit_both)
anova(fit_both)

#----Q1----
#Based on the boxplots, do you think male penguins (of any species) are significantly heavier than female penguins? 
#If the median line of box A lies outside of box B entirely, then there is likely to be a difference between the two groups.
#If both median lines lie within the overlap between two boxes, we will have to take another step to reach a conclusion about their groups.
#Then check the sizes of the boxes and whiskers to have a sense of ranges and variability. Finally, look for outliers if there are any.

#Yes, because all of the medians for males lie outside of the boxes for females (or 50% of the female observations).

#----Q2----
#Do you think adding sex to a model that already includes species will improve the model fit?

#Yes, because if there is a significant difference between male and female body mass, then adding sex as a predictor for body mass could explain more variation in the data.

#----Q3----
fit_both = lm(body_mass_g ~ sex * species, data = penguins)

#----Q4----
#When you fit a model with two categorical predictors, the base case is now a combination of the base level of predictor one and the base level of predictor two
#female Adelie

#----Q5----
#What are the names of the two coefficients (from the first column of the coefficient table) you need to calculate the average mass of female Chinstrap penguins?
#Intercept + speciesChinstrap

#----Q6----
#What is the average mass of female Chinstrap penguins?
3368.84+158.37
