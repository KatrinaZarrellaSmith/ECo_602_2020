#Lab 10: Bending the Linear Model

rm(list = ls()) #clear workspace

#understand the statistical power of experiments in relation to varying sample size and error rates
#explore the possibility that the standard errors among replicates in extended experiments might vary from the first experiment

#rope and blade data
require(here)

rope = read.csv(here("data","rope.csv"))
rope$rope.type = factor(rope$rope.type)

class(rope$rope.type) #factor
#when we use a categorical predictor, we can also think if it as a grouping factor
levels(rope$rope.type)

#total number of observations
n_obs = length(rope$rope.type)
#number of groups (rope types)
n_groups = length(levels(rope$rope.type))

#partition the total variance in the response variable into its components:
#among-group
#within-group, so that we can compute the ratio for our test statistic.
#among+within=total variance

#----SS----
#calculate the "sums of squares"/"sum of squared residuals" for the entire data set
#the squared deviation of each observation from the overall mean

#deterministic model of the percent rope cut is the mean rope cut (of all observations)/the grand mean
grand_mean = mean(rope$p.cut)

ss_tot = sum((rope$p.cut - grand_mean)^2)
df_tot = n_obs - 1

#----SSE----
#calculate the sums of squares within groups (rope types)
#a sum of squares can be calculated for each group
#The sum of each individual group's sum of squares is the within-group sum of squares for the ANOVA
#The SS within is also called the sum of squares error (SSE), or the sum of squares of residuals
#You can think of this quantity as the sum of squares of the refined model: the model that conditions the observations on the grouping factor
#We hope that adding the grouping factor leads to a reduction in the sum of squares

#calculate the group means
aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type), 
  FUN = mean)

#more explicit version
aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) mean(x))

#calculate the squared residuals
agg_sq_resids = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) sum((x - mean(x))^2))

#test
str(agg_sq_resids)

#Sum the group SSs
ss_within = sum(agg_sq_resids$x)
df_within = n_obs - n_groups

#----SSA----
#calculate the "sums of squares among"/SSA
ss_among = ss_tot - ss_within
#The extent to which ss_within is less than ss_tot is a reflection of the magnitude of the differences between the means.
#If ss_within is much smaller than ss_tot, then most of the variation in the response is due to differences among groups (or levels of the independent factor).
#Another way of looking at this is that as the ratio of ss_among to ss_within increases, then an increasing amount of the variation is due to differences among groups.
df_among = n_groups - 1
  
#----Normalize----
#can't compare the sums of squares directly because the numbers of groups are different than the total number of observations
#ex. larger samples will have larger sums of squared residuals, even if the variance is the same
#normalize a sum of squared deviations from a mean to get variance
#We can compare the within-group and among-group variance directly, whereas we cannot directly compare the within-group and among-group SS

#adjust the sums of squares to reflect the degrees of freedom available given the number of treatments (or levels of the independent factor) and the number of replicates per treatment.
df_tot = n_obs - 1
#We lose 1 d.f. because in calculating ss_tot we had to estimate one parameter from the data in advance: the grand mean

#how many degrees of freedom to allocate to ss_within?
#for each group we lose one degree of freedom because we calculated a group mean
#within-group degrees of freedom is just the number of observations minus the number of groups
df_within = n_obs - n_groups

#mean squares are obtained simply by dividing each sum of squares by its respective degrees of freedom
ms_among  =  ss_among / df_among
ms_within = ss_within / df_within

#----F test----
#F-ratio, defined as the among-group variance divided by the within-group variance
f_ratio = ms_among / ms_within
#null=treatment means are all the same (accept when number is small)
#alternative=at least one of the means is significantly different from the others (accept when number is big)

#decide whether the test statistic is big or small by comparing it to the values from an F probability distribution, given the number of degrees of freedom in the numerator and the number of degrees of freedom in the denominator
#we want to know the Type 1 error rate (p-value); i.e., the probability of observing an F-ratio as large as ours given that the null hypothesis is true and thus the treatment means are the same

#use pf() for cumulative probabilities of the F distribution
#In our case the numerator was the among-group variance and the denominator was the within-group variance
f_pval = pf(q = f_ratio, df1 = df_among, df2 =  df_within, lower.tail = FALSE) #P[X > x]

#----Test----
{
# number comparison tolerance
digits_check = 5

# Build the reference model using R functions
fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)
anova_fit_1 = anova(fit_1)

# Check degrees of freedom
anova_fit_1$Df == c(df_among, df_within)

# Check sums of squares
round(anova_fit_1$`Sum Sq`, digits = digits_check) == round(c(ss_among, ss_within), digits = digits_check)

# Check mean squares
round(anova_fit_1$`Mean Sq`, digits = digits_check) == round(c(ms_among, ms_within), digits = digits_check)

# Check the F-ratio
round(anova_fit_1$`F value`[1], digits = digits_check) == round(f_ratio, digits = digits_check)

# Check the F test statistic p-value
round(anova_fit_1$`Pr(>F)`[1], digits = digits_check) == round(f_pval, digits = digits_check)
}

#----ANOVA----
fit_1 = lm(p.cut ~ rope.type, data=rope) #fitted model object
anova_fit_1 = anova(fit_1)
str(anova_fit_1)

anova_fit_1$`Pr(>F)` #extract p-value
anova_fit_1$`Sum Sq` #extract SS
