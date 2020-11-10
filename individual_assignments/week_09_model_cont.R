#LAB 9 Using Models II

catrate = read.csv(here("data", "catrate.csv"))
head(catrate)

#What is the evidence that reproductive success is more likely than reproductive failure? 
#The answer comes from a two-sided binomial test
#How likely is a response of 33/61 if the reproductive success and failure are equally likely, i.e., Pr(success)=0.5
#specifying the number of successes (33) and the total sample size (61)
success = sum(catrate$success)
years = sum(catrate$years)
binom.test(success,years)
#null hypothesis of equal probability of success and failure (i.e., p=0.5)

#What is the evidence that reproductive success is more or less frequent than the late-filling rate?
#In this scenario, we expect successful reproduction in approximately 5 of every 7 years.
#specifying the expected probability of success
binom.test(success, years, p = 5/7) 
#default test is a two-sided alternative

#one-sided alternative hypothesis that the observed success rate is less than the pond late-fill rate
binom.test(success, years, p = 5/7, alternative='less')
#instead of treating each pond-year as a separate observation with a binary outcome (i.e a Bernoulli trial),
#we might instead treat each pond observation and the dependent variable is the frequency of successful years out of the total number of years
#results in proportion data in which the trial size is the total number of years and each year has binary outcome
#If we had some independent variable (or predictor) that we wanted to use to explain or predict the frequency of success, then we could use a logistic regression

#----Variances----
#Comparing two variances

veg = read.csv(here("data", "vegdata.csv"), header=TRUE)
head(veg)
boxplot(pine ~ treatment, data = veg)

#test whether the variance in pine seedling count differs between two treatments
#Fisher's F test
var.test(
pine ~ treatment,
data = veg,
subset = treatment %in% c('control','clipped'))
#null=ratio of the variances will be 1

#Fisher's F test for unequal variances assumes that the data are normally distributed
#test for normality
#Shapiro-Wilk test is a one-sample test, we had to select the records for each treatment and conduct separate tests
shapiro.test(veg$pine[veg$treatment=="control"]) #p>0.05 = normally distributed
shapiro.test(veg$pine[veg$treatment=="clipped"]) #p<0.05 = not normally distributed

#If the results indicate that the data are non-normal, then we should use a non-parametric test of homogeneity of variances, such as the Fligner-Killeen test
#two-sample test
fligner.test(
  pine ~ treatment,
  data = veg,
  subset = treatment %in% c('control','clipped'))
#null=all variances are equal

#k-sample test (more than two groups)
#Bartlett's test for normal data, which we can use to test for homogeneity of variances among all four treatment levels
bartlett.test(pine ~ treatment, data=veg)
#null=no difference in variances between the groups
#highly sensitive to non-normality and the presence of outliers

#The non-parametric alternative test which is largely preferred by many statisticians is called the Fligner-Killeen test. We used it to test two variances above, but it can test n variances as well
fligner.test(pine ~ treatment, data = veg)
#null=all variances are equal

#----Means----
#Comparing two sample means

#Student's t test is appropriate when the samples are independent, the variances constant, and the errors normally distributed
t.test(pine~treatment,data=veg,subset=treatment %in% c('control','clipped'), conf.int=TRUE)
#null=means are not different
#A confidence interval that includes 0 indicates that the sample means are not significantly different

#Wilcoxon's rank-sum test is appropriate when the samples are independent but the errors are not normally distributed
wilcox.test(pine~treatment,data=veg,subset=treatment %in% c('control','clipped'), conf.int=TRUE)
#null=means are not different

#In this case, the samples will exhibit a positive covariance and it will be to our advantage to account for this covariance by using a paired t test
#create separate vectors for the "control" observations and "clipped" observations because the t.test() doesn't accept formula's (as above) for the paired option
control = veg$pine[veg$treatment=='control']
clipped = veg$pine[veg$treatment=='clipped']
#underlying assumptions of constant variance and normally distributed data
t.test(control, clipped, paired=TRUE)

#Wilcoxon's rank-sum test when the samples are independent but the errors are not normally distributed
wilcox.test(control, clipped, paired=TRUE)

#strong correlation between the two measurement variables will affect the test for significant differences between means or medians
#with any two continuous variables, x and y, the question naturally arises as to whether their values are correlated with each other


disp = read.csv(here("data", "salamander_dispersal.csv"), header=TRUE)
head(disp)
plot(disp$disp.rate.ftb, disp$disp.rate.eb)

#whether the dispersal rates for first-time breeders and experienced breeders are correlated
#test the significance of the correlation
cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs')
#null=no correlation
#use='complete.obs' argument to address the missing values for the 700 m distance class (for which there are no ponds in this particular distance interval)
#The default correlation test statistic is based on Pearson's product-moment correlation coefficient (r) cor(x,y) which follows a t distribution with length(x)-2 degrees of freedom if the samples follow independent normal distributions

#If the data are non-normal, then a non-parametric rank-based measure of association is more appropriate
#if the data do not necessarily come from a bivariate normal distribution
#Kendall's tau or Spearman's rho statistic

#Spearman's rank correlation
cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs',
  method='spearman')


#compare the empirical cumulative distributions of the samples
#Kolmogorov-Smirnov test
#Are two sample distributions the same, or are they significantly different from one another in one or more (unspecified) ways?
#Does a particular sample distribution arise from a particular hypothesized theoretical distribution?

#two distributions with exactly the same mean could be significantly different if they differed in variance, or in skew or kurtosis, or both
#Kolmogorov-Smirnov test works on empirical cumulative distribution functions (ecdf)
#Recall that these give the probability that a randomly selected value of X is less than or equal to x

#ecdf for the sample of juvenile dispersal rate
plot(
  ecdf(disp$disp.rate.ftb),
  verticals=TRUE)

#add the ecdf for the adult dispersal rate, but change the line type (lty) so that we can distinguish it from the ecdf for the juvenile dispersal rate
plot(
  ecdf(disp$disp.rate.eb),
  verticals=TRUE,
  lty=3,
  add=TRUE)

#Are these two distributions different? 
#We can use the Kolmogorov-Smirnov test (ks.test) to determine if they differ significantly in any aspect
#test statistic is the maximum difference in value of the cumulative distribution functions; i.e., maximum vertical difference in the curves for a given value of X
ks.test(disp$disp.rate.ftb,disp$disp.rate.eb)

#----Proportions----
#Comparing two or more proportions

#simple binomial proportions test, which we can easily do in R by specifying two vectors
#first containing the number of mortalities for females and males c(4,16)
#second containing the total number of female and male candidates: c(40,250)
prop.test(c(4,16),c(40,250))
#null=proportions are not statistically different
#alternative=the proportions are different between samples;
#that the proportions observed were unlikely to have been drawn from the same underlying population

#----Counts----
#Dependence of variables in a contingency table

#With count data, the number 0 is often the value of the response variable; in other words, there are often observations that receive a count of 0
#With contingency tables, counts are cross-classified according to one or more categorical contingent variables, where the contingencies are all the events of interest that could possibly happen
#contingency table shows the counts of how many times each of the contingencies actually happened in a particular sample
#In a contingency table, each observation is cross-classified according to each of the categorical contingent variables; i.e., each observation is placed into one bin representing a unique categorical level of each contingent variable

#Consider a sample of 40 forest stands where a survey determined that barred owls were either 'present' or 'absent' and where each stand was classified as either 'young' (<80 years) or 'old' (>80 years).
#Thus, each of the 40 observations could be cross-classified into one of the four contingencies: present-old, present-young, absent-old, and absent-young
#We have the following 2x2 contingency table (with expected values shown in parentheses):
owls = matrix(c(16, 9, 4, 11), nrow=2)
rownames(owls) = c("present", "absent")
colnames(owls) = c("old", "young")
owls #present absent old 16 (12.5) 4 (7.5) young 9 (12.5) 11 (7.5)

#know whether the observed counts differ from what we would expect if presence/absence was independent of stand age
#whether the expected frequencies are significantly different from the observed frequencies
#The usual way is with Pearson's chi-squared test (generalized linear models are an alternative)
chisq.test(owls)
#Pearson's chi-squared test expects the expected cell values to be large, generally greater than 4 or 5

#when one or more of the expected frequencies is less than 4 (or 5), then it is wrong to use Pearson's chi-squared test for your contingency table
#because small expected values inflate the value of the test statistic, and it can no longer be assumed to follow the chi-square distribution
#alternative test called Fisher's exact test is more appropriate
fisher.test(owls)

#read in the bird and habitat data and merged them into a single file based on the common fields
birds = read.csv(here("data", "bird.sta.csv"), header=TRUE)
hab = read.csv(here("data", "hab.sta.csv"), header=TRUE)
birdhab = merge(birds, hab, by=c("basin", "sub", "sta"))

# Create a contingency table for edge/interior and brown creeper presence/absence
#table() function to compute the cross-classified counts
table(birdhab$s.edge, birdhab$BRCR > 0) #converted the Brown Creeper counts to presence/absence
# set the presence to be in the first column
#switch the order of the columns so that the present counts were in the first column and the absent counts were in the second column, as this is the expected order in some functions (e.g, prop.test())
br_creeper_table = table(birdhab$s.edge, birdhab$BRCR > 0)[, 2:1]

#Are brown creepers present more or less frequently than expected in forest interiors versus forest edges and is the difference significant?
chisq.test(br_creeper_table)

#----Q1----
#Use bartlett.test() to check for homogeneity of the variances in our model of penguin body mass as predicted by penguin species.
bartlett.test(body_mass_g ~ species, data = penguins)

#----Q2----
#Use bartlett.test() to check for homogeneity of the variances in our model of penguin body mass as predicted by penguin sex.
bartlett.test(body_mass_g ~ sex, data = penguins)

#----Q3----
#extract the body mass observations, grouped by island
dat_groups = aggregate(
  body_mass_g ~ island,
  data = penguins,
  FUN = c)
str(dat_groups)
#extract a list containing the body mass observations in each group
dat_groups$body_mass_g

#extract separate vectors of body masses for each of the sex/species groups
dat_groups = aggregate(
  body_mass_g ~ sex * species,
  data = penguins,
  FUN = c)
str(dat_groups)
dat_groups$body_mass_g #class list

bartlett.test(dat_groups$body_mass_g, data = penguins)

#----Q4----
#null=expected Brown Creeper presence/absence proportions in edge/interior habitats are not significantly different from the observed proportions
#brown creepers are not present more or less frequently than expected/its occurence is the same in forest interiors versus forest edges

#Chi-square test on the contingency table of Brown Creeper presence/absence in edge and interior habitats
#Brown creepers are present more or less frequently than expected in forest interiors versus forest edges (p<0.001).

