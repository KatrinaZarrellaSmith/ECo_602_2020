# Week 6 Liklihoods

#2 sites where I counted 2 and 6 birds
x_observed = c(2, 6)
print(x_observed)

#I think I can model the population of birds at the study sites with a Poisson distribution
#I know that the Poisson distribution has a single parameter: ??
#I also know that the mean and standard deviation of a Poisson distribution are equal to ??

#I think the count of 2 is unusually low, so I decide to propose a Poisson distribution with ??=4.5 as a model of the counts of Wilson's Warblers on my study sites
#I can use dpois() to calculate the probability mass for the two counts given a Poisson distribution with ??=4.5
dpois(x = 2, lambda = 4.5)
dpois(x = 6, lambda = 4.5)
#likelihood of observing those particular counts together is the product of the individual likelihoods
dpois(x = 2, lambda = 4.5) * dpois(x = 6, lambda = 4.5)

#take advantage of vectorization in R by storing the counts in a single vector
wiwa_counts = c(2, 6)
dpois(x = wiwa_counts, lambda = 4.5)
#liklihood
prod(dpois(x = wiwa_counts, lambda = 4.5))
#sum of log-likelihoods
sum(log(dpois(x = wiwa_counts, lambda = 4.5)))

dat_bird = read.csv("bird.sta.csv")
dat_habitat = read.csv("hab.sta.csv")
dat_all = merge(dat_bird, dat_habitat)
summary(dat_all$WIWA)
hist(dat_all$WIWA)
#try a Poisson distribution with lambda = 1.0
sum(log(dpois(x = dat_all$WIWA, lambda = 1.0)))

#Q1 experiment with different values of ?? to find a value that maximizes the sum of log-likelihoods
sum(log(dpois(x = wiwa_counts, lambda = 4)))
#sum(log(dpois(x = wiwa_counts, lambda = 3.998)))
#sum(log(dpois(x = wiwa_counts, lambda = 4.002)))

#Q2 Find the ?? value of a Poisson model that makes all of the observed the Winter Wren's census counts most likely
summary(dat_all$WIWR)
hist(dat_all$WIWR, breaks = 6, main = "Winter Wren Counts", xlab = "Counts")
#try a Poisson distribution with lambda = 1.0
sum(log(dpois(x = dat_all$WIWR, lambda = 1.5)))

#Q3 Find values of the parameters for a binomial distribution that make the vector of census counts most likely
#remove NA's and count total observations by site
n = length(na.omit(dat_all$WIWR))
#trial and error probability
sum(log(dbinom(dat_all$WIWR, n, p = 1)))
sum(log(dbinom(dat_all$WIWR, n, p = .001)))
sum(log(dbinom(dat_all$WIWR, n, p = .1)))
