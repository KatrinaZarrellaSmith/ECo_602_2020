#Week 6 Frequentist Concepts

#for discrete parametric distributions:
#What is the probability that I observe a value of x or less/or more
#pbinom: the cumulative mass function
#Q2 the probability of observing a count of 3 successes or fewer in a binomial distribution with parameters n = 4 and p = 0.75
pbinom(3, size = 4, p = 0.75, lower.tail=TRUE)
#Q3 the probability of observing more than 3 successes in a binomial distribution with parameters n = 5 and p = 0.75
pbinom(3, size = 5, p = 0.75, lower.tail=FALSE)

#What is the probability that I observe a value of exactly  x
#dbinom: the probability mass function
#Q1 the probability of observing a count of exactly 3 successes in a binomial distribution with parameters n = 4 and p = 0.75
dbinom(3, size = 4, p = 0.75)

#What is the median or 50th or 90th percentile
#qbinom: the quantile function

#for continuous parametric distributions
#What is the probability that I observe a value between 1.2 and 2.4
#Q6 probability of observing a value between 1.2 and 3.2 from a normally-distributed population with mean = 2 and standard deviation = 2
pnorm(3.2, mean = 2, sd = 2, lower.tail=TRUE) - pnorm(1.2, mean = 2, sd = 2, lower.tail=TRUE)

#What is the probability that I observe a value of 1.3 or more
#pnorm: the cumulative density function
#What is the probability of observing a value less than 7.5 in a normal distribution with mean 10 and standard deviation 3?
pnorm(7.5, mean = 10, sd = 3, lower.tail=TRUE)
#Q4 probability of observing a value of less than 1.2 from a normally-distributed population with mean = 2 and standard deviation = 2
pnorm(1.2, mean = 2, sd = 2, lower.tail=TRUE)
#Q5 he probability of observing a value of greater than 1.2 from a normally-distributed population with mean = 2 and standard deviation = 2
pnorm(1.2, mean = 2, sd = 2, lower.tail=FALSE)
#the probability of observing the value of 7.5 or higher?
pnorm(7.5, mean = 10, sd = 3, lower.tail = FALSE)

#Is a value of 1.2 or 2.4 more likely?
#dnorm: the probability density function

#What is the 20th percentile of fish lengths
#qnorm: the quantile function

#The law of total probability
#The sum of all events in the sample space is 1.0
#Pr(E)+Pr(Ec)=1.0