
# Week 1 Homework ---------------------------------------------------------
library(rethinking)

# Due Friday January 14
# Review EASY problems at the end of Chapters 1, 2, 3

# 1. Globe tossing = 4 water and 11 land.
# construct the posterior distribution, using grid approx., and same flat prior.

p_grid <- seq(from = 0, to = 1, length.out = 1000) # grid approximation

prob_p <- rep(1,1000) # uniform prior

prob_data <- dbinom(4, 15, prob = p_grid)# binomial density, prob of data

posterior <- prob_data * prob_p
posterior <- posterior/sum(posterior)# normalize

plot(posterior, xlab = "proportion of water")

#left this step out in my homework
set.seed(100)
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)

plot(samples)
dens(samples) # plot the result; more mass over values below 0.5. mean is approx. 0.3


# 2. Data are 4 water and 2 land. Computer posterior but this time use a prior
# that is zero below p = 0.5 and a constant above p = 0.5. This corresponds
# to prior information that a majority of the Earth's surface is water.

prior2 <- ifelse( p_grid < 0.5, 0, 1) # same results just two ways to do it
prior3 <- c(rep(0, 500), rep(1, 500))

p_likelihood <- dbinom(4, 6, prob = p_grid)

posterior2 <- p_likelihood * prior2
posterior2 <- posterior2/sum(posterior2)

set.seed(100)

samples2 <- sample(p_grid, prob = posterior2, size = 1e4, replace = TRUE)

dens(samples2)
# post. mean should be about 0.7


# 3. For the posterior distribution from 2, compute 89% percentile and HPDI intervals.
# Compare the widths of these intervals. Which is wider? Why? If you only had 
# the information in the interval, what might you misunderstand about the shape of the posterior?

set.seed(100)

p.PI <- PI(samples2)
#      5%       94% 
#  0.5245245 0.8798799 

p.HPDI <- HPDI(samples2)

#     |0.89     0.89| 
#  0.5005005 0.8388388 

p.PI < p.HPDI # false

# PI is wider. 

dens(samples2, show.HPDI = TRUE)

# 4. OPTION CHALLENGE. Sampling bias

