
# Week 1 Homework ---------------------------------------------------------

# Due Friday January 14
# Review EASY problems at the end of Chapters 1, 2, 3

# 1. Globe tossing turned out to be 4 water and 2 land.
# construct the posterior distribution, using grid approx., and same flat prior.

# grid approximation
p_grid <- seq(from = 0, to = 1,
              length.out = 1000) 

# uniform prior
prob_p <- rep(1,1000) 

# binomial density
prob_data <- dbinom(4, 6, prob = p_grid)
plot(prob_data)

# posterior
posterior <- prob_data * prob_p

# normalize
posterior <- posterior/sum(posterior)
plot(posterior)





# 2. Data are 4 water and 2 land. Computer posterior but this time use a prior
# that is zero below p = 0.5 adn a constant above p = 0.5. This corresponds
# to prior information that a majority of the Earth's surface is water.



# 3. For the posterior distribution from 2, compute 89% percentile and HPDI intervals.
# Compare the widths of these intervals. Which is wider? Why? If you only had 
# the information in the interval, what might you misunderstand about the shape of the posterior?


# 4. OPTION CHALLENGE. Sampling bias

