
# Week 1 Homework ---------------------------------------------------------
library(rethinking)

# Due Friday January 14
# Review EASY problems at the end of Chapters 1, 2, 3

# 1. Globe tossing = 4 water and 11 land.
# construct the posterior distribution, using grid approx., and same flat prior.

# grid approximation
p_grid <- seq(from = 0, to = 1, length.out = 1000) 

# uniform prior
prob_p <- rep(1,1000) 

# binomial density
prob_data <- dbinom(4, 15, prob = p_grid)
plot(prob_data)

# posterior
posterior <- prob_data * prob_p

# normalize
posterior <- posterior/sum(posterior)
plot(posterior)

#left this step out in my homework
set.seed(100)
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)

plot(samples)
dens(samples)

# 2. Data are 4 water and 2 land. Computer posterior but this time use a prior
# that is zero below p = 0.5 and a constant above p = 0.5. This corresponds
# to prior information that a majority of the Earth's surface is water.

prior2 <- ifelse( p_grid < 0.5, 0, 1)

p_likelihood <- dbinom(4, 6, prob = p_grid)

posterior2 <- p_likelihood * prior2
posterior2 <- posterior2/sum(posterior2)

plot(posterior2 ~ p_grid, type = "l",
     xlab = "Probability of water",
     ylab = "Posterior probability")
mtext("W,W,W,W,L,L")


# 3. For the posterior distribution from 2, compute 89% percentile and HPDI intervals.
# Compare the widths of these intervals. Which is wider? Why? If you only had 
# the information in the interval, what might you misunderstand about the shape of the posterior?

# 89% percentile
q_89 <- quantile(posterior2, 0.9)

#89% 
#0.002860699 

plot(posterior2 ~ p_grid, type = "l",
     xlab = "Probability of water",
     ylab = "Posterior probability")
abline(v = 0.002860699, col = "red")
text(0.15, 0.0025, "89%")

# HPDI (highest posterior density interval)

# narrowest interval containing the specified probability mass
hdpi <- HPDI(posterior2, prob = 0.7)

dens(posterior2, show.HPDI = TRUE)



# 4. OPTION CHALLENGE. Sampling bias

