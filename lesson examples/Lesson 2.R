
# Lesson 2 ----------------------------------------------------------------

# Grid approximation


# set up the grid
# list of possible explanations we will consider
# sequence of values 0 to 1, 1000 of them equally spaced
p_grid <- seq(from = 0, to = 1,
              length.out = 1000) 

plot(p_grid)

# prior probability
# probability of each value of p
# uniform prior distribution = repeat 1

prob_p <- rep(1,1000) 
prob_w <- dbeta(p_grid, 3, 1) # a wet world 
?dbeta

plot(prob_w)

# probably of data
# 6 W of 9 tosses, with all the possible explanations (p_grid)
# from binomial sampling data
prob_data <- dbinom(6, 9, prob = p_grid)

plot(prob_data) # shape of posterior from data

# Normalize that. 
# Multiply prob of data by prob of explanations

posterior <- prob_data * prob_p

plot(posterior)

# normalize the values
# now a probability distribution

posterior <- posterior / sum(posterior)

plot(posterior)


# sample from posterior

samples <- sample(p_grid, prob = posterior, size = 1e4,
                  replace = TRUE)
plot(samples)

w <- rbinom(1e4, size = 9, prob = samples)


