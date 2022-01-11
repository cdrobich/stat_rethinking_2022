
# Book examples -----------------------------------------------------------

# Wk 01: Chp 1-3 -----------------------------------------------------------

#### GRID Approximation ####

# can achieve approx. of continuous posterior distribution by considering a grid of parameter values

# for any particular value of a parameter (p') computer posterior probability by:
# multiply prior probability of p' by the likelihood of p'

# define grid
p_grid <- seq( from = 0, to = 1, length.out = 50) # 20 values from 0 to 1
?seq
# equally spaced values from 0 to 1

#define prior

prior <- rep(1,5)
plot(prior)

# compute likelihood at each value in grid

likelihood <- dbinom(6, 9, prob = p_grid)

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

plot(posterior)

plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )
mtext( "20 points" )


# with informed priors

prior <- ifelse( p_grid < 0.5 , 0 , 1 ) # zero probability to values < 0.5

prior <- exp( -5*abs( p_grid - 0.5 ) ) # peaked prior


#### Quadratic Approximation ####

# log of Gaussian dist is a parabola = Gaussian approx. =  quadratic approx
# reps any log-posterior with a parabola

# Two steps:
# 1. Find peak
# 2. Estimate curvature near peak

library(rethinking)
# quap

globe.qa <- quap(
  alist(
    W ~ dbinom(W+L, p), # binomial likelihood
    p ~ dunif(0,1) # uniform prior
  ),
  data = list(W = 6, L = 3))

# display summary

precis(globe.qa) # brief summary of quadratic approximation

#  mean    sd   5.5%   94.5%
#  p 0.67  0.16  0.42  0.92

# posterior mean (aka peak) = 0.67
# StDev is the curvature 

# Assuming posterior is Gaussian, it is maximized at 0.67 and standard dev is 0.16

# Analytica approach= exact right answer

# analytical calculation 
W <- 6
L <- 3

curve( dbeta( x , W+1 , L+1 ) , from = 0 , to = 1 )
# quadratic approximation
curve( dnorm( x , 0.67 , 0.16 ) , lty = 2 , add = TRUE )


### MCMC Globe Example ####

n_samples <- 1000

p <- rep(NA, n_samples) # samples from posterior dist.

p[1] <- 0.5

W <- 6
L <- 3

for (i in 2:n_samples) {
  p_new <- rnorm(1, p[i-1], 0.1)
  if (p_new < 0) p_new <- abs(p_new)
  if (p_new > 1) p_new <- 2 - p_new
  q0 <- dbinom(W, W+L, p[i-1])
  q1 <- dbinom(W, W+L, p_new)
  p[i] <- ifelse(runif(1) < q1/q0, p_new, p[i-1])
}

# compare to analytical posterior
dens( p , xlim=c(0,1) )
curve( dbeta( x , W+1 , L+1 ) , lty=2 , add=TRUE )


# Chapter 2 practice Q ----------------------------------------------------

# 2M1

p_grid <- seq(0, 1, length.out = 100) 

prior <- rep(1,100) # uniform prior

# W,W,W
likelihood <- dbinom(3, 3, prob = p_grid)
post.un.1 <- likelihood * prior
post.1 <- post.un.1/sum(post.un.1)

plot(post.1,
     xlab="probability of water" , ylab="posterior probability" )
mtext("W,W,W")

# W,W,W,L
likelihood <- dbinom(3, 4, prob = p_grid)
post.un.2 <- likelihood * prior
post.2 <- post.un.2/sum(post.un.2)

plot(post.2,
     xlab="probability of water" , ylab="posterior probability" )
mtext("W,W,W,L")

#L,W,W,L,W,W,W
likelihood <- dbinom(5, 7, prob = p_grid)
post.un.3 <- likelihood * prior
post.3 <- post.un.3/sum(post.un.3)

plot(post.3,
     xlab="probability of water" , ylab="posterior probability" )
mtext("L,W,W,L,W,W,W")
plot(post.3 ~ p_grid, type = "l")

par(mfrow=c(1,3)) 

plot(post.1 ~ p_grid, type = "l")
mtext("W,W,W")

plot(post.2 ~ p_grid, type = "l")
mtext("W,W,W,L")

plot(post.3 ~ p_grid, type = "l")
mtext("L,W,W,L,W,W,W")

# 2M2
??ifelse # test, yes, no

prior <- ifelse( p_grid < 0.5 , 0 , 1 ) 



# Chapter 3 ---------------------------------------------------------------

# vampire example

# pr(positive test |vampire) = 0.95
# pr(positive test | mortal) = 0.01
# pr(vampire) = 0.001

# what is the probability of a positive test = vampire?
# Bayes theorem to invert probability and calculate Pr(vampire|positive)

pr_positive_vamp <- 0.95
pr_poisitive_mortal <- 0.01
pr_vampire <- 0.001

pr_positive <- pr_positive_vamp * pr_vampire +
  pr_poisitive_mortal * (1 - pr_vampire)

(pr_vampire_positive <- pr_positive_vamp * pr_vampire/ pr_positive)
# 0.0867

# when condition of interest is very rare, have a test that find all true cases is
# still no guarantee that a positive result carries much information at all

# most positive results are false positives, even when all true positives are detected correctly

# In a population of 100,000 people, 100 are vampires.
# Of those 100, 95 will test positive for vampirism.
# Of the remaining 99,900 mortals, 999 will test positive for vampirism.

# If we test everyone, what proportion of them will be true positives? (aka vampires we catch)

# 999 + 95 = 1084
# Pr(vampire|positive) = 95/1094 = ~ 0.087

