
# Week 02 -----------------------------------------------------------------

# recap notation

# pr(p|W,N) a binmocial(W|N,p) uniform(p|0,1)

W <- 6
N <- 9
p <- seq(0, 1, len = 100)
PrW <- dbinom(W,N,p)
Prp <- runif(p,0, 1)
posterior <- PrW * Prp
posterior <- posterior/sum(posterior)


# Linear regression -------------------------------------------------------

library(rethinking)

data("Howell1")
d <- Howell1[Howell1$age >= 18,]

# 1. Question/goal/estimand
# Height affects weight; H -> W


# 2. Scientific model
# weight will increase as height increases 
# anatomy of a linear model

# yi ~ Normal(mean[i], stdev)
# mean[i] = expectation
# mean[i] = a + Bxi (a line, slope & intercept)


# Generative model:
# W[i] ~ Normal(u[i], stdev)
# u[i] = a + BH[i] (H[i] = height of individual)

# set alpha, beta, sigma
alpha <- 0 # indv weight when height is 0
beta <- 0.5 # slope
sigma <- 5 # variation
n_individuals <- 100

H <- runif(n_individuals, 130, 170) # sim. heights as uniform dist (between 130 - 170 cm)

mu <- alpha + beta*H # calc. mu (est. mean weight) for each indv. (based on height)
W <- rnorm(n_individuals, mu, sigma) # sample indv. weights using rnorm (random normal deviate)

plot(W)

# 3. Statistical model(s)

# a ~ normal(0,1)
# B ~ normal(0,1)
# st.dev ~ uniform(0,1)

# sampling the prior distribution

n_samples <- 10

alpha <- rnorm(n_samples,0,1)
beta <- rnorm(n_samples, 0, 1)

plot(NULL, xlim= c(-2,2), ylim = c(-2,2),
     xlab = "x", ylab = "y")
for (i in 1:n_samples)
  abline(alpha[i], beta[i], lwd = 4, col = 2) # all over the place

# ask yourself - do these priors make sense?
# can we incorporate more information in ?

# Average adult weight in Africa = 60 kg
# pick uniform height dist. 

# the slopes shouldn't be random

# B = lognormal (log of the normal dist.) - right skewed

n <- 10

alpha <- rnorm(n,60,10) # 60 kg, huge variation
beta <- rnorm(n, 0, 10) # increase variation
beta2 <- rlnorm(n, 0, 1)
  
Hbar <- 150 # average height (cm)
Hseq <- seq(130, 200, len = 30) # uniform height dist. 

plot(NULL, xlim = c(130, 200), ylim = c(10,130),
     xlab = "height(cm)", ylab = "weight(kg)")
for (i in 1:n )
  lines(Hseq, alpha[i] + beta2[i] * (Hseq - Hbar), # indv. - mean
        lwd = 3, col = 2)

# Fitting the model

# Pr(W[i]|u[i], st.dev)
# Pr(a)
# Pr(B)
# Pr (st.dev)

# Posterior is Pr(a, B, st.dev|W,H)

# grid approx. with  many variabels is very intensive
# many posterior dist. are approx. Gaussian

#4 Validate model

# bare minimum - test statistical model with observations from scientific model
# strong test - simulation based calibration

# model formula

# W ~ dnorm(mu, sigma)
# mu <- a + b*(H-Hbar)
# a ~dnorm(60,10),
# b ~ dlnorm(0,1),
# sigma ~ dunif(0,1)

alpha <- 70 # average weight?
beta <- 0.5
sigma <- 5
n_individuals <- 100

H <- runif(n_individuals, 130, 170) # random heights
mu <- alpha + beta * (H - mean(H))
W <- rnorm(n_individuals, mu, sigma)

dat <- list(H = H, W = W, Hbar = mean(H))

m_validate <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + b *(H-Hbar),
    a ~ dnorm(60, 10),
    b ~ dlnorm(0,1),
    sigma ~ dunif(0, 10)
  ), data = dat)

precis(m_validate)

#       mean   sd  5.5% 94.5%
#a     70.43 0.57 69.51 71.34
#b      0.43 0.05  0.34  0.51
#sigma  5.73 0.41  5.08  6.38

data <- list(
  W = d$weight,
  H = d$height,
  Hbar = mean(d$height))

m_adults <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + b *(H-Hbar),
    a ~ dnorm(60, 10),
    b ~ dlnorm(0,1),
    sigma ~ dunif(0, 10)
  ), data = data)

precis(m_adults)

#       mean   sd  5.5% 94.5%
#a     45.00 0.23 44.64 45.36
#b      0.63 0.03  0.58  0.68
#sigma  4.23 0.16  3.98  4.49

# Paramaters are not independent of one another
# INSTEAD
# push out posterior predictions and describe/interpret those

post <- extract.samples(m_adults)
head(post)

# plot the sample

col2 <- col.alpha(2,0.8) # 4.4.3 (pg 98) in book

plot(d$height, d$weight,
     col = col2, lwd = 3,
     cex = 1.2, 
     xlab = "Height (cm)", ylab = "Weight (kg)")


# expectations with 99% compatibility interval
?link

xseq <- seq(130, 190, len = 50)
mu <- link(m0, data = list(H = xseq, # error with m0, maybe check book
                           Hbar = mean(d$height)))


lines(xseq, apply(mu, 2, mean), lwd = 4)
shade(apply(mu, 2, PI, prob = 0.99), xseq,
      col = col.alpha(2,0.5))