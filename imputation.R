library(rethinking)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library(GGally)

data = read.csv("~/Documents/dev/R-Playground/data.csv")
# Standardize Predictors
data$input1= (data$input1 - mean(data$input1)) / sd(data$input1)
data$input2 = (data$input2 - mean(data$input2, na.rm = TRUE)) / sd(data$input2, na.rm = TRUE)
data$input3 = (data$input3 - mean(data$input3, na.rm = TRUE)) / sd(data$input3, na.rm = TRUE)

# Put outcomes on the log scale
data$output = log(data$output)


# Modeling the output:
plot.output = density(data$output)
plot(plot.output, bty="n", lty=2)


# Models using only input1
m0.1 = map2stan(
  alist(
    output ~ dnorm(mu, sigma),
    mu <- a + b * input1,
    c(a, b) ~ dnorm(0, 100),
    sigma ~ dcauchy(0,100)
  ), data=data, iter=1e4 , chains=2)

m0.2 = map2stan(
  alist(
    output ~ dnorm(mu, sigma),
    mu <- a + b * input1,
    c(a, b) ~ dcauchy(0, 100),
    sigma ~ dcauchy(0,100)
  ), data=data, iter=1e4 , chains=2)

compare(m0.1, m0.2)
precis(m0.1)
precis(m0.2)

# Models Using only input2
m0.3 = map2stan(
  alist(
    output ~ dnorm(mu, sigma),
    mu <- a + b * input2,
    input2 ~ dnorm(pmu, psigma),
    c(a, b, pmu) ~ dnorm(0, 100),
    c(sigma, psigma) ~ dcauchy(0,100)
  ), data=data, iter=1e4 , chains=2)

m0.4 = map2stan(
  alist(
    output ~ dcauchy(mu, sigma),
    mu <- a + b * input2,
    input2 ~ dnorm(pmu, psigma),
    c(a, b) ~ dnorm(0, 100),
    c(pmu, psigma) ~ dcauchy(0, 100),
    sigma ~ dcauchy(0,100)
  ), data=data, iter=1e4 , chains=2)

compare(m0.3, m0.4)
precis(m0.3)
precis(m0.4)

# Models Using only input3
m0.5 = map2stan(
  alist(
    output ~ dnorm(mu, sigma),
    mu <- a + b * input3,
    input3 ~ dnorm(pmu, psigma),
    c(a, b, pmu) ~ dnorm(0, 100),
    c(sigma, psigma) ~ dcauchy(0,100)
  ), data=data, iter=1e4 , chains=2)

m0.6 = map2stan(
  alist(
    output ~ dcauchy(mu, sigma),
    mu <- a + b * input3,
    input3 ~ dnorm(pmu, psigma),
    c(a, b) ~ dnorm(0, 100),
    c(pmu, psigma) ~ dcauchy(0, 100),
    sigma ~ dcauchy(0,100)
  ), data=data, iter=1e4 , chains=2)

compare(m0.5, m0.6)
precis(m0.5)
precis(m0.6)

# Model using all three

m0.7 = map2stan(
  alist(
    output ~ dnorm(mu, sigma),
    mu <- a + b1 * input1 + b2 * input2 + b3 * input3,
    input2 ~ dnorm(cmu, csigma),
    input3 ~ dnorm(pmu, psigma),
    c(a, b1, b2, b3, cmu, pmu) ~ dnorm(0, 100),
    c(sigma, csigma, psigma) ~ dcauchy(0, 100)
  ), data=data, iter=1e4 , chains=2)

precis(m0.7)