library(fitdistrplus)

n <- 1024 # sample(200:500, 1)
uniform.min <- 0
uniform.max <- 1
normal.mean <- 0
normal.sd <- 1
exponential.rate <- 0.618
chi_squared.df <- 4
poisson.lambda <- 1.618
binomial.size <- 12
binomial.prob <- 0.618
geometric.prob <- 0.618

# ===================================================

do_chi_squared_test <- function(name, start, sample, cdf, breaks = 16) {
  sample_mean <- mean(sample)
  sample_variance <- var(sample)
  sample_histogram <- hist(sample, breaks = breaks, plot = FALSE)
  
  sample_counts <- sample_histogram$counts
  sample_breaks <- sample_histogram$breaks
  k <- length(sample_counts)
  
  sample_breaks[1] <- -Inf
  sample_breaks[k + 1] <- +Inf
  
  distribution <- cdf(sample_breaks)
  p <- distribution[2:(k + 1)] - distribution[1:k]
  result <- chisq.test(sample_counts, p = p, rescale.p = TRUE) # , simulate.p.value = TRUE)

  plot(sample_histogram, main = name, xlab = '', ylab = '')
  
  estimate <- NULL
  
  if (name != 'binom') {
    estimate <- ifelse(
      length(start) != 0,
      fitdist(sample, name, start = start)$estimate,
      fitdist(sample, name)$estimate
    )
  }
  
  list(
    p_value = result$p.value,
    sample_mean = sample_mean,
    sample_variance = sample_variance,
    estimate = estimate
  )
}

# ===================================================

uniform <- do_chi_squared_test(
  name = 'unif', start = list(min = 0, max = 1),
  sample = runif(n, min = uniform.min, max = uniform.max),
  cdf = function(q) { punif(q, min = uniform.min, max = uniform.max) }
)

normal <- do_chi_squared_test(
  name = 'norm', start = list(mean = 0, sd = 1),
  sample = rnorm(n, mean = normal.mean, sd = normal.sd),
  cdf = function(q) { pnorm(q, mean = normal.mean, sd = normal.sd) }
)

exponential <- do_chi_squared_test(
  name = 'exp', start = list(),
  sample = rexp(n, rate = exponential.rate),
  cdf = function(q) { pexp(q, rate = exponential.rate) }
)

chi_squared <- do_chi_squared_test(
  name = 'chisq',  start = list(df = 2),
  sample = rchisq(n, df = chi_squared.df),
  cdf = function(q) { pchisq(q, df = chi_squared.df) }
)

poisson <- do_chi_squared_test(
  name = 'pois', start = list(),
  sample = rpois(n, lambda = poisson.lambda),
  cdf = function(q) { ppois(q, lambda = poisson.lambda) },
  breaks = 6
)

binomial <- do_chi_squared_test(
  name = 'binom', start = list(),
  sample = rbinom(n, size = binomial.size, prob = binomial.prob),
  cdf = function(q) { pbinom(q, size = binomial.size, prob = binomial.prob) },
  breaks = 6
)

geometric <- do_chi_squared_test(
  name = 'geom', start = list(),
  sample = rgeom(n, prob = geometric.prob),
  cdf = function(q) { pgeom(q, prob = geometric.prob) }
)
