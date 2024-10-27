library(nortest) # install.packages('nortest')

n <- 100

a1 <- rnorm(n, mean = 0, sd = 1)
a2 <- rnorm(n, mean = 0, sd = 1)

b1 <- rnorm(n, mean = 0, sd = 1)
b2 <- rnorm(n, mean = 100, sd = 1)

c1 <- rnorm(n, mean = 0, sd = 1)
c2 <- rnorm(n, mean = 0, sd = 100)

d1 <- rnorm(n, mean = 0, sd = 1)
d2 <- rnorm(n, mean = 100, sd = 100)

# ===================================================

compare <- function(name, x1, x2) {
  par(mfrow = c(1, 2))
  hist(x1, main = sprintf('x1 (%s)', name), xlab = '', ylab = '')
  lines(density(x1), col = '#0a0a0a', lwd = 5)
  abline(v = mean(x1), col = '#80a0e0', lwd = 5)
  
  hist(x2, main = sprintf('x2 (%s)', name), xlab = '', ylab = '')
  lines(density(x2), col = '#0a0a0a', lwd = 5)
  abline(v = mean(x2), col = '#80a0e0', lwd = 5)
  
  par(mfrow = c(1, 1))
  boxplot(x1, x2, names = c('x1', 'x2'), main = sprintf('%s (boxplot)', name))
  
  list(
    x1.shapiro.p_value = shapiro.test(x1)$p.value,
    x2.shapiro.p_value = shapiro.test(x2)$p.value,
    t_test.p_value = t.test(x1, x2)$p.value,
    f_test.p_value = var.test(x1, x2)$p.value
  )
}

# ===================================================

a <- compare(name = 'a', x1 = a1, x2 = a2)
b <- compare(name = 'b', x1 = b1, x2 = b2)
c <- compare(name = 'c', x1 = c1, x2 = c2)
d <- compare(name = 'd', x1 = d1, x2 = d2)
