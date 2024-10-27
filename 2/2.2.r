library(psych) # install.packages('psych')

# ===================================================

n <- 200
mean <- .0
sd <- 1.0

r1 <- runif(1, .05, .15)
r2 <- runif(1, .6, .9)

x1 <- rnorm(n, mean = mean, sd = sd)
x2 <- r1 * x1 + sqrt(1.0 - r1 ^ 2) * rnorm(n, mean = mean, sd = sd)
x3 <- r2 * x1 + sqrt(1.0 - r2 ^ 2) * rnorm(n, mean = mean, sd = sd)

# ===================================================

par(mfrow = c(1, 2))
plot(x1, x2, main = 'x1 / x2', xlab = 'x1', ylab = 'x2')
plot(x1, x3, main = 'x1 / x3', xlab = 'x1', ylab = 'x3')

# ===================================================

correlation_x1_x2 <- cor(x1, x2)
correlation_x1_x3 <- cor(x1, x3)

t_statistic_x1_x2 <- correlation_x1_x2 * sqrt((n - 2) / sqrt(1 - correlation_x1_x2 ^ 2))
t_statistic_x1_x3 <- correlation_x1_x3 * sqrt((n - 2) / sqrt(1 - correlation_x1_x3 ^ 2))

p_value_x1_x2 <- pt(-abs(t_statistic_x1_x2), df = n - 2) * 2 # (1 - pt(abs(t_statistic_x1_x2), df = n - 2)) * 2
p_value_x1_x3 <- pt(-abs(t_statistic_x1_x3), df = n - 2) * 2 # (1 - pt(abs(t_statistic_x1_x3), df = n - 2)) * 2

# ===================================================

# fisher_z_x1_x2 <- log((1 + correlation_x1_x2) / (1 - correlation_x1_x2)) / 2
# fisher_z_x1_x3 <- log((1 + correlation_x1_x3) / (1 - correlation_x1_x3)) / 2
# 
# fisher_lower_x1_x2 <- fisher_z_x1_x2 - qnorm(.95) / sqrt(n - 3)
# fisher_upper_x1_x2 <- fisher_z_x1_x2 + qnorm(.95) / sqrt(n - 3)
# 
# fisher_lower_x1_x3 <- fisher_z_x1_x3 - qnorm(.95) / sqrt(n - 3)
# fisher_upper_x1_x3 <- fisher_z_x1_x3 + qnorm(.95) / sqrt(n - 3)
# 
# confidence_interval_r1 <- list(
#   (exp(2 * fisher_lower_x1_x2) - 1) / (exp(2 * fisher_lower_x1_x2) + 1),
#   (exp(2 * fisher_upper_x1_x2) - 1) / (exp(2 * fisher_upper_x1_x2) + 1)
# )
# 
# confidence_interval_r2 <- list(
#   (exp(2 * z_lower_13) - 1) / (exp(2 * z_lower_13) + 1),
#   (exp(2 * z_upper_13) - 1) / (exp(2 * z_upper_13) + 1)
# )

confidence_interval_r1 <- r.con(r1, n = n, p = .95)
confidence_interval_r2 <- r.con(r2, n = n, p = .95)
