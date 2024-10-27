# ===================================================

error_sd <- 1.618
x_start <- 1
x_step <- .1
alpha <- 0.05

n <- 128
a <- 0.618
b <- 1.618

t <- 1.6574 # n = 128, alpha = 0.05
f <- 3.9201 # n = 128, alpha = 0.05

error <- rnorm(n = n, mean = 0, sd = error_sd)
x <- x_start + x_step * (0 : (n - 1))

# ===================================================

do_fit <- function(x, y) {
  b_est <- (mean(x * y) - mean(x) * mean(y)) / (mean(x ^ 2) - mean(x) ^ 2)
  a_est <- mean(y) - b_est * mean(x)
  y_est <- a_est + b_est * x
  
  y_variance <- 1 / (n - 2) * sum((y - (a_est + b_est * x)) ^ 2)
  b_est_variance <- y_variance / sum((x - mean(x)) ^ 2)
  a_est_variance <- mean(x ^ 2) * b_est_variance
  a_est_sd <- a_est_variance ^ 0.5
  b_est_sd <- b_est_variance ^ 0.5
  
  a_confidence_interval <- list(from = a_est - t * a_est_sd, to = a_est + t * a_est_sd)
  b_confidence_interval <- list(from = b_est - t * b_est_sd, to = b_est + t * b_est_sd)
  
  a_t <- a_est / a_est_sd
  b_t <- b_est / b_est_sd
  is_a_significant <- abs(a_t) > t
  is_b_significant <- abs(b_t) > t
  
  coefficient_of_determination <- 1 - sum((y - y_est) ^ 2) / sum((y - mean(y)) ^ 2)
  
  model_f <- (n - 2) * coefficient_of_determination / (1 - coefficient_of_determination)
  is_model_adequate <- model_f > f
  
  data <- data.frame(cbind(x, y))
  model <- lm(y ~ x, data = data)
  
  prediction <- predict(model, interval = 'pred', level = 1 - alpha)
  confidence <- predict(model, interval = 'conf', level = 1 - alpha)
  
  plot(x = x, y = y, type = 'p', col = '#101010', pch = 18, main = '')
  matlines(data$x, cbind(prediction), col='#80c0e0', lty = c(1, 1, 1), lwd = 1.5)
  matlines(data$x, cbind(confidence), col = '#e08040', lty = c(1, 1, 1), lwd = 1.5)
  
  list(
    a_est = a_est,
    b_est = b_est,
    y_variance = y_variance,
    a_est_variance = a_est_variance,
    b_est_variance = b_est_variance,
    a_confidence_interval = a_confidence_interval,
    b_confidence_interval = b_confidence_interval,
    coefficient_of_determination = coefficient_of_determination,
    is_model_adequate = is_model_adequate,
    is_a_significant = is_a_significant,
    is_b_significant = is_b_significant
  )
}

do_case <- function(name, x, y, x_ln, y_ln) {
  par(mfrow = c(1, 2))
  plot(x = x, y = y, type = 'p', col = '#101010', pch = 18, main = name)
  do_fit(x = x_ln, y = y_ln)
}

# ===================================================

case_a <- do_case (
  name = 'y = a + x ^ b + error',
  x = x,
  y = a + x ^ b + error,
  x_ln = log(x),
  y_ln = log(a) + b * log(x) + error
)

case_b <- do_case (
  name = 'y = a * exp(b * x) * error',
  x = x,
  y = a * exp(b * x) * error,
  x_ln = x,
  y_ln = log(a) + b * x + error
)

case_c <- do_case (
  name = 'y = a + b * log(x) + error',
  x = x,
  y = a + b * log(x) + error,
  x_ln = log(x),
  y_ln = a + b * log(x) + error
)

case_d <- do_case (
  name = 'y = a + b / x + error',
  x = x,
  y = a + b / x + error,
  x_ln = 1 / x,
  y_ln = a + b / x + error
)

# ===================================================
