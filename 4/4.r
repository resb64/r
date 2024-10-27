# ===================================================

library (ROCR) # install.packages('ROCR')

# ===================================================

n <- 32 # sample(20 : 50, size = 1)
error.sd <- 9.48

x.from <- 5.0
x.to <- 80.0

logistic.mu <- 45
logistic.s <- 6

logistic.beta_0 <- -1 * logistic.mu / logistic.s
logistic.beta_1 <- 1 / logistic.s

# logistic <- function(x, mu, s) { (1 + exp(-1 / s * (x - mu))) ^ -1 }
logistic <- function(x, beta_0, beta_1) { (1 + exp(-1 * (beta_0 + beta_1 * x))) ^ -1 }

error <- rnorm(n, mean = .0, sd = error.sd)
x <- sample(x.from : x.to, size = n)
y <- ifelse(logistic(x + error, logistic.beta_0, logistic.beta_1) < .5, 0, 1)

plot(x = x, y = y, type = 'p', col = '#80c0e0', pch = 18, cex = 1.5, main = '', xlab = '', ylab = '')
abline(v = logistic.mu, col = '#e0e0e0'); abline(h = .5, col = '#e0e0e0')
curve(logistic(x, logistic.beta_0, logistic.beta_1), from = x.from, to = x.to, col = '#e08040', lwd = 2, main = '', xlab = '', ylab = '', add = TRUE)

# ===================================================

model <- glm(y ~ x, family = binomial(link = 'logit')) 

estimated.beta_0 <- model$coefficients[1]
estimated.beta_1 <- model$coefficients[2]

curve(logistic(x, estimated.beta_0, estimated.beta_1), from = x.from, to = x.to, col = '#e0804040', lwd = 2, main = '', xlab = '', ylab = '', add = TRUE)
summary(model)

prediction.x <- 51.0
prediction.y <- logistic(x = prediction.x, beta_0 = estimated.beta_0, beta_1 = estimated.beta_1)

# ===================================================

predicted_probabilities = predict(model, type = 'resp')

pred = prediction(predicted_probabilities, y)
perf_roc = performance(pred, measure = 'tpr', x.measure = 'fpr')
auc = performance(pred, measure = 'auc')@y.values[[1]]

plot(perf_roc, col = '#80c0e0', lwd = 1.5, main = '', xlab = 'fpr', ylab = 'tpr')
abline(a = 0, b = 1, col = '#808080', lwd = 1.5)

# ===================================================
