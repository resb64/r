# ===================================================

library(scatterplot3d)
library(lmtest)
library(dplyr)

# ===================================================

filter_outliers <- function(dataframe) {
  dataframe %>%
    mutate(across(where(is.numeric), ~ ifelse(. < (quantile(., 0.25) - 1.5 * IQR(.)) | . > (quantile(., 0.75) + 1.5 * IQR(.)), NA, .))) %>%
    filter(if_all(where(is.numeric), ~ !is.na(.)))
}

# ===================================================

flats <- read.table(
  file = '~/documents/masters-draad/r/3/flats.txt',
  stringsAsFactors = TRUE,
  header = TRUE,
  sep = '\t'
)

flats <- flats %>% rename(Area = 'M2', Furniture = 'F')
flats <- filter_outliers(flats)

# ===================================================

bivariate.model <- lm(Rent ~ Area, data = flats)
summary(bivariate.model)

plot(Rent ~ Area, data = flats, type='p', col = '#101010', pch = 18, main = '')
abline(bivariate.model)

bivariate.fitted_values <- bivariate.model$fitted.values
bivariate.a_est <- bivariate.model$coefficients[1]
bivariate.b_est <- bivariate.model$coefficients[2]

# ===================================================

bivariate.flat <- list(area = 128)
bivariate.predicted_rent <- bivariate.a_est + bivariate.b_est * bivariate.flat$area

# ===================================================

multivariate.model_v1 <- lm(Rent ~ Area + Floor + Total, data = flats)
summary(multivariate.model_v1)

multivariate.model_v2 <- lm(Rent ~ Area + Floor, data = flats)
summary(multivariate.model_v2)

multivariate.model <- multivariate.model_v2

# ===================================================

scatterplot3d (
  x = flats$Area,
  y = flats$Floor,
  z = flats$Rent,
  type = 'h',
  pch =19,
  color='#80c0e0',
  xlab='Area',
  ylab='Floor',
  zlab='Rent',
  main=''
)

# ===================================================

flats_without_cat <- flats
flats_without_cat$Type = NULL
flats_without_cat$Furniture = NULL
cor(flats_without_cat)

# ===================================================

multivariate.flat <- list(area = 128, floor = 8)
multivatiate.predicted_rent <- (
  multivariate.model$coefficients[1] +
  multivariate.model$coefficients[2] * multivariate.flat$area + 
  multivariate.model$coefficients[3] * multivariate.flat$floor + 
  0
)

# ===================================================

bivariate.residuals = bivariate.model$residuals 
multivariate.residuals = multivariate.model$residuals 

par(mfrow = c(1, 2))

plot(bivariate.residuals, col = '#80c0e0', main = 'residuals (bv)')
abline(h = 0, col = '#e08040')

plot(multivariate.residuals, col = '#80c0e0', main = 'residuals (mv)')
abline(h = 0, col = '#e08040')

# ===================================================

bptest(bivariate.model, ~Area, data = flats)
bptest(multivariate.model, ~ Area * Floor + I(Area^2) + I(Floor^2), data = flats)

# ===================================================
