library(dplyr) # install.packages('dplyr')

# ===================================================

flats <- read.table(
  file = '~/documents/masters-draad/r/2/flats.txt',
  stringsAsFactors = TRUE,
  header = TRUE,
  sep = '\t'
)

flats <- flats %>% rename(Area = 'M2', Furniture = 'F')
str(flats)

# ===================================================

filter_outliers <- function(dataframe) {
  dataframe %>%
  mutate(across(where(is.numeric), ~ ifelse(. < (quantile(., 0.25) - 1.5 * IQR(.)) | . > (quantile(., 0.75) + 1.5 * IQR(.)), NA, .))) %>%
  filter(if_all(where(is.numeric), ~ !is.na(.)))
}

flats <- filter_outliers(flats)

# ===================================================

hist(flats$Rent, main = 'Rent', xlab = '', ylab = '')
hist(flats$Floor, main = 'Floor', xlab = '', ylab = '')
hist(flats$Total, main = 'Floors Total', xlab = '', ylab = '')
hist(flats$Area, main = 'Area', xlab = '', ylab = '')

# ===================================================

shapiro.rent <- shapiro.test(flats$Rent)
shapiro.floor <- shapiro.test(flats$Floor)
shapiro.floors_total <- shapiro.test(flats$Total)
shapiro.area <- shapiro.test(flats$Area)

# ===================================================

par(mfrow = c(1, 2))
boxplot(Rent ~ Type, data = flats, main = 'Rent ~ Type', xlab = 'Type', ylab = 'Rent')
boxplot(Area ~ Type, data = flats, main = 'Area ~ Type', xlab = 'Type', ylab = 'Area')

par(mfrow = c(1, 2))
boxplot(Rent ~ Furniture, data = flats, main = 'Rent ~ Furniture', xlab = 'Furniture', ylab = 'Rent')
boxplot(Area ~ Furniture, data = flats, main = 'Area ~ Furniture', xlab = 'Furniture', ylab = 'Area')

t_test_rent_type <- t.test(Rent ~ Type, data = flats)
t_test_area_type <- t.test(Area ~ Type, data = flats)
t_test_rent_furniture <- t.test(Rent ~ Furniture, data = flats)
t_test_area_furniture <- t.test(Area ~ Furniture, data = flats)

# ===================================================

correlation_rent_floor <- cor.test(flats$Rent, flats$Floor, method = 'spearman')
correlation_rent_total <- cor.test(flats$Rent, flats$Total, method = 'spearman')
correlation_area_floor <- cor.test(flats$Area, flats$Floor, method = 'spearman')
correlation_area_total <- cor.test(flats$Area, flats$Total, method = 'spearman')

# ===================================================

contingency_table <- table(flats$Furniture, flats$Type)
chisq.test(contingency_table)
print(contingency_table)
