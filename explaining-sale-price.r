# install_github('scarsdale_assessment_database','tlevine')
library(ggplot2)
library(scales)
library(reshape2)

library(scarsdale)

# From 6 pm Monday
if (!('comparables' %in% ls())) {
  comparables <- query('
SELECT
  RV_SALE_COMP.PROP_NBR "property.number",
  RV_SALE_COMP.MODEL_EST "model.estimate.as.comparable",
  RV_PROP_MAIN.MODEL_EST "model.estimate.as.subject",
  RV_SALE_COMP.SALE_PRICE "sale.price",
  RV_PROP_MAIN.TOTAL_PRE_2014 "comparable.estimate"
FROM RV_SALE_COMP
JOIN RV_PROP_MAIN
WHERE RV_SALE_COMP.MODEL_EST IS NOT NULL
  AND ATT_ISS = 0 AND MODEL_ISS = 0
  AND RV_PROP_MAIN.PROP_NBR = RV_SALE_COMP.PROP_NBR
;')
}

# Deal with the multiple model estimates
model.estimate.columns <- c('model.estimate.as.subject','model.estimate.as.comparable')
comparables$model.estimate <- rowMeans(comparables[model.estimate.columns])

# The two differences we're comparing
comparables$model.error <- comparables$sale.price - comparables$model.estimate
comparables$comparable.error <- comparables$sale.price - comparables$comparable.estimate
p.errors <- ggplot(comparables) +
  ggtitle('Does looking at comparables help us predict sale price?') +
  aes(x = model.error, y = comparable.error, size = sale.price) +
  coord_fixed() +
  scale_x_continuous('Model error: Difference between sale price and the modeled value', labels = dollar) + 
  scale_y_continuous('Comparable error: Difference between the sale price and the comparable-adjusted assigned value', labels = dollar) +
  scale_size_continuous('Sale price of house', labels = dollar) +
  geom_abline(slope = 1, color = 'red') + geom_abline(slope = -1, color = 'red') +
  annotate('text', label = 'Model estimate\nis better.', y = c(-1e6, 1e6), x = 0, color = 'red') +
  annotate('text', label = 'Comparable estimate\nis better.', x = c(-1e6, 1e6), y = 0, color = 'red') +
  geom_point()

.annotation.label <- paste('The', c('model','comparable'), 'estimate is\ncloser to the sale price\nfor these properties.')
p.error.differences <- ggplot(comparables) +
  aes(x = (abs(comparables$comparable.error) - abs(comparables$model.error))) +
  scale_x_continuous('Difference between absolute comparable error and absolute model error', labels = dollar) +
  geom_histogram(binwidth = 1e5) +
  geom_vline(xintercept = 0, color = 'red', size = 2) +
  ylab('Number of properties') +
  ggtitle('Does looking at comparables help us predict sale price?') +
  annotate('text', label = .annotation.label, x = 3e5 * c(-1,1), y = 50, color = 'red', size = 7, hjust = c(1,0))

# Test for a difference from zero
test.1 <- t.test(abs(comparables$model.error) - abs(comparables$comparable.error))
test.2 <- wilcox.test(abs(comparables$model.error) - abs(comparables$comparable.error))
