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
  ggtitle('Does the comparables process help us predict sale price?') +
  aes(x = model.error, y = comparable.error, size = sale.price) +
  coord_fixed() +
  scale_x_continuous('Model error: Difference between sale price and the modeled value', labels = dollar) + 
  scale_y_continuous('Comparable error: Difference between the sale price\nand the comparable-adjusted assigned value', labels = dollar) +
  scale_size_continuous('Sale price of house', labels = dollar) +
  geom_abline(slope = 1, color = 'red') + geom_abline(slope = -1, color = 'red') +
  geom_point() +
  annotate('text', label = 'Model estimate\nis better.', y = c(-1e6, 1e6), x = 0, color = 'red') +
  annotate('text', label = 'Comparable estimate\nis better.', x = c(-1e6, 1e6), y = 0, color = 'red')

.annotation.label <- paste('The', c('model','comparable'), 'estimate is\ncloser to the sale price\nfor these properties.')
p.error.differences <- ggplot(comparables) +
  aes(x = (abs(comparable.error) - abs(model.error))) +
  scale_x_continuous('Difference between absolute comparable error and absolute model error', labels = dollar) +
  geom_histogram(binwidth = 1e5) +
  geom_vline(xintercept = 0, color = 'red', size = 2) +
  ylab('Number of properties') +
  ggtitle('Does looking at comparables help us predict sale price?') +
  annotate('text', label = .annotation.label, x = 3e5 * c(-1,1), y = 50, color = 'red', size = 7, hjust = c(1,0))

p.error.ratios <- ggplot(comparables) +
  aes(x = model.error / sale.price, y = comparable.error / sale.price) +
  geom_point() + coord_fixed() +
  scale_x_continuous('Proportion of sale price that is the model error', labels = percent) +
  scale_y_continuous('Proportion of sale price that is the comparable error', labels = percent)

cor(comparables[c('model.estimate','comparable.estimate','sale.price')], method = 'spearman')

# Test for a difference from zero on average/median
test.1 <- t.test(abs(comparables$model.error) - abs(comparables$comparable.error))
test.2 <- wilcox.test(abs(comparables$model.error) - abs(comparables$comparable.error))

m.1 <- lm(sale.price ~ model.estimate, data = comparables)
m.2 <- lm(sale.price ~ comparable.estimate, data = comparables)

# Draw
ggsave('errors.png', p.errors, width = 11, height = 6)
ggsave('error-differences.png', p.error.differences, width = 11, height = 6)
