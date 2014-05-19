# install_github('scarsdale_assessment_database','tlevine')
library(ggplot2)
library(scales)
library(reshape2)

library(scarsdale)

# Abridged data dictionary
# scarsdale.RV_PROP_MAIN(ATT_ISS) is whether there's an issue with the data in the record.
# scarsdale.RV_PROP_MAIN(MODEL_ISS) is whether there's an issue with the model estimate for the record.

if (!('comparables' %in% ls())) {
  comparables <- query('SELECT PROP_NBR, SALE_PRICE, MODEL_EST, SALE_MINUS_MODEL FROM RV_SALE_COMP;')
}

comparables.vars <- melt(comparables, id.vars = 'PROP_NBR')

p1 <- ggplot(comparables) + aes(x = SALE_PRICE) + geom_histogram()
p2 <- ggplot(comparables) + aes(x = MODEL_EST) + geom_histogram()
p3 <- ggplot(comparables) + aes(x = (SALE_PRICE - MODEL_EST)) + geom_histogram() +
  scale_x_continuous(labels = dollar)
p4 <- ggplot(comparables) +
  aes(x = MODEL_EST, y = SALE_PRICE) + geom_point() +
  scale_x_continuous(labels = dollar)

p5 <- ggplot(comparables.vars) +
  aes(x = value) +
  facet_wrap(~ variable, ncol = 1) +
  geom_histogram() +
  scale_x_continuous(labels = dollar, breaks = seq(-1e7, 1e7, 1e6))
