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
comparables$model.estimate <- mean(comparables$model.estimate.as.subject,comparables$model.estimate.as.comparable)

comparables$model.error <- comparables$sale.price - comparables$model.estimate
comparables$comparable.error <- comparables$sale.price - comparables$comparable.estimate
