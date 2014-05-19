# install_github('scarsdale_assessment_database','tlevine')
library(ggplot2)
library(scales)
library(reshape2)

library(scarsdale)

# Abridged data dictionary
# scarsdale.RV_PROP_MAIN(ATT_ISS) is whether there's an issue with the data in the record.
# scarsdale.RV_PROP_MAIN(MODEL_ISS) is whether there's an issue with the model estimate for the record.

if (!('comparables' %in% ls())) {
  comparables <- query('
SELECT * -- SELECT RV_SALE_COMP.PROP_NBR, SALE_PRICE, RV_SALE_COMP.MODEL_EST, SALE_MINUS_MODEL
FROM RV_SALE_COMP
JOIN RV_PROP_MAIN
WHERE RV_SALE_COMP.MODEL_EST IS NOT NULL
  AND ATT_ISS = 0 AND MODEL_ISS = 0
  AND RV_PROP_MAIN.PROP_NBR = RV_SALE_COMP.PROP_NBR
;')
}

# From 2 pm Monday
m <- lm(SALE_MINUS_MODEL ~ log(LIVING_AREA) + log(LAND_PRE_2014) + OVRL_COND, data = comparables)
plot(comparables$SALE_MINUS_MODEL ~ m$fitted.values, asp = 1, ylab = 'Actual difference between sale and model price', xlab = 'Predicted difference between sale and model price, based on characteristics of the house', main = 'The differences that we can explain with house characteristics are tiny compared to the actual differences.')
