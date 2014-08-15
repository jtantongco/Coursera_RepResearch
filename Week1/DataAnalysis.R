library(kernlab)
data(spam)


#partition data into test set and training set
# Perform the subsampling
set.seed(3435)
trainIndicator = rbinom(4601, size = 1, prob = 0.5)
table(trainIndicator)

trainSpam = spam[trainIndicator == 1, ]
testSpam = spam[trainIndicator == 0, ]

names(trainSpam)
head(trainSpam)

table(trainSpam$type)

plot( trainSpam$capitalAve ~ trainSpam$type)
#if highly skewed data
# note the +1 is to stop the log of 0
plot( log10(trainSpam$capitalAve + 1) ~ trainSpam$type)

#relationship between predictors
plot(log10(trainSpam[,1:4]+1))

#clusters
hCluster = hclust(dist(t(trainSpam[, 1:57])))
plot(hCluster)

#processed clustering
hClusterUpdated = hclust(dist(t(log10(trainSpam[, 1:55] + 1))))
plot(hClusterUpdated)

#logistic regression
# 1 variable
trainSpam$numType = as.numeric(trainSpam$type) - 1
costFunction = function(x, y) sum(x != (y > 0.5))
cvError = rep(NA, 55)
library(boot)
for (i in 1:55) {
  lmFormula = reformulate(names(trainSpam)[i], response = "numType")
  glmFit = glm(lmFormula, family = "binomial", data = trainSpam)
  cvError[i] = cv.glm(trainSpam, glmFit, costFunction, 2)$delta[2]
}

## Which predictor has minimum cross-validated error?
names(trainSpam)[which.min(cvError)]

## Use the best model from the group
predictionModel = glm(numType ~ charDollar, family = "binomial", data = trainSpam)

## Get predictions on the test set
predictionTest = predict(predictionModel, testSpam)
predictedSpam = rep("nonspam", dim(testSpam)[1])

## Classify as `spam' for those with prob > 0.5
predictedSpam[predictionModel$fitted > 0.5] = "spam"

## Classification table
table(predictedSpam, testSpam$type)

# 61 were classified as spam that were not spam
# 451 were classified as non spam that were spam

## Error rate
(61 + 458)/(1346 + 458 + 61 + 449)


