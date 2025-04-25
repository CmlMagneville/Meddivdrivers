################################################################################
##
## An example of how to use decisions trees and random forests based on
## ... "An introduction to Statistical Learning" books
##
## Camille Magneville
##
## 07/05/2024
##
## Learn_trees_and_rf.R
##
################################################################################


library(ISLR2)
head(Boston)

lirary(randomForest)

train <- sample(1: nrow (Boston), nrow (Boston) / 2)
boston.test <- Boston[-train, "medv"]

# Bagging (m = nb of predictors = 12):
bag.boston <- randomForest(medv ~ ., data = Boston,
                           subset = train, mtry = 12, importance = TRUE)
bag.boston
# How well does this bagged model perform on the test set?
yhat.bag <- predict(bag.boston, newdata = Boston [-train , ])
plot(yhat.bag , boston.test)
abline (0, 1)
mean((yhat.bag - boston.test )^2)
# Can change the number of trees:
bag.boston <- randomForest(medv ~ ., data = Boston,
                           subset = train, mtry = 12,
                           ntree = 25, importance = TRUE)
yhat.bag <- predict(bag.boston, newdata = Boston [-train, ])
mean((yhat.bag - boston.test )^2)

# Random forest: (m = sqrt(p) = 6)
rf.boston <- randomForest(medv ~ ., data = Boston ,
                          subset = train, mtry = 6, importance = TRUE)
rf.boston
# How well does this bagged model perform on the test set?
yhat.rf <- predict(rf.boston, newdata = Boston [-train, ])
plot(yhat.rf , boston.test)
abline (0, 1)
mean((yhat.rf - boston.test)^2) # gives the MSE (needs to be low)
# Error based on nb of trees:
plot(rf.boston)    # ok with 100 trees
# Importance of each predictor?
importance(rf.boston)
## 1st col: mean decrease of accuracy in predictions on the out of bag samples when
# ... a given variable is permuted
## 2nd col: measure of the total decrease in node impurity that results from splits
# ... over that variable (measured by the training RSS), averaged over all trees
varImpPlot(rf.boston)


################################################################################

# see: https://www.listendata.com/2014/11/random-forest-with-r.html


