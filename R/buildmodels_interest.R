# buildmodels_interest.R
# using RPART package and utils to generate decision trees and then extract rules from them.

library(rpart)
library(rpart.plot)
library(rpart.utils)
library(rattle)
library(caret)
library(dplyr)

# model_kyphosis
model_kyphosis <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis, minsplit = 10,cp= .01)
rules_kyphosis <- rpart.plot::rpart.rules(model_kyphosis, cover=TRUE, nn=TRUE)
rpart.plot::rpart.rules(model_kyphosis, cover=TRUE, nn=TRUE)
rpart.plot(model_kyphosis)
caret::varImp(model_kyphosis, scale = TRUE)


# model_titanic
data(ptitanic)
model_titanic <- rpart(survived ~ ., data = ptitanic, minsplit = 10,cp = .01)
rules_titanic <- rpart.plot::rpart.rules(model_titanic, cover=TRUE, nn=TRUE)
rpart.plot(model_titanic)
caret::varImp(model_titanic, scale = TRUE)












