# buildmodels_interest.R
# using RPART package and utils to generate decision trees from data and then 
# extract rules from them.

library(rpart)
library(rpart.plot)
library(rpart.utils)
library(rattle)
library(caret)
library(dplyr)
library(FSelector)

# model_kyphosis
model_kyphosis <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis, minsplit = 10,cp= .01)
rules_kyphosis <- rpart.plot::rpart.rules(model_kyphosis, cover=TRUE, extra=4)
rpart.plot::rpart.rules(model_kyphosis, cover=TRUE, nn=TRUE)
rpart.plot(model_kyphosis)
caret::varImp(model_kyphosis, scale = TRUE)

# model_titanic
data(ptitanic)
model_titanic <- rpart(survived ~ ., data = ptitanic, minsplit = 10,cp = .01)
rules_titanic <- rpart.plot::rpart.rules(model_titanic, cover=TRUE,extra=4)
rpart.plot(model_titanic)
caret::varImp(model_titanic, scale = TRUE)

# model iris
data(iris)
model_iris <- rpart(Species ~., data = iris, method = "class",minsplit = 10,cp = .01)
rules_iris <- rpart.plot::rpart.rules(model_iris, cover=TRUE,extra=4,style="wide")
rpart.plot(model_iris)
caret::varImp(model_iris, scale = TRUE)









