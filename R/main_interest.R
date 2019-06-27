# main_interest.R
# https://datascience.stackexchange.com/questions/30644/formulate-a-mdp-for-a-problem-given-below
# https://www.kaggle.com/osbornep/reinforcement-learning-for-meal-planning-in-python/notebook

#library(MDPtoolbox)  # probably best for Markov Decision Processes
#library(igraph)
library(rpart)
library(rpart.plot)
library(rpart.utils)
library(rattle)
library(caret)
library(infotheo)
library(tidyverse)
library(dplyr)
library(FSelector)


# point to where source code lives.
setwd("C:/common_laptop/R-files/reinforcement")

# define functions
source("functions_interest.R")

# create decision tree models and extract IF..THEN rules.
source("buildmodels_interest.R")

# Assess rules for interestingness; provide function with:
# "name", ruleset, rawdata, class labels, penalty class (if there is one,set to zero if not)

ob_kyphosis <- collect_objective_interest(name="Kyphsosis",
                                  ruleset=rules_kyphosis,
                                  dataset=kyphosis,
                                  classes=kyphosis$Kyphosis,
                                  penaltyclass="absent",
                                  IG=information.gain(Kyphosis ~ ., data=kyphosis, unit="log2"))


ob_titanic <-  collect_objective_interest(name="Titanic",
                                 ruleset=rules_titanic,
                                 dataset=ptitanic,
                                 classes=ptitanic$survived,
                                 penaltyclass = "died",
                                 IG=information.gain(survived ~ ., data=ptitanic, unit="log2"))

ob_iris   <-   collect_objective_interest(name="Iris",
                                 ruleset=rules_iris,
                                 dataset=iris,
                                 classes=iris$Species,
                                 penaltyclass = 0,
                                 IG=information.gain(Species ~ ., data=iris, unit="log2"))




