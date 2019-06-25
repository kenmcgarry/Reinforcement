# main_interest.R
# https://datascience.stackexchange.com/questions/30644/formulate-a-mdp-for-a-problem-given-below
# https://www.kaggle.com/osbornep/reinforcement-learning-for-meal-planning-in-python/notebook

library(MDPtoolbox)  # probably best for Markov Decision Processes
library(igraph)
library(rpart)
library(rpart.plot)
library(rpart.utils)
library(rattle)
library(caret)
library(tidyverse)

# point to where source code lives.
setwd("C:/common_laptop/R-files/reinforcement")

# define functions
source("functions_interest.R")

# create decision tree models and extract IF..THEN rules from them.
source("buildmodels_interest.R")

# assess rules for interestingness; provide function with rules and class labels
ob_kyphosis <- objective_interest("Kyphsosis",rules_kyphosis,kyphosis$Kyphosis)
