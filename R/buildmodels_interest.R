# buildmodels_interest.R
# using RPART package and utils to generate decision trees and then extract rules from them.

library(rpart)
library(rpart.plot)
library(rpart.utils)
library(rattle)
library(caret)
library(dplyr)

#model
fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)

#dataframe having leaf node's rule and subrule combination
rule_df <- rpart.rules.table(fit) %>%
  filter(Leaf==TRUE) %>%
  group_by(Rule) %>%
  summarise(Subrules = paste(Subrule, collapse=","))

#final dataframe
df <- kyphosis %>%
  mutate(Rule = row.names(fit$frame)[fit$where]) %>%
  left_join(rule_df, by="Rule")
head(df)

#subrule table
rpart.subrules.table(fit)

rules <- rpart.plot::rpart.rules(fit, cover=TRUE, nn=TRUE)
rpart.rules(fit, cover=TRUE, nn=TRUE)
asRules(fit)
varImp(fit, scale = FALSE)



