# calcobj__interest.R
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

ob_pima   <-   collect_objective_interest(name="Pima",
                                          ruleset=rules_pima,
                                          dataset=pima,
                                          classes=pima$Class,
                                          penaltyclass = "diabetes",
                                          IG=information.gain(Class ~ ., data=pima, unit="log2"))


