# functions_interest.R
# various functions to implement analysis of rule interestingness using
# a variety of measures (Frietas,1999 and McGarry,2005) also see the 
# Hilderman & Hamilton book, (Kluwer, 2001).
# 

# Objective measures: 
# 1. disjunct size  - those rules with coverage <= 5 
# 2. class imbalance
# 3. misclassification costs
# 4. attribute costs
# 5. asymmetry
# 6. complexity
objective_interest <- function(name,ruleset,classes){
  SMALL <- 5  # min coverage
  x <- 0
  
  rulesummary <- data.frame(name=name,smalldis=0,imbalance=0, miscosts=0, 
                            attcosts=0,asymmetry=0, complexity=0, 
                            stringsAsFactors=FALSE) 
  
  for(i in 1:nrow(ruleset)){
    tempr <- as.numeric(sub("%","",ruleset[i,19])) #remove percent sign and convert to number
    if(tempr <= SMALL){
      x <- x+1}}
  rulesummary$smalldis <- x
  
  b <- table(classes)
  c <- length(as.vector(b))
  a <- sum(b)
  if(min(b) < (a/c)){rulesummary$imbalance <- 1}
  
  rulesummary$miscosts <- 0
  rulesummary$attcosts <- 0
  rulesummary$asymmetry <- 0
  rulesummary$complexity <- 0
  
  return(rulesummary)
}


# Subjective domain knowledge, allows for the detection of surprising patterns i.e.
# rules that are contradictory or unexpected and hence interesting to the users.
#
# input: ruleset generated from decision tree and domain rules from expert
# output: subjective ratings for each rule
subjective_interest <- function(ruleset, domain){

    
  return(sub_moi)
}



