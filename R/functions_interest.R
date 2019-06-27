# functions_interest.R
# Various functions to implement analysis of rule interestingness using
# several measures (Frietas,1999 and McGarry,2005) also see the 
# Hilderman & Hamilton book, (Kluwer, 2001).
# 

# Objective measures: 
# 1. disjunct size  - those rules with coverage <= 5 
# 2. class imbalance - any classes under-represented?
# 3. misclassification costs
# 4. attribute information
# 5. asymmetry
# 6. complexity

# 1. for each rule: does it have a small disjunct i.e. coverage?
obj_disjunct <- function(ruleset){
  
  nrules <- nrow(ruleset)  # how many rules?
  nante <- ncol(ruleset)  # how many columns?
  disjun <- seq(0,0,length.out=nrules)  # create space for them
  SMALL <- 5  # min coverage
  x <- 0

  for(i in 1:nrow(ruleset)){
    tempr <- as.numeric(sub("%","",ruleset[i,nante])) #remove percent sign and convert to number
    if(tempr <= SMALL){
      disjun[i] <- 1}}
  
  ruleset <- cbind(ruleset,disjun)
  return(ruleset)
}

# 2. is the dataset imbalanced i.e. more records for certain classes than others?
obj_imbalance <- function(ruleset,classes){
  nrules <- nrow(ruleset)  # how many rules?
  nante <- ncol(ruleset)  # how many columns?
  nrecords <- length(classes)
  imbalance <- seq(0,0,length.out=nrules)  # create space for them (all set to zero or one).
  b <- table(classes)
  c <- length(as.vector(b))  # how many classes?
  a <- sum(b)
  if(min(b) < (a/c)){imbalance <- 1}
  
  ruleset <- cbind(ruleset,imbalance)
  return(ruleset)
}


# 3. misclassification costs, tag a rule if it has a disease class or penalty for getting it wrong
# we dont like false negatives. Class labels are ALWAYS the first column in RPART ruleset.
obj_misclasscosts <- function(ruleset,penaltyclass){
  nrules <- nrow(ruleset)  # how many rules?
  nante <- ncol(ruleset)  # how many columns?
  miscost <- seq(0,0,length.out=nrules)  # create space for them.
  
  if(penaltyclass==0){ # check if we dont have a penalty class
  for(i in 1:nrow(ruleset)){
    if((ruleset[i,1] == penaltyclass)) # string match and if true set to 1
      miscost[i] <- 1
    }}

  ruleset <- cbind(ruleset,miscost)
  
  return(ruleset)
}

# 4. attribute information. Kullback-Liebler divergence is use to calculate the information
# bearing value of each attribute in a rule and average it over the rule.
obj_attribinfo <- function(ruleset,dataset,classlabels,IG){
  nrules <- nrow(ruleset)  # how many rules?
  nante <- ncol(ruleset)  # how many columns?
  attinfo <- seq(0,0,length.out=nrules)  # create space for them.
  tempdata <- rep(0.001,length.out=nrules)
  k <- ncol(dataset)-1  # how many attributes, minus class label?
  cat("\nwe have ",k," attributes")
  
  print(IG)
  IGnames <- rownames(IG); 
  cat("\n variables",IGnames)
  antestring <- unite_(ruleset, "searchstring", colnames(ruleset),remove=TRUE,sep=" ")
  print(antestring)
  
  for(i in 1:nrow(antestring)){  # for each rule
    #for(j in 1:length(IGnames)){  # for every antecedent in rule
      #index <- grep(IGnames[j],antestring[i,1])  # does this anetecedent exist in this rule?
      index <- match(IGnames,antestring[i,1])
      cat("\nindex",index)
      if(length(index)>0){                     # if not zero then it does indeed
        #cat("\nante=",length(index))
        tempdata[i] <- IG[i,1]}# * length(index)}
    #}
    cat("\ninfo value ",tempdata[i])
    #cat("\nsum ",1/(sum(tempdata)/length(which(tempdata !=0))))
    attinfo[i] <- 1/((sum(tempdata[i])/length(index)))
  }
  ruleset <- cbind(ruleset,attinfo)  
  return(antestring)
}




# perform the five objective interestingness measures (calculations) on the ruleset. It will
# add various columns to the rule data structure pertaining to the measures.
collect_objective_interest <- function(name,ruleset,dataset,classes,penaltyclass,IG){
  
  cat("\nCalculating interestingness measures for ", name)
  ruleset <- obj_disjunct(ruleset)
  ruleset <- obj_imbalance(ruleset,classes)    
  ruleset <- obj_misclasscosts(ruleset,penaltyclass)
  ruleset <- obj_attribinfo(ruleset,dataset,classes,IG)
  
  return(ruleset)
}


# Subjective domain knowledge, allows for the detection of surprising patterns i.e.
# rules that are contradictory or unexpected and hence interesting to the users.
#
# input: ruleset generated from decision tree and domain rules from expert
# output: subjective ratings for each rule
subjective_interest <- function(ruleset, domain){

    
  return(sub_moi)
}



list.rules.rpart <- function(model)
{
  if (!inherits(model, "rpart")) stop("Not a legitimate rpart tree")
  #
  # Get some information.
  #
  frm     <- model$frame
  names   <- row.names(frm)
  ylevels <- attr(model, "ylevels")
  ds.size <- model$frame[1,]$n
  #
  # Print each leaf node as a rule.
  #
  for (i in 1:nrow(frm))
  {
    if (frm[i,1] == "<leaf>")
    {
      # The following [,5] is hardwired - needs work!
      cat("\n")
      cat(sprintf(" Rule number: %s ", names[i]))
      cat(sprintf("[yval=%s cover=%d (%.0f%%) prob=%0.2f]\n",
                  ylevels[frm[i,]$yval], frm[i,]$n,
                  round(100*frm[i,]$n/ds.size), frm[i,]$yval2[,5]))
      pth <- path.rpart(model, nodes=as.numeric(names[i]), print.it=FALSE)
      cat(sprintf("   %s\n", unlist(pth)[-1]), sep="")
    }
  }
}

# https://cran.r-project.org/web/packages/data.tree/vignettes/applications.html
informationgain <- function(tble) {
  entropyBefore <- entropy(colSums(tble))
  s <- rowSums(tble)
  entropyAfter <- sum (s / sum(s) * apply(tble, MARGIN = 1, FUN = entropy ))
  infogain <- entropyBefore - entropyAfter
  return (infogain)
}

entropy <- function(vls) {
  res <- vls/sum(vls) * log2(vls/sum(vls))
  res[vls == 0] <- 0
  -sum(res)
}

# test matrix for informationgain & entropy
#B = matrix( c(2, 4, 3, 1, 5, 7), nrow=3,ncol=2)
#informationgain(B)




