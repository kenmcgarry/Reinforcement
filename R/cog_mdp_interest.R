# mdp_interesting.R
# 23/05/2019 - commenced work.
# Embedding an MDP (reinforcement learner) for the asssessment of useful and novel
# patterns during datamining. 

setwd("C:/common_laptop/R-files/reinforcement")  # library(help="MDPtoolbox" ) library(help="pomdp")

library(MDPtoolbox)  # probably best package so far for MDP

# 1.  Select a data mining algorithm and data sets for RL [ENVIRONMENT]
# 2.  Define cognitive heuristic measures, in a bayesian framework [AGENT]
# 3.a Identify interestingness measures and parameters from literature [ENVIRONMENT]
# 3.b Describe these in terms of the environment
# 4.  Define policy(s) for RL actions [AGENT]

# With a non-sparse matrix
P <- array(0, c(2,2,2))
P[,,1] <- matrix(c(0.5, 0.5, 0.8, 0.2), 2, 2, byrow=TRUE)
P[,,2] <- matrix(c(0, 1, 0.1, 0.9), 2, 2, byrow=TRUE)
R <- matrix(c(5, 10, -1, 2), 2, 2, byrow=TRUE)
results <- mdp_Q_learning(P=P, R=R, discount=0.9)


# 1. Defining the Set of Actions - Left, Right, Up and Down for 2*2 matrix
# Remember! This will be a probability matrix, so we will use the matrix() function such that the sum of probabilities in each row is 1

# Up Action
up=matrix(c( 1, 0, 0, 0,
             0.7, 0.2, 0.1, 0,
             0, 0.1, 0.2, 0.7,
             0, 0, 0, 1),
          nrow=4,ncol=4,byrow=TRUE)

# Down Action
down=matrix(c(0.3, 0.7, 0, 0,
              0, 0.9, 0.1, 0,
              0, 0.1, 0.9, 0,
              0, 0, 0.7, 0.3),
            nrow=4,ncol=4,byrow=TRUE)

# Left Action
left=matrix(c( 0.9, 0.1, 0, 0,
               0.1, 0.9, 0, 0,
               0, 0.7, 0.2, 0.1,
               0, 0, 0.1, 0.9),
            nrow=4,ncol=4,byrow=TRUE)

# Right Action
right=matrix(c( 0.9, 0.1, 0, 0,
                0.1, 0.2, 0.7, 0,
                0, 0, 0.9, 0.1,
                0, 0, 0.1, 0.9),
             nrow=4,ncol=4,byrow=TRUE)

# Combined Actions matrix
Actions=list(up=up, down=down, left=left, right=right)

# 2. Defining the rewards and penalties
Rewards=matrix(c( -1, -1, -1, -1,
                  -1, -1, -1, -1,
                  -1, -1, -1, -1,
                  10, 10, 10, 10),
               nrow=4,ncol=4,byrow=TRUE)

# 3. Solving the navigation
solver=mdp_policy_iteration(P=Actions, R=Rewards, discount = 0.1)

# 4. Getting the policy
solver$policy #2 4 1 1
names(Actions)[solver$policy] #"down"  "right" "up" "up"

# 5. Getting the Values at each step. These values can be different in each run
solver$V #58.25663  69.09102  83.19292 100.00000

# 6. Additional information: Number of iterations
solver$iter #2

# 7. Additional information: Time taken. This time can be different in each run
solver$time #Time difference of 0.009523869 secs
