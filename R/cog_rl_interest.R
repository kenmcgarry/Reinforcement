# cog_rl_interest.R
# 23/05/2019 - commenced work.
# Embedding a reinforcement learner (RL) for the asssessment of useful and novel patterns during datamining. 
# https://markusdumke.github.io/reinforcelearn/articles/environments.html

setwd("C:/common_laptop/R-files/reinforcement")  # library(help<-"MDPtoolbox" ) library(help<-"pomdp")

library(reinforcelearn)  # 

# 1.  Select a data mining algorithm and data sets for RL [ENVIRONMENT]
# 2.  Define cognitive heuristic measures, in a bayesian framework [AGENT]
# 3.a Identify interestingness measures and parameters from literature [ENVIRONMENT]
# 3.b Describe these in terms of the environment [ENVIRONMENT]
# 4.  Define policy(s) for RL actions [AGENT]

# Using Markus Dumke reinforcelearn package examples

policy <- makePolicy("epsilon.greedy", epsilon = 0.1)
alg <- makeAlgorithm("qlearning", lambda = 0.8, traces = "accumulate")
agent <- makeAgent("softmax", "table", alg)

# mountain car problem
step <- function(self, action) {
  position <- self$state[1]
  velocity <- self$state[2]
  velocity <- (action - 1L) * 0.001 + cos(3 * position) * (-0.0025)
  velocity <- min(max(velocity, -0.07), 0.07)
  position <- position + velocity
  if (position < -1.2) {
    position <- -1.2
    velocity <- 0
  }
  state <- matrix(c(position, velocity), ncol = 2)
  reward = -1
  if (position >= 0.5) {
    done <- TRUE
    reward <- 0
  } else {
    done <- FALSE
  }
  list(state, reward, done)
}

reset <- function(self) {
  position <- runif(1, -0.6, -0.4)
  velocity <- 0
  state <- matrix(c(position, velocity), ncol = 2)
  state
}

env <- makeEnvironment("custom", step = step, reset = reset)
env$reset()

# Create qlearning agent with softmax policy and tabular value function.
policy <- makePolicy("epsilon.greedy", epsilon = 0.1)
values <- makeValueFunction("table", n.states = env$n.states, n.actions = env$n.actions)
algorithm <- makeAlgorithm("qlearning", lambda = 0.8, traces = "accumulate")
agent <- makeAgent(policy, values, algorithm)

interact(env, agent, n.episodes = 5)

env$visualize()



