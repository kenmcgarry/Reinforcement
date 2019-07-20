# mdp_interesting.R
# 23/05/2019 - commenced work.
# Embedding a Q-learner (reinforcement learning) for the asssessment of useful and novel
# patterns during datamining. 

setwd("C:/common_laptop/R-files/reinforcement")  # library(help="MDPtoolbox" ) library(help="pomdp")

library(MDPtoolbox)  # probably best package so far for MDP

simulateEnvironment <- function(state, action) {
  # Calculate next state (according to sample grid with wall)
  # Default: remain in a state if action tries to leave grid
  next_state <- state
  if(state == "s0" && action == "down")  next_state <- "s1"
  if(state == "s1" && action == "up")    next_state <- "s0"
  if(state == "s1" && action == "right") next_state <- "s2"
  if(state == "s2" && action == "left")  next_state <- "s1"
  if(state == "s2" && action == "up")    next_state <- "s3"
  if(state == "s3" && action == "down")  next_state <- "s2"
  if(state == "s4" && action == "stay")  next_state <- "s3"
  # Calculate reward
  if(next_state == "s3"){reward <- 10}
    else{reward <- -1}
  return(list(state=next_state, reward=reward))}


Qlearning <-function(n, s_0, s_terminal,epsilon, learning_rate) {
  # Initialize state-action function Q to zero
  Q <- matrix(0, nrow=length(states), ncol=length(actions),dimnames=list(states, actions))
  # Perform n episodes/iterations of Q-learning
  for(i in 1:n) {
    Q <-learnEpisode(s_0, s_terminal,epsilon, learning_rate, Q)}
  return(Q)}

learnEpisode <-function(s_0, s_terminal, epsilon, learning_rate, Q) {
  state <- s_0 # set cursor to initial state
  while(state != s_terminal) {# epsilon-greedy action selection
    if(runif(1) <= epsilon) {
      action <-sample(actions, 1)# pick random action
      }else{action <-which.max(Q[state, ])# pick first best action
      }# get next state and reward from environment
    response <- simulateEnvironment(state, action)# update rule for Q-learning
    Q[state, action] <- Q[state, action] + learning_rate*(response$reward +
                                            max(Q[response$state, ]) - Q[state, action])
    state <- response$state # move to next state
  }
  return(Q)}

epsilon <- 0.1
learning_rate <- 0.1
actions <-c("up", "left", "down", "right","stay")
states <-c("s0", "s1", "s2", "s3","s4")
set.seed(0)

Q <- Qlearning(1000, "s0", "s3", epsilon, learning_rate)

Q
# note: problematic for states with ties
actions[max.col(Q)]




