peeps <- 100

sig <- 1
coef <- rnorm(3, mean = 1:3, sd = 3)
errors <- rnorm(peeps, sd = sig)
attributes <- lapply(seq_len(peeps), function(x) rnorm(3, mean = c(0,2,1), sd = 1))
names(attributes) <- seq_len(peeps)
log_alpha <- sapply( seq_len(peeps), function(x) sum(coef * attributes[[x]]) + errors[x])
alphas <- exp(log_alpha)


player_1 <- sample(seq_len(peeps), size = 200, replace = T)
player_2 <- sapply(player_1, function(x) sample(seq_len(peeps)[-x], size=1))

winner <- c()
for (i in seq_along(player_1)) {
  p1 <- player_1[i]
  p2 <- player_2[i]
  
  a1 <- alphas[p1]
  a2 <- alphas[p2]
  
  prob_1_wins <- a1 / (a1+a2)
  
  if (runif(1) < prob_1_wins){
    winner <- c(winner, p1)
  } else{
    winner <- c(winner, p2)
  }
  
}

contest_results <- data.frame(player_1, player_2, winner)

library(dplyr)
abilities <- as.data.frame(t(data.frame(attributes)) )
abilities$player <- seq_len(peeps) 

simulated_data <- list('contest_results' = contest_results, 'attributes' = abilities, 'error_terms' = errors)

fp <- '/homes/thornton/Documents/OxWaSP_Modules/Network_Analysis/simulated_data.RDS'
saveRDS(simulated_data, fp)
readRDS(fp)
