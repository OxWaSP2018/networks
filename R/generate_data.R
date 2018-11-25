
library(dplyr)
num_covariates <- 3
coef <- rnorm(num_covariates, mean = 1:num_covariates, sd = 1)

peeps <- 100
num_contests <- 1000
sig <- 1
cov_means <- c(0,2,1)
cov_sd <-1

attributes <- lapply(seq_len(peeps), function(x) rnorm(num_covariates, mean = cov_means, sd = cov_sd))
names(attributes) <- seq_len(peeps)


gen_data <- function(coef, attributes, peeps, num_contests) {


  errors <- rnorm(peeps, sd = sig)
  log_alpha <- sapply( seq_len(peeps), function(x) sum(coef * attributes[[x]]) + errors[x])
  alphas <- exp(log_alpha)

  player_1 <- sample(seq_len(peeps), size = num_contests, replace = T)
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




  abilities <- as.data.frame(t(data.frame(attributes)) ) %>%
    mutate(player = seq_len(peeps))

  contest_results <- data.frame(player_1, player_2, winner) %>%
    dplyr::mutate(
      loser = ifelse(winner == player_1, player_2, player_1),
      winner = winner,
      loser = loser
    ) %>%
    dplyr::select(winner, loser)

  contest_results$winner <- factor(contest_results$winner,
                                   levels = seq_len(peeps),
                                   labels = seq_len(peeps))
  contest_results$loser <- factor(contest_results$loser,
                                  levels = seq_len(peeps),
                                  labels = seq_len(peeps))




  simulated_data <- list('contest_results' = contest_results,
                         'attributes' = abilities,
                         'error_terms' = errors,
                         'coef'=coef)

  simulated_data


}




pairwise_diff <- function(cols){
  length_cols <- length(cols)
  length_along <- seq_along(cols)
  cited <- factor(rep(length_along, length_cols))
  citing <- factor(rep(length_along, rep(length_cols, length_cols)))

  X <- model.matrix(~ -1 + cited) - model.matrix(~ -1 + citing)
  colnames(X) <- cols
  zero_ind <- rowSums(abs(X)) ==0
  X[!zero_ind,]
}











