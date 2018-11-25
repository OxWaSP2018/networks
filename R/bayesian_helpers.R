library(dplyr)
library(tidyr)# Liklihood funcs ----

bin_loglik <- function(w_ij, p, n){
  dbinom(x = w_ij, size = n, prob = p, log = T)
}

prob_alpha_ij <- function(alpha_i, alpha_j) {
  alpha_i / (alpha_i + alpha_j)
}

prob_ij <- function(lam_i, lam_j) {
  (1+exp(-(lam_i-lam_j)))^-1
}

error_loglik <- function(errors, var) {
  sum(dnorm(erros, mean = 0, sd = var, log = T))
}

likli_ij <- function(contest_out, coef, errors, var, attributes) {
  covariates_winner <- attributes[contest_out$winner, c('V1','V2','V3')]
  covariates_loser <- attributes[contest_out$loser, c('V1','V2','V3')]

  error_winner <- errors[as.numeric(contest_out$winner)]
  error_loser <- errors[as.numeric(contest_out$loser)]

  log_a_winner <- as.matrix(covariates_winner) %*% coef + error_winner
  log_a_loser <- as.matrix(covariates_winner) %*% coef + error_loser

  p_ij <- prob_ij(log_a_winner, log_a_loser)
  n_ij <- contest_out$total
  w_ij <- contest_out$wins

  bin_likli <- sum(bin_loglik(w_ij, p = p_ij, n = n_ij))
  bin_likli

}


# Sampling func
sample_coefficients <-  function(contest_out, X, errors) {
  error_i <- errors[as.numeric(contest_out$winner)]
  error_j <- errors[as.numeric(contest_out$loser)]
  err <- error_i - error_j

  bin_mod <- glm(cbind(contest_out$wins, contest_out$losses) ~ -1 +X,
                 offset = err,
                 family = binomial)
  list(coef = coef(bin_mod), var = vcov(bin_mod))

}


beta_step <- function(contest_out, X, errors, attributes) {
  params <- sample_coefficients(contest_out, X, errors)
  mean <- params$coef
  cov <- params$var

  try_again <- T
  while (try_again) {
    prop_b <- mvtnorm::rmvnorm(1, mean = mean, sigma = cov) %>% as.vector
    g_dens <- mvtnorm::dmvnorm(prop_b, mean = mean, sigma = cov, log = T)
    f_dens <- likli_ij(contest_out, prop_b, errors, var, attributes)

    K <- 1
    rej <- f_dens - g_dens - log(K) < log(runif(1))
    try_again <- ! rej
  }

  return(list(accepted_prop = prop_b,
              mu = mean,
              vcov = cov)
  )

}


sample_D <- function(errors) {
  as.numeric(riwish(length(errors), sum(errors^2)))
}



sample_error <- function(contest_out, attributes,coef, prev_errors, D) {


  mode_func <- function(errors){
    likli_ij(contest_out, coef, errors, var, attributes) +
      sum(dnorm(errors, log=T, sd = sqrt(D)))
  }

  err_mode <- optim(rnorm(100), mode_func)
  log_c <- err_mode$value - sum(dnorm(err_mode$par, log=T))

  errors <- rnorm(length(prev_errors))
  den_dens_errors <- sum(dnorm(errors, log=T))
  num_dens_errors <- sum(dnorm(errors, log=T, sd = sqrt(D)))
  bin_lik <- likli_ij(contest_out, coef, errors, var, attributes)

  prop_num <- bin_lik + num_dens_errors


  if (prop_num - den_dens_errors -log_c < log(runif(1))) {
    return(errors)
  } else {
    return(prev_errors)
  }
}


