
library(glmmsr)
library(BradleyTerry2)


#### glmmsr example ####
## BTm()
result <- rep(1, nrow(flatlizards$contests))
lizards_mod_BTm <- BTm(result, winner, loser, ~ SVL[..] + (1|..),
                       family = binomial(link = "probit"), data = flatlizards)
summary(lizards_mod_BTm)

## Laplace
flatlizards_glmmsr <- c(list(result = result,
                             winner = flatlizards$contests$winner,
                             loser = flatlizards$contests$loser),
                        flatlizards$predictors)
lizards_mod_Laplace <- glmm(result ~ 0 + Sub(ability[winner] - ability[loser]),
                            ability[liz] ~ 0 + SVL[liz] + (1 | liz),
                            data = flatlizards_glmmsr, family = binomial(link = "probit"),
                            method = "Laplace")
summary(lizards_mod_Laplace)

## SR_2
lizards_mod_SR_2 <- glmm(result ~ 0 + Sub(ability[winner] - ability[loser]),
                         ability[liz] ~ 0 + SVL[liz] + (1 | liz),
                         data = flatlizards_glmmsr, family = binomial(link = "probit"),
                         method = "SR", control = list(nSL = 2),
                         prev_fit = lizards_mod_Laplace)
summary(lizards_mod_SR_2)

## SR_3
lizards_mod_SR_3 <- glmm(result ~ 0 + Sub(ability[winner] - ability[loser]),
                         ability[liz] ~ 0 + SVL[liz] + (1 | liz),
                         data = flatlizards_glmmsr, family = binomial(link = "probit"),
                         method = "SR", control = list(nSL = 3),
                         prev_fit = lizards_mod_SR_2)
summary(lizards_mod_SR_3)

## loglikelihood
modfr_lizards <-  find_modfr_glmm(result ~ 0 + Sub(ability[winner] - ability[loser]),
                                  ability[liz] ~ 0 + SVL[liz] + (1 | liz),
                                  data = flatlizards_glmmsr,
                                  family = binomial(link = "probit"))
theta_poss <- cbind(seq(0, 3, by = 0.25), 0.3)
l_SR_theta_poss <- list()
for(i in 0:4) {
  lfun_SR_i <- find_lfun_glmm(modfr_lizards, method = "SR", control = list(nSL = i))
  l_SR_theta_poss[[i + 1]] <- apply(theta_poss, 1, lfun_SR_i)
}
plot(theta_poss[,1], l_SR_theta_poss[[5]], type = "l", col = 5,
     xlab = "sigma", ylab = "log-likelihood")
for(i in 1:4) {
  lines(theta_poss[,1], l_SR_theta_poss[[i]], col = i)
}
legend("bottomright", legend = paste("nSL =", 0:4), col = 1:5, lty = 1, bty = "n")


###########################################################################


#### trail with lizards data ####
result <- rep(1, nrow(flatlizards$contests))
flatlizards_glmmsr <- c(list(result = result,
                             winner = flatlizards$contests$winner,
                             loser = flatlizards$contests$loser),
                        flatlizards$predictors)

mod_SR_2 <- glmm(result ~ 0 + Sub(ability[winner] - ability[loser]),
                         ability[liz] ~ 0 + SVL[liz] + (1 | liz),
                         data = flatlizards_glmmsr, family = binomial,
                         method = "SR", control = list(nSL = 2))
summary(mod_SR_2)

mod_SR_3 <- glmm(result ~ 0 + Sub(ability[winner] - ability[loser]),
                         ability[liz] ~ 0 + SVL[liz] + (1 | liz),
                         data = flatlizards_glmmsr, family = binomial,
                         method = "SR", control = list(nSL = 3),
                         prev_fit = mod_SR_2)
summary(mod_SR_3)
# Warning messages:
#   1: In optimize_glmm(lfun, p_beta = p_beta, p_theta = p_theta, prev_fit = prev_fit,  :
#                         May have problem in convergence: Sigma has a very small eigenvalue
#   2: The approximate MLE might not be finite

###########################################################################


#### data simulation ####
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

###########################################################################


#### simulation study -- all three variables ####
result <- rep(1, nrow(sim_data$contest_results))
form_data_sr <- c(list(result = result,
                    winner = sim_data$contest_results$winner,
                    loser = sim_data$contest_results$loser,
                    player = as.factor(sim_data$attributes$player),
                    V1 = sim_data$attributes$V1,
                    V2 = sim_data$attributes$V2,
                    V3 = sim_data$attributes$V3))

sim_m_sr <- glmm(result ~ 0 + Sub(ability[winner] - ability[loser]),
                 ability[liz] ~ 0 + V1[liz] + V2[liz] + V3[liz] + (1 | liz),
                 data = form_data_sr, family = binomial,
                 method = "SR", control = list(nSL = 1))
summary(sim_m_sr)

sim_m_sr$estim[2:4]
sim_data$coef

nsim <- 20
results_sr <- list()
for (i in seq_len(nsim)) {
  print(i)
  sim_data <- gen_data(coef, attributes, peeps, num_contests)
  form_data_sr <- c(list(result = result,
                         winner = sim_data$contest_results$winner,
                         loser = sim_data$contest_results$loser,
                         player = as.factor(sim_data$attributes$player),
                         V1 = sim_data$attributes$V1,
                         V2 = sim_data$attributes$V2,
                         V3 = sim_data$attributes$V3))

  sim_m_sr <- glmm(result ~ 0 + Sub(ability[winner] - ability[loser]),
                   ability[liz] ~ 0 + V1[liz] + V2[liz] + V3[liz] + (1 | liz),
                   data = form_data_sr, family = binomial,
                   method = "SR", control = list(nSL = 1))

  results_sr[[i]] <- sim_m_sr

}

coef



results <- results_sr

results_to_plot <- function(results) {
  res <- bind_rows(lapply(results, function(x) data.frame(t(x$estim[1:4]))))
  #sigs <- sapply(results, function(x) x$sigma)
  #res[,'sig'] <- sigs

  true_coef <- sim_data$coef
  names(res) <- c('Sigma', paste0("Coef: ",1:3))
  p <- res %>%
    tidyr::gather(key = 'Parameter', value='Value')  %>%
    ggplot( aes(x=Parameter, y=Value, color = Parameter) ) +
    geom_boxplot() + ylim(-1,5) +
    geom_segment(aes(x = 0.5, xend = 1.5, y = true_coef[1], yend = true_coef[1]), colour = 'black') +
    geom_segment(aes(x = 1.5, xend = 2.5, y = true_coef[2], yend = true_coef[2]), colour = 'black') +
    geom_segment(aes(x = 2.5, xend = 3.5, y = true_coef[3], yend = true_coef[3]), colour = 'black') +
    geom_segment(aes(x = 3.5, xend = 4.5, y = 1, yend = 1), colour = 'black')
  p
}

true_coef
coef
plot_sr <- results_to_plot(results_sr) + theme_gray(base_size = 14)


nsim <- 20
results_bt <- list()

for (i in seq_len(nsim)) {
  print(i)
  sim_data <- gen_data(coef, attributes, peeps, num_contests)
  form_data_bt <- list('contest' = sim_data$contest_results,
                    'att' = sim_data$attributes)

  sim_m_bt <- BTm(1, form_data_bt$contest$winner, form_data_bt$contest$loser,
               ~ V1[..] + V2[..] + V3[..] + (1|..), data = form_data_bt)

  results_bt[[i]] <- sim_m_bt


}

results_to_plot <- function(results) {
  res <- bind_rows(lapply(results, function(x) data.frame(t(coef(x)[1:3]))))
  sigs <- sapply(results, function(x) x$sigma)
  res[,'sig'] <- sigs

  true_coef <- sim_data$coef
  names(res) <- c(paste0("Coef: ",1:3), 'Sigma')
  p <- res %>%
    tidyr::gather(key = 'Parameter', value='Value')  %>%
    ggplot( aes(x=Parameter, y=Value, color = Parameter) ) +
    geom_boxplot() + ylim(-1, 5) +
  geom_segment(aes(x = 0.5, xend = 1.5, y = true_coef[1], yend = true_coef[1]), colour = 'black') +
    geom_segment(aes(x = 1.5, xend = 2.5, y = true_coef[2], yend = true_coef[2]), colour = 'black') +
    geom_segment(aes(x = 2.5, xend = 3.5, y = true_coef[3], yend = true_coef[3]), colour = 'black') +
    geom_segment(aes(x = 3.5, xend = 4.5, y = 1, yend = 1), colour = 'black')
  p
}

plot_bt <- results_to_plot(results_bt) + theme_gray(base_size = 14)

require(gridExtra)
grid.arrange(plot_sr, plot_bt, ncol=2)

ggsave("sr_bt_plot.pdf", arrangeGrob(plot_sr, plot_bt, ncol=2))

###########################################################################


#### simulation study -- 100 contests, 100 players ####
num_covariates <- 3
coef <- rnorm(num_covariates, mean = 1:num_covariates, sd = 1)

peeps <- 100
num_contests <- 100
sig <- 1
cov_means <- c(0,2,1)
cov_sd <-1

attributes <- lapply(seq_len(peeps), function(x) rnorm(num_covariates, mean = cov_means, sd = cov_sd))
names(attributes) <- seq_len(peeps)

nsim <- 1
results_sr <- list()
for (i in seq_len(nsim)) {
  print(i)
  sim_data <- gen_data(coef, attributes, peeps, num_contests)
  result <- rep(1, nrow(sim_data$contest_results))
  form_data_sr <- c(list(result = result,
                         winner = sim_data$contest_results$winner,
                         loser = sim_data$contest_results$loser,
                         player = as.factor(sim_data$attributes$player),
                         V1 = sim_data$attributes$V1,
                         V2 = sim_data$attributes$V2,
                         V3 = sim_data$attributes$V3))

  sim_m_sr <- glmm(result ~ 0 + Sub(ability[winner] - ability[loser]),
                   ability[liz] ~ 0 + V1[liz] + V2[liz] + V3[liz] + (1 | liz),
                   data = form_data_sr, family = binomial,
                   method = "SR", control = list(nSL = 3))

  results_sr[[i]] <- sim_m_sr

}


results_to_plot(results_sr)



# Error: The sequential reduction approximation with 2 sparse levels
# is too difficult to compute in this case.
# Consider reducing nSL, or using a different approximation method.


