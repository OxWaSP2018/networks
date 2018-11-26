library(dplyr)
library(tidyr)
# Generate and tidy data ----
data <- gen_data(coef0, peeps, num_contests)
attributes <- data$attributes
victory_matrix <- as.matrix(table(data$contest_results))
victory_df <- data.frame(victory_matrix) %>%
  dplyr::mutate(winner = as.numeric(winner),
                loser = as.numeric(loser))
names(victory_df) <- c("winner", 'loser', 'wins')

totals_matrix <- victory_matrix + t(victory_matrix)
totals_df <- data.frame(totals_matrix) %>%
  dplyr::mutate(winner = as.numeric(winner),
         loser = as.numeric(loser))
totals_df <- totals_df %>% dplyr::filter(winner < loser)
names(totals_df) <- c("winner", 'loser', 'total')

contest_out <- totals_df %>%
  dplyr::left_join(victory_df, by= c("winner", 'loser')) %>%
  dplyr::filter(total > 0) %>%
  dplyr::mutate(losses <- total - wins)





X_i <- data$attributes[as.numeric(contest_out$winner), c('V1','V2','V3')]
X_j <- data$attributes[as.numeric(contest_out$loser), c('V1','V2','V3')]
error_i <- data$error_terms[as.numeric(contest_out$winner)]
error_j <- data$error_terms[as.numeric(contest_out$loser)]
X <- X_i - X_j
X <- as.matrix(X)
err <- error_i - error_j
errors <- data$error_terms

store <- list()
errors <- data$error_terms
coef <- coef0
for (i in 1:1000){
  # Beta step
  beta_out <- beta_step(contest_out, X, errors, attributes)
  coef <- beta_out$accepted_prop
  #coef <- rw_beta_step(contest_out, coef, X, errors, attributes)

  #sample D
  D <- sample_D(errors)

  # Error step
  prev_errors <- errors
  errors <- sample_error(contest_out, attributes,coef, prev_errors, D)

  store[[i]] <- list(err=errors, coef=coef, D=D)
  print(i)
  print(data$coef)

  res <- data.frame(t(sapply(store, function(x) x$coef)))
  length(store)
  print(colSums(res) / length(store))

}

res <- data.frame(t(sapply(store, function(x) x$coef)))
length(store)
colSums(res) / length(store)
data$coef
res[,'sigma'] <- sapply(store, function(x) x$D)



true_coef <- data$coef
names(res) <- c(paste0("Coef: ",1:3), 'Sigma')
p <- res %>%
  tidyr::gather(key = 'Parameter', value='Value')  %>%
  ggplot( aes(x=Parameter, y=Value, color = Parameter) ) +
  geom_boxplot() + ylim(0,7) +
geom_segment(aes(x = 0.5, xend = 1.5, y = true_coef[1], yend = true_coef[1]), colour = 'black') +
  geom_segment(aes(x = 1.5, xend = 2.5, y = true_coef[2], yend = true_coef[2]), colour = 'black') +
  geom_segment(aes(x = 2.5, xend = 3.5, y = true_coef[3], yend = true_coef[3]), colour = 'black') +
  geom_segment(aes(x = 3.5, xend = 4.5, y = 1, yend = 1), colour = 'black')
p
gridExtra::grid.arrange(p,p2, nrow=1)



lik <- sapply(store, function(x)
  likli_ij(contest_out,
           x$coef,
           x$err,
           x$D,attributes = attributes))

lik <- sapply(store, function(x) sum((c(x$coef,x$D)-c(data$coef,1))^2))

lik <- sapply(store, function(x) sum(likli_ij(contest_out,
                                              coef = x$coef,
                                              errors = x$err,
                                              attributes = attributes,
                                              var=x$D)))
min_ind <- which.min(lik)
min_ind <- which.max(lik)
store[[min_ind]]
