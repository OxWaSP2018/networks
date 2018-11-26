library(dplyr)
library(tidyr)
library(BradleyTerry2)

# Generate and tidy data ----



victory_matrix <- as.matrix(table(flatlizards$contests))
victory_df <- data.frame(victory_matrix) %>%
  dplyr::mutate(winner = as.numeric(winner),
                loser = as.numeric(loser))
names(victory_df) <- c("winner", 'loser', 'wins')
liz_key <- rownames(flatlizards$predictors)

totals_matrix <- victory_matrix + t(victory_matrix)
winner_id <- match(liz_key, rownames(totals_matrix))
loser_id <- match(liz_key, colnames(totals_matrix))
rownames(totals_matrix) <- winner_id
colnames(totals_matrix) <- loser_id
totals_df <- data.frame(totals_matrix)
names(totals_df) <- c("winner", 'loser', 'total')
totals_df <- totals_df %>%
  mutate(winner = as.numeric(winner),
          loser = as.numeric(loser))
totals_df <- totals_df %>% dplyr::filter(winner < loser)


contest_out <- totals_df %>%
  dplyr::left_join(victory_df, by= c("winner", 'loser')) %>%
  dplyr::filter(total > 0) %>%
  dplyr::mutate(losses <- total - wins)


attributes <- flatlizards$predictors

winner_id <- match(liz_key[contest_out$winner], rownames(attributes))
loser_id <- match(liz_key[contest_out$loser], rownames(attributes))
X_i <- attributes[winner_id,]
X_j <- attributes[loser_id,]

X <- X_i - X_j
X <- as.matrix(X)
err <- error_i - error_j
errors <- data$error_terms

store <- list()
for (i in 1:1000){
  # Beta step
  beta_out <- beta_step(contest_out, X, errors, attributes)

  coef <- beta_out$accepted_prop
  beta_mean <- beta_out$mu
  beta_var <- beta_out$vcov

  #sample D
  D <- sample_D(errors)

  # Error step
  prev_errors <- errors
  errors <- sample_error(contest_out, attributes,coef, prev_errors, D)

  store[[i]] <- list(err=errors, coef=coef, D=D)
  print(i)

}

res <- data.frame(t(sapply(store, function(x) x$coef)))
length(store)
colSums(res) / length(store)
store[[510]]
res[,'sigma'] <- sapply(store, function(x) x$D)

lapply

true_coef <- data$coef
names(res) <- c(paste0("Coef: ",1:3), 'Sigma')
p <- res %>%
  tidyr::gather(key = 'Parameter', value='Value')  %>%
  ggplot( aes(x=Parameter, y=Value, color = Parameter) ) +
  geom_boxplot() +
geom_segment(aes(x = 0.5, xend = 1.5, y = true_coef[1], yend = true_coef[1]), colour = 'black') +
  geom_segment(aes(x = 1.5, xend = 2.5, y = true_coef[2], yend = true_coef[2]), colour = 'black') +
  geom_segment(aes(x = 2.5, xend = 3.5, y = true_coef[3], yend = true_coef[3]), colour = 'black') +
  geom_segment(aes(x = 3.5, xend = 4.5, y = 1, yend = 1), colour = 'black')
p




