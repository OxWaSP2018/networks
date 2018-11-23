library(BradleyTerry2)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)







nsim <- 1000
results <- list()
br_results <- list()
for (i in seq_len(nsim)) {
  print(i)
  sim_data <- gen_data(coef, attributes, peeps, num_contests)
  form_data <- list('contest' = sim_data$contest_results,
                    'att' = sim_data$attributes)

  sim_m <- BTm(1, form_data$contest$winner, form_data$contest$loser,
               ~ V1[..] + V2[..] + V3[..] + (1|..), data = form_data)

  results[[i]] <- sim_m

  br_results[[i]] <- sim_m <- BTm(1, form_data$contest$winner, form_data$contest$loser,
                                  ~ V1[..] + V2[..] + V3[..] + (1|..), data = form_data, br = T)

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
    geom_boxplot() +
    geom_segment(aes(x = 0.5, xend = 1.5, y = true_coef[1], yend = true_coef[1]), colour = 'black') +
    geom_segment(aes(x = 1.5, xend = 2.5, y = true_coef[2], yend = true_coef[2]), colour = 'black') +
    geom_segment(aes(x = 2.5, xend = 3.5, y = true_coef[3], yend = true_coef[3]), colour = 'black') +
    geom_segment(aes(x = 3.5, xend = 4.5, y = 1, yend = 1), colour = 'black')
  p
}

library(gridExtra)
p1 <- results_to_plot(results)
p2 <- results_to_plot(br_results)
grid.arrange(p1, p2, nrow = 1)


res <- bind_rows(lapply(results, function(x) data.frame(t(coef(x)[1:3]))))
sigs <- sapply(results, function(x) x$sigma)
res[,'sig'] <- sigs

true_coef <- sim_data$coef
names(res) <- c(paste0("Coef: ",1:3), 'Sigma')

pql_res <- res

res <- bind_rows(lapply(br_results, function(x) data.frame(t(coef(x)[1:3]))))
sigs <- sapply(results, function(x) x$sigma)
res[,'sig'] <- sigs

true_coef <- sim_data$coef
names(res) <- c(paste0("Coef: ",1:3), 'Sigma')
br_res <- res

pql_res[,'type'] <- 'PQL'
br_res[,'type'] <- 'BR'

rs <- rbind(pql_res, br_res)

p <- rs %>%
  tidyr::gather(key = 'Parameter', value='Value', setdiff(names(.), "type"))  %>%
  ggplot( aes(x=Parameter, y=Value, group = interaction(type,Parameter)) ) +
  geom_boxplot() +
  geom_segment(aes(x = 0.5, xend = 1.5, y = true_coef[1], yend = true_coef[1]), colour = 'black') +
  geom_segment(aes(x = 1.5, xend = 2.5, y = true_coef[2], yend = true_coef[2]), colour = 'black') +
  geom_segment(aes(x = 2.5, xend = 3.5, y = true_coef[3], yend = true_coef[3]), colour = 'black') +
  geom_segment(aes(x = 3.5, xend = 4.5, y = 1, yend = 1), colour = 'black')
p

rs
results[[1]]$sigma
results[[2]]$sigma

gen_data(coef, attributes, peeps, num_contests)$coef

