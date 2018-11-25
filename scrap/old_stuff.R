for (i in unique(data$contest_results$winner)) {
  df <- data$contest_results %>%
    dplyr::filter(winner == i)
  for (j in unique(df$loser)) {
    count <- count + 1
    w_ij <- dim(df %>%
                  dplyr::filter(loser == j))[1]
    contest_out[count, 'i'] <- i
    contest_out[count, 'j'] <- j
    contest_out[count, 'w'] <- w_ij
  }
}


victory_matrix[victory_matrix$winner]
contest_out <- data$contest_results %>%
  (function(data) data.frame(as.matrix(table(data)))) %>%
  dplyr::filter(Freq > 0)
names(contest_out) <- c('i','j', 'w')
contest_out <- contest_out %>%
  left_join(contest_out, by = c('i'='j','j'='i'))
contest_out$w.y[is.na(contest_out$w.y)] <- 0
