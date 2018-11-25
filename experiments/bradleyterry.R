library(BradleyTerry2)
library(dplyr)
data("flatlizards", package = "BradleyTerry2")
lizModel <- BTm(1, winner, loser, ~ SVL[..] + (1|..), data = flatlizards)

lizModel2 <- BTm(1, winner, loser, ~ throat.PC1[..] + throat.PC3[..] + head.length[..] + SVL[..] + (1|..),
                 data = flatlizards)


sim_data <- readRDS('generate_data/simulated_data.RDS')

form_data <- list('contest' = sim_data$contest_results,
                  'att' = sim_data$attributes)

sim_m <- BTm(1, form_data$contest$winner, form_data$contest$loser,
             ~ V1[..] + V2[..] + V3[..] + (1|..), data = form_data)



coef(sim_m)[1:3]
sim_data$coef
