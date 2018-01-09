library(qcapower)
library(devtools)

qp_sina_data <- qcapower(cases = 10, null_hypo = 0.8, alt_hypo = 1, sims = 1000, perms = 10000)
use_data(qp_sina_data)

qp_sim_power <- read.csv("simulations.csv", as.is = TRUE)
use_data(qp_sim_power)
