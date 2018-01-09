library(dplyr)
library(readr)

if("qcpapower" %in% rownames(installed.packages())) {
  library(qcapower)
} else {
  source("simulations-qcapower.R")
}

## Set simulation parameters

file_name <- 'simulations.csv'  # results from previous simulations

hypo_seq <- seq(0.75, 1.0, 0.05)  #, 0.01)        # vector for hypotheses levels simulated
sim_params <- list(cases = seq(5, 50, 1),  #, 1)  # vector for number of cases simulated
                   null_hypo = hypo_seq, alt_hypo = hypo_seq,
                   sims = c(5000),  #, 5000)
                   perms = c(50000))  #, 100000))

## Run simulations and save results

get_power <- function(...) {
  params <- list(...)
  params$power <- unique(do.call(qcapower, params)$power)
  return(params)
  }

# tmp <- get_power(cases = 20, null_hypo = 0.8, alt_hypo = 1, sims = 250, perms = 500)

# create simulation parameters

sim_values <- expand.grid(sim_params)
sim_values <- sim_values %>% filter(null_hypo < alt_hypo)  # Alt-Hypo > Null-Hypo

sim_values <- sim_values %>% filter(alt_hypo %in% c(0.85, 1))  # TODO -- remove later (maybe)

create_row_names <- function(df) apply(df, 1, paste, collapse = '-')

row.names(sim_values) <- create_row_names(sim_values)

# run simulations
row_select <- c(TRUE)  # initial values to start simulation loop

while(TRUE %in% row_select) {
  if(file.exists(file_name)) {
    suppressMessages(sim_data <- read_csv(file_name) %>% as.data.frame)
    row.names(sim_data) <- create_row_names(sim_data %>% select(-power))
  } else {
    sim_data <- data.frame()
  }

  row_select <- ! row.names(sim_values) %in% row.names(sim_data)
  if( all(row_select)) {
    stop("calculated all simulations")
  }

  sim_select <- sim_values %>% filter(row_select) %>% sample_n(1)

  sprintf('time: %s -- remaining: %d -- params: %s', strftime(Sys.time(), '%T'),
          row_select %>% as.integer %>% sum, paste(sim_select, collapse = ' ')) %>% print

  tmp <- sim_select %>% as.list %>% do.call(get_power, .) %>% as.data.frame

  sim_data <- rbind(sim_data, tmp) %>%
    mutate(null_hypo = round(null_hypo, 2),
           alt_hypo = round(alt_hypo, 2)) %>%
    arrange(null_hypo, alt_hypo, cases, sims, perms) %>%
    distinct(.keep_all = TRUE)

  write_csv(sim_data, file_name)
}
