# "qcapower()" function from qcapower-package
# to run simulations in "simulation.R" without installing package

qcapower <- function(cases, null_hypo, alt_hypo, sims = 1000, perms = 10000,
                     alpha = 0.05, cons_threshold = 0.01, set_seed = 135) {
  null_sig <- vector("numeric", length = sims)
  null_p <- vector("numeric", length = sims)
  quant <- vector("numeric", length = sims)

  set.seed(set_seed)
  calc_consistency <- function(a, y) sum(pmin(a, y)) / sum(a)
  random_draw <- function() round(runif(cases), digits = 2)
  df <- data.frame(index=1:cases)

  for(i in 1:sims) {
    df$a <- random_draw()
    df$y <- random_draw()
    cons <- calc_consistency(df$a, df$y)
    while(abs(alt_hypo - cons) > cons_threshold) {
      if(cons < alt_hypo) {
        sample_row <- sample(df[df$a > df$y, 'index'], 1)
        df[sample_row, 'y'] <- runif(1, min = df[sample_row, 'y'], max = 1)
      } else if(cons > alt_hypo) {
        sample_row <- sample(df[ , 'index'], 1)
        df[sample_row, 'y'] <- runif(1, min = 0, max = df[sample_row, 'y'])
      }
      cons <- calc_consistency(df$a, df$y)
    }
    cons_perms <- sapply(1:perms, function(x) calc_consistency(df$a, sample(df$y)))
    quant[i] <- quantile(cons_perms, 0.05)
    null_p[i] <- ecdf(cons_perms)(null_hypo) # p-value of null-hypo
    null_sig[i] <- ifelse(null_p[i] + 1.96 * null_p[i] / sqrt(perms) < alpha, 1, 0)
  }
  power <- sum(null_sig) / sims
  powercum <- cumsum(null_sig) / 1:sims
  estim <- data.frame(power, powercum, alt_hypo, null_hypo, cases, quant)

  return(estim)
}
