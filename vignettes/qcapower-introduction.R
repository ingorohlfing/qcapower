## ----load package-------------------------------------------------------------
devtools::load_all()

## ----qcapower example, eval = c(2, 3)-----------------------------------------
power_example <- qcapower(cases = 15, alt_hypo = 1, null_hypo = 0.85)
load("qcapower_vign.RData")
head(qcapower_vign)

## ---- eval = F----------------------------------------------------------------
#  mean(qcapower(cases = 10, alt_hypo = 1, null_hypo = 0.8, sims = 10, perms = 1000)$power)
#  qcapower(cases = 10, alt_hypo = 1, null_hypo = 0.8, sims = 10, perms = 1000)$power[1,]

## ----qp_run_plot, fig.cap = "Fig. 1: Plot of running power estimate"----------
qp_run_plot(qcapower_vign)

## ----qp_quant_plot, fig.cap="Fig. 2: Plot of 5%-quantiles"--------------------
qp_quant_plot(qcapower_vign)

## ----qp_cases-----------------------------------------------------------------
qp_cases(0.9, null_hypo = 0.80, alt_hypo = 1)

## ----qp_cases_brute, eval = F-------------------------------------------------
#  # not run
#  qp_cases_brute(0.9, null_hypo = 0.80, alt_hypo = 1)

