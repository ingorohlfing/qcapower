#' Data simulated power estimates
#'
#' A dataset containing power simulations for different number of cases and
#' different values for null- and alternative hypothesis
#'
#' \describe{
#' \item{cases}{number of cases}
#' \item{null_hypo}{null hypothesis (H0)}
#' \item{alt_hypo}{alternative hypothesis}
#' \item{sims}{number of simulations}
#' \item{perms}{number of permutations}
#' \item{perms}{calculate power}
#' }
#'
#' @format A dataframe with simulation parameters and calculated power
"qp_sim_power"


#' Data simulated power estimates for plotting of 5\%-quantiles
#'
#' A dataset containing the estimated 5\%-quantiles from a power simulation with
#' 1000 simulations each with 10000 permutations. The value for the alternative
#' hypothesis was set to 1.
#'
#' \describe{
#' \item{power}{power estimate over 1000 simulations}
#' \item{powercum}{running power estimate for ith simulation}
#' \item{null_hypo}{null hypothesis (H0), set to 0.8 (irrelevant here)}
#' \item{alt_hypo}{alternative hypothesis (H1), set to 1}
#' \item{cases}{number of cases, set to 10}
#' \item{quant}{estimated 5\%-quantiles per simulations}
#' }
#'
#' @format A dataframe with 1000 rows and 6 variables:
"qp_sina_data"
