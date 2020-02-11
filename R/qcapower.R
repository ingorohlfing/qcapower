#' Estimate power for a term in Qualitative Comparative Analysis (QCA).
#'
#' \code{qcapower} returns a power estimate with regard to the consistency
#' of a term, given information about the required parameters
#'
#' \code{qcapower} allows you to estimate power for a term. Probability
#' is the probability of rejecting the null hypothesis that no set relation
#' is in plaace when it is in place, in fact. A term can be a single condition,
#' a conjunction, or a disjunction of any combination of the two.
#'
#' @param cases Number of cases. In fuzzy-set QCA, equal to total number of
#'   cases in the analysis
#' @param null_hypo Null hypothesis (\emph{H0}). Consistency value separating
#'   consistent from inconsistent terms. It is the highest possible consistency
#'   value that would let you conclude that no set relation is given.
#' @param alt_hypo Alternative hypothesis (\emph{H1}). Expected, actual
#'   consistency value of term.
#' @param sims Number of simulations for calculating power
#' @param perms Number of permutations of hypothetical dataset per simulation run
#' @param alpha Level of alpha at which statistical significance of H0 is tested
#' @param cons_threshold Degree of tolerance in generating hypothetical data
#'  with consistency equaling \code{alt_hypo} (see vignette)
#' @param set_seed Parameter for achieving reproducibility of estimate
#' @return A dataframe with rows equaling the number of \code{sims}.
#'   \code{power} is the power estimate and is identical for each rows.
#'   \code{powercum} is the running power estimate up to this row. \code{quant}
#'   is the 5\%-quantile of the permuted distributions. See the vignette for
#'   more information.
#' @seealso \code{\link{qp_quant_plot}} and \code{\link{qp_run_plot}}
#' @examples
#' power_data <- qcapower(cases = 20, null_hypo = 0.8, alt_hypo = 0.95, sims = 10, perms = 1000)
#' head(power_data)
#' @export
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


#' Calculte the number of cases for a particular case target based on simulated
#' data
#'
#' \code{qp_cases} calculates the number of cases needed for a particular
#' power level. It is based on the presimulated data using \code{qcapower}.
#' See the vignette for more details.
#'
#' @param power_target Desired level of power
#' @param null_hypo Null hypothesis (\emph{H0}). Consistency value separating
#'   consistent from inconsistent terms.
#' @param alt_hypo Alternative hypothesis (\emph{H1}). Expected, actual
#'   consistency value of term.
#'
#' @seealso \code{\link{qp_cases_brute}}
#'
#' @examples
#' qp_cases(0.1, null_hypo = 0.8, alt_hypo = 1)
#'
#' @export
qp_cases <- function(power_target, null_hypo, alt_hypo) {
  sim_data <- qp_sim_power
  if(power_target < 0 | power_target > 1) {
    stop("'power target' must be between 0 and 1")
  }
  if(! null_hypo %in% sim_data$null_hypo | ! alt_hypo %in% sim_data$alt_hypo) {
    hypo_values <- sapply(c("null_hypo", "alt_hypo"),
                          function(.x) paste(unique(sim_data[[.x]]), collapse = ", "))
    stop(sprintf("\n\nnull hypothesis must be %s\n\nalternative hypothesis must be %s",
                  hypo_values[["null_hypo"]], hypo_values[["alt_hypo"]]))
  }

  sim_select <- sim_data[sim_data$power >= power_target
                         & sim_data$null_hypo == null_hypo
                         & sim_data$alt_hypo == alt_hypo, "cases"]

  if(length(sim_select) == 0) {
    return(NA)
  } else {
    return(min(sim_select))
  }
}


#' Calculate the number of cases for a particular case target with iterative
#' simulations (brute force)
#'
#' \code{qp_cases_brute} calculates the number of cases needed for a particular
#' power level. The function starts with the number of cases given by \code{start_value}
#' and iteratively simulates power and adjusts the number of cases
#'  until the \code{power_target} is met or the \code{max_value} has been reached.
#'  Running the function can take a lot of time. Use \code{\link{qp_cases}} to
#'
#' @param power_target Power level target
#' @param start_value Default number of cases for initial search
#' @param max_value Default maximum number of cases for search
#' @param progress Show progress of calculation (default \code{TRUE})
#' @param ... \code{qcapower} parameters -- see \code{\link{qcapower}}
#'
#' @seealso \code{\link{qp_cases_brute}}
#'
#' @examples
#' \dontrun{
#' qp_cases_brute(power_target = 0.9, null_hypo = 0.80, alt_hypo = 1)
#'
#' qp_cases_brute(power_target = 0.9, null_hypo = 0.80, alt_hypo = 1, start_value = 20,
#'                max_value = 50, perms = 500)
#' }
#'
#' @export
qp_cases_brute <- function(power_target, start_value = 2, max_value = 100,
                           progress = TRUE, ...) {
  cases <- start_value - 1
  params <- list(...)
  power <- 0

  while(power < power_target) {
    cases <- cases + 1
    if(cases > max_value | cases <= 0) {
      stop("Number of cases smaller than 0 or larger than 'max_value'")
    }
    params[['cases']] <- cases
    power <- unique(do.call(qcapower, params)$power)
    if(progress) {
      print(sprintf('cases = %d -- power = %.2f', cases, power))
    }
  }
  return(cases)
}


#' Plot of power estimate against the number of simulations
#'
#' \code{qp_run_plot} allows you to plot the running power estimate to
#' determine whether \code{sims} is sufficiently large to derive a reliable
#' estimate
#'
#' Creates a plot with \code{ggplot2}
#'
#' @param power_est Dataframe containing the simulation results (see
#'   \code{\link{qcapower}})
#' @param title Option for adding title to plot (default \code{FALSE})
#' @examples
#' power_data <- qcapower(cases = 20, null_hypo = 0.8, alt_hypo = 0.95, sims = 10, perms = 1000)
#' qp_run_plot(power_data)
#'
#' # Using data with 10000 estimates
#' data(qp_sina_data)
#' qp_run_plot(qp_sina_data)
#' @import ggplot2
#' @import ggforce
#' @export
qp_run_plot <- function(power_est, title = FALSE) {
  pl_title = "Power estimate over simulations"
  pl_y_label = "running power estimate"

  power_est$id <- seq(1, nrow(power_est), by = 1)
  pl <- ggplot(data=power_est, mapping=aes(x = id, y = powercum)) +
    geom_line() +
    scale_x_continuous(name = "simulation index",
                       breaks = c(1, nrow(power_est))) +
    scale_y_continuous(name = pl_y_label,
                       breaks = seq(0, 1, by = 0.2),
                       limits = c(0, 1)) +
    theme_bw()

  if(title == TRUE) {
    pl <- pl +
      ggtitle(pl_title) +
      theme(plot.title = element_text(hjust = 0.5))
  }

  print(pl)
}


#' Sina plot of 5%-quantiles for assessing the dispersion of the permutated
#' distributions
#'
#' Depending on the number of cases, the permuted distributions of consistency
#' values can differ narrowly or widely in terms of their location on the
#' spectrum and their shape.
#'
#' Creates a sina plot with \code{ggforce}
#'
#' @param power_est Dataframe containing simulation results (see
#'   \code{\link{qcapower}})
#' @param title Option for adding title to plot (default \code{FALSE})
#' @examples
#' sim_data <- qp_sina_data
#' qp_quant_plot(sim_data)
#' @import ggplot2
#' @import ggforce
#' @export
qp_quant_plot <- function(power_est, title = FALSE) {
  pl_title = "Distribution of 5%-quantiles"
  pl_y_label = "estimated 5%-quantiles"

  pl <- ggplot(data=power_est, mapping=aes(factor(cases), quant)) +
    geom_sina(size=0.1) +
    scale_x_discrete(labels = NULL, name = NULL) +
    scale_y_continuous(name = pl_y_label,
                       breaks = seq(0, 1, by = 0.2),
                       limits = c(0, 1)) +
    theme_bw()

  if(title == TRUE) {
    pl <- pl +
      ggtitle(pl_title)
      theme(plot.title = element_text(hjust = 0.5))
  }

  print(pl)
}
