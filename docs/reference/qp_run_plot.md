# Plot of power estimate against the number of simulations

`qp_run_plot` allows you to plot the running power estimate to determine
whether `sims` is sufficiently large to derive a reliable estimate

## Usage

``` r
qp_run_plot(power_est, title = FALSE)
```

## Arguments

- power_est:

  Dataframe containing the simulation results (see
  [`qcapower`](qcapower.md))

- title:

  Option for adding title to plot (default `FALSE`)

## Value

A line plot (\`gg\` object).

## Details

Creates a plot with `ggplot2`

## Examples

``` r
power_data <- qcapower(cases = 20, null_hypo = 0.8, alt_hypo = 0.95, sims = 10, perms = 1000)
qp_run_plot(power_data)


# Using data with 10000 estimates
data(qp_sina_data)
qp_run_plot(qp_sina_data)
```
