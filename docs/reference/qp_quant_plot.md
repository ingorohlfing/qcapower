# Sina plot of 5 distributions

Depending on the number of cases, the permuted distributions of
consistency values can differ narrowly or widely in terms of their
location on the spectrum and their shape.

## Usage

``` r
qp_quant_plot(power_est, title = FALSE)
```

## Arguments

- power_est:

  Dataframe containing simulation results (see
  [`qcapower`](qcapower.md))

- title:

  Option for adding title to plot (default `FALSE`)

## Value

A sina plot using the cases to visualize the density distribution
(\`gg\` object).

## Details

Creates a sina plot with `ggforce`

## Examples

``` r
sim_data <- qp_sina_data
qp_quant_plot(sim_data)
```
