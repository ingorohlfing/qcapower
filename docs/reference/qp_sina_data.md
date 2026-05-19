# Data simulated power estimates for plotting of 5%-quantiles

A dataset containing the estimated 5%-quantiles from a power simulation
with 1000 simulations each with 10000 permutations. The value for the
alternative hypothesis was set to 1.

## Usage

``` r
qp_sina_data
```

## Format

A dataframe with 1000 rows and 6 variables:

## Details

- power:

  power estimate over 1000 simulations

- powercum:

  running power estimate for ith simulation

- null_hypo:

  null hypothesis (H0), set to 0.8 (irrelevant here)

- alt_hypo:

  alternative hypothesis (H1), set to 1

- cases:

  number of cases, set to 10

- quant:

  estimated 5%-quantiles per simulations
