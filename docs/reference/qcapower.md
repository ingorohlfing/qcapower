# Estimate power for a term in Qualitative Comparative Analysis (QCA). `qcapower` returns a power estimate with regard to the consistency of a term, given information about the required parameters

`qcapower` allows you to estimate power for a term. Probability is the
probability of rejecting the null hypothesis that no set relation is in
plaace when it is in place, in fact. A term can be a single condition, a
conjunction, or a disjunction of any combination of the two.

## Usage

``` r
qcapower(
  cases,
  null_hypo,
  alt_hypo,
  sims = 1000,
  perms = 10000,
  alpha = 0.05,
  cons_threshold = 0.01,
  set_seed = 135
)
```

## Arguments

- cases:

  Number of cases. In fuzzy-set QCA, equal to total number of cases in
  the analysis

- null_hypo:

  Null hypothesis (*H0*). Consistency value separating consistent from
  inconsistent terms. It is the highest possible consistency value that
  would let you conclude that no set relation is given.

- alt_hypo:

  Alternative hypothesis (*H1*). Expected, actual consistency value of
  term.

- sims:

  Number of simulations for calculating power

- perms:

  Number of permutations of hypothetical dataset per simulation run

- alpha:

  Level of alpha at which statistical significance of H0 is tested

- cons_threshold:

  Degree of tolerance in generating hypothetical data with consistency
  equaling `alt_hypo` (see vignette)

- set_seed:

  Parameter for achieving reproducibility of estimate

## Value

A dataframe with rows equaling the number of `sims`. `power` is the
power estimate and is identical for each rows. `powercum` is the running
power estimate up to this row. `quant` is the 5%-quantile of the
permuted distributions. See the vignette for more information.

## See also

[`qp_quant_plot`](qp_quant_plot.md) and [`qp_run_plot`](qp_run_plot.md)

## Examples

``` r
power_data <- qcapower(cases = 20, null_hypo = 0.8, alt_hypo = 0.95, sims = 10, perms = 1000)
head(power_data)
#>   power  powercum alt_hypo null_hypo cases     quant
#> 1   0.5 1.0000000     0.95       0.8    20 0.8073593
#> 2   0.5 1.0000000     0.95       0.8    20 0.8882059
#> 3   0.5 0.6666667     0.95       0.8    20 0.7908847
#> 4   0.5 0.5000000     0.95       0.8    20 0.7778945
#> 5   0.5 0.4000000     0.95       0.8    20 0.7656623
#> 6   0.5 0.5000000     0.95       0.8    20 0.8117470
```
