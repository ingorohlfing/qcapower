# Calculte the number of cases for a particular case target based on simulated data

`qp_cases` calculates the number of cases needed for a particular power
level. It is based on the presimulated data using `qcapower`. See the
vignette for more details.

## Usage

``` r
qp_cases(power_target, null_hypo, alt_hypo)
```

## Arguments

- power_target:

  Desired level of power

- null_hypo:

  Null hypothesis (*H0*). Consistency value separating consistent from
  inconsistent terms.

- alt_hypo:

  Alternative hypothesis (*H1*). Expected, actual consistency value of
  term.

## Value

An integer showing how many cases are needed to achieve the target level
of power.

## See also

[`qp_cases_brute`](qp_cases_brute.md)

## Examples

``` r
qp_cases(0.1, null_hypo = 0.8, alt_hypo = 1)
#> [1] 2
```
