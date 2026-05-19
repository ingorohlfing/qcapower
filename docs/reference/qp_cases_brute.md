# Calculate the number of cases for a particular case target with iterative simulations (brute force)

`qp_cases_brute` calculates the number of cases needed for a particular
power level. The function starts with the number of cases given by
`start_value` and iteratively simulates power and adjusts the number of
cases until the `power_target` is met or the `max_value` has been
reached. Running the function can take a lot of time. Use
[`qp_cases`](qp_cases.md) to

## Usage

``` r
qp_cases_brute(
  power_target,
  start_value = 2,
  max_value = 100,
  progress = TRUE,
  ...
)
```

## Arguments

- power_target:

  Power level target

- start_value:

  Default number of cases for initial search

- max_value:

  Default maximum number of cases for search

- progress:

  Show progress of calculation (default `TRUE`)

- ...:

  `qcapower` parameters – see [`qcapower`](qcapower.md)

## Value

An integer showing how many cases are needed to achieve the target level
of power.

## See also

`qp_cases_brute`

## Examples

``` r
if (FALSE) { # \dontrun{
qp_cases_brute(power_target = 0.9, null_hypo = 0.80, alt_hypo = 1)

qp_cases_brute(power_target = 0.9, null_hypo = 0.80, alt_hypo = 1, start_value = 20,
               max_value = 50, perms = 500)
} # }
```
