[![CRAN](http://www.r-pkg.org/badges/version/qcapower)](https://cran.r-project.org/package=qcapower)
[![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/grand-total/qcapower)](https://CRAN.R-project.org/package=qcapower)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

## Estimating power and required sample size in QCA
The `qcapower` package for R allows researchers working with Qualitative Comparative Analysis (QCA) to 

* estimate power using permutation tests and create diagnostic plots;
* estimate the required sample size for a target power level.

Version 0.1.0 of the package is available on [CRAN](https://cran.r-project.org/package=qcapower). 
Please read the vignette on CRAN for information on what you can do with the package.
The current Github version is 0.2.0 that only differs from the version 0.1.0 
regarding updated author information and minor fixes that do not concern the main
functions.

```r
install.packages("qcapower")
library(qcapower)
```

The current version can be installed from Github. 

```r
pak::pak("ingorohlfing/qcapower")
library(qcapower)
```
***

We received funding from the European Research Council (ERC) under the European 
Union’s Horizon 2020 research and innovation program (grant agreement nr. 638425,
*Enhanced Qualitative and Multimethod Research*).
