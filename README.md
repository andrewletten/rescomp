
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rescomp

<!-- badges: start -->
<!-- badges: end -->

The goal of rescomp is to simplify the process of defining, simulating
and visualizing the output of ode models of consumer-resource
interactions.

## Installation

You can install `rescomp` from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("andrewletten/rescomp")
```

## Example

``` r
library(rescomp)
library(deSolve)
```

``` r
pars <- make_par_list(
  spnum = 2, 
  resnum = 1,
  linear = FALSE,
  mumatrix = list(matrix(c(0.7,0.05), 
                    nrow = 2, 
                    ncol = 1,
                    byrow = TRUE)),
  kmatrix = matrix(c(2, 0.015), 
                   nrow = 2, 
                   ncol = 1, 
                   byrow = TRUE),  
  resspeed = 3,
  resconc = 0.2
)
#> Model properties: 
#>  * 2 consumer(s) and 1 resource(s)
#>  * Consumers have type 2 functional responses
#>  * Resources are substitutable (ignore if only a single resource)
#>  * Resources grow logistically
#>  * Mortality is continuous (equal to resource dilution rate?)
#>  * Parameters are constant through time
```

``` r
plot_funcresp(pars, maxx = 0.2)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="40%" />

``` r
happenings <- time_vals(total = 2000)

m1 <- ode(
  func = def_cr_ode,
  y = initiate_state(vars=pars),
  parms = pars,
  times = happenings$totaltime,
  method = "lsoda"
  )
```

``` r
plot_crsim(m1, pars) 
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="40%" />
