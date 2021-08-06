
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

The main user function in `rescomp` is `make_par_list`, which
facilitates the definition and parameterisation of a desired model. The
default output from `make_par_list` is list defining a model for a
single consumer (type I \[linear\] functional response) and a single
logistically growing resource.

``` r
pars <- make_par_list()
#> Model properties: 
#>  * 1 consumer(s) and 1 resource(s)
#>  * Consumers have type 1 functional responses
#>  * Resources are substitutable (ignore if only a single resource)
#>  * Resources grow logistically
#>  * Mortality is continuous (equal to resource dilution rate?)
#>  * Parameters are constant through time
```

`rescomp::funcresp` plots the functional response for easy visualistion
prior to running a simulation.

``` r
plot_funcresp(pars)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="40%" />

`rescomp::time_vals` sets the simulation length (and other resource
pulse frequency if required). The default simulation length is 1000. The
function `initiate_state` sets the starting values of state variables
defaulting to 10 for consumers and the resource supply concentration for
resources.

The model is passed to `deSolve::ode` to simulate.

``` r
happenings <- time_vals()

m1 <- ode(
  func = def_cr_ode,
  y = initiate_state(vars=pars),
  parms = pars,
  times = happenings$totaltime,
  method = "lsoda"
  )
```

Output dynamics can be visualised with `rescomp::plot_crsim`.

``` r
plot_crsim(m1, pars) 
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="80%" />

The main utility of `rescomp` comes with specifying more elaborate
models. Features/options include:

-   Number of consumers/resources
-   Consumer functional response (type I or type II)
-   Resource dynamic (chemostat, logistic and/or pulsed)
-   Resource type (substitutable or essential)
-   Time dependent consumption parameters

See `?make_par_list` for all argument options.

The below example demonstrates hot build and simulate a model for two
consumers with type II functional responses on a single logistically
growing resources. A wide range of other examples can be found in the
package vignette.

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

<img src="man/figures/README-unnamed-chunk-8-1.png" width="40%" />

``` r
happenings <- time_vals(total = 2000)

m2 <- ode(
  func = def_cr_ode,
  y = initiate_state(vars=pars),
  parms = pars,
  times = happenings$totaltime,
  method = "lsoda"
  )
```

``` r
plot_crsim(m2, pars) 
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="80%" />
