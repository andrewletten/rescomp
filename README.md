
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rescomp

<!-- badges: start -->
<!-- badges: end -->

The goal of the R package `rescomp` is to simplify the process of
defining, simulating and visualizing the output of ODE models of
consumer-resource interactions. In essence, it is a consumer-resource
modelling focused interface to the excellent `deSolve` package.

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
facilitates i) the definition and parameterisation of a desired
consumer-resource model, and ii) the specification of simulation
parameters. The default output from `make_par_list` is a list defining a
model for a single type I consumer (linear functional response) and a
single logistically growing resource.

``` r
pars <- make_par_list()
#> Model properties: 
#>  * 1 consumer(s) and 1 resource(s)
#>  * Consumers have type 1 functional responses
#>  * Resources grow logistically
#>  * Mortality is continuous
#>  * Parameters are constant through time
#> 
#> Simulation properties: 
#>  * Total simulation time: 1000 time steps
```

`rescomp::funcresp` plots the functional response for easy visualistion
prior to running a simulation.

``` r
plot_funcresp(pars)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="40%" />

<!-- `rescomp::time_vals` sets the simulation length (and other resource pulse frequency if required). The default simulation length is 1000. The function `initiate_state` sets the starting values of state variables defaulting to 10 for consumers and the resource supply concentration for resources.  -->

The model is then simulated via `rescomp::sim_rescomp` (a wrapper for
`deSolve::ode` with convenient defaults).

``` r
m1 <- sim_rescomp(pars)
```

Output dynamics can be visualised with `rescomp::plot_crsim`.

``` r
plot_crsim(m1, pars) 
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="80%" />

The main utility of `rescomp` comes with specifying more elaborate
models and simulation dynamics. Features/options include:

-   Number of consumers/resources
-   Consumer functional response (type I or type II)
-   Resource dynamic (chemostat, logistic and/or pulsed)
-   Resource type (substitutable or essential)
-   Continuous or intermittent mortality (e.g. serial transfer)
-   Time dependent consumption parameters

See `?make_par_list` for all argument options.

The below example demonstrates how to build and simulate a model for two
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
  resconc = 0.2,
  totaltime = 2000
)
#> Model properties: 
#>  * 2 consumer(s) and 1 resource(s)
#>  * Consumers have type 2 functional responses
#>  * Resources grow logistically
#>  * Mortality is continuous
#>  * Parameters are constant through time
#> 
#> Simulation properties: 
#>  * Total simulation time: 2000 time steps
```

``` r
plot_funcresp(pars, maxx = 0.2)
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="40%" />

``` r
m2 <- sim_rescomp(pars)
plot_crsim(m2, pars) 
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="80%" />

Disclaimer: As a biologist with no formal training in software
development, I cannot vouch 100% that `rescomp` is bug free, maximally
efficient or entirely consistent with ‘tidy’ principles. I have done my
best but please use with caution!
