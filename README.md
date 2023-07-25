
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NetBlotch

<!-- badges: start -->
<!-- badges: end -->

The goal of NetBlotch is to investigate fungicide resistance management
of net blotch in Western Australia.

## Installation

You can install the development version of NetBlotch like so:

``` r
#install.packages("devtools")
library(devtools)
install_github("joehelps/netblotch_2023")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(NetBlotch)
#> 
#> Attaching package: 'NetBlotch'
#> The following object is masked from 'package:stats':
#> 
#>     simulate

pars <- parameters(n_years = 5)

sim <- simulate(pars)
```

## Paper figures

The figures in the paper can be reproduced using the code in
`Figures.Rmd` in this repository.
