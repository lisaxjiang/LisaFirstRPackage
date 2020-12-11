[![Travis build status](https://travis-ci.com/lisaxjiang/stat302project2.svg?branch=master)](https://travis-ci.com/lisaxjiang/stat302project2)

## Installation

To download the corncob package, use the code below.

``` r
# install.packages("stat302project2")
devtools::install_github("lisaxjiang/stat302project2")
library(stat302project2)
```
## Use

The vignette demonstrates example usage of all main functions. You can see the vignette by using the following code (note that this requires a TeX installation to view properly):

``` r
devtools::install_github("lisaxjiang/stat302project2", build_vignette = TRUE, build_opts = c())
library(stat302project2)
# Use this to view the vignette in the Demo HTML help
help(package = "stat302project2", help_type = "html")
# Use this to view the vignette as an isolated HTML file
utils::browseVignettes(package = "stat302project2")
```
