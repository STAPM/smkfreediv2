






<!-- README.md is generated from README.Rmd. Please edit that file -->

# Estimating the smoke-free dividend <img src="hex-smkfreediv2.png" align="right" style="padding-left:10px;background-color:white;" width="100" height="100"/>

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

<!-- badges: end -->

## Motivation

The motivation for `smkfreediv2` is to develop a set of standard
functions for obtaining and processing raw data from OHID local tobacco
profiles and combining this with data from the Smoking Toolkit Study
(STS) to produce estimates of the smoke-free dividend for local
authorities in England.

The package uses the methods developed in [Morris et
al. (2024)](https://tobaccocontrol.bmj.com/content/early/2024/02/05/tc-2023-058264),
which should be cited with any use of this code.

## Usage

The `smkfreediv2` package contains functions which read in raw data
files, process them into clean output variables, and combines all data
files into a single output data table. The **inputs** are the raw
Smoking Toolkit Study (STS) data files in SPSS data format.

The package uses data on smoking prevalence from the Office for Health
Improvement and Disparities (OHID) local tobacco profiles. The functions
in the package download these data using a separate R package. To use
`smkfreediv2` you will also need to install
[`fingertipsR`](https://github.com/ropensci/fingertipsR).

``` r
# Enable repository from ropensci
options(repos = c(
  ropensci = 'https://ropensci.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

# Download and install fingertipsR in R
install.packages('fingertipsR')
```

## Installation

`smkfreediv2` is available on GitHub. If you are on a Windows machine
you will also need to [install
Rtools](https://www.rdocumentation.org/packages/installr/versions/0.22.0/topics/install.Rtools).  
Once that is sorted, you can install the latest version or a specified
version from GitHub with:

``` r
#install.packages("devtools")
#install.packages("getPass")
#install.packages("git2r")

devtools::install_git(
  "https://github.com/STAPM/smkfreediv2.git", 
  ref = "x.x.x",
  build_vignettes = TRUE
)
```

## Reference

Morris D, Gillespie D, Dockrell MJ, et al. Potential smoke-free dividend
across local areas in England: a cross-sectional analysis Tobacco
Control Published Online First: 20 March 2024. doi:
10.1136/tc-2023-058264
