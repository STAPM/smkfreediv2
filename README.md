






<!-- README.md is generated from README.Rmd. Please edit that file -->

# Estimating the smoke-free dividend <img src="hex-smkfreediv2.png" align="right" style="padding-left:10px;background-color:white;" width="100" height="100"/>

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![](https://img.shields.io/badge/doi-10.17605/OSF.IO/GNSEJ-green.svg)](https://doi.org/10.17605/OSF.IO/GNSEJ)

<!-- badges: end -->

## Citation

Morris, D. (2023). smkfreediv2: An R package for estimating the
smoke-free dividend for local authorities England. doi: \<\>

## Motivation

The motivation for `smkfreediv2` is to develop a set of standard
functions for obtaining and processing raw data from OHID local tobacco
profiles and combining this with data from the Smoking Toolkit Study
(STS) to produce estimates of the smoke-free dividend for local
authorities in England.

## Usage

The `smkfreediv2` package contains functions which read in raw data
files, process them into clean output variables, and combines all data
files into a single output data table. The **inputs** are the raw
Smoking Toolkit Study (STS) data files obtained from…

The package also downloads data from the Office for Health Improvement
and Disparities (OHID) local tobacco profiles. To use this package you
will also need to install
[`fingertipsR`](https://github.com/ropensci/fingertipsR).

``` r
# Enable repository from ropensci
options(repos = c(
  ropensci = 'https://ropensci.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

# Download and install fingertipsR in R
install.packages('fingertipsR')
```

A typical workflow for using the package looks as follows,

``` r

### Define arguments 

root <- "C:/"
file <- "Documents/Datasets/Family Resources Survey/tab"
ages <- 16:64
years <- 2020
keep_vars <- NULL
complete_vars <- NULL

#########################################
### Read in and combine years of data ###

data <- frsclean(root = root,
                 file = file,
                 ages = ages,
                 years = years,
                 keep_vars = keep_vars,
                 complete_vars = complete_vars)
```

The **output** of these functions are a single data table of processed
FRS data

## Installation

`smkreediv2` is available on GitHub. If you are on a Windows machine you
will also need to [install
Rtools](https://www.rdocumentation.org/packages/installr/versions/0.22.0/topics/install.Rtools).  
Once that is sorted, you can install the latest version or a specified
version from GitHub with:

``` r
#install.packages("devtools")
#install.packages("getPass")
#install.packages("git2r")

devtools::install_git(
  "https://github.com/djmorris1989/frsclean.git", 
  ref = "x.x.x",
  build_vignettes = TRUE
)
```

## References
