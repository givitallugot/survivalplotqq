---
output: 
  html_document:
    keep_md: true
---

<!-- README.md is generated from README.Rmd. Please edit that file -->


**NOTE: This is a package created for experimental purposes, for the second edition of [R Packages](https://r-pkgs.org). But this can be useful for drawing various Q-Q plots to eventually inference which distribution is the best for the observed data. [forcats](https://forcats.tidyverse.org).**

# survivalplotqq

<!-- badges: start -->
<!-- badges: end -->

The goal of survivalplotqq is to draw Q-Q plots with 8 different distribution for the observed survival data (Complete cases should be used.) This can be very helpful to find exact distribution for the observed data. Weibull, Log-Normal, Gompertz, Gumbel, Normal, Laplace, Pareto, Exponential distributions are used as candidate distributions.

## Installation

``` r
devtools::install_github("givitallugot/survivalplotqq")
```

## Example

This is a basic example which shows you how to solve a common problem:


```r
library(survivalplotqq)

# loco <- read.csv("./locomotive.csv", header=T)
# loco$lnmiles <- log(loco$miles)
# data <- loco[loco$tag == 1,] # Use only complete cases
# colnames(data) <- c('x', 'tag', 'lnx')
# 
# plotqq.survival(data)
```
