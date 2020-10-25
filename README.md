---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- rmarkdown v1 -->




# stray {STReam AnomalY} <img src="man/figures/logo.png" align="right" height="150" />

[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Licence](https://img.shields.io/badge/licence-GPL--2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html)

[![Build Status](https://travis-ci.org/pridiltal/stray.svg?branch=master)](https://travis-ci.org/pridiltal/stray)
 
---
 
[![minimal R version](https://img.shields.io/badge/R%3E%3D-3.4.0-6666ff.svg)](https://cran.r-project.org/)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/stray)](https://cran.r-project.org/package=stray)
[![packageversion](https://img.shields.io/badge/Package%20version-0.1.0-orange.svg?style=flat-square)](commits/master)
 
---
 
[![Last-changedate](https://img.shields.io/badge/last%20change-2020--10--26-yellowgreen.svg)](/commits/master)


<!-- README.md is generated from README.Rmd. Please edit that file -->





Anomaly Detection in High Dimensional Data Space

This package is a modification of [HDoutliers package](https://CRAN.R-project.org/package=HDoutliers). The HDoutliers algorithm is a powerful unsupervised algorithm for detecting anomalies in high-dimensional data, with a strong theoretical foundation. However, it suffers from some
limitations that significantly hinder its performance level, under certain circumstances. In this package, we propose an algorithm that addresses these limitations. We define an anomaly as an
observation that deviates markedly from the majority with a large distance gap. An approach based on extreme value theory is used for the anomalous threshold calculation. 


A companion paper to this work is available [here](https://arxiv.org/pdf/1908.04000.pdf). Using various
synthetic and real datasets, we demonstrate the wide applicability and usefulness of our algorithm, which we call the stray algorithm. We also demonstrate how this algorithm can
assist in detecting anomalies present in other data structures using feature engineering. We show the situations where the stray algorithm outperforms the HDoutliers algorithm both in
accuracy and computational time. 


This package is still under development and this repository contains a development version of the R package *stray*.

## Installation

You can install the stable version from CRAN.


```r
install.packages('stray', dependencies = TRUE)
```
You can install stray from github with:

```r
# install.packages("devtools")
devtools::install_github("pridiltal/stray")
```

## Example

### One dimensional data set with one outlier

```r
library(stray)
require(ggplot2)
#> Loading required package: ggplot2
set.seed(1234)
data <- c(rnorm(1000, mean = -6), 0, rnorm(1000, mean = 6))
outliers <- find_HDoutliers(data, knnsearchtype = "brute")
names(outliers)
#> [1] "outliers"   "out_scores" "type"
display_HDoutliers(data, outliers)
```

![plot of chunk onedim](man/figures/README-onedim-1.png)

### Two dimensional dataset with 8 outliers

```r
set.seed(1234)
n <- 1000 # number of observations
nout <- 10 # number of outliers
typical_data <- matrix(rnorm(2*n), ncol = 2, byrow = TRUE)
out <- matrix(5*runif(2*nout,min=-5,max=5), ncol = 2, byrow = TRUE)
data <- rbind(out, typical_data )
outliers <- find_HDoutliers(data, knnsearchtype = "brute")
display_HDoutliers(data, outliers)
```

![plot of chunk twodim](man/figures/README-twodim-1.png)

### Three dimensional dataset with 2 outliers

For data with more than two dimensions, two dimensional scatterplot is produced using the first two pricipal components.


```r
data <- rbind(matrix(rnorm(144), ncol = 3), c(10,12,10),c(3,7,10))
output <- find_HDoutliers(data, knnsearchtype = "brute")
display_HDoutliers(data, out = output)
```

![plot of chunk datad3](man/figures/README-datad3-1.png)
gitn

More examples are available from our paper [Anomaly Detection in High Dimensional Data](https://www.monash.edu/business/ebs/research/publications/ebs/wp20-2019.pdf) 


```r
outliers<-find_HDoutliers(data_c[,1:2], knnsearchtype= "brute")
p <- display_HDoutliers(data_c[,1:2], outliers)+
      ggplot2::ggtitle("data_c")

print(p)
```

![plot of chunk dataa](man/figures/README-dataa-1.png)


```r
outliers<-find_HDoutliers(data_d[,1:2], knnsearchtype= "brute")
p <- display_HDoutliers(data_d[,1:2], outliers)+
      ggplot2::ggtitle("data_d")

print(p)
```

![plot of chunk datad](man/figures/README-datad-1.png)

