
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chessboard <img src="man/figures/package-sticker.png" align="right" style="float:right; height:120px;"/>

<!-- badges: start -->

[![R CMD
Check](https://github.com/frbcesab/chessboard/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/frbcesab/chessboard/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/FRBCesab/chessboard/branch/main/graph/badge.svg?token=qH71uWUiot)](https://app.codecov.io/gh/FRBCesab/chessboard)
[![Website](https://github.com/frbcesab/chessboard/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/frbcesab/chessboard/actions/workflows/pkgdown.yaml)
<!-- [![CRAN status](https://www.r-pkg.org/badges/version/chessboard)](https://CRAN.R-project.org/package=chessboard) -->
[![License: GPL (\>=
2)](https://img.shields.io/badge/License-GPL%20%28%3E%3D%202%29-blue.svg)](https://choosealicense.com/licenses/gpl-2.0/)
<!-- badges: end -->

The aim of the package `chessboard` is to provide functions to work with
**directed** (asymmetric) and **undirected** (symmetrical) spatial
**networks**. It makes easier the creation of **connectivity matrix**,
i.e. a binary matrix of dimensions `n x n`, where `n` is the number of
**nodes** (sampling units) indicating the presence (`1`) or the absence
(`0`) of an **edge** (link) between pairs of nodes. Different network
objects can be produced by `chessboard`:

- **nodes list**
- **neighbors list**
- **edges list**
- **connectivity matrix**

In addition, the package can also produce objects that can be used later
in Asymetric Eigenvector Maps (AEM, Blanchet *et al.* 2008), method
available in the package
[`adespatial`](https://cran.r-project.org/package=adespatial) (Dray *et
al.* 2022): **nodes by edges matrix** and **edges weights vector**.

<br/>

This package has been developed for the
[FRB-CESAB](https://www.fondationbiodiversite.fr/en/about-the-foundation/le-cesab/)
working group
[Bridge](https://www.fondationbiodiversite.fr/en/the-frb-in-action/programs-and-projects/le-cesab/bridge/)
that aims to better understand the role of local and regional
environmental factors in shaping the taxonomic and functional diversity
of plant communities established along river corridors, roadside
corridors and cultivated field margins.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("frbcesab/chessboard")
```

Then you can attach the package `chessboard`:

``` r
library("chessboard")
```

## Get started

Please read the [Get
started](https://frbcesab.github.io/chessboard/articles/chessboard.html)
vignette.

## Citation

Please cite this package as:

> Casajus N (2023) chessboard: An R package for neighborhood and
> connectivity in spatial networks. R package version 0.1.

## Code of Conduct

Please note that the `chessboard` project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
