
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nflpredictr <img src="man/figures/logo.png" align="right" width="25%" />

<!-- badges: start -->

[![Dependencies](https://img.shields.io/badge/dependencies-7/26-orange?style=flat)](#)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://choosealicense.com/licenses/mit/)
<!-- badges: end -->

<div align="left">

<p align="left">
<a href="http://68.183.25.9:8000/__docs__/"><strong>« API »</strong></a>
<br /> <a href="https://anguswg-ucsb.github.io/nfl_wins/">Model
Details</a>
</p>

</div>

<hr>

The goal of `nflpredictr` is to access the [NFL Win Predictor
API](http://68.183.25.9:8000/__docs__/) to predictions on past and
upcoming NFL games.

<hr>

## Installation

You can install the development version of `nflpredictr` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("anguswg-ucsb/nflpredictr")
```

## Example

`nflpredictr` provides utility functions for accessing predictions from
the [NFL Win Predictor API](http://68.183.25.9:8000/__docs__/) If no
inputs are given to `predict_games()`, the default behavior is to make a
prediction for the upcoming week of the current NFL season

``` r

# Make an API request using predict_games() for the upcoming week of the current season specific year and week
# nflpredictr::predict_games()
```

## Past predictions

Predictions can also be requested for past weeks, going back to the 2016
season

``` r
# Make an API request using predict_games() for a specific year and week
# nflpredictr::predict_games(
#   year = 2017,
#   week = 8
#   )
```
