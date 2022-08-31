---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# nflpredictr <img src="man/figures/logo.png" align="right" width="25%" />

<!-- badges: start -->
[![Dependencies](https://img.shields.io/badge/dependencies-7/26-orange?style=flat)](#)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://choosealicense.com/licenses/mit/)
<!-- badges: end -->

<div align="left">

  <p align="left">
    <a href="http://68.183.25.9:8000/__docs__/"><strong>« API »</strong></a>
    <br />
    <a href="https://anguswg-ucsb.github.io/nfl_wins/">Model Details</a>
  </p>
</div>

<hr>

The goal of `nflpredictr` is to access the [NFL Win Predictor API](http://68.183.25.9:8000/__docs__/) to predictions on past and upcoming NFL games.

<hr>

## Installation

You can install the development version of `nflpredictr` from [GitHub](https://github.com/) with:


```r
# install.packages("devtools")
devtools::install_github("anguswg-ucsb/nflpredictr")
```

## Example

`nflpredictr` provides utility functions for accessing predictions from the [NFL Win Predictor API](http://68.183.25.9:8000/__docs__/)
If no inputs are given to `predict_games()`, the default behavior is to make a prediction for the upcoming week of the current NFL season

```r
# Load package
library(nflpredictr)

# Make an API request using predict_games()
nflpredictr::predict_games(
  year = 2022, 
  week = 1
  )
#> 
#> 
#> Sending request to nflwinpredictor API...
#> Request URL:
#> http://68.183.25.9:8000/predict-new-data?year=2022&pred_week=1
#> # A tibble: 16 × 8
#>    season  week game_id         home_team away_team .pred_class .pred_1 .pred_0
#>     <int> <int> <chr>           <chr>     <chr>     <chr>         <dbl>   <dbl>
#>  1   2022     1 2022_01_NYG_TEN TEN       NYG       1             0.777   0.223
#>  2   2022     1 2022_01_JAX_WAS WAS       JAX       1             0.740   0.260
#>  3   2022     1 2022_01_DEN_SEA SEA       DEN       1             0.610   0.390
#>  4   2022     1 2022_01_BUF_LA  LA        BUF       1             0.551   0.449
#>  5   2022     1 2022_01_KC_ARI  ARI       KC        1             0.545   0.455
#>  6   2022     1 2022_01_PIT_CIN CIN       PIT       1             0.532   0.468
#>  7   2022     1 2022_01_GB_MIN  MIN       GB        0             0.433   0.567
#>  8   2022     1 2022_01_LV_LAC  LAC       LV        0             0.420   0.580
#>  9   2022     1 2022_01_TB_DAL  DAL       TB        0             0.408   0.592
#> 10   2022     1 2022_01_NE_MIA  MIA       NE        0             0.383   0.617
#> 11   2022     1 2022_01_BAL_NYJ NYJ       BAL       0             0.375   0.625
#> 12   2022     1 2022_01_PHI_DET DET       PHI       0             0.328   0.672
#> 13   2022     1 2022_01_NO_ATL  ATL       NO        0             0.308   0.692
#> 14   2022     1 2022_01_IND_HOU HOU       IND       0             0.255   0.745
#> 15   2022     1 2022_01_SF_CHI  CHI       SF        0             0.218   0.782
#> 16   2022     1 2022_01_CLE_CAR CAR       CLE       0             0.185   0.815
```

<br>

## Make predictions on past games
Predictions can also be requested for past weeks, going back to the 2016 season

```r
# Make an API request using predict_games() for a specific year and week
nflpredictr::predict_games(
  year = 2017,
  week = 8
  )
#> 
#> 
#> Sending request to nflwinpredictor API...
#> Request URL:
#> http://68.183.25.9:8000/predict-new-data?year=2017&pred_week=8
#> # A tibble: 13 × 8
#>    season  week game_id         home_team away_team .pred_class .pred_1 .pred_0
#>     <int> <int> <chr>           <chr>     <chr>     <chr>         <dbl>   <dbl>
#>  1   2017     8 2017_08_SF_PHI  PHI       SF        1             0.842   0.158
#>  2   2017     8 2017_08_LV_BUF  BUF       LV        1             0.680   0.320
#>  3   2017     8 2017_08_LAC_NE  NE        LAC       1             0.654   0.346
#>  4   2017     8 2017_08_DEN_KC  KC        DEN       1             0.629   0.371
#>  5   2017     8 2017_08_CHI_NO  NO        CHI       1             0.598   0.402
#>  6   2017     8 2017_08_CAR_TB  TB        CAR       1             0.572   0.428
#>  7   2017     8 2017_08_HOU_SEA SEA       HOU       1             0.524   0.476
#>  8   2017     8 2017_08_IND_CIN CIN       IND       1             0.511   0.489
#>  9   2017     8 2017_08_DAL_WAS WAS       DAL       0             0.488   0.512
#> 10   2017     8 2017_08_ATL_NYJ NYJ       ATL       0             0.437   0.563
#> 11   2017     8 2017_08_MIA_BAL BAL       MIA       0             0.398   0.602
#> 12   2017     8 2017_08_PIT_DET DET       PIT       0             0.394   0.606
#> 13   2017     8 2017_08_MIN_CLE CLE       MIN       0             0.224   0.776
```

## Plot the teams favored to win that week
`nflpredictr` contains a few functions for quick plotting the outputs from `predict_games()`. The `plot_favored()` function will plot the teams favored by the NFL Win Prediction API model in order of win probability.

```r
# Plot the outputs from predict_games()
fav_plot <- nflpredictr::plot_favored(
  predictions = nflpredictr::predict_games(
    year = 2022,
    week = 1
  ),
  prob_alpha  = FALSE
  )
```

<img src="man/figures/plot_fav.png" align="center" height = "100%" width="100%" />

<br>

## Retrieve current Las Vegas betting odds 
The `get_vegas()` function will retrieve Vegas spreads, money lines, and totals for the current season

```r
# vegas_odds <- nflpredictr::get_vegas()

# vegas_odds
```
