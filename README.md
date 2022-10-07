
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

The `nflpredictr` R package provides a lightweight R client for
retrieving predictions for upcoming (and past) NFL games via the [NFL
Win Predictor API](http://68.183.25.9:8000/__docs__/). Details on the
model used to make the predictions can be found
[here](https://anguswg-ucsb.github.io/nfl_wins/)

`nflpredictr` provides four main functionalities: 1. Retrieves new data
to use as the inputs into a [ML model trained to predict the outcomes of
NFL games](https://anguswg-ucsb.github.io/nfl_wins/) 2. Provide the new
data to the [API](http://68.183.25.9:8000/__docs__/) and return
predictions 3. Visualize results. 4. Retrieve Las Vegas Odds and lines
to compare with the predictions generated from `nflpredictr`

<hr>

## Installation

You can install the development version of `nflpredictr` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("anguswg-ucsb/nflpredictr")
```

## Example

`nflpredictr` provides utility functions for accessing game predictions
from a Logistic Regression ML model that was trained on \~20 years of
historic NFL data and [correctly predicts the outcomes of games \~69% of
the
time](https://anguswg-ucsb.github.io/nfl_wins/#Model_Performance_on_Test_Data).
If no inputs are given to `predict_games()`, the default behavior is to
make a prediction for the upcoming week of the current NFL season

``` r
# Load package
library(nflpredictr)

# Make an API request using predict_games(), default week is the upcoming week of the season
nflpredictr::predict_games(
  year = 2022,
  week = 5
  )
#> 
#> 
#> Sending request to nflwinpredictor API...
#> Request URL:
#> http://68.183.25.9:8000/predict-new-data?year=2022&pred_week=5
#> # A tibble: 16 × 8
#>    season  week game_id         home_team away_team win   home_win_prob away_w…¹
#>     <int> <int> <chr>           <chr>     <chr>     <chr>         <dbl>    <dbl>
#>  1   2022     5 2022_05_HOU_JAX JAX       HOU       1             0.724    0.276
#>  2   2022     5 2022_05_PIT_BUF BUF       PIT       1             0.706    0.294
#>  3   2022     5 2022_05_CHI_MIN MIN       CHI       1             0.661    0.339
#>  4   2022     5 2022_05_LV_KC   KC        LV        1             0.637    0.363
#>  5   2022     5 2022_05_NYG_GB  GB        NYG       1             0.591    0.409
#>  6   2022     5 2022_05_MIA_NYJ NYJ       MIA       1             0.589    0.411
#>  7   2022     5 2022_05_ATL_TB  TB        ATL       1             0.584    0.416
#>  8   2022     5 2022_05_DET_NE  NE        DET       1             0.511    0.489
#>  9   2022     5 2022_05_DAL_LA  LA        DAL       0             0.492    0.508
#> 10   2022     5 2022_05_SF_CAR  CAR       SF        0             0.492    0.508
#> 11   2022     5 2022_05_CIN_BAL BAL       CIN       0             0.491    0.509
#> 12   2022     5 2022_05_IND_DEN DEN       IND       0             0.476    0.524
#> 13   2022     5 2022_05_TEN_WAS WAS       TEN       0             0.475    0.525
#> 14   2022     5 2022_05_LAC_CLE CLE       LAC       0             0.462    0.538
#> 15   2022     5 2022_05_SEA_NO  NO        SEA       0             0.457    0.543
#> 16   2022     5 2022_05_PHI_ARI ARI       PHI       0             0.372    0.628
#> # … with abbreviated variable name ¹​away_win_prob
```

<br>

## Make predictions on past games

Predictions can also be requested for weeks of past NFL seasons, going
as far back as the 2016 season.

``` r
# Make an API request using predict_games() for a specific year and week
nflpredictr::predict_games(
  year = 2018,
  week = 8
  )
#> 
#> 
#> Sending request to nflwinpredictor API...
#> Request URL:
#> http://68.183.25.9:8000/predict-new-data?year=2018&pred_week=8
#> # A tibble: 14 × 8
#>    season  week game_id         home_team away_team win   home_win_prob away_w…¹
#>     <int> <int> <chr>           <chr>     <chr>     <chr>         <dbl>    <dbl>
#>  1   2018     8 2018_08_GB_LA   LA        GB        1             0.758    0.242
#>  2   2018     8 2018_08_CLE_PIT PIT       CLE       1             0.729    0.271
#>  3   2018     8 2018_08_NYJ_CHI CHI       NYJ       1             0.683    0.317
#>  4   2018     8 2018_08_DEN_KC  KC        DEN       1             0.650    0.350
#>  5   2018     8 2018_08_MIA_HOU HOU       MIA       1             0.639    0.361
#>  6   2018     8 2018_08_SF_ARI  ARI       SF        1             0.563    0.437
#>  7   2018     8 2018_08_TB_CIN  CIN       TB        1             0.561    0.439
#>  8   2018     8 2018_08_BAL_CAR CAR       BAL       1             0.555    0.445
#>  9   2018     8 2018_08_SEA_DET DET       SEA       1             0.552    0.448
#> 10   2018     8 2018_08_NO_MIN  MIN       NO        1             0.524    0.476
#> 11   2018     8 2018_08_PHI_JAX JAX       PHI       0             0.440    0.560
#> 12   2018     8 2018_08_WAS_NYG NYG       WAS       0             0.406    0.594
#> 13   2018     8 2018_08_IND_LV  LV        IND       0             0.397    0.603
#> 14   2018     8 2018_08_NE_BUF  BUF       NE        0             0.317    0.683
#> # … with abbreviated variable name ¹​away_win_prob
```

## Make a tile plot of win probabilities

`nflpredictr` contains a few functions for quick plotting the outputs
from `predict_games()`. The `plot_tile()` function will plot each match
up side by side on a tile plot and color code teams by their predicted
win probability from the NFL Win Prediction API.

``` r
# Plot the outputs from predict_games()
tile_plot <- nflpredictr::plot_tile(
                  predictions = nflpredictr::predict_games(
                    year = 2022,
                    week = 5
                  )
              )
```

<img src="man/figures/tile_plot.png" align="center" height = "100%" width="100%" />

<br>

## Plot the teams favored to win that week

The `plot_favored()` function will plot the teams favored by the NFL Win
Prediction API model in order of win probability.

``` r
# Plot the outputs from predict_games()
fav_plot <- nflpredictr::plot_favored(
                predictions = nflpredictr::predict_games(
                                  year = 2022,
                                  week = 5
                                ),
                prob_alpha  = FALSE
                )
```

<img src="man/figures/plot_fav.png" align="center" height = "100%" width="100%" />

<br>

## Retrieve current Las Vegas betting odds

The `get_vegas()` function will retrieve Vegas spreads, money lines, and
totals for the current season

``` r
vegas_odds <- nflpredictr::get_vegas()
#> Retrieving Las Vegas betting odds...
#> URL: 4
#> Date input: 2022-10-07
#> Retrieving dates of NFL weeks: 2022

vegas_odds
#> # A tibble: 15 × 10
#>    season  week game_id    date       home_…¹ away_…² favored spread   win total
#>     <dbl> <dbl> <chr>      <date>     <chr>   <chr>     <dbl>  <dbl> <dbl> <dbl>
#>  1   2022     5 2022_05_N… 2022-10-09 GB      NYG           1   -9    -380    41
#>  2   2022     5 2022_05_A… 2022-10-09 TB      ATL           1  -10    -480    46
#>  3   2022     5 2022_05_C… 2022-10-09 MIN     CHI           1   -7.5  -360    44
#>  4   2022     5 2022_05_D… 2022-10-09 NE      DET           1   -3    -170    46
#>  5   2022     5 2022_05_H… 2022-10-09 JAX     HOU           1   -7    -340    44
#>  6   2022     5 2022_05_L… 2022-10-09 CLE     LAC           0    2     115    47
#>  7   2022     5 2022_05_M… 2022-10-09 NYJ     MIA           0    3.5   150    46
#>  8   2022     5 2022_05_P… 2022-10-09 BUF     PIT           1  -14    -950    46
#>  9   2022     5 2022_05_S… 2022-10-09 NO      SEA           1   -5.5  -235    46
#> 10   2022     5 2022_05_T… 2022-10-09 WAS     TEN           0    1.5   110    43
#> 11   2022     5 2022_05_S… 2022-10-09 CAR     SF            0    7     230    39
#> 12   2022     5 2022_05_D… 2022-10-09 LA      DAL           1   -5.5  -235    43
#> 13   2022     5 2022_05_P… 2022-10-09 ARI     PHI           0    5     190    49
#> 14   2022     5 2022_05_C… 2022-10-09 BAL     CIN           1   -3.5  -175    48
#> 15   2022     5 2022_05_L… 2022-10-10 KC      LV            1   -7    -340    51
#> # … with abbreviated variable names ¹​home_team, ²​away_team
```
