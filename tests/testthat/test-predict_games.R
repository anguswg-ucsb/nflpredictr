context("predict_games function")

test_that("pred0ict_games errors...", {

  expect_error(
    predict_games(year = NA, week = NA),
    paste0("\nInvalid inputs:\nyear = ", NA,
           "\nweek = ", NA)
  )

  expect_error(
    predict_games(year = -2122, week = NA),
    paste0("\nInvalid inputs:\nyear = ", -2122,
           "\nweek = ", NA)
  )

  expect_error(
    predict_games(year = -2122, week = "fgsds"),
    paste0("\nInvalid inputs:\nyear = ", -2122,
           "\nweek = ", "fgsds")
  )

  expect_error(
    predict_games(year = 2021, week = NA),
    paste0("\nInvalid inputs:\nyear = ", 2021,
           "\nweek = ", NA)
  )

  expect_error(
    predict_games(year = NULL, week = NA),
    paste0("\nInvalid inputs:\nyear = ", 2022,
           "\nweek = ", NA)
  )

  expect_error(
    predict_games(year = "20420", week = NULL, base_url = "mndfg"),
    paste0("Could not resolve host: mndfgyear=20420&pred_week=1")
  )

  expect_error(
    predict_games(year = 2021, week = 3, base_url = "www.google.com"),
    paste0("Could not resolve host: www.google.comyear=2021&pred_week=3")
  )

  expect_error(
    predict_games(year = 2021, week = 3, base_url = NA),
    paste0("Could not resolve host: NAyear=2021&pred_week=3")
  )
})

