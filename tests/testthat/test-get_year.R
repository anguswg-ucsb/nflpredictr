context("get_year function")

test_that("get_year errors...", {
  expect_error(
    get_year(date = "adssssssssjkjk4"),
    "Incorrect date format, date must be in YYYY-MM-DD format"
  )

  expect_error(
    get_year(date = "02-01-2022"),
    "Incorrect date format, date must be in YYYY-MM-DD format"
  )

  expect_error(
    get_year(date = "20245-01-05"),
    "Incorrect date format, date must be in YYYY-MM-DD format"
  )

})
