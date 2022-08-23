context("get_week function")

test_that("get_week errors...", {
  expect_error(
    get_week(date = NA),
    "Incorrect date format, date must be in YYYY-MM-DD format"
  )

  expect_error(
    get_week(date = "02-01-2022"),
    "Incorrect date format, date must be in YYYY-MM-DD format"
  )

  expect_error(
    get_year(date = "a date string"),
    "Incorrect date format, date must be in YYYY-MM-DD format"
  )

  expect_error(
    get_week(date = "NULL"),
    "Incorrect date format, date must be in YYYY-MM-DD format"
  )

})
