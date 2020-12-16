#within test-my_rf_cv.R
my_penguins <- na.omit(my_penguins)

test_that("my_rf_cv works", {
  expect_is(my_rf_cv(my_penguins[,3:5], my_penguins$body_mass_g, 2), "numeric")
})

test_that("my_rf_cv works", {
  expect_is(my_rf_cv(my_penguins[,3:5], my_penguins$body_mass_g, 5), "numeric")
})

test_that("my_rf_cv works", {
  expect_is(my_rf_cv(my_penguins[,3:5], my_penguins$body_mass_g, 10), "numeric")
})

test_that("non-numeric input throws error", {
  expect_error(my_rf_cv(my_penguins[,3:5], my_penguins$body_mass_g, "string"))
})

test_that("incomplete input throws error", {
  expect_error(my_rf_cv(my_penguins))
})

test_that("incomplete input throws error", {
  expect_error(my_rf_cv(5))
})
