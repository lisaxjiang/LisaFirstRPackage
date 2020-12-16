#within test-my_rf_cv.R
test_that("my_rf_cv works", {
  expect_is(my_rf_cv(k = 5), "numeric")
})

test_that("non-numeric input throws error", {
  expect_error(my_rf_cv("string"))
})

test_that("non-numeric input throws error", {
  expect_error(my_rf_cv(my_penguins))
})
