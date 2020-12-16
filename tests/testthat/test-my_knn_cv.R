#within test-my_knn_cv.R
my_penguins <- na.omit(my_penguins)
test_that("my_knn_cv works", {
  expect_is(my_knn_cv(my_penguins[,3:6], my_penguins$species, 5, 1), "list")
})

test_that("my_knn_cv works", {
  expect_is(my_knn_cv(my_penguins[,3:6], my_penguins$species, 5, 10), "list")
})

test_that("non-list train input throws error", {
  expect_error(my_knn_cv("string", my_penguins$species, 5, 1))
})

test_that("non-list cl input throws error", {
  expect_error(my_knn_cv(my_penguins[,3:6], "spring", 5, 1))
})

test_that("non-numeric k_cv input throws error", {
  expect_error(my_knn_cv(my_penguins[,3:6], my_penguins$species, "string", 1))
})

test_that("non-numeric k_nn input throws error", {
  expect_error(my_knn_cv(my_penguins[,3:6], my_penguins$species, 5, "string"))
})
