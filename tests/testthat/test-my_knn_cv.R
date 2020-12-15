#within test-my_knn_cv.R
penguins <- my_penguins[,3:6]
test_that("my_knn_cv works", {
  expect_is(my_knn_cv(penguins, my_penguins$species, 5, 1), "list")
})

test_that("my_knn_cv works", {
  expect_is(my_knn_cv(penguins, my_penguins$species, 5, 10), "list")
})

test_that("non-list train input throws error", {
  expect_error(my_knn_cv("string", my_penguins$species, 5, 1))
})

test_that("non-list cl input throws error", {
  expect_error(my_knn_cv(penguins, "spring", 5, 1))
})

test_that("non-valid k_nn throws error", {
  expect_error(my_knn_cv(penguins, my_penguins$species, 5, 0))
})

test_that("non-valid k_cv throws error", {
  expect_error(my_knn_cv(penguins, my_penguins$species, 0, 1))
})
