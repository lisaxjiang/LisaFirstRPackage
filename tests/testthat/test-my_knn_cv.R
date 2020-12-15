#within test-my_knn_cv.R
test_that("my_knn_cv works", {
  expect_is(my_knn_cv(penguins, penguins$species, 1, 5), "list")
})

test_that("my_knn_cv works", {
  expect_is(my_knn_cv(penguins, penguins$species, 10, 5), "list")
})

test_that("non-list train input throws error", {
  expect_error(my_knn_cv("string", penguins$species, 1, 5))
})

test_that("non-list cl input throws error", {
  expect_error(my_knn_cv(penguins, "spring", 1, 5))
})
