#within test-my_knn_cv.R
penguins <- data.frame("species" = my_penguins[[1]],
                       "bill_length_mm" = my_penguins[[3]],
                       "bill_depth_mm" = my_penguins[[4]],
                       "flipper_length_mm" = my_penguins[[5]],
                       "body_mass_g" = my_penguins[[6]])

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
