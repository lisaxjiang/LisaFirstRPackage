# within test-f_to_c.R
test_that("my_t.test alternative is less properly", {
  expect_is(my_t.test(my_gapminder[[4]], "less", 60), "list")
})

test_that("my_t.test alternative is greater properly", {
  expect_is(my_t.test(my_gapminder[[4]], "greater", 60), "list")
})

test_that("my_t.test alternative is two.sided properly", {
  expect_is(my_t.test(my_gapminder[[4]], "two.sided", 60), "list")
})

test_that("String input throws error", {
  expect_error(my_t.test(my_gapminder[[4]], 1, 60))
})

test_that("Numeric input throws error", {
  expect_error(my_t.test(my_gapminder[[4]], "less", "mu"))
})
