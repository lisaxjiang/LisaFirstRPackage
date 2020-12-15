#within test-my_lm.R
test_that("my_lm works", {
  expect_is(my_lm(lifeExp ~ gdpPercap + continent, data = my_gapminder), "table")
})

test_that("non-table input throws error", {
  expect_error(my_lm(1, "string"))
})
