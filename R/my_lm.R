#' Linear model function
#'
#' This function fits a linear model according to a function and a data frame,
#'   and returns a summary table.
#' @param formula A formula class object of the relationship of a response
#'   variable and explanatory variables in the columns of \code{data}.
#' @param data Input data frame containing a response variable and
#'   explanatory variables.
#' @keywords prediction
#'
#' @return A table given by the lm() function.
#'
#' @examples
#' my_lm(lifeExp ~ gdpPercap + continent, data = my_gapminder)
#'
#' @export
#' @import magrittr
my_lm <- function(formula, data) {
  # get the response variable
  Y <- model.response(model.frame(formula, data))
  # get the independent variables
  X <- model.matrix(formula, data)

  # get the transpose matrix of X
  X_t <- t(X)
  # get the linear coefficients
  beta <- solve(X_t %*% X) %*% X_t %*% Y

  # calculate degree of freedom
  df <- nrow(X) - ncol(X)

  # calculate the variance and the standard error
  var <- sum((Y - X %*% beta)^2 / df)
  se <- sqrt(diag(var * solve(X_t %*% X)))
  # calcualte the test statistic for beta
  test_stat <- (beta - 0) / se

  # get the p-value from the two sided t-test
  p_val <- 2 * pt(abs(test_stat), df, lower.tail = FALSE)

  # generate a table for output components
  my_table <- as.table(cbind(beta, se, test_stat, p_val))
  # add column names to the output table
  colnames(my_table) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  # return table
  return(my_table)
}
