#' T-test function
#'
#' This function performs a sample t-test based on the given input.
#'
#' @param x Numeric set of data.
#' @param alternative A string indicating the t-test type for \code{x} and
#'   \code{mu}, with input choices "less", "greater", and "two.sided".
#' @param mu Numeric value indicating the null hypothesis value of the mean.
#' @keywords prediction
#'
#' @return A list containing a numeric indicating the test statistic, degree
#'   of freedom, a string indicating the type of t-test, and the p-value of
#'   the t-test.
#'
#' @examples
#' my_t.test(my_gapminder[[4]], "less", 1)
#' my_t.test(my_gapminder[[4]], "greater", 3)
#' my_t.test(my_gapminder[[4]], "two.sided", 6)
#'
#' @export
#' @import magrittr
my_t.test <- function(x, alternative, mu) {
  # check t-test type, and send error message when the second parameter is not
  # one of "less", "greater" or "two.sided"
  if(alternative != "less" & alternative != "greater" &
     alternative != "two.sided") {
    stop("The second parameter must be \"less\" or \"greater\" or
         \"two.sided\"")
  }

  # calculate the mean, standard deviation, and sample size of x
  x_mean <- mean(x)
  x_sd <- sd(x)
  x_size <- length(x)

  # get the test statistic and degree of freedom of one sample t-test
  test_stat <- (x_mean - mu) / (x_sd / sqrt(x_size))
  df <- x_size - 1

  # calculate the p-value based on the "alternative" condition
  if(alternative == "less") {
    p_val <- pt(test_stat, df, lower.tail = TRUE)
  } else if(alternative == "greater") {
    p_val <- pt(test_stat, df, lower.tail = FALSE)
  } else if(alternative == "two.sided") {
    if(test_stat < 0) {
      p_val <- 2 * pt(test_stat, df, lower.tail = TRUE)
    } else {
      p_val <- 2 * pt(test_stat, df, lower.tail = FALSE)
    }
  }

  # create and return a list for the 4 return objects of the t-test
  my_list <- list("test_stat" = test_stat,
                  "df" = df,
                  "alternative" = alternative,
                  "p_value" = p_val)
  return(my_list)
}
