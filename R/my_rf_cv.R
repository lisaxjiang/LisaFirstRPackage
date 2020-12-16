#' Random Forest Cross-Validation function
#'
#' This function performs a \code{k} fold random forest cross-validation
#'   on the penguin data.
#' @param train A given data frame.
#' @param cl True values of the training data \code{train}.
#' @param k A numeric indicating the number of folds.
#' @keywords prediction
#'
#' @return A numeric indicating the mean square error.
#'
#' @examples
#' my_rf_cv(2)
#' my_rf_cv(5)
#' my_rf_cv(10)
#'
#' @export
#' @import class magrittr randomForest
my_rf_cv <- function(k) {
  # stop the program if there are any invalid inputs
  if(!is.numeric(k) || k < 1) {
    stop("\"k\" must be an integer larger than zero")
  }

  # omit NA's in my_penguins
  my_penguins <- na.omit(my_penguins)
  # generate a list of integers from 1 to k of length cl randomly
  fold <- sample(rep(1:k, length = ncol(my_penguins)))

  # # create empty list to store cv estimated mean sqaured error
  mse_val <- rep(NA, k)

  # loop through all folds
  for (i in 1:k) {
    # get train and test data in train variables
    data_train <- my_penguins[fold != i,]
    data_test <-  my_penguins[fold == i,]
    # train models
    cl_train <- my_penguins$body_mass_g[fold != i]
    cl_test <- my_penguins$body_mass_g[fold == i]
    # build model using random forest
    my_model <- randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm +
                                           flipper_length_mm,
                             data = data_train, ntree = 200)
    # use model to generate a list of predictions
    predictions <- predict(my_model, data_test[, -1])
    # calculate the mean squared error
    mse_val[i] <- mean((predictions - cl_test)^2)
  }
  # find average MSE
  MSE <- mean(mse_val)
  # return MSE
  return(MSE)
}
