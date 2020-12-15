#' k-Nearest Neighbors Cross-Validation function
#'
#' This function performs a k-nearest neighbors cross-validation.
#' @param train A given data frame.
#' @param cl True value of the training data \code{train}.
#' @param k_nn An integer indicating the number of neighbors.
#' @param k_cv An integer indicating the number of folds.
#' @keywords prediction
#'
#' @return A list containing (1) a set of the predicted variable for all the
#'   observatoin and (2) a numeric value representing the cross-validation
#'   misclassification error.
#'
#' @examples
#' my_knn_cv(my_penguins, my_gapminder$species, 5, 5)
#' my_knn_cv(my_penguins, my_gapminder$species, 10, 5)
#'
#' @export
#' @import class magrittr randomForest
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # use knn() to find predicted values
  predicted <- knn(train = train,
                   cl = cl,
                   test = train,
                   k = k_nn)

  # split data in k_cv parts randomly
  inds <- sample(rep(1:k_cv, length = length(cl)))
  cross_val <- rep(NA, k_cv)

  # loop through all folds
  for(i in 1:k_cv) {
    # get x_i and x_i*
    data_train <- train[fold != i,]
    data_test <- train[fold == i,]
    # train models
    cl_train <- cl[fold != i]
    cl_test <- cl[fold == i]
    # use knn() to find predicted values in each fold
    fold_knn <- knn(train = data_train,
                      cl = cl_train,
                      test = data_test,
                      k = k_nn)
    # compare outputs and calculate cv misclassification errors
    cross_val[i] <- sum(fold_knn == cl_test) / length(cl_test)
  }
  # calculate the average misclassification rate
  cv_error <- mean(cross_val)

  # create a list of the predicted variables and the average cv
  # misclassification rate
  my_list <- list("class" = predicted,
                 "cv_error" = cv_error)
  return(my_list)
}
