#' k-Nearest Neighbors Cross-Validation function
#'
#' This function performs a k-nearest neighbors cross-validation.
#' @param train A given data frame.
#' @param cl True values of the training data \code{train}.
#' @param k_cv An integer indicating the number of folds.
#' @param k_nn An integer indicating the number of neighbors.
#' @keywords prediction
#'
#' @return A list containing (1) a set of the predicted variable for all the
#'   observatoin and (2) a numeric value representing the cross-validation
#'   misclassification error.
#'
#' @examples
#' my_knn_cv(penguins, my_penguins$species, 5, 1)
#' my_knn_cv(penguins, my_penguins$species, 5, 10)
#'
#' @export
#' @import class magrittr randomForest
my_knn_cv <- function(train, cl, k_cv, k_nn) {
  # find predicted values using full data
  predicted <- knn(train = train,
                   cl = cl,
                   test = train,
                   k = k_nn)

  # split data in k_cv parts randomly
  inds <- sample(rep(1:k_cv, length = length(cl)))

  # create empty list to store cv misclassification errors
  cross_val <- rep(NA, k_cv)

  # loop through all folds
  for(i in 1:k_cv) {
    # get train and test data in train
    data_train <- train[inds != i,]
    data_test <- train[inds == i,]
    # train models
    cl_train <- cl[inds != i]
    cl_test <- cl[inds == i]
    # find predicted values in each fold
    fold_knn <- knn(train = data_train,
                    cl = cl_train,
                    test = data_test,
                    k = k_nn)
    # compare outputs with cl_test and calculate cv misclassification errors
    cross_val[i] <- sum(fold_knn == cl_test) / length(cl_test)
  }
  # calculate the average misclassification rate
  cv_error <- mean(cross_val)

  # misclassification rate
  my_list <- list("class" = predicted,
                 "cv_error" = cv_error)
  return(my_list)
}
