#' @title Split a time series into training and testing sets
#' @importFrom stats ts start frequency
#' @param y A univariate time series
#' @param test_size The number of observations to keep in the test set
#' @return
#' A list with train and test elements
#' @author Resul Akay
#' @examples
#' \dontrun{
#'
#' dlist <- split_ts(retail_wide[,1], test_size = 12)
#'
#' }
#'
#'
#'@export
split_ts <- function (y, test_size = 10)
{
  if ("ts" %notin% class(y) | "mts" %in% class(y)){

    stop("y must be a univariate time series class of 'ts'")
      }

  num_train <- length(y) - test_size

  train_start <- stats::start(y)

  freq <- stats::frequency(y)

  test_start <- min(time(y)) + num_train/freq

  train = stats::ts(y[1:num_train], start = train_start, frequency = freq)

  test = stats::ts(y[(num_train + 1):length(y)],
                   start = test_start, frequency =freq)

  output <- list("train" = train, "test" = test)

  return(output)
}
