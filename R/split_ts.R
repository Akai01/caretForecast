#' @title Split a time series into training and testing sets
#' @importFrom stats ts
#' @param y Time series
#' @param test_size The number of observations to keep in the test set
#' @author Resul Akay
#'
#'@export

split_ts <- function (y, test_size = 10)
{
  if (!inherits(y, "ts"))
    stop("y must be a time series object.")

  num_train <- length(y) - test_size

  ts_attr = attributes(y)$tsp
  train = stats::ts(y[1:num_train], start = ts_attr[1], freq = ts_attr[3])
  test = stats::ts(y[(num_train + 1):length(y)],
            start = (ts_attr[1] + num_train/ts_attr[3]), freq = ts_attr[3])
  return(list(train = train, test = test))
}
