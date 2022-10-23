#' @importFrom forecast fourier
#' @importFrom caret varImp
pred_func <- function(i, x, y, newxreg, object, freq, fourier_h) {
  newxreg_in <- newxreg[i,]
  new_data <- c(y[length(y)], x[nrow(x), 1:(object$max_lag - 1)])
  if (object$max_lag == 1) {
    new_data = new_data[-2]
  }
  if (object$seasonal == TRUE & freq > 1)
  {
    new_data <- c(new_data, fourier_h[i, ])
  }
  if (!is.null(newxreg_in)) {
    new_data <- c(new_data, newxreg_in)
  }
  new_data <- matrix(new_data, nrow = 1)
  colnames(new_data) <- colnames(x)
  pred <- predict(object$model, newdata = new_data)
  return(list("x" = rbind(x, new_data),
              "y" = c(y, pred)))
}

forecast_loop <- function(object, xreg, h) {
  x <- object$x
  y <- object$y_modified
  freq <- stats::frequency(object$y_modified)
  if (object$seasonal == TRUE & freq > 1)
  {
    fourier_h <-
      forecast::fourier(object$y_modified, K = object$K, h = h)
  }
  for (i in 1:h) {
    fc_x <- pred_func(
      i,
      x = x,
      y = y,
      newxreg = xreg,
      object = object,
      freq = freq,
      fourier_h = fourier_h
    )
    x <- fc_x$x
    y <- fc_x$y
  }
  y <- ts(y[-(1:length(object$y_modified))],
          frequency = freq,
          start = max(time(object$y)) + 1 / freq)
  x <- x[-(1:nrow(object$x)),]

  return(list("x" = x,
              "y" = y))
}

#' @title Variable importance for forecasting model.
#'
#' @param object A list class of ARml or forecast object derived from ARml
#' @param plot Boolean, if TRUE, variable importance will be ploted.
#' @return A list class of "varImp.train". See \code{\link[caret]{varImp}} or a
#' "trellis" plot.
#' @author Resul Akay
#' @examples
#'
#' train <- window(AirPassengers, end = c(1959, 12))
#'
#' test <- window(AirPassengers, start = c(1960, 1))
#'
#' ARml(train, caret_method = "lm", max_lag = 12, trend_method = "none",
#'  pre_process = "center") -> fit
#'
#' forecast(fit, h = length(test), level = c(80,95)) -> fc
#'
#' autoplot(fc)+ autolayer(test)
#'
#' accuracy(fc, test)
#'
#' get_var_imp(fc, plot = TRUE)
#'
#'
#' @export

get_var_imp <- function(object, plot = TRUE) {
  if ("forecastARml" %notin% class(object)) {
    stop("object must be an forecastARml or ARml object")
  }
  if (plot) {
    return(plot(varImp(object$model)))
  }
  if (!plot) {
    return(varImp(object$model))
  }
}

lag_maker <- function(y, max_lag) {
  if ("ts" %notin% class(y)) {
    stop("y must be a 'ts' object")
  }

  max_lag1 <- round(max_lag)
  if (max_lag1 != max_lag) {
    message(
      paste(
        "'max_lag' should not be a fractional number.",
        "'max_lag' rounde to",
        max_lag1,
        sep = " "
      )
    )
  }
  length_y <- length(y)
  n_col <- max_lag1 + 1
  dta <- apply(
    array(seq(
      from = 1, to = n_col, by = 1
    )),
    1,
    FUN = function(i) {
      y[(max_lag1 + 2 - i):(length_y + 1 - i)]
    }
  )

  colnames(dta) <-
    c("y", paste0("y_lag", seq(
      from = 1, to = max_lag1, by = 1
    )))

  dta <- dta[,-1]

  return(dta)
}

`%notin%` <- Negate(`%in%`)

#' Sales data from an Australian Retailer in time series format
#'
#' A dataset containing 42 products' sales
#'
#' @format
#' An object of class mts (inherits from ts, matrix)
#' with 333 rows and 43 columns.
#' \describe{
#'   This data set is the wide format of \code{\link{retail}} data.
#' }
#' @source \url{https://robjhyndman.com/data/ausretail.csv}
"retail_wide"

#' Grouped sales data from an Australian Retailer
#'
#' A dataset containing 42 products' sales
#'
#' @format A data class of "tbl_df", "tbl", "data.frame" with 13986 rows and 3 columns:
#' \describe{
#'   \item{date}{date}
#'   \item{item}{products}
#'   \item{value}{sales}
#' }
#' @source \url{https://robjhyndman.com/data/ausretail.csv}
"retail"

#' @title Split a time series into training and testing sets
#' @importFrom stats ts start frequency
#' @param y A univariate time series
#' @param test_size The number of observations to keep in the test set
#' @return
#' A list with train and test elements
#' @author Resul Akay
#' @examples
#'
#' dlist <- split_ts(retail_wide[,1], test_size = 12)
#'
#'@export
split_ts <- function (y, test_size = 10) {
  if ("ts" %notin% class(y) | "mts" %in% class(y)) {
    stop("y must be a univariate time series class of 'ts'")
  }
  num_train <- length(y) - test_size
  train_start <- stats::start(y)
  freq <- stats::frequency(y)
  test_start <- min(time(y)) + num_train / freq
  train = stats::ts(y[1:num_train], start = train_start, frequency = freq)
  test = stats::ts(y[(num_train + 1):length(y)], start = test_start,
                   frequency = freq)
  output <- list("train" = train, "test" = test)
  return(output)
}

#' @title Suggested methods for ARml
#' @return A character vector of Suggested methods
#' @author Resul Akay
#' @examples
#'
#' suggested_methods()
#'
#' @export
suggested_methods <- function() {
  message("In general user can train any method which supported by caret.
          \nThe following methods are suggested"
          )
  caret_methods <- c("spikeslab", "bagEarth", "bagEarthGCV", "blasso",
                     "cforest", "earth","extraTrees", "gbm_h2o", "glmStepAIC",
                     "parRF", "qrf", "Rborist", "rf", "rqlasso", "rqnc",
                     "spikeslab", "xgbDART", "xgbLinear", "ranger", "cubist",
                     "svmLinear", "enet", "bridge", "glmboost", "ridge",
                     "lasso", "relaxo", "M5Rules", "M5", "lm", "gaussprLinear",
                     "glm", "glmnet", "pcr", "ppr", "foba", "gbm", "svmLinear2",
                     "glm.nb", "gcvEarth", "lars2", "lars", "icr", "ctree2",
                     "ctree", "bayesglm")

  return(caret_methods)
}

#' @export
residuals.ARml <- function(object, ...){
  res <- object$y - object$fitted
  return(res)
}

conformal_intervals <- function(residuals, y_hat, level){
  level <- c(level/100)
  conf_reg <- conformalRegressor(residuals)
  conf_pred <- predict(conf_reg, y_hat = y_hat, confidence = level)
  upper <- ts(dplyr::select(conf_pred, dplyr::starts_with("upper_")))
  tsp(upper) <- tsp(y_hat)
  lower <- ts(dplyr::select(conf_pred, dplyr::starts_with("lower_")))
  tsp(lower) <- tsp(y_hat)
  out <- list(upper = upper, lower = lower)
return(out)
}

#' @importFrom forecast autoplot
#' @export
forecast::autoplot

#' @importFrom forecast autolayer
#' @export
forecast::autolayer

#' @importFrom generics accuracy
#' @export
generics::accuracy

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom magrittr %<>%
#' @export
magrittr::`%<>%`
