#' Autoregressive forecasting using various Machine Learning models.
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' Autoregressive forecasting using various Machine Learning models.
#'
#' @import forecast
#' @import caret
#' @import tseries
#' @importFrom methods is
#' @importFrom stats frequency is.ts predict sd start time ts
#' @import lifecycle
#' @param y  A univariate time series object
#' @param xreg Optionally, a numerical vector or matrix of external regressors,
#' which must have the same number of rows as y.
#'  (It should not be a data frame.)
#' @param maxlag Maximum value of lag
#' @param caret_method A string specifying which classification or
#' regression model to use.
#' Possible values are found using names(getModelInfo()).
#' See \url{http://topepo.github.io/caret/train-models-by-tag.html}.
#' A list of functions can also be passed for a custom model function.
#' See \url{http://topepo.github.io/caret/using-your-own-model-in-train.html}
#' for details.
#' @param pre_process A string vector that defines a pre-processing of the
#' predictor data.
#' Current possibilities are "BoxCox", "YeoJohnson", "expoTrans", "center",
#' "scale", "range",
#' "knnImpute", "bagImpute", "medianImpute", "pca", "ica" and "spatialSign".
#' The default is no pre-processing.
#' See preProcess and trainControl on the procedures and how to adjust them.
#'  Pre-processing code is only designed to work when x is a simple matrix or
#'   data frame.
#' @param cv_horizon The number of consecutive values in test set sample
#' @param initial_window The initial number of consecutive values in each
#' training set sample
#' @param fixed_window Logical, if FALSE, all training samples start at 1
#' @param verbose A logical for printing a training log
#' @param K Maximum order(s) of Fourier terms
#' @param tune_grid A data frame with possible tuning values.
#' The columns are named the same as the tuning parameters.
#' Use getModelInfo to get a list of tuning parameters for each model or
#' see \url{http://topepo.github.io/caret/available-models.html}.
#' (NOTE: If given, this argument must be named.)
#' @param lambda BoxCox transformation parameter. If \code{lambda = NULL}
#' If \code{lambda = "auto"}, then the transformation parameter lambda is chosen
#'  using \code{\link[forecast]{BoxCox.lambda}}.
#' @param BoxCox_method \code{\link[forecast]{BoxCox.lambda}} argument.
#' Choose method to be used in calculating lambda.
#' @param BoxCox_lower \code{\link[forecast]{BoxCox.lambda}} argument.
#' Lower limit for possible lambda values.
#' @param BoxCox_upper \code{\link[forecast]{BoxCox.lambda}} argument.
#' Upper limit for possible lambda values.
#' @param BoxCox_biasadj \code{\link[forecast]{InvBoxCox}} argument.
#' Use adjusted back-transformed mean for Box-Cox transformations.
#' If transformed data is used to produce forecasts and fitted values,
#' a regular back transformation will result in median forecasts.
#' If biasadj is TRUE, an adjustment will be made to produce mean
#' forecasts and fitted values.
#' @param BoxCox_fvar \code{\link[forecast]{InvBoxCox}} argument.
#' Optional parameter required if biasadj=TRUE.
#' Can either be the forecast variance, or a list containing the interval level,
#'  and the corresponding upper and lower intervals.
#' @param allow_parallel If a parallel backend is loaded and available,
#' should the function use it?
#' @param ... Ignored
#' @return A list class of forecast containing the following elemets
#' \item{x}{The input time series}
#' \item{method}{The name of the forecasting method as a character string}
#' \item{mean}{Point forecasts as a time series}
#' \item{lower}{Lower limits for prediction intervals}
#' \item{upper}{Upper limits for prediction intervals}
#' \item{level}{The confidence values associated with the prediction intervals}
#' \item{model}{A list containing information about the fitted model}
#' \item{newx}{A matrix containing regressors}
#' @author Resul Akay
#' @note See \code{\link[forecast]{nnetar}} and \code{forecastxgb}
#' @examples
#' \dontrun{
#' library(forecast)
#' library(caretForecast)
#' train <- window(AirPassengers, end = c(1959, 12))
#'
#' test <- window(AirPassengers, start = c(1960, 1))
#'
#' ARml(train, caret_method = "svmLinear2", maxlag = 12, trend_method = "none",
#'  pre_process = "center") -> fit
#'
#' forecast(fit, h = length(test), level = NULL, PI = T) -> fc
#' plot(fc)+ autolayer(test)
#' accuracy(fc, test)
#' }
#' @export

ARml <- function(y,
                 xreg = NULL,
                 maxlag = 5,
                 caret_method = "cubist",
                 pre_process = NULL,
                 cv_horizon = 4,
                 initial_window = length(y) - maxlag -  cv_horizon*2,
                 fixed_window =FALSE,
                 verbose = TRUE,
                 seasonal = TRUE,
                 K =  frequency(y)/2 -1,
                 tune_grid = NULL,
                 lambda = "auto",
                 BoxCox_method = c("guerrero", "loglik"),
                 BoxCox_lower = -1,
                 BoxCox_upper = 2,
                 BoxCox_biasadj = FALSE,
                 BoxCox_fvar = NULL,
                 allow_parallel = FALSE,
                 ...){


  if("ts" %notin% class(y)){
    stop("y must be a univariate time series")
  }

  if(!is.null(xreg)){
    if("matrix" %notin% class(xreg)){
      xreg <- as.matrix(xreg)
    }
  }

  freq <- stats::frequency(y)

  original_y <- y

  if(is.null(lambda)){
    modified_y <- y
  }
  if(!is.null(lambda)){
  if(lambda=="auto"){
    lambda  <- forecast::BoxCox.lambda(y, method = BoxCox_method,
                                       lower = BoxCox_lower,
                                       upper = BoxCox_upper)
    modified_y <- forecast::BoxCox(y, lambda)
  }

  if(is.numeric(lambda)){
    modified_y <- forecast::BoxCox(y, lambda)
  }
}
  length_y <- length(y)

  if(length_y < 6){
    stop("Your time series is too short. Length of y must be > 6")
  }

  if(maxlag > (length_y - freq - round(freq / 4))){

    warning(paste("Input data is too short. Reducing maxlags to",
                  length_y - freq - round(freq / 4)))
    maxlag <- length_y - freq - round(freq / 4)
  }

  if (maxlag != round(maxlag)){
    maxlag <- round(maxlag)
    if(verbose)
    {
      message(paste("maxlag must be an integer, maxlag rounded to", maxlag))
      }
  }

  origxreg <- xreg

  n <- length_y - maxlag

  y2 <- ts(modified_y[-(1:(maxlag))], start = time(modified_y)[maxlag + 1],
           frequency = freq)

  if(seasonal == TRUE | freq > 1)
  {
    ncolx <- maxlag + K * 2
    }
  if(seasonal == FALSE | freq == 1)
    {
    ncolx <- maxlag
    }

  x <- matrix(0, nrow = n, ncol = ncolx)

  x[ , 1:maxlag] <- lag_maker(modified_y, maxlag, keeporig = FALSE)


  if(seasonal == TRUE & freq > 1)
    {
    fx <- fourier(y2, K = K)
    x[ , (maxlag + 1):ncolx] <- fx
    colnames(x) <- c(paste0("lag", 1:maxlag), colnames(fx))
  }

  if(seasonal == FALSE | freq == 1)
    {
    colnames(x) <- c(paste0("lag", 1:maxlag))
  }

  if(!is.null(xreg)){
    xreg <- xreg[-(1:maxlag),]
    x <- cbind(x, xreg[ , , drop = FALSE])
  }


  model <- caret::train(x = x,
                      y = as.numeric(y2),
                      method = caret_method,
                      preProcess = pre_process,
                      weights = NULL,
                      metric =  "RMSE",
                      trControl = caret::trainControl(
                        method = "timeslice",
                        initialWindow = initial_window,
                        horizon = cv_horizon,
                        fixedWindow = fixed_window,
                        verboseIter = verbose,
                        allowParallel = allow_parallel),
                      tuneGrid = tune_grid)

  fitted <- ts(c(rep(NA, maxlag),
                 predict(model, newdata = x)),
               frequency = freq, start = min(time(modified_y)))

  if(!is.null(lambda)){
    fitted <- forecast::InvBoxCox(fitted, lambda = lambda,
                                  biasadj = BoxCox_biasadj,
                                fvar = BoxCox_fvar)
    }

  K_m <- 0

  if(seasonal == TRUE & freq > 1)
  {
    K_m <- K
  }
  method <- paste0("ARml(", maxlag, paste(", ", K_m), ")")

  output <- list(
    y =  original_y,
    y2 = y2,
    x = x,
    model = model,
    fitted = fitted,
    maxlag = maxlag,
    lambda = lambda,
    seasonal = seasonal,
    BoxCox_biasadj = BoxCox_biasadj,
    BoxCox_fvar = BoxCox_fvar,
    method = paste0("caret method ",caret_method, " with ", method)
  )
  if(seasonal == TRUE & freq > 1)
    {
    output$fx <- fx
    output$K <- K
    }
  if(!is.null(xreg)){
    output$ origxreg = origxreg
    output$ncolxreg <- ncol(origxreg)
  }
  class(output) <- "ARml"
  return(output)
}