#' Autoregressive forecasting using various Machine Learning models.
#'
#' @importFrom methods is
#' @importFrom stats frequency is.ts predict sd start time ts
#' @importFrom forecast is.constant na.interp BoxCox.lambda BoxCox InvBoxCox
#' @importFrom caret train trainControl
#' @param y  A univariate time series object.
#' @param xreg Optionally, a numerical vector or matrix of external regressors,
#' which must have the same number of rows as y.
#'  (It should not be a data frame.).
#' @param max_lag Maximum value of lag.
#' @param caret_method A string specifying which classification or
#' regression model to use.
#' Possible values are found using names(getModelInfo()).
#' A list of functions can also be passed for a custom model function.
#' See \url{http://topepo.github.io/caret/} for details.
#' @param metric A string that specifies what summary metric will be used to
#' select the optimal model. See \code{?caret::train}.
#' @param pre_process A string vector that defines a pre-processing of the
#' predictor data.
#' Current possibilities are "BoxCox", "YeoJohnson", "expoTrans", "center",
#' "scale", "range",
#' "knnImpute", "bagImpute", "medianImpute", "pca", "ica" and "spatialSign".
#' The default is no pre-processing.
#' See preProcess and trainControl on the procedures and how to adjust them.
#'  Pre-processing code is only designed to work when x is a simple matrix or
#'   data frame.
#' @param cv Logical, if \code{cv = TRUE} model selection will be done via
#' cross-validation. If \code{cv = FALSE} user need to provide a specific model
#' via \code{tune_grid} argument.
#' @param cv_horizon The number of consecutive values in test set sample.
#' @param initial_window The initial number of consecutive values in each
#' training set sample.
#' @param fixed_window Logical, if FALSE, all training samples start at 1.
#' @param verbose A logical for printing a training log.
#' @param seasonal Boolean. If \code{seasonal = TRUE} the fourier terms will be
#'  used for modeling seasonality.
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
#' @param ... Ignored.
#' @return A list class of forecast containing the following elemets
#' * x : The input time series
#' * method : The name of the forecasting method as a character string
#' * mean : Point forecasts as a time series
#' * lower : Lower limits for prediction intervals
#' * upper : Upper limits for prediction intervals
#' * level : The confidence values associated with the prediction intervals
#' * model : A list containing information about the fitted model
#' * newx : A matrix containing regressors
#' @author Resul Akay
#'
#' @examples
#' \dontrun{
#'
#'library(caretForecast)
#'
#'train_data <- window(AirPassengers, end = c(1959, 12))
#'
#'test <- window(AirPassengers, start = c(1960, 1))
#'
#'ARml(train_data, caret_method = "svmLinear2", max_lag = 12) -> fit
#'
#'forecast(fit, h = length(test), level = NULL, PI = T) -> fc
#'
#'fc_plot(fc)+ autolayer(test)
#'
#'accuracy(fc, test)
#' }
#' @export

ARml <- function(y,
                 max_lag = 5,
                 xreg = NULL,
                 caret_method = "cubist",
                 metric = "RMSE",
                 pre_process = NULL,
                 cv = TRUE,
                 cv_horizon = 4,
                 initial_window = length(y) - max_lag -  cv_horizon*2,
                 fixed_window = FALSE,
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

  length_y <- length(y)

  freq <- stats::frequency(y)

  if(length_y < freq){
    stop("Not enough data to fit a model")
  }

  if(max_lag <= 0){
    warning("max_lag increased to 1. max_lag must be max_lag >= 1")
    max_lag <- 1
  }

  if(c(length_y - freq - round(freq / 4)) < max_lag){

    warning(paste("Input data is too short. Reducing max_lags to ",
                  round(length_y - freq - round(freq / 4))))
    max_lag <- round(length_y - freq - round(freq / 4))
  }

  if (max_lag != round(max_lag)){
    max_lag <- round(max_lag)

    message(paste("max_lag must be an integer, max_lag rounded to", max_lag))
  }

  if(!is.null(xreg)){
    if("matrix" %notin% class(xreg)){
      xreg <- as.matrix(xreg)
    }
  }

  constant_y <- forecast::is.constant(forecast::na.interp(y))

  if(constant_y) {
    warning("Constant data, setting max_lag = 1, lambda = NULL")
    lambda <- NULL
    max_lag = 1
  }

  if(!is.null(xreg))
  {
    ncolxreg <- ncol(xreg)
  }

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

  modified_y_2 <- ts(modified_y[-(1:(max_lag))], start = time(modified_y)[max_lag + 1],
           frequency = freq)

  if(seasonal == TRUE | freq > 1)
  {
    ncolx <- max_lag + K * 2
    }
  if(seasonal == FALSE | freq == 1)
    {
    ncolx <- max_lag
  }

  x <- matrix(0, nrow = c(length_y - max_lag), ncol = ncolx)

  x[ , 1:max_lag] <- lag_maker(modified_y, max_lag)

  if(seasonal == TRUE & freq > 1)
    {
    fourier_s <- fourier(modified_y_2, K = K)
    x[ , (max_lag + 1):ncolx] <- fourier_s
    colnames(x) <- c(paste0("lag", 1:max_lag), colnames(fourier_s))
  }

  if(seasonal == FALSE | freq == 1)
    {
    colnames(x) <- c(paste0("lag", 1:max_lag))
  }

  if(!is.null(xreg)){
    xreg <- xreg[-(1:max_lag),]
    x <- cbind(x, xreg[ , , drop = FALSE])
  }

  training_method <- "timeslice"

  if(!cv){
    training_method <- "none"
    if(is.null(tune_grid)){
      stop("Only one model should be specified in tune_grid with no resampling")
    }
  }

  model <- caret::train(x = x,
                      y = as.numeric(modified_y_2),
                      method = caret_method,
                      preProcess = pre_process,
                      weights = NULL,
                      metric =  metric,
                      trControl = caret::trainControl(
                        method = training_method,
                        initialWindow = initial_window,
                        horizon = cv_horizon,
                        fixedWindow = fixed_window,
                        verboseIter = verbose,
                        allowParallel = allow_parallel),
                      tuneGrid = tune_grid)

  fitted <- ts(c(rep(NA, max_lag),
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

  method <- paste0("ARml(", max_lag, paste(", ", K_m), ")")

  output <- list(
    y =  y,
    y_modified = modified_y_2,
    x = x,
    model = model,
    fitted = fitted,
    max_lag = max_lag,
    lambda = lambda,
    seasonal = seasonal,
    BoxCox_biasadj = BoxCox_biasadj,
    BoxCox_fvar = BoxCox_fvar,
    method = paste0("caret method ",caret_method, " with ", method)
  )

  if(seasonal == TRUE & freq > 1)
    {
    output$fourier_s <- fourier_s
    output$K <- K
  }
  output$xreg_fit <- NULL
  if(!is.null(xreg)){
    output$xreg_fit <- xreg
  }
  class(output) <- "ARml"
  return(output)
}
