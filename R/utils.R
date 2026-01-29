#' @importFrom forecast fourier
#' @importFrom caret varImp
pred_func <- function(i, x, y, newxreg, object, freq, fourier_h) {
  newxreg_in <- if (!is.null(newxreg)) newxreg[i,] else NULL
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
  fourier_h <- NULL
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

  if (max_lag <= 0) {
    stop("max_lag must be greater than 0")
  }

  if (max_lag >= length(y)) {
    stop("max_lag must be less than length(y)")
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

  if (test_size <= 0) {
    stop("test_size must be greater than 0")
  }

  if (test_size >= length(y)) {
    stop("test_size must be less than length(y)")
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
                     "xgbDART", "xgbLinear", "ranger", "cubist",
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

#' Perform rolling-origin calibration with recursive forecasting
#'
#' This function computes horizon-specific nonconformity scores by performing
#' rolling-origin evaluation with recursive multi-step forecasting. This ensures
#' proper out-of-sample calibration that respects the exchangeability assumption
#' required for valid conformal prediction intervals.
#'
#' @param y Original time series (untransformed)
#' @param y_modified Transformed time series (Box-Cox if applicable)
#' @param max_lag Maximum lag used in the model
#' @param caret_method The caret method name
#' @param seasonal Logical, whether seasonal terms are used
#' @param K Fourier order for seasonality
#' @param lambda Box-Cox transformation parameter
#' @param pre_process Pre-processing specification
#' @param tune_grid Tuning grid (uses best parameters from initial fit)
#' @param xreg External regressors matrix (optional)
#' @param calibration_horizon Maximum forecast horizon for calibration
#' @param n_windows Number of rolling windows for calibration
#' @param initial_window Initial training window size for calibration
#' @param verbose Logical, print progress
#' @return A list with horizon-indexed vectors of sorted absolute errors
#' @keywords internal
calibrate_horizon_scores <- function(y, y_modified, max_lag, caret_method,
                                      seasonal, K, lambda, pre_process,
                                      tune_grid, xreg = NULL,
                                      calibration_horizon,
                                      n_windows = NULL,
                                      initial_window = NULL,
                                      verbose = FALSE) {

  freq <- stats::frequency(y)
  length_y <- length(y)


  # Determine calibration windows
  # We need enough data for: initial_window + max_lag + calibration_horizon
  min_required <- max_lag + calibration_horizon + 10  # minimum 10 training obs

  if (is.null(initial_window)) {
    # Use 70% of available data as initial window
    initial_window <- max(min_required, floor(0.7 * (length_y - calibration_horizon)))
  }

  if (is.null(n_windows)) {
    # Number of possible windows
    n_windows <- length_y - initial_window - calibration_horizon + 1
    n_windows <- min(n_windows, 50)  # Cap at 50 windows for computational efficiency
  }

  if (n_windows < 2) {
    warning("Not enough data for proper calibration. Using fallback method.")
    return(NULL)
  }

  # Calculate window start positions (evenly spaced if we have more possible than requested)
  max_possible_windows <- length_y - initial_window - calibration_horizon + 1
  if (max_possible_windows > n_windows) {
    window_starts <- round(seq(1, max_possible_windows, length.out = n_windows))
  } else {
    window_starts <- 1:max_possible_windows
    n_windows <- max_possible_windows
  }

  # Initialize storage for errors by horizon
  # Each element will be a vector of absolute errors for that horizon
  horizon_errors <- vector("list", calibration_horizon)
  for (h in 1:calibration_horizon) {
    horizon_errors[[h]] <- numeric(0)
  }

  if (verbose) {
    message(sprintf("Calibrating conformal scores using %d rolling windows...", n_windows))
  }

  for (w_idx in seq_along(window_starts)) {
    w_start <- window_starts[w_idx]
    train_end <- initial_window + w_start - 1
    test_start <- train_end + 1
    test_end <- min(test_start + calibration_horizon - 1, length_y)
    actual_horizon <- test_end - train_end

    if (actual_horizon < 1) next

    # Extract training data
    y_train <- ts(y[1:train_end], start = stats::start(y), frequency = freq)
    y_mod_train <- ts(y_modified[1:train_end], start = stats::start(y_modified),
                      frequency = freq)

    # Extract test actuals
    y_test <- y[(test_start):test_end]

    # Build feature matrix for training
    y_mod_for_lags <- y_mod_train
    modified_y_2 <- ts(y_mod_for_lags[-seq_len(max_lag)],
                       start = time(y_mod_for_lags)[max_lag + 1],
                       frequency = freq)

    train_length <- length(y_mod_for_lags)

    # Determine if seasonal terms should be used
    use_seasonal <- seasonal && freq > 1 && (train_length - max_lag >= freq + 1)

    # Build feature matrix
    if (use_seasonal) {
      if (K == freq / 2) {
        ncolx <- max_lag + K * 2 - 1
      } else {
        ncolx <- max_lag + K * 2
      }
    } else {
      ncolx <- max_lag
    }

    x_train <- matrix(0, nrow = train_length - max_lag, ncol = ncolx)
    x_train[, seq_len(max_lag)] <- lag_maker(y_mod_for_lags, max_lag)

    if (use_seasonal) {
      fourier_s <- forecast::fourier(modified_y_2, K = K)
      x_train[, (max_lag + 1):ncolx] <- fourier_s
      colnames(x_train) <- c(paste0("lag", 1:max_lag), colnames(fourier_s))
    } else {
      colnames(x_train) <- paste0("lag", 1:max_lag)
    }

    # Add xreg if present
    if (!is.null(xreg)) {
      xreg_train <- xreg[1:train_end, , drop = FALSE]
      xreg_train <- xreg_train[-seq_len(max_lag), , drop = FALSE]
      x_train <- cbind(x_train, xreg_train)
    }

    # Train model on this window (with fixed hyperparameters from tune_grid)
    tryCatch({
      model_cal <- caret::train(
        x = x_train,
        y = as.numeric(modified_y_2),
        method = caret_method,
        preProcess = pre_process,
        trControl = caret::trainControl(method = "none"),
        tuneGrid = tune_grid
      )

      # Create a temporary object for recursive forecasting
      temp_object <- list(
        y = y_train,
        y_modified = modified_y_2,
        x = x_train,
        model = model_cal,
        max_lag = max_lag,
        lambda = lambda,
        seasonal = use_seasonal,
        K = if (use_seasonal) K else NULL
      )

      # Prepare xreg for forecasting if needed
      xreg_fc <- NULL
      if (!is.null(xreg)) {
        xreg_fc <- xreg[test_start:test_end, , drop = FALSE]
      }

      # Perform recursive forecasting
      fc_result <- forecast_loop_cal(temp_object, xreg = xreg_fc, h = actual_horizon)
      forecasts <- as.numeric(fc_result$y)

      # Apply inverse Box-Cox if needed
      if (!is.null(lambda)) {
        forecasts <- forecast::InvBoxCox(forecasts, lambda = lambda)
      }

      # Calculate absolute errors for each horizon
      for (h in 1:actual_horizon) {
        abs_error <- abs(forecasts[h] - y_test[h])
        horizon_errors[[h]] <- c(horizon_errors[[h]], abs_error)
      }

    }, error = function(e) {
      if (verbose) {
        message(sprintf("Window %d failed: %s", w_idx, e$message))
      }
    })
  }

  # Check if we have enough calibration samples
  min_samples <- min(sapply(horizon_errors, length))
  if (min_samples < 2) {
    warning("Insufficient calibration samples. Using fallback method.")
    return(NULL)
  }

  # Sort the errors for each horizon (for quantile computation)
  for (h in 1:calibration_horizon) {
    horizon_errors[[h]] <- sort(horizon_errors[[h]])
  }

  names(horizon_errors) <- paste0("h", 1:calibration_horizon)

  if (verbose) {
    message(sprintf("Calibration complete. Samples per horizon: %d to %d",
                    min_samples, max(sapply(horizon_errors, length))))
  }

  return(horizon_errors)
}

#' Internal forecast loop for calibration (without time series attributes)
#' @keywords internal
forecast_loop_cal <- function(object, xreg, h) {
  x <- object$x
  y <- as.numeric(object$y_modified)
  freq <- stats::frequency(object$y_modified)

  fourier_h <- NULL
  if (isTRUE(object$seasonal) && freq > 1 && !is.null(object$K)) {
    fourier_h <- forecast::fourier(object$y_modified, K = object$K, h = h)
  }

  for (i in 1:h) {
    # Build new data point
    newxreg_in <- if (!is.null(xreg)) xreg[i, ] else NULL
    new_data <- c(y[length(y)], x[nrow(x), 1:(object$max_lag - 1)])
    if (object$max_lag == 1) {
      new_data <- new_data[-2]
    }
    if (isTRUE(object$seasonal) && freq > 1 && !is.null(fourier_h)) {
      new_data <- c(new_data, fourier_h[i, ])
    }
    if (!is.null(newxreg_in)) {
      new_data <- c(new_data, newxreg_in)
    }
    new_data <- matrix(new_data, nrow = 1)
    colnames(new_data) <- colnames(x)

    pred <- predict(object$model, newdata = new_data)
    x <- rbind(x, new_data)
    y <- c(y, pred)
  }

  # Extract only the forecasted values
  forecasts <- y[-(1:length(object$y_modified))]

  return(list("x" = x, "y" = forecasts))
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
