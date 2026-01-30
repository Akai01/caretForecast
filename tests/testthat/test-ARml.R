# A unit test for ARml function
if(require(testthat)){
  test_that("tests for some arguments in ARml", {
    ARml(AirPassengers, caret_method = "lm", max_lag = 12,  K = 5) -> fit

    class_fit <- class(fit)

    expect_that(class_fit, equals("ARml"))

  })
}

if(require(testthat)){
  test_that("tests for some arguments in ARml", {
    ARml(AirPassengers, caret_method = "lm", max_lag = 12, lambda = NULL,
         K= 5) -> fit

    class_fit <- class(fit)

    expect_that(class_fit, equals("ARml"))

  })
}

if(require(testthat)){
  test_that("tests for some arguments in ARml", {
    ARml(AirPassengers, caret_method = "lm", max_lag = 12, lambda = "auto",
         BoxCox_method = "loglik", K = 5) -> fit

    class_fit <- class(fit)

    expect_that(class_fit, equals("ARml"))

  })
}


if(require(testthat)){
  test_that("tests for some arguments in ARml", {
    ARml(AirPassengers, caret_method = "lm", max_lag = 10,
         xreg = forecast::seasonaldummy(AirPassengers), seasonal = F) -> fit

    class_fit <- class(fit)

    expect_that(class_fit, equals("ARml"))

  })
}

if(require(testthat)){
  test_that("ARml works with max_lag = 1", {
    # Test fitting with max_lag = 1
    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 1,
                seasonal = FALSE, verbose = FALSE, calibrate = FALSE)

    expect_s3_class(fit, "ARml")
    expect_equal(fit$max_lag, 1)

    # Test forecasting with max_lag = 1 (single step)
    fc <- forecast(fit, h = 1)
    expect_s3_class(fc, "forecast")
    expect_equal(length(fc$mean), 1)

    # Test forecasting multiple steps with max_lag = 1
    fc_multi <- forecast(fit, h = 5)
    expect_s3_class(fc_multi, "forecast")
    expect_equal(length(fc_multi$mean), 5)
    expect_false(any(is.na(fc_multi$mean)))
  })
}

if(require(testthat)){
  test_that("ARml works with max_lag = 1 and xreg", {
    n <- length(AirPassengers)
    xreg <- matrix(rnorm(n), ncol = 1)
    colnames(xreg) <- "x1"

    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 1,
                xreg = xreg, seasonal = FALSE, verbose = FALSE, calibrate = FALSE)

    expect_s3_class(fit, "ARml")
    expect_equal(fit$max_lag, 1)

    # Forecast with new xreg
    new_xreg <- matrix(rnorm(5), ncol = 1)
    colnames(new_xreg) <- "x1"

    fc <- forecast(fit, xreg = new_xreg)
    expect_s3_class(fc, "forecast")
    expect_equal(length(fc$mean), 5)
  })
}

if(require(testthat)){
  test_that("ARml with xreg and calibration preserves xreg correctly", {
    set.seed(123)
    n <- length(AirPassengers)
    xreg <- matrix(rnorm(n), ncol = 1)
    colnames(xreg) <- "x1"

    # This tests that xreg_original is properly saved before truncation
    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 3,
                xreg = xreg, seasonal = FALSE, verbose = FALSE,
                calibrate = TRUE, calibration_horizon = 3, n_cal_windows = 3)

    expect_s3_class(fit, "ARml")

    # Forecast should work
    new_xreg <- matrix(rnorm(4), ncol = 1)
    colnames(new_xreg) <- "x1"

    fc <- forecast(fit, xreg = new_xreg)
    expect_s3_class(fc, "forecast")
    expect_equal(length(fc$mean), 4)
  })
}

if(require(testthat)){
  test_that("ARml with lambda = 'auto' applies BoxCox correctly", {
    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 5,
                lambda = "auto", verbose = FALSE, calibrate = FALSE)

    expect_s3_class(fit, "ARml")
    expect_true(is.numeric(fit$lambda))
    expect_true(fit$lambda >= -1 && fit$lambda <= 2)

    fc <- forecast(fit, h = 5)
    expect_s3_class(fc, "forecast")
    expect_equal(length(fc$mean), 5)
    # Forecasts should be positive (back-transformed)
    expect_true(all(fc$mean > 0))
  })
}

if(require(testthat)){
  test_that("ARml with numeric lambda applies BoxCox correctly", {
    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 5,
                lambda = 0.5, verbose = FALSE, calibrate = FALSE)

    expect_s3_class(fit, "ARml")
    expect_equal(fit$lambda, 0.5)

    fc <- forecast(fit, h = 5)
    expect_s3_class(fc, "forecast")
    expect_equal(length(fc$mean), 5)
  })
}

if(require(testthat)){
  test_that("single step forecast (h = 1) works correctly", {
    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 5,
                verbose = FALSE, calibrate = FALSE)

    # Single step forecast
    fc <- forecast(fit, h = 1)

    expect_s3_class(fc, "forecast")
    expect_equal(length(fc$mean), 1)
    expect_false(is.na(fc$mean))

    # newxreg should be a matrix, not a vector
    expect_true(is.matrix(fc$newxreg) || is.null(fc$newxreg) || NROW(fc$newxreg) == 1)
  })
}

# Test cv = FALSE requires tune_grid
if(require(testthat)){
  test_that("ARml with cv = FALSE requires tune_grid", {
    expect_error(
      ARml(AirPassengers, caret_method = "lm", max_lag = 5,
           cv = FALSE, verbose = FALSE, calibrate = FALSE),
      "Only one model should be specified in tune_grid"
    )
  })
}

# Test cv = FALSE with tune_grid works
if(require(testthat)){
  test_that("ARml with cv = FALSE and tune_grid works", {
    tune_grid <- data.frame(intercept = TRUE)
    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 5,
                cv = FALSE, tune_grid = tune_grid,
                verbose = FALSE, calibrate = FALSE)

    expect_s3_class(fit, "ARml")
  })
}

# Test non-ts input error
if(require(testthat)){
  test_that("ARml rejects non-ts input", {
    expect_error(
      ARml(c(1, 2, 3, 4, 5), caret_method = "lm"),
      "y must be a univariate time series"
    )
  })
}

# Test fractional max_lag gets rounded
if(require(testthat)){
  test_that("ARml rounds fractional max_lag", {
    expect_message(
      fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 5.7,
                  verbose = FALSE, calibrate = FALSE),
      "max_lag must be an integer"
    )
    expect_equal(fit$max_lag, 6)
  })
}

# Test max_lag <= 0 warning
if(require(testthat)){
  test_that("ARml warns and corrects max_lag <= 0", {
    expect_warning(
      fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 0,
                  verbose = FALSE, calibrate = FALSE),
      "max_lag increased to 1"
    )
    expect_equal(fit$max_lag, 1)
  })
}

# Test constant data handling
if(require(testthat)){
  test_that("ARml handles constant data", {
    constant_ts <- ts(rep(5, 20), frequency = 1)
    expect_warning(
      fit <- ARml(constant_ts, caret_method = "lm", max_lag = 3,
                  verbose = FALSE, calibrate = FALSE),
      "Constant data"
    )
    expect_s3_class(fit, "ARml")
    expect_equal(fit$max_lag, 1)
  })
}

# Test non-seasonal data (frequency = 1)
if(require(testthat)){
  test_that("ARml works with non-seasonal data (frequency = 1)", {
    non_seasonal <- ts(cumsum(rnorm(50)), frequency = 1)
    fit <- ARml(non_seasonal, caret_method = "lm", max_lag = 5,
                verbose = FALSE, calibrate = FALSE)

    expect_s3_class(fit, "ARml")
    # For frequency = 1, no Fourier terms are added (fourier_s should be NULL)
    expect_null(fit$fourier_s)

    fc <- forecast(fit, h = 5)
    expect_equal(length(fc$mean), 5)
  })
}

# Test fixed_window = TRUE
if(require(testthat)){
  test_that("ARml works with fixed_window = TRUE", {
    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 5,
                fixed_window = TRUE, verbose = FALSE, calibrate = FALSE)

    expect_s3_class(fit, "ARml")
  })
}

# Test different K values for Fourier terms
if(require(testthat)){
  test_that("ARml works with different K values", {
    # K = 1
    fit1 <- ARml(AirPassengers, caret_method = "lm", max_lag = 5,
                 K = 1, verbose = FALSE, calibrate = FALSE)
    expect_s3_class(fit1, "ARml")
    expect_equal(fit1$K, 1)

    # K = 3
    fit3 <- ARml(AirPassengers, caret_method = "lm", max_lag = 5,
                 K = 3, verbose = FALSE, calibrate = FALSE)
    expect_s3_class(fit3, "ARml")
    expect_equal(fit3$K, 3)
  })
}

# Test different metric values
if(require(testthat)){
  test_that("ARml works with different metrics", {
    fit_mae <- ARml(AirPassengers, caret_method = "lm", max_lag = 5,
                    metric = "MAE", verbose = FALSE, calibrate = FALSE)
    expect_s3_class(fit_mae, "ARml")
  })
}

# Test pre_process options
if(require(testthat)){
  test_that("ARml works with pre_process options", {
    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 5,
                pre_process = c("center", "scale"),
                verbose = FALSE, calibrate = FALSE)
    expect_s3_class(fit, "ARml")
  })
}

# Test residuals.ARml
if(require(testthat)){
  test_that("residuals.ARml returns correct residuals", {
    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 5,
                verbose = FALSE, calibrate = FALSE)

    res <- residuals(fit)

    expect_true(is.ts(res))
    expect_equal(length(res), length(AirPassengers))
    # First max_lag residuals should be NA
    expect_true(all(is.na(res[1:fit$max_lag])))
  })
}

# Test constant xreg warning
if(require(testthat)){
  test_that("ARml warns for constant xreg column", {
    n <- length(AirPassengers)
    xreg <- matrix(rep(5, n), ncol = 1)  # constant column
    colnames(xreg) <- "const"

    expect_warning(
      fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 5,
                  xreg = xreg, seasonal = FALSE,
                  pre_process = "center",
                  verbose = FALSE, calibrate = FALSE),
      "Constant xreg column"
    )
  })
}

# Test xreg as vector gets converted to matrix
if(require(testthat)){
  test_that("ARml converts xreg vector to matrix", {
    n <- length(AirPassengers)
    xreg <- rnorm(n)  # vector, not matrix

    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 5,
                xreg = xreg, seasonal = FALSE,
                verbose = FALSE, calibrate = FALSE)

    expect_s3_class(fit, "ARml")
    expect_true(is.matrix(fit$xreg_fit))
  })
}

# Test multiple xreg columns
if(require(testthat)){
  test_that("ARml works with multiple xreg columns", {
    n <- length(AirPassengers)
    xreg <- matrix(rnorm(n * 3), ncol = 3)
    colnames(xreg) <- c("x1", "x2", "x3")

    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 5,
                xreg = xreg, seasonal = FALSE,
                verbose = FALSE, calibrate = FALSE)

    expect_s3_class(fit, "ARml")
    expect_equal(ncol(fit$xreg_fit), 3)

    # Forecast with matching xreg
    new_xreg <- matrix(rnorm(15), ncol = 3)
    colnames(new_xreg) <- c("x1", "x2", "x3")

    fc <- forecast(fit, xreg = new_xreg)
    expect_equal(length(fc$mean), 5)
  })
}

# Test quarterly data
if(require(testthat)){
  test_that("ARml works with quarterly data", {
    quarterly <- ts(cumsum(rnorm(40)), frequency = 4, start = c(2010, 1))

    fit <- ARml(quarterly, caret_method = "lm", max_lag = 4,
                K = 2, verbose = FALSE, calibrate = FALSE)

    expect_s3_class(fit, "ARml")
    expect_equal(fit$K, 2)

    fc <- forecast(fit, h = 4)
    expect_equal(length(fc$mean), 4)
  })
}

# Test weekly data (frequency = 52)
if(require(testthat)){
  test_that("ARml works with weekly data", {
    weekly <- ts(cumsum(rnorm(104)), frequency = 52, start = c(2020, 1))

    fit <- ARml(weekly, caret_method = "lm", max_lag = 4,
                K = 10, verbose = FALSE, calibrate = FALSE)

    expect_s3_class(fit, "ARml")

    fc <- forecast(fit, h = 4)
    expect_equal(length(fc$mean), 4)
  })
}

# Test allow_parallel = TRUE (just checks it doesn't error)
if(require(testthat)){
  test_that("ARml accepts allow_parallel = TRUE", {
    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 5,
                allow_parallel = TRUE,
                verbose = FALSE, calibrate = FALSE)

    expect_s3_class(fit, "ARml")
  })
}

# Test data too short error
if(require(testthat)){
  test_that("ARml errors for length < 3 data", {
    very_short <- ts(c(1, 2), frequency = 1)

    expect_error(
      ARml(very_short, caret_method = "lm"),
      "Not enough data"
    )
  })
}

# Test initial_window parameter
if(require(testthat)){
  test_that("ARml works with custom initial_window", {
    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 5,
                initial_window = 100,
                verbose = FALSE, calibrate = FALSE)

    expect_s3_class(fit, "ARml")
  })
}

# Test cv_horizon parameter
if(require(testthat)){
  test_that("ARml works with custom cv_horizon", {
    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 5,
                cv_horizon = 6,
                verbose = FALSE, calibrate = FALSE)

    expect_s3_class(fit, "ARml")
  })
}
