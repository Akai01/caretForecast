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
