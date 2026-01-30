# A unit test for forecast function
if(require(testthat)){

  test_that("tests for some arguments in forecast 1", {

    forecast(ARml(AirPassengers, caret_method = "lm", max_lag = 12, K=5),
             h = 2) -> fc
    values <- as.numeric(ceiling(c(fc$mean)))
    expect_equal(values, c(459, 429), tolerance = 1)

  })
}



if(require(testthat)){

  test_that("tests for some arguments in forecast 2", {

    ARml(AirPassengers, caret_method = "lm", max_lag = 10,
         xreg = forecast::seasonaldummy(AirPassengers), seasonal = F) -> fit
    forecast(fit, h = 2, xreg = forecast::seasonaldummy(AirPassengers, h = 2)) -> fc
    values <- as.numeric(ceiling(c(fc$mean)))
    expect_equal(values, c(446, 445), tolerance = 1)

  })
}


if(require(testthat)){

  test_that("tests for some arguments in forecast 3", {

    forecast(ARml(AirPassengers, caret_method = "lm", max_lag = 12,
                  seasonal = F), h = 2) -> fc
    values <- as.numeric(ceiling(c(fc$mean)))
    expect_equal(values, c(465, 429), tolerance = 1)

  })
}

# Test forecast with h = NULL uses default
if(require(testthat)){
  test_that("forecast with h = NULL uses default horizon", {
    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 5,
                verbose = FALSE, calibrate = FALSE)

    fc <- forecast(fit, h = NULL)

    # Default h should be 2 * frequency for seasonal data
    expect_equal(length(fc$mean), 2 * frequency(AirPassengers))
  })
}

# Test forecast error: no regressors provided when model has xreg
if(require(testthat)){
  test_that("forecast errors when xreg missing but model has xreg", {
    n <- length(AirPassengers)
    xreg <- matrix(rnorm(n), ncol = 1)
    colnames(xreg) <- "x1"

    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 5,
                xreg = xreg, seasonal = FALSE,
                verbose = FALSE, calibrate = FALSE)

    expect_error(
      forecast(fit, h = 5),
      "No regressors provided"
    )
  })
}

# Test forecast error: xreg provided but model has no xreg
if(require(testthat)){
  test_that("forecast errors when xreg provided but model has none", {
    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 5,
                verbose = FALSE, calibrate = FALSE)

    new_xreg <- matrix(rnorm(5), ncol = 1)

    expect_error(
      forecast(fit, xreg = new_xreg),
      "No regressors provided to fitted model"
    )
  })
}

# Test forecast error: wrong number of xreg columns
if(require(testthat)){
  test_that("forecast errors when xreg columns don't match", {
    n <- length(AirPassengers)
    xreg <- matrix(rnorm(n), ncol = 1)
    colnames(xreg) <- "x1"

    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 5,
                xreg = xreg, seasonal = FALSE,
                verbose = FALSE, calibrate = FALSE)

    # Provide xreg with 2 columns instead of 1
    new_xreg <- matrix(rnorm(10), ncol = 2)

    expect_error(
      forecast(fit, xreg = new_xreg),
      "Number of regressors does not match"
    )
  })
}

# Test forecast with multiple confidence levels
if(require(testthat)){
  test_that("forecast works with multiple confidence levels", {
    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 5,
                verbose = FALSE, calibrate = FALSE)

    fc <- forecast(fit, h = 5, level = c(80, 90, 95))

    expect_s3_class(fc, "forecast")
    expect_equal(fc$level, c(80, 90, 95))
    expect_equal(ncol(fc$lower), 3)
    expect_equal(ncol(fc$upper), 3)
  })
}

# Test forecast with calibrated model
if(require(testthat)){
  test_that("forecast works with calibrated model", {
    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 5,
                verbose = FALSE, calibrate = TRUE,
                calibration_horizon = 5, n_cal_windows = 5)

    fc <- forecast(fit, h = 5, level = c(80, 95))

    expect_s3_class(fc, "forecast")
    expect_false(is.null(fc$lower))
    expect_false(is.null(fc$upper))
  })
}

# Test forecast output structure
if(require(testthat)){
  test_that("forecast output has correct structure", {
    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 5,
                verbose = FALSE, calibrate = FALSE)

    fc <- forecast(fit, h = 5)

    expect_true("x" %in% names(fc))
    expect_true("mean" %in% names(fc))
    expect_true("fitted" %in% names(fc))
    expect_true("level" %in% names(fc))
    expect_true("method" %in% names(fc))
    expect_true("model" %in% names(fc))

    expect_true(is.ts(fc$mean))
    expect_equal(length(fc$x), length(AirPassengers))
  })
}
