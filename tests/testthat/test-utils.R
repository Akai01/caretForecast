# Unit tests for utility functions

if(require(testthat)){

  # Test lag_maker basic functionality
  test_that("lag_maker creates correct lag matrix", {
    y <- ts(1:10, frequency = 1)
    lags <- caretForecast:::lag_maker(y, max_lag = 3)

    expect_true(is.matrix(lags))
    expect_equal(nrow(lags), 7)  # length(y) - max_lag
    expect_equal(ncol(lags), 3)  # max_lag columns
  })

  # Test lag_maker with max_lag = 1 returns matrix (not vector)
  test_that("lag_maker with max_lag = 1 returns matrix", {
    y <- ts(1:10, frequency = 1)
    lags <- caretForecast:::lag_maker(y, max_lag = 1)

    expect_true(is.matrix(lags))
    expect_equal(ncol(lags), 1)
    expect_equal(nrow(lags), 9)
  })

  # Test lag_maker error: non-ts input
  test_that("lag_maker rejects non-ts input", {
    expect_error(
      caretForecast:::lag_maker(c(1, 2, 3, 4, 5), max_lag = 2),
      "y must be a 'ts' object"
    )
  })

  # Test lag_maker error: max_lag <= 0
  test_that("lag_maker rejects max_lag <= 0", {
    y <- ts(1:10, frequency = 1)

    expect_error(
      caretForecast:::lag_maker(y, max_lag = 0),
      "max_lag must be greater than 0"
    )

    expect_error(
      caretForecast:::lag_maker(y, max_lag = -1),
      "max_lag must be greater than 0"
    )
  })

  # Test lag_maker error: max_lag >= length(y)
  test_that("lag_maker rejects max_lag >= length(y)", {
    y <- ts(1:10, frequency = 1)

    expect_error(
      caretForecast:::lag_maker(y, max_lag = 10),
      "max_lag must be less than length"
    )

    expect_error(
      caretForecast:::lag_maker(y, max_lag = 15),
      "max_lag must be less than length"
    )
  })

  # Test lag_maker message for fractional max_lag
  test_that("lag_maker warns for fractional max_lag", {
    y <- ts(1:20, frequency = 1)

    expect_message(
      caretForecast:::lag_maker(y, max_lag = 3.5),
      "should not be a fractional"
    )
  })

  # Test %notin% operator
  test_that("%notin% operator works correctly", {
    `%notin%` <- caretForecast:::`%notin%`

    expect_true(5 %notin% c(1, 2, 3))
    expect_false(2 %notin% c(1, 2, 3))
    expect_true("d" %notin% c("a", "b", "c"))
    expect_false("b" %notin% c("a", "b", "c"))
  })

}

# Test conformal_intervals internal function
if(require(testthat)){
  test_that("conformal_intervals works correctly", {
    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 5,
                verbose = FALSE, calibrate = FALSE)
    res <- residuals(fit)
    fc <- forecast(fit, h = 5)

    intervals <- caretForecast:::conformal_intervals(
      residuals = res,
      y_hat = fc$mean,
      level = c(80, 95)
    )

    expect_true("upper" %in% names(intervals))
    expect_true("lower" %in% names(intervals))
    expect_true(is.ts(intervals$upper))
    expect_true(is.ts(intervals$lower))
  })
}

# Test pred_func internal function
if(require(testthat)){
  test_that("pred_func works correctly", {
    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 5,
                verbose = FALSE, calibrate = FALSE)

    # Get internal objects
    x <- fit$x
    y <- as.numeric(fit$y_modified)
    freq <- frequency(fit$y_modified)

    fourier_h <- NULL
    if (fit$seasonal && freq > 1) {
      fourier_h <- forecast::fourier(fit$y_modified, K = fit$K, h = 1)
    }

    result <- caretForecast:::pred_func(
      i = 1,
      x = x,
      y = y,
      newxreg = NULL,
      object = fit,
      freq = freq,
      fourier_h = fourier_h
    )

    expect_true("x" %in% names(result))
    expect_true("y" %in% names(result))
    expect_equal(nrow(result$x), nrow(x) + 1)
    expect_equal(length(result$y), length(y) + 1)
  })
}

# Test forecast_loop internal function
if(require(testthat)){
  test_that("forecast_loop works correctly", {
    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 5,
                verbose = FALSE, calibrate = FALSE)

    result <- caretForecast:::forecast_loop(
      object = fit,
      xreg = NULL,
      h = 3
    )

    expect_true("x" %in% names(result))
    expect_true("y" %in% names(result))
    expect_true(is.ts(result$y))
    expect_equal(length(result$y), 3)
  })
}
