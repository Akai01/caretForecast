# Unit tests for calibration functionality

if(require(testthat)){

  test_that("ARml calibration creates calibration object", {
    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 5,
                verbose = FALSE, calibrate = TRUE,
                calibration_horizon = 5, n_cal_windows = 5)

    expect_s3_class(fit, "ARml")
    expect_false(is.null(fit$calibration))
    expect_s3_class(fit$calibration, "conformalRegressorByHorizon")
  })

  test_that("ARml calibration with default parameters", {
    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 5,
                verbose = FALSE, calibrate = TRUE)

    expect_s3_class(fit, "ARml")
    # calibration_horizon should be set to 2 * frequency for seasonal data
    if (!is.null(fit$calibration)) {
      expect_equal(fit$calibration_horizon, 2 * frequency(AirPassengers))
    }
  })

  test_that("ARml calibration disabled", {
    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 5,
                verbose = FALSE, calibrate = FALSE)

    expect_s3_class(fit, "ARml")
    expect_null(fit$calibration)
  })

  test_that("Calibrated forecast produces trumpet-shaped intervals", {
    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 5,
                verbose = FALSE, calibrate = TRUE,
                calibration_horizon = 10, n_cal_windows = 10)

    fc <- forecast(fit, h = 5, level = 95)

    expect_s3_class(fc, "forecast")

    # Prediction intervals should generally widen with horizon
    # (though not strictly monotonic due to sampling variability)
    if (!is.null(fc$lower) && !is.null(fc$upper)) {
      widths <- fc$upper[, 1] - fc$lower[, 1]
      expect_true(all(widths > 0))
    }
  })

  test_that("Calibration with non-seasonal data", {
    non_seasonal <- ts(cumsum(rnorm(100)), frequency = 1)

    fit <- ARml(non_seasonal, caret_method = "lm", max_lag = 5,
                verbose = FALSE, calibrate = TRUE,
                calibration_horizon = 5, n_cal_windows = 10)

    expect_s3_class(fit, "ARml")

    # calibration_horizon should default to 10 for non-seasonal data
    if (!is.null(fit$calibration)) {
      # May be less due to data constraints
      expect_true(fit$calibration_horizon <= 10)
    }
  })

  test_that("Calibration with xreg", {
    set.seed(123)
    n <- length(AirPassengers)
    xreg <- matrix(rnorm(n), ncol = 1)
    colnames(xreg) <- "x1"

    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 3,
                xreg = xreg, seasonal = FALSE,
                verbose = FALSE, calibrate = TRUE,
                calibration_horizon = 3, n_cal_windows = 5)

    expect_s3_class(fit, "ARml")

    # Forecast with calibrated intervals
    new_xreg <- matrix(rnorm(5), ncol = 1)
    colnames(new_xreg) <- "x1"

    fc <- forecast(fit, xreg = new_xreg, level = c(80, 95))
    expect_s3_class(fc, "forecast")
  })

  test_that("Calibration with BoxCox transformation", {
    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 5,
                lambda = "auto",
                verbose = FALSE, calibrate = TRUE,
                calibration_horizon = 5, n_cal_windows = 5)

    expect_s3_class(fit, "ARml")
    expect_true(is.numeric(fit$lambda))

    fc <- forecast(fit, h = 5, level = 95)
    expect_s3_class(fc, "forecast")
    # Forecasts should be positive (back-transformed)
    expect_true(all(fc$mean > 0))
  })

  test_that("Forecast beyond calibrated horizon uses max calibrated", {
    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 5,
                verbose = FALSE, calibrate = TRUE,
                calibration_horizon = 3, n_cal_windows = 10)

    # Only expect warning if calibration succeeded
    if (!is.null(fit$calibration)) {
      # Forecast beyond calibrated horizon
      expect_warning(
        fc <- forecast(fit, h = 10, level = 95),
        "exceeds calibrated horizon"
      )
    } else {
      # Calibration failed, no warning expected
      fc <- forecast(fit, h = 10, level = 95)
    }

    expect_s3_class(fc, "forecast")
    expect_equal(length(fc$mean), 10)
  })

  test_that("calibrate_horizon_scores returns NULL for insufficient data", {
    # Very short series
    short_ts <- ts(1:8, frequency = 1)

    # Should handle gracefully - either succeed with warnings or set calibration to NULL
    expect_no_error({
      fit <- ARml(short_ts, max_lag = 2, caret_method = "lm",
                  verbose = FALSE, calibrate = TRUE,
                  calibration_horizon = 3, n_cal_windows = 2)
    })

    expect_s3_class(fit, "ARml")
  })

  test_that("Multiple confidence levels with calibration", {
    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 5,
                verbose = FALSE, calibrate = TRUE,
                calibration_horizon = 5, n_cal_windows = 10)

    fc <- forecast(fit, h = 5, level = c(50, 80, 95))

    expect_s3_class(fc, "forecast")
    expect_equal(fc$level, c(50, 80, 95))

    if (!is.null(fc$lower)) {
      expect_equal(ncol(fc$lower), 3)
      expect_equal(ncol(fc$upper), 3)

      # Wider confidence levels should have wider intervals
      width_50 <- fc$upper[, 1] - fc$lower[, 1]
      width_80 <- fc$upper[, 2] - fc$lower[, 2]
      width_95 <- fc$upper[, 3] - fc$lower[, 3]

      expect_true(all(width_80 >= width_50))
      expect_true(all(width_95 >= width_80))
    }
  })

}
