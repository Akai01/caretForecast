# Unit tests for short time series handling

if (require(testthat)) {

  # Helper to create short time series
  make_short_ts <- function(n, frequency = 1, start = 1) {
    ts(rnorm(n, mean = 100, sd = 10), frequency = frequency, start = start)
  }

  test_that("ARml handles minimum length series (n=3)", {
    set.seed(123)
    short_ts <- ts(c(10, 12, 11), frequency = 1)

    expect_warning(
      fit <- ARml(short_ts, caret_method = "lm", verbose = FALSE, calibrate = FALSE),
      "Input data is too short"
    )

    expect_s3_class(fit, "ARml")
    expect_equal(fit$max_lag, 1)

    # Should be able to forecast
    fc <- forecast(fit, h = 2, PI = FALSE)
    expect_equal(length(fc$mean), 2)
  })

  test_that("ARml handles short series with automatic max_lag adjustment", {
    set.seed(456)
    short_ts <- ts(1:8, frequency = 1)

    expect_warning(
      fit <- ARml(short_ts, max_lag = 12, caret_method = "lm",
                  verbose = FALSE, calibrate = FALSE),
      "Input data is too short"
    )

    expect_s3_class(fit, "ARml")
    expect_true(fit$max_lag <= 3)
  })

  test_that("ARml disables seasonality for very short seasonal series", {
    set.seed(789)
    # Monthly data but only 10 observations - not enough for seasonal
    short_seasonal <- ts(rnorm(10), frequency = 12, start = c(2020, 1))

    fit <- ARml(short_seasonal, max_lag = 2, caret_method = "lm",
                seasonal = TRUE, verbose = FALSE, calibrate = FALSE)

    expect_s3_class(fit, "ARml")
    # Seasonality should be disabled due to short series
    expect_false(fit$seasonal)
  })

  test_that("ARml works with short quarterly series", {
    set.seed(101)
    # 12 quarters of data
    quarterly_ts <- ts(cumsum(rnorm(12)), frequency = 4, start = c(2020, 1))

    fit <- ARml(quarterly_ts, max_lag = 4, caret_method = "lm",
                verbose = FALSE, calibrate = FALSE)

    expect_s3_class(fit, "ARml")

    fc <- forecast(fit, h = 4, PI = FALSE)
    expect_equal(length(fc$mean), 4)
  })

  test_that("Forecasting short series produces valid output", {
    set.seed(202)
    short_ts <- ts(c(5, 7, 6, 8, 7, 9), frequency = 1)

    fit <- ARml(short_ts, max_lag = 2, caret_method = "lm",
                verbose = FALSE, calibrate = FALSE)

    fc <- forecast(fit, h = 3, PI = FALSE)

    expect_s3_class(fc, "forecast")
    expect_equal(length(fc$mean), 3)
    expect_false(any(is.na(fc$mean)))
  })

  test_that("Short series with prediction intervals (non-calibrated)", {
    set.seed(303)
    short_ts <- ts(c(10, 12, 14, 13, 15, 14, 16), frequency = 1)

    fit <- ARml(short_ts, max_lag = 2, caret_method = "lm",
                verbose = FALSE, calibrate = FALSE)

    fc <- forecast(fit, h = 3, level = c(80, 95), PI = TRUE)

    expect_s3_class(fc, "forecast")
    expect_equal(nrow(fc$lower), 3)
    expect_equal(nrow(fc$upper), 3)
    # Lower should be less than mean, upper should be greater
    expect_true(all(fc$lower[, 1] <= fc$mean))
    expect_true(all(fc$upper[, 1] >= fc$mean))
  })

  test_that("Short series with calibration enabled falls back gracefully", {
    set.seed(404)
    short_ts <- ts(1:10, frequency = 1)

    # Calibration may warn or fail but should not error out
    expect_no_error({
      fit <- ARml(short_ts, max_lag = 2, caret_method = "lm",
                  verbose = FALSE, calibrate = TRUE,
                  calibration_horizon = 3, n_cal_windows = 3)
    })

    expect_s3_class(fit, "ARml")
  })

  test_that("Short series with external regressors", {
    set.seed(505)
    n <- 10
    short_ts <- ts(1:n + rnorm(n), frequency = 1)
    xreg <- matrix(rnorm(n), ncol = 1)
    colnames(xreg) <- "x1"

    fit <- ARml(short_ts, max_lag = 2, caret_method = "lm",
                xreg = xreg, verbose = FALSE, calibrate = FALSE)

    expect_s3_class(fit, "ARml")

    # Forecast with new xreg
    new_xreg <- matrix(rnorm(3), ncol = 1)
    colnames(new_xreg) <- "x1"

    fc <- forecast(fit, h = 3, xreg = new_xreg, PI = FALSE)
    expect_equal(length(fc$mean), 3)
  })

  test_that("Short series rejects length < 3",
    expect_error(
      ARml(ts(c(1, 2)), caret_method = "lm", verbose = FALSE),
      "Not enough data"
    )
  )

  test_that("Short series with BoxCox transformation", {
    set.seed(606)
    short_ts <- ts(c(10, 15, 12, 18, 14, 20, 16), frequency = 1)

    fit <- ARml(short_ts, max_lag = 2, caret_method = "lm",
                lambda = "auto", verbose = FALSE, calibrate = FALSE)

    expect_s3_class(fit, "ARml")
    expect_false(is.null(fit$lambda))

    fc <- forecast(fit, h = 2, PI = FALSE)
    expect_equal(length(fc$mean), 2)
  })

  test_that("Short monthly series with limited Fourier terms", {
    set.seed(707)
    # 20 monthly observations - can support some seasonality
    monthly_ts <- ts(rnorm(20, mean = 50, sd = 5), frequency = 12,
                     start = c(2020, 1))

    fit <- ARml(monthly_ts, max_lag = 3, caret_method = "lm",
                seasonal = TRUE, K = 2, verbose = FALSE, calibrate = FALSE)

    expect_s3_class(fit, "ARml")

    fc <- forecast(fit, h = 6, PI = FALSE)
    expect_equal(length(fc$mean), 6)
  })

  test_that("split_ts works correctly with short series", {
    short_ts <- ts(1:15, frequency = 1)

    result <- split_ts(short_ts, test_size = 3)

    expect_equal(length(result$train), 12)
    expect_equal(length(result$test), 3)
  })

  test_that("Accuracy metrics work for short series forecasts", {
    set.seed(808)
    full_ts <- ts(1:12 + rnorm(12, sd = 0.5), frequency = 1)
    dtlist <- split_ts(full_ts, test_size = 3)

    fit <- ARml(dtlist$train, max_lag = 2, caret_method = "lm",
                verbose = FALSE, calibrate = FALSE)

    fc <- forecast(fit, h = 3, PI = FALSE)
    acc <- accuracy(fc, dtlist$test)

    expect_true("RMSE" %in% colnames(acc))
    expect_true("MAE" %in% colnames(acc))
  })

  test_that("residuals work for short series model", {
    set.seed(909)
    short_ts <- ts(c(5, 8, 6, 9, 7, 10, 8), frequency = 1)

    fit <- ARml(short_ts, max_lag = 2, caret_method = "lm",
                verbose = FALSE, calibrate = FALSE)

    res <- residuals(fit)

    expect_true(is.ts(res))
    expect_equal(length(res), length(short_ts))
  })

  test_that("get_var_imp works for short series model", {
    set.seed(111)
    short_ts <- ts(c(5, 8, 6, 9, 7, 10, 8, 11), frequency = 1)

    fit <- ARml(short_ts, max_lag = 3, caret_method = "lm",
                verbose = FALSE, calibrate = FALSE)

    fc <- forecast(fit, h = 2, PI = FALSE)

    # Should not error
    expect_no_error(
      imp <- get_var_imp(fc, plot = FALSE)
    )
  })

}
