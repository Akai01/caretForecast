if(require(testthat)){
  test_that("tests for some arguments in ARml", {

    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 10)

    fc <- forecast(fit, h = 1)

    res <- residuals(fit)
    conf_reg <- conformalRegressor(res)
    conf_pred <- predict(conf_reg, y_hat = fc$mean, confidence = 0.9)
    conf_pred <- mean(unlist(conf_pred))

    expect_equal(conf_pred, 446.7539, tolerance = 5)

  })
}

# Test conformalRegressor basic functionality
if(require(testthat)){
  test_that("conformalRegressor creates valid object", {
    residuals <- rnorm(100)
    conf_reg <- conformalRegressor(residuals)

    expect_s3_class(conf_reg, "conformalRegressor")
    expect_true("alphas" %in% names(conf_reg))
    expect_true("normalized" %in% names(conf_reg))
    expect_false(conf_reg$normalized)
    expect_equal(length(conf_reg$alphas), length(residuals))
  })
}

# Test conformalRegressor with sigmas (normalized)
if(require(testthat)){
  test_that("conformalRegressor works with sigmas (normalized)", {
    residuals <- rnorm(100)
    sigmas <- abs(rnorm(100)) + 0.1

    conf_reg <- conformalRegressor(residuals, sigmas = sigmas)

    expect_s3_class(conf_reg, "conformalRegressor")
    expect_true(conf_reg$normalized)
  })
}

# Test predict.conformalRegressor with multiple confidence levels
if(require(testthat)){
  test_that("predict.conformalRegressor works with multiple confidence levels", {
    residuals <- rnorm(100)
    conf_reg <- conformalRegressor(residuals)
    y_hat <- c(10, 20, 30)

    pred <- predict(conf_reg, y_hat = y_hat, confidence = c(0.8, 0.9, 0.95))

    expect_equal(nrow(pred), 3)
    expect_equal(ncol(pred), 6)  # 2 columns per confidence level
    expect_true(all(grepl("lower_|upper_", names(pred))))
  })
}

# Test predict.conformalRegressor with y_min and y_max bounds
if(require(testthat)){
  test_that("predict.conformalRegressor respects y_min and y_max bounds", {
    residuals <- rnorm(100, sd = 10)
    conf_reg <- conformalRegressor(residuals)
    y_hat <- c(5, 10, 15)

    pred <- predict(conf_reg, y_hat = y_hat, confidence = 0.95,
                    y_min = 0, y_max = 20)

    expect_true(all(pred$lower_95 >= 0))
    expect_true(all(pred$upper_95 <= 20))
  })
}

# Test predict.conformalRegressor error: non-numeric y_hat
if(require(testthat)){
  test_that("predict.conformalRegressor rejects non-numeric y_hat", {
    residuals <- rnorm(100)
    conf_reg <- conformalRegressor(residuals)

    expect_error(
      predict(conf_reg, y_hat = c("a", "b", "c")),
      "y_hat must be a numeric vector"
    )
  })
}

# Test predict.conformalRegressor error: non-numeric sigmas
if(require(testthat)){
  test_that("predict.conformalRegressor rejects non-numeric sigmas", {
    residuals <- rnorm(100)
    conf_reg <- conformalRegressor(residuals, sigmas = abs(rnorm(100)) + 0.1)

    expect_error(
      predict(conf_reg, y_hat = c(1, 2, 3), sigmas = c("a", "b", "c")),
      "sigmas must be a numeric vector"
    )
  })
}

# Test predict.conformalRegressor error: non-numeric y_min
if(require(testthat)){
  test_that("predict.conformalRegressor rejects non-numeric y_min", {
    residuals <- rnorm(100)
    conf_reg <- conformalRegressor(residuals)

    expect_error(
      predict(conf_reg, y_hat = c(1, 2, 3), y_min = "zero"),
      "y_min must be a numeric"
    )
  })
}

# Test predict.conformalRegressor error: non-numeric y_max
if(require(testthat)){
  test_that("predict.conformalRegressor rejects non-numeric y_max", {
    residuals <- rnorm(100)
    conf_reg <- conformalRegressor(residuals)

    expect_error(
      predict(conf_reg, y_hat = c(1, 2, 3), y_max = "hundred"),
      "y_max must be a numeric"
    )
  })
}

# Test predict.conformalRegressor warning: multiple y_min values
if(require(testthat)){
  test_that("predict.conformalRegressor warns for multiple y_min values", {
    residuals <- rnorm(100)
    conf_reg <- conformalRegressor(residuals)

    expect_warning(
      predict(conf_reg, y_hat = c(1, 2, 3), y_min = c(0, 1)),
      "Only the first element of y_min"
    )
  })
}

# Test predict.conformalRegressor warning: multiple y_max values
if(require(testthat)){
  test_that("predict.conformalRegressor warns for multiple y_max values", {
    residuals <- rnorm(100)
    conf_reg <- conformalRegressor(residuals)

    expect_warning(
      predict(conf_reg, y_hat = c(1, 2, 3), y_max = c(100, 200)),
      "Only the first element of y_max"
    )
  })
}

# Test conformalRegressorByHorizon basic functionality
if(require(testthat)){
  test_that("conformalRegressorByHorizon creates valid object", {
    horizon_errors <- list(
      h1 = sort(abs(rnorm(50))),
      h2 = sort(abs(rnorm(50))),
      h3 = sort(abs(rnorm(50)))
    )

    conf_reg <- conformalRegressorByHorizon(horizon_errors)

    expect_s3_class(conf_reg, "conformalRegressorByHorizon")
    expect_equal(conf_reg$max_horizon, 3)
    expect_equal(length(conf_reg$alphas_by_horizon), 3)
  })
}

# Test conformalRegressorByHorizon error: non-list input
if(require(testthat)){
  test_that("conformalRegressorByHorizon rejects non-list input", {
    expect_error(
      conformalRegressorByHorizon(c(1, 2, 3)),
      "horizon_errors must be a list"
    )
  })
}

# Test conformalRegressorByHorizon error: empty list
if(require(testthat)){
  test_that("conformalRegressorByHorizon rejects empty list", {
    expect_error(
      conformalRegressorByHorizon(list()),
      "must contain at least one horizon"
    )
  })
}

# Test predict.conformalRegressorByHorizon basic functionality
if(require(testthat)){
  test_that("predict.conformalRegressorByHorizon works correctly", {
    horizon_errors <- list(
      h1 = sort(abs(rnorm(50))),
      h2 = sort(abs(rnorm(50))) * 1.2,
      h3 = sort(abs(rnorm(50))) * 1.5
    )

    conf_reg <- conformalRegressorByHorizon(horizon_errors)
    y_hat <- c(10, 20, 30)

    pred <- predict(conf_reg, y_hat = y_hat, confidence = 0.9)

    expect_equal(nrow(pred), 3)
    expect_true("lower_90" %in% names(pred))
    expect_true("upper_90" %in% names(pred))
  })
}

# Test predict.conformalRegressorByHorizon error: missing y_hat
if(require(testthat)){
  test_that("predict.conformalRegressorByHorizon requires y_hat", {
    horizon_errors <- list(h1 = sort(abs(rnorm(50))))
    conf_reg <- conformalRegressorByHorizon(horizon_errors)

    expect_error(
      predict(conf_reg),
      "y_hat must be provided"
    )
  })
}

# Test predict.conformalRegressorByHorizon error: non-numeric y_hat
if(require(testthat)){
  test_that("predict.conformalRegressorByHorizon rejects non-numeric y_hat", {
    horizon_errors <- list(h1 = sort(abs(rnorm(50))))
    conf_reg <- conformalRegressorByHorizon(horizon_errors)

    expect_error(
      predict(conf_reg, y_hat = c("a", "b")),
      "y_hat must be a numeric vector"
    )
  })
}

# Test predict.conformalRegressorByHorizon warning: horizon exceeds calibrated
if(require(testthat)){
  test_that("predict.conformalRegressorByHorizon warns when horizon exceeds calibrated", {
    horizon_errors <- list(
      h1 = sort(abs(rnorm(50))),
      h2 = sort(abs(rnorm(50)))
    )
    conf_reg <- conformalRegressorByHorizon(horizon_errors)
    y_hat <- c(10, 20, 30, 40, 50)  # 5 horizons but only 2 calibrated

    expect_warning(
      predict(conf_reg, y_hat = y_hat, confidence = 0.9),
      "exceeds calibrated horizon"
    )
  })
}

# Test predict.conformalRegressorByHorizon with bounds
if(require(testthat)){
  test_that("predict.conformalRegressorByHorizon respects y_min and y_max", {
    horizon_errors <- list(
      h1 = sort(abs(rnorm(50, sd = 10))),
      h2 = sort(abs(rnorm(50, sd = 10)))
    )
    conf_reg <- conformalRegressorByHorizon(horizon_errors)
    y_hat <- c(5, 10)

    pred <- predict(conf_reg, y_hat = y_hat, confidence = 0.9,
                    y_min = 0, y_max = 20)

    expect_true(all(pred$lower_90 >= 0))
    expect_true(all(pred$upper_90 <= 20))
  })
}

# Test predict.conformalRegressorByHorizon error: invalid confidence
if(require(testthat)){
  test_that("predict.conformalRegressorByHorizon rejects invalid confidence", {
    horizon_errors <- list(h1 = sort(abs(rnorm(50))))
    conf_reg <- conformalRegressorByHorizon(horizon_errors)

    expect_error(
      predict(conf_reg, y_hat = c(10), confidence = 1.5),
      "confidence must be in the interval"
    )

    expect_error(
      predict(conf_reg, y_hat = c(10), confidence = -0.5),
      "confidence must be in the interval"
    )
  })
}
