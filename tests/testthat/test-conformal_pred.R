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
