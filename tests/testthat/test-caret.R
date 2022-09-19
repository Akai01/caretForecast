if(require(testthat)){
  test_that("tests for some arguments in ARml", {
    model_spec = CARET(
      formula = value ~ order(12) + fourier(K = 6) + trend(),
      caret_method = "lm",
      verbose = FALSE)

    AirPassengers %>% tsibble::as_tsibble() %>%
      model(model_spec) -> fit

    fc_df <- forecast(fit, h = 1)
    fc_df <- round(fc_df[[".mean"]])

    expect_that(fc_df, equals(454))

  })
}
