# A unit test for ARml_grouped function
if(require(testthat)){

  test_that("tests for some arguments in ARml_grouped", {

    data(retail, package = "caretForecast")
    fc <- ARml_grouped(retail, outcome_col_name = "value",
                       date_col = 1,
                       horizons = c(1, 9, 18),
                       lookback = c(1:24),
                       frequency = "1 month",
                       groups = "items",
                       dynamic_features = c("month", "year", "quarter"),
                       train_control = caret::trainControl(method = "none",
                                                           number = 3,
                                                           p = 0.75,
                                                           search = "grid",
                                                           selectionFunction = "best",
                                                           verboseIter = T,
                                                           trim = FALSE,
                                                           allowParallel = FALSE),
                       tune_grid = NULL,
                       caret_method = "lm",
                       return_model = F,
                       return_input_data = F,
                       return_all_horizon_forecast = TRUE,
                       return_fitted_values = TRUE)


    class_fc <- class(fc)

    expect_that(class_fc, equals("MLforecast_groupedTS"))

  })
}



# A unit test for ARml_grouped function
if(require(testthat)){

  test_that("tests for some arguments in ARml_grouped", {

    data(retail, package = "caretForecast")
    fc <- ARml_grouped(retail, outcome_col_name = "value",
                       date_col = 1,
                       horizons = c(1, 9, 18),
                       lookback = c(1:24),
                       frequency = "1 month",
                       groups = "items",
                       dynamic_features = c("month", "year", "quarter", "week"),
                       train_control = caret::trainControl(method = "none",
                                                           number = 3,
                                                           p = 0.75,
                                                           search = "grid",
                                                           selectionFunction = "best",
                                                           verboseIter = T,
                                                           trim = FALSE,
                                                           allowParallel = TRUE),
                       tune_grid = NULL,
                       caret_method = "lm",
                       return_model = TRUE,
                       return_input_data = TRUE,
                       return_all_horizon_forecast = TRUE,
                       return_fitted_values = TRUE)


    class_fc <- class(fc)

    expect_that(class_fc, equals("MLforecast_groupedTS"))

  })
}
