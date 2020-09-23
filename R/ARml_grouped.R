#' @title Forecasting grouped time series using caret
#' @description
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' Forecasting grouped time series using caret
#'
#' @import caret
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel stopCluster makePSOCKcluster
#' @import forecastML
#' @import dplyr
#' @import lubridate
#' @param data A grouped time series class of data.frame
#' @param outcome_col_name Names of the output column
#' @param date_col An integer indicating the location of the date column
#' @param horizons A numeric vector of one or more forecast horizons, h, measured in dataset rows.
#' If dates are given, a horizon of 1, for example, would equal 1 * frequency in calendar time.
#' @param lookback A numeric vector giving the lags–in dataset rows–for creating the lagged features.
#' All non-grouping, non-static, and non-dynamic features in the input dataset, data, are lagged by the same values.
#' The outcome is also lagged by default. Either lookback or lookback_control need to be specified–but not both.
#' @param frequency Date/time frequency. Required if dates are given.
#' A string taking the same input as base::seq.Date(..., by = "frequency") or
#' base::seq.POSIXt(..., by = "frequency") e.g., '1 hour', '1 month', '7 days', '10 years' etc.
#' The highest frequency supported at present is '1 sec'.
#' @param groups A character vector of column names that identify
#' the groups/hierarchies when multiple time series are present.
#' These columns are used as model features but are not lagged.
#' Note that combining feature lags with grouped time series will result in NA values throughout the data.
#' @param dynamic_features A character vector to define date features, "month",
#' "year", "week", "day", "quarter" are supported features.
#' @param static_features For grouped time series only.
#' A character vector of column names that identify features that do not change through time.
#' These columns are not lagged.
#' If type = "forecast", these features will be filled forward using the most recent value for the group.
#' @param train_control Control parameters for caret train see \code{\link[caret]{trainControl}}
#' @param ncore If \code{allowParallel = TRUE} in \code{\link[caret]{trainControl}}
#' user can specify number of cores to be used.
#' @param tune_grid A data frame with possible tuning values.
#' The columns are named the same as the tuning parameters.
#'  Use getModelInfo to get a list of tuning parameters for each model or see
#'  \url{http://topepo.github.io/caret/available-models.html}.
#' (NOTE: If given, this argument must be named.)
#' @param pre_process A string vector that defines a pre-processing of the predictor data.
#' Current possibilities are "BoxCox", "YeoJohnson", "expoTrans", "center", "scale", "range",
#' "knnImpute", "bagImpute", "medianImpute", "pca", "ica" and "spatialSign".
#' The default is no pre-processing.
#' See preProcess and trainControl on the procedures and how to adjust them.
#' Pre-processing code is only designed to work when x is a simple matrix or data frame.
#' @param weights A numeric vector of case weights.
#' This argument will only affect models that allow case weights.
#' @param caret_method A string (or a self defined model list see
#' \url{http://topepo.github.io/caret/using-your-own-model-in-train.html})
#' specifying which classification or regression model to use.
#' Possible values are found using names(getModelInfo()).
#' See  \url{http://topepo.github.io/caret/train-models-by-tag.html}.
#' A list of functions can also be passed for a custom model function.
#' See \url{http://topepo.github.io/caret/using-your-own-model-in-train.html} for details.
#' @param metric 	A string that specifies what summary metric will be used to select the optimal model.
#' By default, possible values are "RMSE" and "Rsquared" for regression and
#' "Accuracy" and "Kappa" for classification.
#' If custom performance metrics are used (via the summaryFunction argument in
#' trainControl, the value of metric should match one of the arguments.
#' If it does not, a warning is issued and the first metric given by the summaryFunction is used.
#' (NOTE: If given, this argument must be named.)
#' @param return_model Boolean. If \code{TRUE} trained model will be returned.
#' @param return_input_data Boolean. If \code{TRUE} input data will be returned.
#' @param return_all_horizon_forecast Boolean. If \code{TRUE} forecast
#' from all horizon forecast  will be returned.
#' @param return_fitted_values Boolean. If \code{TRUE}fitted values will be returned.
#' @param set_seed set random number generation
#'
#' @author Res Akay
#'
#' @examples
#'  \dontrun{
#'
#'data(retail, package = "MLforecast")
#'
#'fc <- ARml_grouped(retail, outcome_col_name = "value",
#'                   date_col = 1,
#'                   horizons = c(1, 9, 18),
#'                   lookback = c(1:24),
#'                   frequency = "1 month",
#'                   groups = "items",
#'                   dynamic_features = c("month", "year", "quarter"),
#'                   caret_method = "cubist",
#'                   return_model = F,
#'                  return_input_data = F,
#'                   return_all_horizon_forecast = TRUE,
#'                   return_fitted_values = TRUE)
#'
#'
#'
#'plot(fc$fitted_values, group_filter = "items == 'V10'")
#'
#'plot(fc$combined_forecasts, data_actual = retail,
#'     actual_indices = retail$date, group_filter = "items == 'V3'")
#' }
#'
#'
#'
#'
#' @export
ARml_grouped <- function(data, outcome_col_name,
                         date_col = 1,
                         horizons = c(1, 6, 12),
                         lookback = c(1:5),
                         frequency = "1 month",
                         groups = NULL,
                         dynamic_features = c("month", "year", "week", "day", "quarter"),
                         static_features = NULL,
                         train_control = caret::trainControl(method = "cv",
                                                             number = 3,
                                                             p = 0.75,
                                                             search = "grid",
                                                             selectionFunction = "best",
                                                             verboseIter = T,
                                                             trim = FALSE,
                                                             allowParallel = FALSE),
                         ncore = 2,
                         tune_grid = NULL,
                         pre_process = NULL,
                         weights = NULL,
                         caret_method = "lm",
                         metric = "RMSE",
                         return_model = TRUE,
                         return_input_data = FALSE,
                         return_all_horizon_forecast = FALSE,
                         return_fitted_values = TRUE,
                         set_seed = 42)
{

  set.seed(set_seed)
  if(train_control$verboseIter){
    message("Preparing input data")
  }


  # helper functions----------------------------
  fit_with_caret <- function(my_data, my_outcome_col = outcome_col_name,
                             my_method = caret_method,
                             my_train_control = train_control,
                             my_tune_grid = tune_grid, my_metric = metric,
                             my_preProcess = pre_process, my_weights = weights){


    y_cal <- which(colnames(my_data)==paste(my_outcome_col))

    x <- my_data[,-y_cal]
    y <- as.vector(t(my_data[,y_cal]))

    fit <- caret::train(x = x,
                        y = y,
                        method = my_method,
                        preProcess = my_preProcess,
                        weights = my_weights,
                        metric =  my_metric,
                        trControl = my_train_control,
                        tuneGrid = my_tune_grid)

    return(fit)
  }

  prediction_function <- function(model, data_features) {

    data_pred <- data.frame("y_pred" = predict(model, data_features))
    return(data_pred)
  }


  # AR----------------------------------

  stopifnot(dynamic_features %in% c("month", "year", "week", "day", "quarter",
                                    NULL))



  ds <- dplyr::select(data, date_col)

  colnames(ds)[1] <- "dates"

  data <- dplyr::select(data, -date_col)

  if("month" %in% dynamic_features){
    data$month <- (lubridate::month(ds$dates))
  }

  if("year" %in% dynamic_features){
    data$year <- (lubridate::year(ds$dates))
  }

  if("week" %in% dynamic_features){
    data$week <- (lubridate::week(ds$dates))
  }

  if("day" %in% dynamic_features){
    data$day <- (lubridate::mday(ds$dates))
  }

  if("quarter" %in% dynamic_features){
    data$quarter <- (lubridate::quarter(ds$dates))
  }


  outcome_col <- which(colnames(data)==outcome_col_name)

  data_train <- forecastML::create_lagged_df(data, type = "train",
                                             method = "direct",
                                             outcome_col = outcome_col,
                                             horizons = horizons,
                                             lookback = lookback,
                                             dates = ds$dates,
                                             frequency = frequency,
                                             dynamic_features = dynamic_features,
                                             groups = groups,
                                             static_features = static_features)

  windows <- forecastML::create_windows(data_train, window_length = 0)

  if(train_control$allowParallel){
    cl <- makePSOCKcluster(ncore)
    registerDoParallel(cl)
  }

  if(train_control$verboseIter){
    message(paste("Training a ", caret_method))
  }

  model_results <- forecastML::train_model(lagged_df = data_train,
                                           windows = windows,
                                           model_name = "auto_forecast",
                                           model_function = fit_with_caret,
                                           use_future = FALSE)

  if(train_control$allowParallel){
    stopCluster(cl)
  }

  if(train_control$verboseIter){
    message(paste("Predicting fitted values ..."))
  }

  fitted <- predict(model_results,
                    prediction_function = list(prediction_function),
                    data = data_train)
  if(train_control$verboseIter){
    message(paste("Preparing the final forecast ..."))
  }

  data_forecast <- forecastML::create_lagged_df(data, type = "forecast",
                                                method = "direct",
                                                outcome_col = outcome_col,
                                                horizons = horizons,
                                                lookback = lookback,
                                                dates = ds$dates,
                                                frequency = frequency,
                                                dynamic_features = dynamic_features,
                                                groups = groups,
                                                static_features = static_features)
  for (i in seq_along(data_forecast)) {
    if("month" %in% dynamic_features){
      data_forecast[[i]]$month <- (lubridate::month(data_forecast[[i]]$index))
    }
    if("year" %in% dynamic_features){
      data_forecast[[i]]$year <- (lubridate::year(data_forecast[[i]]$index))
    }

    if("week" %in% dynamic_features){
      data_forecast[[i]]$week <- (lubridate::week(data_forecast[[i]]$index))
    }
    if("day" %in% dynamic_features){
      data_forecast[[i]]$day <- (lubridate::mday(data_forecast[[i]]$index))
    }

    if("quarter" %in% dynamic_features){
      data_forecast[[i]]$quarter <- (lubridate::quarter(data_forecast[[i]]$index))
    }
  }

  forecast_all_horizones <- predict(model_results,
                                    prediction_function = list(prediction_function),
                                    data = data_forecast)

  combined_forecasts <- forecastML::combine_forecasts(forecast_all_horizones,
                                                      type = "horizon")

  obj <- list()

  if(return_model){
    obj[["model"]] <- model_results
  }

  if(return_fitted_values){
    obj[["fitted_values"]] <- fitted
  }

  if(return_all_horizon_forecast){
    obj[["forecast_all_horizones"]] <- forecast_all_horizones
  }

  if(return_input_data){
    obj[["data_train"]] <- data_train
    obj[["data_forecast"]] <- data_forecast
  }


  obj[["combined_forecasts"]] <- combined_forecasts

  class(obj) <- "MLforecast_groupedTS"

  return(obj)

}

