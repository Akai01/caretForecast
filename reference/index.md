# Package index

## Model Fitting

Functions for fitting machine learning models to time series data.

- [`ARml()`](https://taf-society.github.io/caretForecast/reference/ARml.md)
  : Autoregressive forecasting using various Machine Learning models.
- [`suggested_methods()`](https://taf-society.github.io/caretForecast/reference/suggested_methods.md)
  : Suggested methods for ARml

## Forecasting

Generate point forecasts and prediction intervals.

- [`forecast(`*`<ARml>`*`)`](https://taf-society.github.io/caretForecast/reference/forecast.ARml.md)
  : Forecasting using ARml model

## Conformal Prediction

Functions for computing conformal prediction intervals with proper
coverage guarantees.

- [`conformalRegressor()`](https://taf-society.github.io/caretForecast/reference/conformalRegressor.md)
  : Fit a conformal regressor.
- [`conformalRegressorByHorizon()`](https://taf-society.github.io/caretForecast/reference/conformalRegressorByHorizon.md)
  : Fit a horizon-specific conformal regressor for time series
  forecasting.
- [`predict(`*`<conformalRegressor>`*`)`](https://taf-society.github.io/caretForecast/reference/predict.conformalRegressor.md)
  : Predict a conformalRegressor
- [`predict(`*`<conformalRegressorByHorizon>`*`)`](https://taf-society.github.io/caretForecast/reference/predict.conformalRegressorByHorizon.md)
  : Predict intervals from a horizon-specific conformal regressor

## Model Diagnostics

Tools for evaluating and understanding model performance.

- [`get_var_imp()`](https://taf-society.github.io/caretForecast/reference/get_var_imp.md)
  : Variable importance for forecasting model.

## Data Utilities

Helper functions for preparing time series data.

- [`split_ts()`](https://taf-society.github.io/caretForecast/reference/split_ts.md)
  : Split a time series into training and testing sets

## Datasets

Example datasets included in the package.

- [`retail`](https://taf-society.github.io/caretForecast/reference/retail.md)
  : Grouped sales data from an Australian Retailer
- [`retail_wide`](https://taf-society.github.io/caretForecast/reference/retail_wide.md)
  : Sales data from an Australian Retailer in time series format

## Re-exports

Functions re-exported from other packages for convenience.

- [`reexports`](https://taf-society.github.io/caretForecast/reference/reexports.md)
  [`forecast`](https://taf-society.github.io/caretForecast/reference/reexports.md)
  [`autoplot`](https://taf-society.github.io/caretForecast/reference/reexports.md)
  [`autolayer`](https://taf-society.github.io/caretForecast/reference/reexports.md)
  [`accuracy`](https://taf-society.github.io/caretForecast/reference/reexports.md)
  [`%>%`](https://taf-society.github.io/caretForecast/reference/reexports.md)
  [`%<>%`](https://taf-society.github.io/caretForecast/reference/reexports.md)
  : Objects exported from other packages
