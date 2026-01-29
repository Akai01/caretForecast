# Package index

## Model Fitting

Functions for fitting machine learning models to time series data.

- [`ARml()`](https://akai01.github.io/caretForecast/reference/ARml.md) :
  Autoregressive forecasting using various Machine Learning models.
- [`suggested_methods()`](https://akai01.github.io/caretForecast/reference/suggested_methods.md)
  : Suggested methods for ARml

## Forecasting

Generate point forecasts and prediction intervals.

- [`forecast(`*`<ARml>`*`)`](https://akai01.github.io/caretForecast/reference/forecast.ARml.md)
  : Forecasting using ARml model

## Conformal Prediction

Functions for computing conformal prediction intervals with proper
coverage guarantees.

- [`conformalRegressor()`](https://akai01.github.io/caretForecast/reference/conformalRegressor.md)
  : Fit a conformal regressor.
- [`conformalRegressorByHorizon()`](https://akai01.github.io/caretForecast/reference/conformalRegressorByHorizon.md)
  : Fit a horizon-specific conformal regressor for time series
  forecasting.
- [`predict(`*`<conformalRegressor>`*`)`](https://akai01.github.io/caretForecast/reference/predict.conformalRegressor.md)
  : Predict a conformalRegressor
- [`predict(`*`<conformalRegressorByHorizon>`*`)`](https://akai01.github.io/caretForecast/reference/predict.conformalRegressorByHorizon.md)
  : Predict intervals from a horizon-specific conformal regressor

## Model Diagnostics

Tools for evaluating and understanding model performance.

- [`get_var_imp()`](https://akai01.github.io/caretForecast/reference/get_var_imp.md)
  : Variable importance for forecasting model.

## Data Utilities

Helper functions for preparing time series data.

- [`split_ts()`](https://akai01.github.io/caretForecast/reference/split_ts.md)
  : Split a time series into training and testing sets

## Datasets

Example datasets included in the package.

- [`retail`](https://akai01.github.io/caretForecast/reference/retail.md)
  : Grouped sales data from an Australian Retailer
- [`retail_wide`](https://akai01.github.io/caretForecast/reference/retail_wide.md)
  : Sales data from an Australian Retailer in time series format

## Re-exports

Functions re-exported from other packages for convenience.

- [`reexports`](https://akai01.github.io/caretForecast/reference/reexports.md)
  [`forecast`](https://akai01.github.io/caretForecast/reference/reexports.md)
  [`autoplot`](https://akai01.github.io/caretForecast/reference/reexports.md)
  [`autolayer`](https://akai01.github.io/caretForecast/reference/reexports.md)
  [`accuracy`](https://akai01.github.io/caretForecast/reference/reexports.md)
  [`%>%`](https://akai01.github.io/caretForecast/reference/reexports.md)
  [`%<>%`](https://akai01.github.io/caretForecast/reference/reexports.md)
  : Objects exported from other packages
