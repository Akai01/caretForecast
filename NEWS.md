# caretForecast 0.1.3

## Bug Fixes

* Fixed `get_var_imp()` to accept both `ARml` and `forecastARml` objects as documented
* Fixed dimension drop in `lag_maker()` when `max_lag = 1`
* Fixed dimension drop in `forecast_loop()` when forecasting single step
* Fixed `xreg` being lost during calibration - now properly preserved before truncation
* Fixed redundant BoxCox transformation when `lambda = "auto"`
* Fixed invalid `trend_method` parameter in `get_var_imp()` example
* Fixed syntax error in test file

## Tests

* Added comprehensive test suite with 246 tests covering:
  - `ARml()`: parameter validation, edge cases (`max_lag = 1`, `cv = FALSE`), constant data, non-seasonal data, different frequencies (quarterly, weekly), xreg handling, BoxCox transformations
  - `forecast.ARml()`: error handling, multiple confidence levels, calibrated forecasts, output structure
  - `split_ts()`: input validation, time series attribute preservation
  - Conformal prediction: `conformalRegressor()`, `conformalRegressorByHorizon()`, and their predict methods with bounds and error handling
  - Calibration: horizon-specific calibration, trumpet-shaped intervals, xreg with calibration
  - Internal utilities: `lag_maker()`, `%notin%`, `pred_func()`, `forecast_loop()`

# caretForecast 0.1.2

* Implemented horizon-specific conformal prediction intervals with proper out-of-sample calibration
* Added new parameters to `ARml()`: `calibrate`, `calibration_horizon`, `n_cal_windows`
* New functions: `conformalRegressorByHorizon()`, `calibrate_horizon_scores()`
* Prediction intervals now properly widen with forecast horizon (trumpet shape)
* Added comprehensive test suite for short time series handling

# caretForecast 0.1.1

# caretForecast 0.0.3

# caretForecast 0.0.2

* Added a `NEWS.md` file to track changes to the package.
