# caretForecast 0.1.3

* Fixed `get_var_imp()` to accept both `ARml` and `forecastARml` objects as documented
* Fixed dimension drop in `lag_maker()` when `max_lag = 1`
* Fixed dimension drop in `forecast_loop()` when forecasting single step
* Fixed `xreg` being lost during calibration - now properly preserved before truncation
* Fixed redundant BoxCox transformation when `lambda = "auto"`
* Fixed invalid `trend_method` parameter in `get_var_imp()` example
* Fixed syntax error in test file

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
