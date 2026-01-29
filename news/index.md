# Changelog

## caretForecast 0.1.2

- Implemented horizon-specific conformal prediction intervals with
  proper out-of-sample calibration
- Added new parameters to
  [`ARml()`](https://akai01.github.io/caretForecast/reference/ARml.md):
  `calibrate`, `calibration_horizon`, `n_cal_windows`
- New functions:
  [`conformalRegressorByHorizon()`](https://akai01.github.io/caretForecast/reference/conformalRegressorByHorizon.md),
  [`calibrate_horizon_scores()`](https://akai01.github.io/caretForecast/reference/calibrate_horizon_scores.md)
- Prediction intervals now properly widen with forecast horizon (trumpet
  shape)
- Added comprehensive test suite for short time series handling

## caretForecast 0.1.1

CRAN release: 2022-10-24

## caretForecast 0.0.3

CRAN release: 2022-05-02

## caretForecast 0.0.2

CRAN release: 2022-01-27

- Added a `NEWS.md` file to track changes to the package.
