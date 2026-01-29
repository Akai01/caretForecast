# Perform rolling-origin calibration with recursive forecasting

This function computes horizon-specific nonconformity scores by
performing rolling-origin evaluation with recursive multi-step
forecasting. This ensures proper out-of-sample calibration that respects
the exchangeability assumption required for valid conformal prediction
intervals.

## Usage

``` r
calibrate_horizon_scores(
  y,
  y_modified,
  max_lag,
  caret_method,
  seasonal,
  K,
  lambda,
  pre_process,
  tune_grid,
  xreg = NULL,
  calibration_horizon,
  n_windows = NULL,
  initial_window = NULL,
  verbose = FALSE
)
```

## Arguments

- y:

  Original time series (untransformed)

- y_modified:

  Transformed time series (Box-Cox if applicable)

- max_lag:

  Maximum lag used in the model

- caret_method:

  The caret method name

- seasonal:

  Logical, whether seasonal terms are used

- K:

  Fourier order for seasonality

- lambda:

  Box-Cox transformation parameter

- pre_process:

  Pre-processing specification

- tune_grid:

  Tuning grid (uses best parameters from initial fit)

- xreg:

  External regressors matrix (optional)

- calibration_horizon:

  Maximum forecast horizon for calibration

- n_windows:

  Number of rolling windows for calibration

- initial_window:

  Initial training window size for calibration

- verbose:

  Logical, print progress

## Value

A list with horizon-indexed vectors of sorted absolute errors
