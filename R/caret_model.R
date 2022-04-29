#' Tidy time series forecasting using various Machine Learning models.
#'
#' @description
#'
#' `r lifecycle::badge('maturing')`
#'
#'
#' Tidy time series forecasting using fabletools and caret packages.
#'
#' @param formula A formula object to specify the model (see "Specials" section).
#'
#' @param caret_method A string specifying which regression model to use.
#' Possible values are found using names \code{\link[caret]{getModelInfo}}.
#' A list of functions can also be passed for a custom model function.
#' See \url{http://topepo.github.io/caret/} for details.
#'
#' @param metric A string that specifies what summary metric will be used to
#' select the optimal model. See \code{\link[caret]{train}}.
#'
#' @param cv_horizon The number of consecutive values in test set sample.
#'
#' @param initial_window The initial number of consecutive values in each
#' training set sample. Please set to NULL, if your data is a unbalanced panel
#' data.
#' @param fixed_window Logical, if FALSE, all training samples start at 1.
#'
#' @param verbose A logical for printing a training log.
#'
#' @param tune_grid A data frame with possible tuning values.
#' The columns are named the same as the tuning parameters.
#' Use getModelInfo to get a list of tuning parameters for each model or
#' see \url{http://topepo.github.io/caret/available-models.html}.
#' (NOTE: If given, this argument must be named.)
#'
#' @param allow_parallel If a parallel backend is loaded and available,
#' should the function use it?
#' @param ... Ignored.
#'
#'
#' @section Specials:
#'
#' \subsection{order}{
#' The `order` special is used to specify the lag order for the auto-regression.
#' \preformatted{
#' order(p = 1, period = NULL)
#' }
#'
#'
#'  * `p` he order of the auto-regressive (AR) terms. Only one integer allowed
#'  * `period` The periodic nature of the seasonality. This can be either a
#'  number indicating the number of observations in each seasonal period, or
#'   text to indicate the duration of the seasonal window (for example, annual
#'   seasonality would be "1 year").
#'
#' }
#'
#' \subsection{xreg}{
#' Exogenous regressors can be included in an AR model without explicitly using
#'  the `xreg()` special. Common exogenous regressor specials as specified in
#'   [`common_xregs`] can also be used. These regressors are handled using
#'   [stats::model.frame()], and so interactions and other functionality
#'   behaves similarly to [stats::lm()].
#'
#' The inclusion of a constant in the model follows the similar rules to
#' [`stats::lm()`], where including `1` will add a constant and `0` or `-1`
#' will remove the constant. If left out, the inclusion of a constant will be
#' determined by minimising `ic`.
#'
#' \preformatted{
#' xreg(..., fixed = list())
#' }
#'
#'
#'  * `...` are expressions for the exogenous regressors (such as `log(x)`)
#'  * `fixed` A named list of fixed parameters for coefficients.
#'   The names identify the coefficient, and should match the name of the
#'    regressor.
#'   For example, \code{fixed = list(constant = 20)}.
#'
#' }
#'
#' @returns
#'
#' For Formula interface:
#'
#' A model specification.
#'
#' @examples
#'
#'
#'
#' model_spec = CARET(
#'   object = value ~ order(5) + trend() + fourier(K = 6),
#'   caret_method = "lm",
#'   verbose = FALSE)
#'
#' AirPassengers %>% tsibble::as_tsibble() %>%
#'   model(model_spec) -> fit
#'
#' fc_df <- forecast(fit, h = 23)
#'
#' fc_df%>%autoplot(AirPassengers%>% tsibble::as_tsibble())
#'
#' @importFrom purrr map map_lgl compose
#' @importFrom fabletools new_model_class new_model_definition new_specials
#' @importFrom fabletools get_frequencies
#' @importFrom rlang enquo
#' @export
CARET <- function(formula,
                  caret_method = "cubist",
                  metric = "RMSE",
                  initial_window = NULL,
                  cv_horizon = 6,
                  fixed_window = TRUE,
                  verbose = TRUE,
                  allow_parallel = FALSE,
                  tune_grid = NULL,
                  ...) {

  model_caret <- new_model_class(
    "CARET ",
    train = train_caret,
    specials = specials_caret,
    check = function(.data) {
      if (!tsibble::is_regular(.data)) {
        stop("Data must be regular")
      }
    }
  )

  new_model_definition(
    model_caret,!!enquo(formula),
    caret_method = caret_method,
    metric =  metric,
    initial_window = initial_window,
    cv_horizon = cv_horizon,
    fixed_window = fixed_window,
    verbose = verbose,
    allow_parallel = allow_parallel,
    tune_grid = tune_grid
  )
}

#' @importFrom fabletools new_specials common_xregs
#' @importFrom rlang get_env is_empty env_parents new_formula call2 enexprs
#' @importFrom rlang enquos
specials_caret <- fabletools::new_specials(
  order = function(p = 1, period = NULL) {
    period <- get_frequencies(period, self$data, .auto = "smallest")
    list(p = p, period = period)
  },

  common_xregs,
  xreg = function(..., fixed = list()) {
    dots <- enexprs(...)
    env <- purrr::map(enquos(...), rlang::get_env)
    env[purrr::map_lgl(env, purrr::compose(is_empty, env_parents))] <-
      NULL
    env <- if (!is_empty(env)) {
      rlang::get_env(env[[1]])
    } else {
      base_env()
    }

    constants <- purrr::map_lgl(dots, inherits, "numeric")
    constant_given <-
      any(purrr::map_lgl(dots[constants], `%in%`, -1:1))

    model_formula <- new_formula(lhs = NULL,
                                 rhs = purrr::reduce(dots, function(.x, .y) {
                                   call2("+", .x, .y)
                                 }))
    xreg <-
      model.frame(model_formula,
                  data = env,
                  na.action = stats::na.pass)
    list(constant = if (constant_given) {
      as.logical(terms(xreg) %@% "intercept")
    } else {
      c(TRUE, FALSE)
    },
    xreg = if (NCOL(xreg) == 0) {
      NULL
    } else {
      as.matrix(xreg)
    },
    fixed = fixed)
  },
  .required_specials = c("order"),
  .xreg_specials = names(common_xregs)
)

#' @importFrom tsibble measured_vars
train_caret <- function(.data,
                        specials,
                        caret_method = "cubist",
                        metric = "RMSE",
                        initial_window = NULL,
                        cv_horizon = 6,
                        fixed_window = TRUE,
                        verbose = TRUE,
                        allow_parallel = FALSE,
                        tune_grid = NULL) {
  max_lag <- specials$order[[1]]$p
  freq <- specials$order[[1]][["period"]][[1]]

  xreg <- specials$xreg[[1]]

  mv <- tsibble::measured_vars(.data)

  if (length(mv) > 1) {
    stop("ARml() is a univariate model.")
  }
  y <- .data[[mv]]
  y <- as.numeric(y)
  # check inputs
  length_y <- length(y)

  if (length_y < freq) {
    stop("Not enough data to fit a model")
  }

  if (max_lag <= 0) {
    warning("p increased to 1. p must be p >= 1")
    max_lag <- 1
  }

  if (c(length_y - freq - round(freq / 4)) < max_lag) {
    max_lag <- round(length_y - freq - round(freq / 4))
    warning(paste("Input data is too short. p reduced to ", max_lag))
  }

  if (max_lag != round(max_lag)) {
    max_lag <- round(max_lag)
    message(paste("p must be an integer, p rounded to", max_lag))
  }

  constant_y <- is_constant(y)

  if (constant_y) {
    warning("Constant data, setting p = 1")
    max_lag <- 1
  }
  # end check
  x <- lag_maker_fable(y , max_lag = max_lag)

  x_future <- x

  if ("matrix" %in% class(xreg[["xreg"]])) {
    xreg2 <- as.data.frame(xreg[["xreg"]])
    x <- dplyr::bind_cols(x, xreg2)
    rm(xreg2)
  }

  if (is.null(initial_window)) {
    initial_window <- length(y) - max_lag -  cv_horizon * 2
  }
  rows_to_omit <- seq_len(max_lag)
  x <- as.matrix(x)
  x <- x[-rows_to_omit, ]
  y2 <- y[-rows_to_omit]
  x_future <- x_future[-rows_to_omit, ]

  fit_m <- fit_base(
    y = y2,
    x = x,
    caret_method = caret_method,
    metric =  metric,
    initial_window = initial_window,
    cv_horizon = cv_horizon,
    fixed_window = fixed_window,
    verbose = verbose,
    allow_parallel = allow_parallel,
    tune_grid = tune_grid
  )

  out <- list(
    model = fit_m,
    freq = freq,
    max_lag = max_lag,
    x_future = as.matrix(x_future),
    x = x,
    y = y,
    y_modified = y2
  )
  out <- structure(out, class = "CARET")

  return(out)
}

#' Forecast a CARET model
#' @param object The time series model used to produce the forecasts
#' @param new_data A tsibble containing future information used to forecast.
#' @param specials Model specials, passed by fabletools::forecast.mdl_df().
#' @param ... Additional arguments for forecast model methods.
#'
#' @importFrom stats predict residuals
#' @importFrom fabletools forecast
#' @importFrom distributional dist_normal
#'
#' @export
forecast.CARET <- function(object, new_data, specials = NULL, ...) {
  xreg <- specials$xreg[[1]][["xreg"]]
  maxlag <- object$max_lag
  x_future <- object$x_future
  lags <- seq_len(maxlag)
  h <- NROW(new_data)
  fc <- numeric(h)
  x = object$x
  y = object$y_modified

  for (i in 1:h) {
    pred <- NULL
    newxreg_in <- xreg[i,]
    new_data <- c(y[length(y)], x[nrow(x), 1:(object$max_lag - 1)])
    if (object$max_lag == 1) {
      new_data = new_data[-2]
    }
    if (!is.null(newxreg_in)) {
      new_data <- c(new_data, newxreg_in)
    }
    new_data <- matrix(new_data, nrow = 1)
    colnames(new_data) <- colnames(object$x)
    pred <- predict(object$model, newdata = new_data)

    x = rbind(x, new_data)
    y = c(y, pred)
  }

  fc <- y[-seq_len(length(object$y_modified))]
  y <- y[seq_len(length(object$y_modified))]

  se <- tryCatch({
    sd(residuals(object$model), na.rm = TRUE) * sqrt(h * (1 + h / length(y)))
  }, error = function(err){
    1
  })

  return(distributional::dist_normal(fc, se))
}

#' @importFrom dplyr bind_cols lag
lag_maker_fable <- function(y, max_lag) {
  out <- matrix(nrow = length(y), ncol = max_lag)
  for (i in seq_len(max_lag)) {
    out[, i] <- dplyr::lag(y, i)
  }
  colnames(out) <- paste0("y_lag", seq_len(max_lag))
  return(out)
}

fit_base <- function(x,
                     y,
                     caret_method = "cubist",
                     metric = "RMSE",
                     initial_window = 300,
                     cv_horizon = 6,
                     fixed_window = TRUE,
                     verbose = TRUE,
                     allow_parallel = FALSE,
                     tune_grid = NULL) {
  out <- caret::train(
    x = x,
    y = y,
    method = caret_method,
    metric =  metric,
    trControl = caret::trainControl(
      method = "timeslice",
      initialWindow = initial_window,
      horizon = cv_horizon,
      fixedWindow = fixed_window,
      verboseIter = verbose,
      allowParallel = allow_parallel
    ),
    tuneGrid = tune_grid
  )
  return(out)
}
