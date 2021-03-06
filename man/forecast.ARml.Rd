% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forecast.arml.R
\name{forecast.ARml}
\alias{forecast.ARml}
\title{Forecasting an ARml object}
\usage{
\method{forecast}{ARml}(object, h = frequency(object$y), xreg = NULL, level = c(80, 95), ...)
}
\arguments{
\item{object}{A list class of ARml}

\item{h}{forecast horizon}

\item{xreg}{Optionally, a numerical vector or matrix of future external
regressors}

\item{level}{Confidence level for prediction intervals.}

\item{...}{Other arguments pased to forecast()}
}
\value{
A list class of forecast containing the following elemets
\item{x}{The input time series}
\item{method}{The name of the forecasting method as a character string}
\item{mean}{Point forecasts as a time series}
\item{lower}{Lower limits for prediction intervals}
\item{upper}{Upper limits for prediction intervals}
\item{level}{The confidence values associated with the prediction intervals}
\item{model}{A list containing information about the fitted model}
\item{newxreg}{A matrix containing regressors}
}
\description{
\Sexpr[results=rd, stage=render]{lifecycle::badge("maturing")}
}
\note{
See \code{\link[forecast]{nnetar}}
}
\examples{
\dontrun{
library(forecast)

library(caretForecast)

train_data <- window(AirPassengers, end = c(1959, 12))

test <- window(AirPassengers, start = c(1960, 1))

ARml(train_data, caret_method = "svmLinear2", max_lag = 12) -> fit

forecast(fit, h = length(test), level = NULL, PI = T) -> fc

fc_plot(fc)+ autolayer(test)

accuracy(fc, test)
}


}
\author{
Resul Akay
}
