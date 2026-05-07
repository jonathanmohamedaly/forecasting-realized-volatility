rm(list = ls())

required_packages <- c(
  "quantmod",
  "xts",
  "zoo",
  "PerformanceAnalytics",
  "tseries",
  "ggplot2", 
  "rugarch",
  "dplyr",
  "tidyr",
  "ggplot2",
  "scales"
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

invisible(lapply(required_packages, install_if_missing))
invisible(lapply(required_packages, library, character.only = TRUE))

source("R/data_cleaning.R")
source("R/realized_volatility.R")
source("R/implied_volatility.R")
source("R/garch_models.R")
source("R/forecast_evaluation.R")