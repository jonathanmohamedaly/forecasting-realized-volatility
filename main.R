rm(list = ls())

required_packages <- c(
  "quantmod", "xts", "zoo", "PerformanceAnalytics", "tseries",
  "ggplot2", "rugarch", "dplyr", "tidyr", "scales"
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

invisible(lapply(required_packages, install_if_missing))

set.seed(123)

source("R/data_cleaning.R")
source("R/realized_volatility.R")
source("R/implied_volatility.R")
source("R/garch_models.R")
source("R/har_rv_model.R")
source("R/forecast_evaluation.R")
source("R/diebold_mariano_tests.R")
source("R/risk_backtesting.R")