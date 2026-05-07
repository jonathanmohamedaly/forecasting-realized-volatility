library(xts)
library(ggplot2)
library(tidyr)
library(dplyr)
# Load data
forecast_data <- read.csv("data/garch_forecasts.csv")
forecast_data <- na.omit(forecast_data)


target <- forecast_data$rv_21d

models <- c(
  "VIX",
  "garch_std_vol",
  "egarch_std_vol",
  "gjr_std_vol"
)


# Loss functions
mse_loss <- function(actual, forecast) {
  mean((actual - forecast)^2, na.rm = TRUE)
}

mae_loss <- function(actual, forecast) {
  mean(abs(actual - forecast), na.rm = TRUE)
}

qlike_loss <- function(actual, forecast) {
  actual_var <- actual^2
  forecast_var <- forecast^2
  
  eps <- 1e-8
  actual_var <- pmax(actual_var, eps)
  forecast_var <- pmax(forecast_var, eps)
  
  mean(log(forecast_var) + actual_var / forecast_var, na.rm = TRUE)
}


# Evaluation table

evaluation_results <- data.frame(
  model = character(),
  MSE = numeric(),
  MAE = numeric(),
  QLIKE = numeric(),
  stringsAsFactors = FALSE
)

for (m in models) {
  
  forecast <- forecast_data[, m]
  
  evaluation_results <- rbind(
    evaluation_results,
    data.frame(
      model = m,
      MSE = mse_loss(target, forecast),
      MAE = mae_loss(target, forecast),
      QLIKE = qlike_loss(target, forecast)
    )
  )
}

evaluation_results <- evaluation_results[order(evaluation_results$QLIKE), ]


# Relative performance vs VIX
vix_qlike <- evaluation_results$QLIKE[evaluation_results$model == "VIX"]

evaluation_results$QLIKE_relative_to_VIX <- evaluation_results$QLIKE / vix_qlike

# Save table
write.csv(
  evaluation_results,
  "data/forecast_evaluation_results.csv",
  row.names = FALSE
)

print("===== RELATIVE PERFORMANCE VS VIX =====")
print(evaluation_results)


# Plot absolute forecast errors
error_plot_data <- forecast_data %>%
  transmute(
    date = as.Date(date.1),
    VIX = abs(rv_21d - VIX),
    GARCH = abs(rv_21d - garch_std_vol),
    EGARCH = abs(rv_21d - egarch_std_vol),
    `GJR-GARCH` = abs(rv_21d - gjr_std_vol)
  ) %>%
  pivot_longer(
    cols = -date,
    names_to = "series",
    values_to = "absolute_error"
  )

ggplot(error_plot_data, aes(x = date, y = absolute_error, color = series)) +
  geom_line(linewidth = 0.8, alpha = 0.95) +
  
  scale_color_manual(
    values = c(
      "VIX" = "#9467BD",
      "GARCH" = "#1F77B4",
      "EGARCH" = "#D62728",
      "GJR-GARCH" = "#2CA02C"
    )
  ) +
  
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b %Y",
    expand = c(0.01, 0.01)
  ) +
  
  labs(
    title = "Absolute Forecast Errors",
    subtitle = "Absolute errors between realized volatility and volatility forecasts",
    x = NULL,
    y = "Absolute Error",
    color = NULL
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.title = element_text(face = "bold", size = 17),
    plot.subtitle = element_text(size = 11, color = "grey35"),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 35, hjust = 1),
    plot.margin = margin(10, 15, 10, 10)
  )
