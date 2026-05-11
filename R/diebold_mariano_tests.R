library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)

# ============================================================
# Load forecast data
# ============================================================

forecast_data <- read.csv("data/garch_forecasts.csv")
forecast_data <- na.omit(forecast_data)

forecast_data$date <- as.Date(forecast_data$date)

# ============================================================
# Loss functions
# ============================================================

squared_error <- function(actual, forecast) {
  (actual - forecast)^2
}

absolute_error <- function(actual, forecast) {
  abs(actual - forecast)
}

qlike_error <- function(actual, forecast) {
  actual_var <- actual^2
  forecast_var <- forecast^2
  
  eps <- 1e-8
  actual_var <- pmax(actual_var, eps)
  forecast_var <- pmax(forecast_var, eps)
  
  log(forecast_var) + actual_var / forecast_var
}

# ============================================================
# Diebold-Mariano helper
# ============================================================

run_dm_test <- function(actual, forecast_1, forecast_2, model_1, model_2, loss_name) {
  
  if (loss_name == "MSE") {
    e1 <- actual - forecast_1
    e2 <- actual - forecast_2
    
    dm <- dm.test(e1, e2, alternative = "two.sided", h = 1, power = 2)
    
  } else if (loss_name == "MAE") {
    e1 <- actual - forecast_1
    e2 <- actual - forecast_2
    
    dm <- dm.test(e1, e2, alternative = "two.sided", h = 1, power = 1)
    
  } else if (loss_name == "QLIKE") {
    loss_1 <- qlike_error(actual, forecast_1)
    loss_2 <- qlike_error(actual, forecast_2)
    
    loss_diff <- loss_1 - loss_2
    
    dm_stat <- mean(loss_diff, na.rm = TRUE) /
      sqrt(var(loss_diff, na.rm = TRUE) / length(na.omit(loss_diff)))
    
    p_value <- 2 * (1 - pnorm(abs(dm_stat)))
    
    return(data.frame(
      model_1 = model_1,
      model_2 = model_2,
      loss = loss_name,
      DM_statistic = dm_stat,
      p_value = p_value,
      mean_loss_difference = mean(loss_diff, na.rm = TRUE),
      preferred_model = ifelse(mean(loss_diff, na.rm = TRUE) < 0, model_1, model_2)
    ))
  }
  
  loss_diff <- if (loss_name == "MSE") {
    squared_error(actual, forecast_1) - squared_error(actual, forecast_2)
  } else {
    absolute_error(actual, forecast_1) - absolute_error(actual, forecast_2)
  }
  
  data.frame(
    model_1 = model_1,
    model_2 = model_2,
    loss = loss_name,
    DM_statistic = as.numeric(dm$statistic),
    p_value = as.numeric(dm$p.value),
    mean_loss_difference = mean(loss_diff, na.rm = TRUE),
    preferred_model = ifelse(mean(loss_diff, na.rm = TRUE) < 0, model_1, model_2)
  )
}

# ============================================================
# Model comparisons
# ============================================================

target <- forecast_data$rv_21d

comparisons <- list(
  c("VIX", "garch_std_vol"),
  c("VIX", "egarch_std_vol"),
  c("garch_std_vol", "egarch_std_vol"),
  c("egarch_std_vol", "gjr_std_vol")
)

losses <- c("MSE", "MAE", "QLIKE")

dm_results <- data.frame()

for (comparison in comparisons) {
  model_1 <- comparison[1]
  model_2 <- comparison[2]
  
  for (loss_name in losses) {
    dm_results <- bind_rows(
      dm_results,
      run_dm_test(
        actual = target,
        forecast_1 = forecast_data[[model_1]],
        forecast_2 = forecast_data[[model_2]],
        model_1 = model_1,
        model_2 = model_2,
        loss_name = loss_name
      )
    )
  }
}

dm_results <- dm_results %>%
  mutate(
    significant_5pct = ifelse(p_value < 0.05, "Yes", "No"),
    significant_10pct = ifelse(p_value < 0.10, "Yes", "No")
  )

print("===== DIEBOLD-MARIANO TEST RESULTS =====")
print(dm_results)

write.csv(
  dm_results,
  "data/diebold_mariano_results.csv",
  row.names = FALSE
)

# ============================================================
# Plot DM statistics
# ============================================================

dm_plot_data <- dm_results %>%
  mutate(
    comparison = paste(model_1, "vs", model_2),
    comparison = factor(comparison, levels = unique(comparison))
  )

ggplot(dm_plot_data, aes(x = comparison, y = DM_statistic, fill = loss)) +
  geom_col(position = "dodge", alpha = 0.85) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 1.96, linetype = "dotted") +
  geom_hline(yintercept = -1.96, linetype = "dotted") +
  labs(
    title = "Diebold-Mariano Test Statistics",
    subtitle = "Forecast accuracy comparison across volatility models",
    x = NULL,
    y = "DM Statistic",
    fill = "Loss"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, color = "grey35"),
    axis.text.x = element_text(angle = 35, hjust = 1),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )