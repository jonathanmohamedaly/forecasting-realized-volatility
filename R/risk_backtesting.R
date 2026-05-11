library(rugarch)
library(dplyr)
library(tidyr)
library(ggplot2)

# Load 

roll_garch_std  <- readRDS("data/roll_garch_std.rds")
roll_egarch_std <- readRDS("data/roll_egarch_std.rds")
roll_gjr_std    <- readRDS("data/roll_gjr_std.rds")



safe_log <- function(x) log(pmax(x, 1e-12))

kupiec_test <- function(violations, alpha) {
  n <- length(violations)
  x <- sum(violations)
  phat <- x / n
  
  lr_uc <- -2 * (
    (n - x) * safe_log(1 - alpha) + x * safe_log(alpha) -
      ((n - x) * safe_log(1 - phat) + x * safe_log(phat))
  )
  
  p_value <- 1 - pchisq(lr_uc, df = 1)
  
  return(c(LR_uc = lr_uc, p_value_uc = p_value))
}

christoffersen_test <- function(violations) {
  v <- as.integer(violations)
  
  v_lag <- v[-length(v)]
  v_now <- v[-1]
  
  n00 <- sum(v_lag == 0 & v_now == 0)
  n01 <- sum(v_lag == 0 & v_now == 1)
  n10 <- sum(v_lag == 1 & v_now == 0)
  n11 <- sum(v_lag == 1 & v_now == 1)
  
  pi01 <- n01 / max(n00 + n01, 1)
  pi11 <- n11 / max(n10 + n11, 1)
  pi   <- (n01 + n11) / max(n00 + n01 + n10 + n11, 1)
  
  log_l_unrestricted <-
    n00 * safe_log(1 - pi01) +
    n01 * safe_log(pi01) +
    n10 * safe_log(1 - pi11) +
    n11 * safe_log(pi11)
  
  log_l_restricted <-
    (n00 + n10) * safe_log(1 - pi) +
    (n01 + n11) * safe_log(pi)
  
  lr_ind <- -2 * (log_l_restricted - log_l_unrestricted)
  p_value <- 1 - pchisq(lr_ind, df = 1)
  
  return(c(LR_ind = lr_ind, p_value_ind = p_value))
}

extract_var_backtest <- function(roll_object, model_name, alpha) {
  
  df <- as.data.frame(roll_object)
  var_df <- as.data.frame(roll_object@forecast$VaR)
  
  realized <- df$Realized
  
  if (alpha == 0.01) {
    var_col <- grep("1|0.01", colnames(var_df), value = TRUE)[1]
  } else if (alpha == 0.05) {
    var_col <- grep("5|0.05", colnames(var_df), value = TRUE)[1]
  }
  
  var_forecast <- var_df[[var_col]]
  
  valid_idx <- complete.cases(realized, var_forecast)
  realized <- realized[valid_idx]
  var_forecast <- var_forecast[valid_idx]
  
  violations <- realized < var_forecast
  
  kupiec <- kupiec_test(violations, alpha)
  christoffersen <- christoffersen_test(violations)
  
  realized_es <- mean(realized[violations], na.rm = TRUE)
  
  data.frame(
    model = model_name,
    alpha = alpha,
    observations = length(violations),
    expected_violations = alpha * length(violations),
    actual_violations = sum(violations),
    violation_rate = mean(violations),
    realized_ES = realized_es,
    LR_uc = kupiec["LR_uc"],
    p_value_uc = kupiec["p_value_uc"],
    LR_ind = christoffersen["LR_ind"],
    p_value_ind = christoffersen["p_value_ind"]
  )
}


# Backtesting table

risk_results <- bind_rows(
  extract_var_backtest(roll_garch_std,  "GARCH",     0.01),
  extract_var_backtest(roll_garch_std,  "GARCH",     0.05),
  extract_var_backtest(roll_egarch_std, "EGARCH",    0.01),
  extract_var_backtest(roll_egarch_std, "EGARCH",    0.05),
  extract_var_backtest(roll_gjr_std,    "GJR-GARCH", 0.01),
  extract_var_backtest(roll_gjr_std,    "GJR-GARCH", 0.05)
)

risk_results <- risk_results %>%
  mutate(
    coverage_ratio = violation_rate / alpha,
    kupiec_result = ifelse(p_value_uc > 0.05, "Pass", "Reject"),
    christoffersen_result = ifelse(p_value_ind > 0.05, "Pass", "Reject")
  )

print("===== VaR / Expected Shortfall Backtesting =====")
print(risk_results)

write.csv(
  risk_results,
  "data/risk_backtesting_results.csv",
  row.names = FALSE
)


# Plot 

ggplot(risk_results, aes(x = model, y = violation_rate, fill = model)) +
  geom_col(alpha = 0.85) +
  geom_hline(aes(yintercept = alpha), linetype = "dashed") +
  facet_wrap(~ alpha, scales = "free_y") +
  labs(
    title = "VaR Backtesting: Violation Rates",
    subtitle = "Observed VaR violations versus expected violation probability",
    x = NULL,
    y = "Violation Rate",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, color = "grey35"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )
