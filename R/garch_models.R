library(rugarch)
library(xts)
library(zoo)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Load data

data_full <- read.csv("data/sp500_full_data.csv")

data_full$date <- as.Date(data_full$date.1)

returns_pct <- xts(
  data_full$log_return * 100,
  order.by = data_full$date
)
colnames(returns_pct) <- "returns_pct"

# ============================================================
# Train / test split
# ============================================================

train_end <- "2021-12-31"

returns_train <- returns_pct[paste0("/", train_end)]
returns_test  <- returns_pct[paste0("2022-01-01/")]

print("===== GARCH MODELING SUMMARY =====")
print(paste("Training observations:", nrow(returns_train)))
print(paste("Testing observations:", nrow(returns_test)))


# Models

spec_garch_norm <- ugarchspec(
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(1, 1)
  ),
  mean.model = list(
    armaOrder = c(0, 0),
    include.mean = TRUE
  ),
  distribution.model = "norm"
)

spec_garch_std <- ugarchspec(
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(1, 1)
  ),
  mean.model = list(
    armaOrder = c(0, 0),
    include.mean = TRUE
  ),
  distribution.model = "std"
)

spec_egarch_std <- ugarchspec(
  variance.model = list(
    model = "eGARCH",
    garchOrder = c(1, 1)
  ),
  mean.model = list(
    armaOrder = c(0, 0),
    include.mean = TRUE
  ),
  distribution.model = "std"
)

spec_gjr_std <- ugarchspec(
  variance.model = list(
    model = "gjrGARCH",
    garchOrder = c(1, 1)
  ),
  mean.model = list(
    armaOrder = c(0, 0),
    include.mean = TRUE
  ),
  distribution.model = "std"
)

# Fit models

fit_garch_norm <- ugarchfit(spec = spec_garch_norm, data = returns_train)
fit_garch_std  <- ugarchfit(spec = spec_garch_std,  data = returns_train)
fit_egarch_std <- ugarchfit(spec = spec_egarch_std, data = returns_train)
fit_gjr_std    <- ugarchfit(spec = spec_gjr_std,    data = returns_train)

print("===== INFORMATION CRITERIA =====")
print(infocriteria(fit_garch_norm))
print(infocriteria(fit_garch_std))
print(infocriteria(fit_egarch_std))
print(infocriteria(fit_gjr_std))

# Save fitted models
saveRDS(fit_garch_norm, "data/fit_garch_norm.rds")
saveRDS(fit_garch_std,  "data/fit_garch_std.rds")
saveRDS(fit_egarch_std, "data/fit_egarch_std.rds")
saveRDS(fit_gjr_std,    "data/fit_gjr_std.rds")

# One-step-ahead rolling forecasts

forecast_window <- nrow(returns_test)

roll_garch_std <- ugarchroll(
  spec = spec_garch_std,
  data = returns_pct,
  n.ahead = 1,
  forecast.length = forecast_window,
  refit.every = 21,
  refit.window = "moving",
  solver = "hybrid",
  calculate.VaR = TRUE,
  VaR.alpha = c(0.01, 0.05),
  keep.coef = TRUE
)

roll_egarch_std <- ugarchroll(
  spec = spec_egarch_std,
  data = returns_pct,
  n.ahead = 1,
  forecast.length = forecast_window,
  refit.every = 21,
  refit.window = "moving",
  solver = "hybrid",
  calculate.VaR = TRUE,
  VaR.alpha = c(0.01, 0.05),
  keep.coef = TRUE
)

roll_gjr_std <- ugarchroll(
  spec = spec_gjr_std,
  data = returns_pct,
  n.ahead = 1,
  forecast.length = forecast_window,
  refit.every = 21,
  refit.window = "moving",
  solver = "hybrid",
  calculate.VaR = TRUE,
  VaR.alpha = c(0.01, 0.05),
  keep.coef = TRUE
)

saveRDS(roll_garch_std,  "data/roll_garch_std.rds")
saveRDS(roll_egarch_std, "data/roll_egarch_std.rds")
saveRDS(roll_gjr_std,    "data/roll_gjr_std.rds")

# Extract forecasts
extract_sigma <- function(roll_object, model_name) {
  df <- as.data.frame(roll_object)
  
  sigma_daily <- xts(
    df$Sigma / 100,
    order.by = as.Date(rownames(df))
  )
  
  sigma_annualized <- sigma_daily * sqrt(252)
  colnames(sigma_annualized) <- model_name
  
  return(sigma_annualized)
}

sigma_garch <- extract_sigma(roll_garch_std, "garch_std_vol")
sigma_egarch <- extract_sigma(roll_egarch_std, "egarch_std_vol")
sigma_gjr <- extract_sigma(roll_gjr_std, "gjr_std_vol")

garch_forecasts <- merge(
  sigma_garch,
  sigma_egarch,
  sigma_gjr
)
garch_forecasts_clean <- data.frame(date = as.Date(index(garch_forecasts)),
                                    garch_std_vol = garch_forecasts$garch_std_vol,
                                    egarch_std_vol = garch_forecasts$egarch_std_vol,
                                    gjr_std_vol = garch_forecasts$gjr_std_vol)



# Merge with realized and implied volatility
data_full$date <- as.Date(data_full$date)
garch_forecasts_clean$date <- as.Date(garch_forecasts_clean$date)

forecast_data <- garch_forecasts_clean %>%
  select(date, garch_std_vol, egarch_std_vol, gjr_std_vol) %>%
  left_join(
    data_full %>% select(date, rv_21d, VIX),
    by = "date"
  )

forecast_data <- na.omit(forecast_data)


# Save 
write.csv(
  data.frame(date = index(forecast_data), coredata(forecast_data)),
  "data/garch_forecasts.csv",
  row.names = FALSE
)


# Plot 
plot_data <- forecast_data %>%
  select(date, rv_21d, garch_std_vol, egarch_std_vol, gjr_std_vol, VIX) %>%
  pivot_longer(
    cols = -date,
    names_to = "series",
    values_to = "volatility"
  ) %>%
  mutate(
    series = recode(
      series,
      rv_21d = "Real. Vol. 21d",
      garch_std_vol = "GARCH",
      egarch_std_vol = "EGARCH",
      gjr_std_vol = "GJR-GARCH",
      VIX = "VIX"
    )
  )

ggplot(plot_data, aes(x = date, y = volatility, color = series)) +
  geom_line(linewidth = 0.8, alpha = 0.95) +
  scale_color_manual(
    values = c(
      "Real. Vol. 21d" = "#111111",
      "GARCH" = "#1F77B4",
      "EGARCH" = "#D62728",
      "GJR-GARCH" = "#2CA02C",
      "VIX" = "#9467BD"
    )
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b %Y",
    expand = c(0.01, 0.01)
  ) +
  labs(
    title = "GARCH Forecasts vs Realized Volatility",
    subtitle = "Annualized volatility comparison: real. volatility, VIX and GARCH-family forecasts",
    x = NULL,
    y = "Annualized Volatility (%)",
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
