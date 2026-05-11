library(dplyr)
library(zoo)
library(ggplot2)

# Load 

data_full <- read.csv("data/sp500_full_data.csv")
data_full$date <- as.Date(data_full$date.1)


har_data <- data_full %>%
  arrange(date) %>%
  mutate(
    rv_daily_proxy = rv_21d,
    rv_week = rollapply(rv_daily_proxy, width = 5, FUN = mean, align = "right", fill = NA),
    rv_month = rollapply(rv_daily_proxy, width = 22, FUN = mean, align = "right", fill = NA),
    rv_next = lead(rv_daily_proxy, 1)
  ) %>%
  select(date, rv_next, rv_daily_proxy, rv_week, rv_month, rv_21d, VIX) %>%
  na.omit()


# Train / test split
train_end <- as.Date("2021-12-31")

har_train <- har_data %>%
  filter(date <= train_end)

har_test <- har_data %>%
  filter(date > train_end)

print("===== HAR-RV MODEL SUMMARY =====")
print(paste("Training observations:", nrow(har_train)))
print(paste("Testing observations:", nrow(har_test)))

# Fit 

har_model <- lm(
  rv_next ~ rv_daily_proxy + rv_week + rv_month,
  data = har_train
)

print(summary(har_model))

saveRDS(har_model, "data/fit_har_rv.rds")


# forecasts

har_test$har_rv_vol <- predict(har_model, newdata = har_test)

# Avoid negative volatility forecasts
har_test$har_rv_vol <- pmax(har_test$har_rv_vol, 1e-8)

har_forecasts <- har_test %>%
  transmute(
    date = date,
    har_rv_vol = har_rv_vol,
    rv_21d = rv_next,
    VIX = VIX
  )

write.csv(
  har_forecasts,
  "data/har_rv_forecasts.csv",
  row.names = FALSE
)

print("===== HAR-RV FORECAST SUMMARY =====")
print(summary(har_forecasts$har_rv_vol))


# Merge HAR-RV with GARCH forecasts

garch_forecasts <- read.csv("data/garch_forecasts.csv")
garch_forecasts$date <- as.Date(garch_forecasts$date.1)

forecast_data_har <- garch_forecasts %>%
  left_join(
    har_forecasts %>% select(date, har_rv_vol),
    by = "date"
  ) %>%
  na.omit()

write.csv(
  forecast_data_har,
  "data/forecast_data_with_har.csv",
  row.names = FALSE
)

# Plot 
plot_data <- forecast_data_har %>%
  select(date, rv_21d, VIX, garch_std_vol, egarch_std_vol, gjr_std_vol, har_rv_vol) %>%
  pivot_longer(
    cols = -date,
    names_to = "series",
    values_to = "volatility"
  ) %>%
  mutate(
    series = recode(
      series,
      rv_21d = "Realized Vol. 21d",
      VIX = "VIX",
      garch_std_vol = "GARCH",
      egarch_std_vol = "EGARCH",
      gjr_std_vol = "GJR-GARCH",
      har_rv_vol = "HAR-RV"
    )
  )

ggplot(plot_data, aes(x = date, y = volatility, color = series)) +
  geom_line(linewidth = 0.8, alpha = 0.95) +
  scale_color_manual(
    values = c(
      "Realized Vol. 21d" = "#111111",
      "VIX" = "#9467BD",
      "GARCH" = "#1F77B4",
      "EGARCH" = "#D62728",
      "GJR-GARCH" = "#2CA02C",
      "HAR-RV" = "#FF7F00"
    )
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b %Y",
    expand = c(0.01, 0.01)
  ) +
  labs(
    title = "Volatility Forecasts",
    subtitle = "Comparison between implied volatility, GARCH-family models and HAR-RV",
    x = NULL,
    y = "Annualized Volatility",
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

plot_data_har <- forecast_data_har %>%
  select(date, rv_21d, har_rv_vol) %>%
  pivot_longer(
    cols = -date,
    names_to = "series",
    values_to = "volatility"
  ) %>%
  mutate(
    series = recode(
      series,
      rv_21d = "Realized Vol. 21d",
      har_rv_vol = "HAR-RV"
    )
  )

ggplot(plot_data_har, aes(x = date, y = volatility, color = series)) +
  geom_line(linewidth = 0.9, alpha = 0.95) +
  
  scale_color_manual(
    values = c(
      "Realized Vol. 21d" = "#111111",
      "HAR-RV" = "#FF7F00"
    )
  ) +
  
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b %Y",
    expand = c(0.01, 0.01)
  ) +
  
  labs(
    title = "HAR-RV Forecast vs Realized Volatility",
    subtitle = "Comparison between realized volatility and HAR-RV forecasts",
    x = NULL,
    y = "Annualized Volatility",
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
