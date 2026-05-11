library(zoo)
library(xts)
library(ggplot2)

data_clean <- read.csv("data/sp500_clean_data.csv")

returns <- data_clean$log_return

rolling_windows <- c(5, 10, 21, 63)

realized_vol_list <- list()

for (w in rolling_windows) {
  
  rv <- rollapply(
    returns^2,
    width = w,
    FUN = mean,
    align = "right",
    fill = NA
  )
  
  rv <- sqrt(252 * rv)
  
  
  realized_vol_list[[paste0("rv_", w, "d")]] <- rv
}
realized_vol <- data.frame(rv_5d = realized_vol_list$rv_5d, 
                           rv_10d = realized_vol_list$rv_10d,
                           rv_21d = realized_vol_list$rv_21d,
                           rv_63d = realized_vol_list$rv_63d)

data_rv <- data.frame(date = dates, Adj_Close = sp500_price, 
                      log_return = log_returns, log_return_std = log_returns_std, 
                      outlier_flag = data_clean$outlier_flag,
                      rv_5d = realized_vol_list$rv_5d, 
                      rv_10d = realized_vol_list$rv_10d,
                      rv_21d = realized_vol_list$rv_21d,
                      rv_63d = realized_vol_list$rv_63d, row.names = NULL)
data_rv <- na.omit(data_rv)

print("===== REALIZED VOLATILITY SUMMARY =====")
print(paste("Number of observations:", nrow(data_rv)))
print(summary(data_rv[, paste0("rv_", rolling_windows, "d")]))


write.csv(data_rv,
  "data/sp500_realized_volatility.csv",
  row.names = FALSE
)

ggplot(data_rv, aes(x = date)) +
  
  geom_line(aes(y = rv_5d, color = "5d"), linewidth = 0.8) +
  geom_line(aes(y = rv_21d, color = "21d"), linewidth = 0.9) +
  geom_line(aes(y = rv_63d, color = "63d"), linewidth = 0.8) +
  geom_line(aes(y = rv_63d, color = "10d"), linewidth = 0.8) +
  
  
  labs(
    title = "S&P 500 Realized Volatility - Rolling Windows",
    x = "Date",
    y = "Annualized Volatility",
    color = "Window"
  ) +
  
  scale_color_manual(values = c(
    "5d"  = "#1b9e77",   
    "21d" = "#d95f02",   
    "63d" = "#7570b3",   
    "10d" = "#1f78b4"   
  )) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
