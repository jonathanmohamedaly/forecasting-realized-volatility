library(quantmod)
library(zoo)
library(moments)
library(tseries)
library(ggplot2)
getSymbols("^GSPC", src = "yahoo", from = "2015-01-01")

sp500_price <- Ad(GSPC)
colnames(sp500_price) <- " Adj close"

log_returns <- dailyReturn(sp500_price, type = "log")
log_returns <- na.omit(log_returns)
colnames(log_returns) <- "log_return"

log_returns_std <- log_returns / sd(log_returns, na.rm = TRUE)
colnames(log_returns_std) <- "log_return_std"
dates <- index(log_returns_std)


data_clean <- data.frame(date = dates, Adj_Close = sp500_price, 
                         log_return = log_returns, 
                         log_return_std = log_returns_std, row.names = NULL)

print("===== DATA CLEANING SUMMARY =====")
print(paste("Start date:", first(data_clean$date)))
print(paste("End date:", last(data_clean$date)))
print(paste("Number of observations:", nrow(data_clean)))
print(paste("Number of missing values:", sum(is.na(data_clean))))
print(paste("Number of duplicated dates:", sum(duplicated(index(data_clean)))))

date_gaps <- diff(index(sp500_price))
gap_table <- table(date_gaps)

# Outliers detection
abs_returns <- abs(data_clean$log_return)
outlier_threshold <- quantile(abs_returns, 0.999, na.rm = TRUE)
data_clean$outlier_flag <- abs_returns > outlier_threshold

print(paste("Outlier threshold:", round(outlier_threshold, 5)))
print(paste("Number of extreme return days:", sum(data_clean$outlier_flag)))

#stationnarity : 
adf.test(log_returns_std)

#Plot
ggplot(data_clean, aes(x = date, y = X.Adj.close)) +
  geom_line(color = "#1f78b4", linewidth = 1) +
  
  labs(
    title = "S&P 500 Adjusted Close Since 2015",
    x = "Date",
    y = "Price"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()   
  )


ggplot(data_clean, aes(x = date, y = log_return_std)) +
  
  geom_ribbon(
    aes(ymin = -2, ymax = 2),
    fill = "gray90"
  ) +
  
  geom_line(color = "#54278f", linewidth = 0.6) +
  
  geom_hline(yintercept = 0, color = "black") +
  
  labs(
    title = "S&P 500 Standardized Log-Returns",
    x = "Date",
    y = "Standardized Returns"
  ) +
  
  theme_minimal()
hist(log_returns_std, breaks = 100, main = "Histogram of S&P 500 log-returns")
qqnorm(as.numeric(log_returns_std), main = "Q-Q Plot of log_returns_std")
qqline(as.numeric(log_returns_std))
boxplot(log_returns_std)
quantile(log_returns_std, probs = c(0.001, 0.999))

# Save processed data
write.csv(data_clean,
          "data/sp500_clean_data.csv",
          row.names = FALSE)










