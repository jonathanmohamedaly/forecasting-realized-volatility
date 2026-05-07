library(quantmod)
library(dplyr)
library(tibble)
start_date <- "2015-01-01"

getSymbols("^VIX", from = start_date, auto.assign = TRUE)

vix <- Cl(VIX)
colnames(vix) <- "VIX"
class(vix)

vix <- vix / 100
# Load realized vol data
data_rv <- read.csv("data/sp500_realized_volatility.csv")

# merge of the values
vix_clean <- data.frame(
  date = as.Date(index(vix)),
  VIX = coredata(vix)
)

data_rv_clean <- data_rv %>%
  mutate(date = as.Date(date))

data_full <- data_rv_clean %>%
  left_join(vix_clean, by = "date")

data_full <- na.omit(data_full)
data_full$vrp <- data_full$VIX - data_full$rv_21d

print("===== IMPLIED VOLATILITY SUMMARY =====")
print(paste("Number of observations:", nrow(data_full)))

# Save

write.csv(
  data.frame(date = index(data_full), coredata(data_full)),
  "data/sp500_full_data.csv",
  row.names = FALSE
)

#Plot
ggplot(data_full, aes(x = date)) +
  
  geom_line(aes(y = rv_21d, color = "Realized Vol (21d)"),
            linewidth = 1) +
  
  geom_line(aes(y = VIX, color = "VIX"),
            linewidth = 1,
            linetype = "dashed") +
  
  labs(
    title = "Implied vs Realized Volatility (SP&500)",
    x = "Date",
    y = "Volatility",
    color = ""
  ) +
  
  scale_color_manual(values = c(
    "Realized Vol (21d)" = "#1f77b4",
    "VIX" = "#d62728"
  )) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggplot(data_full, aes(x = date, y = vrp)) +
  
  geom_ribbon(aes(ymin = 0, ymax = vrp),
              fill = "#1f78b4", alpha = 0.2) +
  
  geom_line(color = "#1f78b4", linewidth = 1) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(
    title = "Volatility Risk Premium (VIX - RV)",
    x = "Date",
    y = "VRP"
  ) +
  
  theme_minimal()
