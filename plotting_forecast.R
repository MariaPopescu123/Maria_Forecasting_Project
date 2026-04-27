library(ggplot2)
library(dplyr)
library(patchwork)

# Compute mean + 90% ribbon
summary_df <- forecast_output |>
  group_by(datetime) |>
  summarise(
    mean_pred  = mean(prediction, na.rm = TRUE),
    lower      = quantile(prediction, 0.05, na.rm = TRUE),
    upper      = quantile(prediction, 0.95, na.rm = TRUE),
    .groups = "drop"
  )

# Spaghetti plot
p0 <- forecast_output |>
  mutate(parameter = as.factor(parameter)) |>
  ggplot(aes(x = datetime, y = prediction, group = parameter)) +
  geom_line(alpha = 0.25, linewidth = 0.4, color = "#1D9E75") +
  labs(
    title    = "DCM Depth Forecast - Ensemble members",
    subtitle = paste0("Reference: ", unique(forecast_output$reference_datetime), " · fcre · n=", n_distinct(forecast_output$parameter)),
    x        = NULL,
    y        = "DCM Depth (m)"
  ) +
  scale_y_reverse() +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

# 1. Mean + ribbon
p1 <- summary_df |>
  ggplot(aes(x = datetime)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#1D9E75", alpha = 0.2) +
  geom_line(aes(y = mean_pred), color = "#1D9E75", linewidth = 1) +
  labs(
    title    = "DCM Depth Forecast - Ensemble mean ± 90% interval",
    subtitle = paste0("Reference: ", unique(forecast_output$reference_datetime), " · fcre"),
    x        = "Date",
    y        = "DCM Depth (m)"
  ) +
  scale_y_reverse() +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

p1
p0

check <- forecast_output |>
  mutate(parameter = as.numeric(parameter))|>
  group_by(datetime) |>
  arrange(parameter, .by_group = TRUE)
