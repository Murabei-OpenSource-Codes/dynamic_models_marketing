rm(list = ls())
library(RBATS)
library(dplyr)
library(ggplot2)
library(cowplot)
theme_set(
  theme_cowplot(font_size = 14, font_family = "Palatino") +
    background_grid() +
    theme(text = element_text(size = 14, family = "Palatino"))
)
blue <- rgb(32, 114, 184, maxColorValue = 255)
red <- rgb(237, 0, 0, maxColorValue = 255)

# Data --------------------------------------------------------------------

plot(AirPassengers)
y <- c(AirPassengers)
dates <- seq.Date(as.Date("1949-01-01"), as.Date("1960-12-01"), by = "month")
df_dates <- data.frame(t = seq.int(1, length(y)), time = dates)
length(dates) == length(y)

chosen_cols <- c("t", "sample", "y", "mean", "ci_lower", "ci_upper")

# Fit model ---------------------------------------------------------------
model_object <- dlm(polynomial_order = 2,
                    seasonal = list(type = "fourier", period = 12,
                                    harmonics = 1:2),
                    discount_factors = list(polynomial = 0.95, seasonal = 0.98))

# First split
y_fit <- y[df_dates$time <= "1955-01-01"]
y_future <- y[df_dates$time > "1955-01-01"][1:36]

fitted_model <- fit(model_object, y = y_fit, prior_length = 10)
forecast_model <- forecast(fitted_model$model, horizon = 36)

fitted_model$data_predictive$sample <- "1-step ahead forecast"
fitted_model$data_predictive$y <- y_fit
forecast_model$predictive$sample <- "k-step ahead forecast"
forecast_model$predictive$y <- y_future

data_results <- rbind(fitted_model$data_predictive[, chosen_cols],
                      forecast_model$predictive[, chosen_cols])
data_results <- data_results %>% 
  left_join(df_dates, by = "t") %>% 
  filter(time >= "1950-01-01")

p1 <- ggplot(data_results, aes(x = time, y = y)) +
  geom_point(size = 2) +
  geom_line(aes(y = mean, col = sample), size = 0.8, show.legend = FALSE) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, col = "grey69") +
  scale_x_date(breaks = scales::pretty_breaks(6), date_labels = "%b/%Y") +
  scale_y_continuous(breaks = scales::pretty_breaks(6)) +
  labs(x = "Time", y = "Airline passengers", col = "") +
  scale_color_manual(values = c(red, blue)) +
  theme(legend.position = "top")

# Second split
y_fit <- y[df_dates$time <= "1957-01-01"]
y_future <- y[df_dates$time > "1957-01-01"][1:12]

fitted_model <- fit(model_object, y = y_fit, prior_length = 10)
forecast_model <- forecast(fitted_model$model, horizon = 12)

fitted_model$data_predictive$sample <- "1-step ahead forecast"
fitted_model$data_predictive$y <- y_fit
forecast_model$predictive$sample <- "k-step ahead forecast"
forecast_model$predictive$y <- y_future

data_results <- rbind(fitted_model$data_predictive[, chosen_cols],
                      forecast_model$predictive[, chosen_cols])
data_results <- data_results %>% 
  left_join(df_dates, by = "t") %>% 
  filter(time >= "1950-01-01")

p2 <- ggplot(data_results, aes(x = time, y = y)) +
  geom_point(size = 2) +
  geom_line(aes(y = mean, col = sample), size = 0.8, show.legend = FALSE) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, col = "grey69") +
  scale_x_date(breaks = scales::pretty_breaks(6), date_labels = "%b/%Y") +
  scale_y_continuous(breaks = scales::pretty_breaks(6)) +
  labs(x = "Time", y = "Airline passengers", col = "") +
  scale_color_manual(values = c(red, blue)) +
  theme(legend.position = "top")


p_grid <- plot_grid(p1, p2, ncol = 2, labels = "AUTO", label_size = 20)
# x11(); p_grid
save_plot(filename = "./figures/air_passengers_learning.eps",
          plot = p_grid, base_height = 4, base_width = 18, device = cairo_ps)
