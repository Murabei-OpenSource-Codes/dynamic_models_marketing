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
length(dates) == length(y)

# Fit model ---------------------------------------------------------------
model_object <- dlm(polynomial_order = 2,
                    seasonal = list(type = "fourier", period = 12, harmonics = 1:2),
                    discount_factors = list(polynomial = 0.95, seasonal = 0.98))
fitted_model <- fit(model_object, y = c(AirPassengers), prior_length = 13)
logLik(fitted_model)


# Tidy the data -----------------------------------------------------------

nparms <- nrow(fitted_model$model$FF)
df_posterior <- fitted_model$data_posterior %>% 
  mutate(y = rep(y, times = nparms), time = rep(dates, times = nparms),
         type = "filter") %>% 
  as_tibble() |> 
  bind_rows(
    fitted_model$data_posterior_smooth %>% 
      mutate(y = rep(y, times = nparms), time = rep(dates, times = nparms),
             type = "smooth") %>% 
      as_tibble()
  )
df_predictive <- fitted_model$data_predictive %>% 
  mutate(y = y, time = dates, type = "filter") %>% 
  as_tibble() |> 
  bind_rows(
    fitted_model$data_predictive_smooth %>% 
      mutate(y = y, time = dates, type = "smooth") %>% 
      as_tibble()
  )


# Plotting ----------------------------------------------------------------
p_ts <- df_predictive %>% 
  filter(type == "smooth") |> 
  ggplot(aes(x = time, y = y)) +
  geom_point(size = 2) +
  scale_x_date(breaks = scales::pretty_breaks(6), date_labels = "%b/%Y") +
  scale_y_continuous(breaks = scales::pretty_breaks(6)) +
  labs(x = "Time", y = "Airline passengers")
# save_plot("./latex/figures/air_passengers_data.pdf", plot = p_ts,
#           base_height = 6, base_width = 10)


# Predictive --------------------------------------------------------------
p_predictive_filter <- df_predictive %>% 
  filter(type != "smooth") |> 
  slice(-(1:12)) %>% 
  ggplot(aes(x = time, y = y)) +
  geom_point(size = 2) +
  geom_line(aes(y = mean), col = blue, size = 1.2) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), col = "grey69", alpha = 0.2) +
  scale_x_date(breaks = scales::pretty_breaks(6), date_labels = "%b/%Y") +
  scale_y_continuous(breaks = scales::pretty_breaks(6)) +
  labs(x = "Time", y = "Airline passengers")
save_plot("./figures/air_passengers_predictive_filter.eps",
          plot = p_predictive_filter, base_height = 4, base_width = 10,
          device = cairo_ps)

p_predictive_smooth <- df_predictive %>% 
  filter(type == "smooth") |> 
  slice(-(1:12)) %>% 
  ggplot(aes(x = time, y = y)) +
  geom_point(size = 2) +
  geom_line(aes(y = mean), col = blue, size = 1.2) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), col = "grey69", alpha = 0.2) +
  scale_x_date(breaks = scales::pretty_breaks(6), date_labels = "%b/%Y") +
  scale_y_continuous(breaks = scales::pretty_breaks(6)) +
  labs(x = "Time", y = "Airline passengers")
# save_plot("./latex/figures/air_passengers_predictive_smooth.pdf",
#           plot = p_predictive_smooth, base_height = 6, base_width = 10)


# Posterior ---------------------------------------------------------------
p_level_filter <- df_posterior %>% 
  filter(type != "smooth", parameter == "level") %>% 
  slice(-(1:12)) %>%
  ggplot(aes(x = time, y = y)) +
  geom_point(size = 2) +
  geom_line(aes(y = mean), col = blue, size = 1.2) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), col = "grey69", alpha = 0.2) +
  scale_x_date(breaks = scales::pretty_breaks(6), date_labels = "%b/%Y") +
  scale_y_continuous(breaks = scales::pretty_breaks(6)) +
  labs(x = "Time", y = "Airline passengers")
# save_plot("./latex/figures/air_passengers_level_filter.pdf",
#           plot = p_level_filter, base_height = 6, base_width = 10)

p_level_smooth <- df_posterior %>% 
  filter(type == "smooth", parameter == "level") %>% 
  slice(-(1:12)) %>%
  ggplot(aes(x = time, y = y)) +
  geom_point(size = 2) +
  geom_line(aes(y = mean), col = blue, size = 1.2) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), col = "grey69", alpha = 0.2) +
  scale_x_date(breaks = scales::pretty_breaks(6), date_labels = "%b/%Y") +
  scale_y_continuous(breaks = scales::pretty_breaks(6)) +
  labs(x = "Time", y = "Airline passengers")
# save_plot("./latex/figures/air_passengers_level_smooth.pdf",
#           plot = p_level_smooth, base_height = 6, base_width = 10)

# growth
p_growth_filter <- df_posterior %>% 
  filter(type == "filter", parameter == "growth") %>% 
  slice(-(1:12)) %>%
  ggplot(aes(x = time, y = mean)) +
  geom_line(col = blue, size = 1.2) +
  # geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), col = "grey69", alpha = 0.2) +
  scale_x_date(breaks = scales::pretty_breaks(6), date_labels = "%b/%Y") +
  scale_y_continuous(breaks = scales::pretty_breaks(6)) +
  labs(x = "Time", y = "Growth")
# save_plot("./latex/figures/air_passengers_growth_filter.pdf",
#           plot = p_growth_filter, base_height = 6, base_width = 10)

p_growth_smooth <- df_posterior %>% 
  filter(type == "smooth", parameter == "growth") %>% 
  slice(-(1:12)) %>%
  ggplot(aes(x = time, y = mean)) +
  geom_line(col = blue, size = 1.2) +
  # geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), col = "grey69", alpha = 0.2) +
  scale_x_date(breaks = scales::pretty_breaks(6), date_labels = "%b/%Y") +
  scale_y_continuous(breaks = scales::pretty_breaks(6)) +
  labs(x = "Time", y = "Growth")
# save_plot("./latex/figures/air_passengers_growth_smooth.pdf",
#           plot = p_growth_smooth, base_height = 6, base_width = 10)

# Seasonality
df_seasonality <- df_posterior %>% 
  filter(!(parameter %in% c("growth", "level"))) %>% 
  group_by(time, type) %>% 
  summarise(mean = sum(mean), ci_lower = sum(ci_lower),
            ci_upper = sum(ci_upper), .groups = "drop")

p_seasonality_filter <- df_seasonality %>% 
  filter(type == "filter") |> 
  slice(-(1:12)) %>% 
  ggplot(aes(x = time, y = mean)) +
  geom_line(col = blue, size = 1.2) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), col = "grey69", alpha = 0.2) +
  scale_x_date(breaks = scales::pretty_breaks(6), date_labels = "%b/%Y") +
  scale_y_continuous(breaks = scales::pretty_breaks(6)) +
  labs(x = "Time", y = "Seasonality")
# save_plot("./latex/figures/air_passengers_seasonality_filter.pdf",
#           plot = p_seasonality_filter, base_height = 6, base_width = 10)

p_seasonality_smooth <- df_seasonality %>% 
  filter(type == "smooth") |> 
  slice(-(1:12)) %>% 
  ggplot(aes(x = time, y = mean)) +
  geom_line(col = blue, size = 1.2) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), col = "grey69", alpha = 0.2) +
  scale_x_date(breaks = scales::pretty_breaks(6), date_labels = "%b/%Y") +
  scale_y_continuous(breaks = scales::pretty_breaks(6)) +
  labs(x = "Time", y = "Seasonality")
# save_plot("./latex/figures/air_passengers_seasonality_smooth.pdf",
#           plot = p_seasonality_smooth, base_height = 6, base_width = 10)

# Grids -------------------------------------------------------------------

p_grid_filter <- plot_grid(
  p_ts, p_level_filter, p_growth_filter, p_seasonality_filter,
  ncol = 2, labels = "AUTO", label_size = 20)
save_plot("./figures/air_passengers_grid_filter.eps",
          plot = p_grid_filter, base_height = 6, base_width = 16,
          bg = "white", device = cairo_ps)

p_grid_smooth <- plot_grid(
  p_predictive_smooth, p_level_smooth, p_growth_smooth, p_seasonality_smooth,
  ncol = 2, labels = "AUTO", label_size = 20)
save_plot("./figures/air_passengers_grid_smooth.eps",
          plot = p_grid_smooth, base_height = 6, base_width = 16,
          device = cairo_ps)
