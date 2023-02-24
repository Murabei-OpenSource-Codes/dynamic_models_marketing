rm(list = ls())
library(RBATS)
library(dplyr)
library(ggplot2)
library(cowplot)
library(xtable)
library(Metrics)
theme_set(
  theme_cowplot(font_size = 14, font_family = "Palatino") +
    background_grid() +
    theme(text = element_text(size = 14, family = "Palatino"))
)
blue <- rgb(32, 114, 184, maxColorValue = 255)
red <- rgb(237,0,0, maxColorValue = 255)

# Data --------------------------------------------------------------------
data_predictive <- read.csv(file = "./data/turkey_sales__predictive.csv") |> 
  mutate(time = as.Date(time))
data_posterior <- read.csv(file = "./data/turkey_sales__posterior.csv") |> 
  mutate(time = as.Date(time))
glimpse(data_predictive)
glimpse(data_posterior)

# Plot predictive ---------------------------------------------------------
p_predictive <- data_predictive |> 
  filter(type == "filter") |> 
  slice(-(1:6)) |>
  ggplot(aes(x = time, y = sale)) +
  geom_point(size = 2.0) +
  geom_line(aes(y = mode), col = blue, size = 1.2) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), col = "grey69", alpha = 0.2) +
  scale_x_date(breaks = scales::pretty_breaks(8), date_labels = "%b/%Y") +
  scale_y_continuous(breaks = scales::pretty_breaks(6)) +
  labs(x = "Time", y = "Sales")
save_plot(filename = "./figures/turkey_predictive.eps",
          plot = p_predictive, base_height = 4, base_width = 12,
          device = cairo_ps) 
data_predictive |> 
  filter(type == "filter") |> 
  slice(-(1:6)) |> 
  summarise(mape = mean(abs((mode - sale) / sale)))


# Plot posterior ----------------------------------------------------------
data_posterior <- data_posterior |> 
  mutate(ci_lower = mean - 2 * sqrt(variance),
         ci_upper = mean + 2 * sqrt(variance))

# Level
p_level <- data_posterior |> 
  filter(type == "smooth", parameter == "Intercept") |> 
  # slice(-(1:12)) |> 
  ggplot(aes(x = time, y = sale)) +
  geom_point(size = 2) +
  geom_line(aes(y = exp(mean)), col = blue, size = 1.2) +
  geom_ribbon(aes(ymin = exp(ci_lower), ymax = exp(ci_upper)), col = "grey69",
              alpha = 0.2) +
  scale_x_date(breaks = scales::pretty_breaks(8), date_labels = "%b/%Y") +
  scale_y_continuous(breaks = scales::pretty_breaks(4)) +
  labs(x = "Time", y = "Sales")

# Growth
p_growth <- data_posterior |> 
  filter(type == "smooth", parameter == "Local Slope") |> 
  # slice(-(1:12)) |> 
  ggplot(aes(x = time, y = exp(mean))) +
  geom_line(aes(y = mean), col = blue, size = 1.2) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), col = "grey69",
              alpha = 0.2) +
  scale_x_date(breaks = scales::pretty_breaks(8), date_labels = "%b/%Y") +
  scale_y_continuous(breaks = scales::pretty_breaks(4)) +
  labs(x = "Time", y = "Growth")

# Seasonality
p_seasonal <- data_posterior |> 
  filter(type == "smooth", parameter == "Sum Seas 1") |> 
  # slice(-(1:12)) |> 
  ggplot(aes(x = time, y = mean)) +
  geom_line(aes(y = mean), col = blue, size = 1.2) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), col = "grey69",
              alpha = 0.2) +
  scale_x_date(breaks = scales::pretty_breaks(8), date_labels = "%b/%Y") +
  scale_y_continuous(breaks = scales::pretty_breaks(4)) +
  labs(x = "Time", y = "Seasonality")

p_data <- data_predictive |> 
  ggplot(aes(x = time, y = sale)) +
  geom_point(size = 2.0) +
  scale_x_date(breaks = scales::pretty_breaks(8), date_labels = "%b/%Y") +
  scale_y_continuous(breaks = scales::pretty_breaks(6)) +
  labs(x = "Time", y = "Sales")

p_predictive_smooth <- data_predictive |> 
  filter(type == "smooth") |> 
  ggplot(aes(x = time, y = sale)) +
  geom_point(size = 2.0) +
  geom_line(aes(y = mean), col = blue, size = 1.2) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), col = "grey69",
              alpha = 0.2) +
  scale_x_date(breaks = scales::pretty_breaks(8), date_labels = "%b/%Y") +
  scale_y_continuous(breaks = scales::pretty_breaks(6)) +
  labs(x = "Time", y = "Sales")


p_grid <- plot_grid(p_predictive_smooth, p_level, p_growth, p_seasonal,
                    ncol = 2, labels = "AUTO", label_size = 20)
p_grid
save_plot(filename = "./figures/turkey_grid.eps",
          plot = p_grid, base_height = 6, base_width = 18,
          device = cairo_ps) 
