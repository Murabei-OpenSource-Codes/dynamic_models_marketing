rm(list = ls())
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

data_posterior_cp6 <- read.csv("./data/posterior_intervention__cp6.csv")
data_posterior_cp6$time <- as.Date(data_posterior_cp6$time)
glimpse(data_posterior_cp6)

data_curr <- data_posterior_cp6 |>
  filter(parameter == "Intercept") |>
  mutate(
    intervention = intervention == "True",
    lab_interv = ifelse(intervention, "With intervention",
                        "Without intervention"))
head(data_curr)

brks <- as.Date(c("1955-03-01", "1956-03-01", "1957-03-01", "1958-03-01",
                  "1959-03-01", "1960-03-01"))

# Model metrics
data_metrics <- data_curr |>
  group_by(lab_interv) |>
  summarise(mape = mean(abs(sales - mean) / sales),
            mspe = mean(((sales - mean) / sales)^2))

# Plotting ----------------------------------------------------------------
p <- ggplot(data_curr, aes(x = time, y = sales)) +
  facet_wrap(~lab_interv) +
  geom_point(size = 3) +
  geom_line(aes(y = mean), col = blue, size = 1.2) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), col = "grey69", alpha = 0.2) +
  scale_x_date(breaks = scales::pretty_breaks(6), date_labels = "%b/%Y") +
  scale_y_continuous(breaks = scales::pretty_breaks(8)) +
  labs(x = "Time", y = "Sales")

# save_plot(filename = file.path("./latex/figures", "cp6_intervention__facet.pdf"),
#           plot = p, base_height = 6, base_width = 12)

p_without <- data_curr |>
  filter(!intervention) |>
  ggplot(aes(x = time, y = sales)) +
  geom_point(size = 2) +
  geom_line(aes(y = mean), col = blue, size = 1.2) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), col = "grey69", alpha = 0.2) +
  scale_x_date(breaks = scales::pretty_breaks(6), date_labels = "%b/%Y") +
  scale_y_continuous(breaks = scales::pretty_breaks(8)) +
  labs(x = "Time", y = "Sales")

p_with <- data_curr |>
  filter(intervention) |>
  ggplot(aes(x = time, y = sales)) +
  geom_point(size = 2) +
  geom_line(aes(y = mean), col = blue, size = 1.2) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), col = "grey69", alpha = 0.2) +
  scale_x_date(breaks = scales::pretty_breaks(6), date_labels = "%b/%Y") +
  scale_y_continuous(breaks = scales::pretty_breaks(8)) +
  labs(x = "Time", y = "Sales")
p_grid_smooth <- plot_grid(
  p_without, p_with, ncol = 2, labels = "AUTO", label_size = 20)
save_plot("./figures/cp6_intervention.eps",
          plot = p_grid_smooth, base_height = 4, base_width = 18,
          device = cairo_ps)
