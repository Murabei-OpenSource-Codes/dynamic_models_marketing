rm(list = ls())
library(RBATS)
library(dplyr)
library(ggplot2)
library(cowplot)
library(xtable)
library(Metrics)
theme_set(
  theme_cowplot(font_size = 18, font_family = "Palatino") +
    background_grid() +
    theme(text = element_text(size = 18, family = "Palatino"))
)
blue <- rgb(32, 114, 184, maxColorValue = 255)
red <- rgb(237, 0, 0, maxColorValue = 255)

# Data --------------------------------------------------------------------
data_predictive <- read.csv("./data/predictive__telephone_calls.csv") |>
  mutate(time = as.Date(time),
         monitor = monitor == "True") |>
  as_tibble()
glimpse(data_predictive)

tab_interventions <- data_predictive |>
  filter(monitor, what_detected != "nothing") |>
  group_by(t) |>
  mutate(H = min(H_lower, H_upper), L = min(L_lower, L_upper),
         l = max(l_lower, l_upper)) |>
  ungroup() |>
  select(t, time, what_detected, e, l, L, H) |>
  mutate(time = paste0(lubridate::month(time, label = TRUE, abbr = TRUE), "/",
                       lubridate::year(time)))
print(xtable(tab_interventions), include.rownames = FALSE)


# x11()
data_predictive <- data_predictive |>
  mutate(which_shape = what_detected != "nothing")
ylim <- c(min(data_predictive$ci_lower), max(data_predictive$ci_upper))

p_monitor <- data_predictive |>
  filter(monitor) |>
  slice(-(1:4)) |>
  ggplot(aes(x = time, y = y)) +
  geom_point(aes(shape = which_shape, size = which_shape), show.legend = FALSE) +
  geom_line(aes(y = f), col = blue) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), col = "grey69", alpha = 0.2) +
  scale_shape_manual(values = c(19, 4)) +
  scale_x_date(breaks = scales::pretty_breaks(10), date_labels = "%b/%Y") +
  scale_y_continuous(breaks = scales::pretty_breaks(6), limits = ylim) +
  labs(x = "Time", y = "Average daily calls")

p_no_monitor <- data_predictive |>
  filter(!monitor) |>
  slice(-(1:4)) |>
  ggplot(aes(x = time, y = y)) +
  geom_point(aes(shape = which_shape), show.legend = FALSE, size = 1.2) +
  geom_line(aes(y = f), col = blue) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), col = "grey69", alpha = 0.2) +
  scale_shape_manual(values = c(19, 4)) +
  scale_x_date(breaks = scales::pretty_breaks(10), date_labels = "%b/%Y") +
  scale_y_continuous(breaks = scales::pretty_breaks(6), limits = ylim) +
  labs(x = "Time", y = "Average daily calls")

p_grid <- plot_grid(p_monitor, p_no_monitor, ncol = 2,
                    labels = "AUTO", label_size = 20)
p_grid
save_plot(filename = "./figures/telephone_calls_grid.eps",
          plot = p_grid, base_height = 4, base_width = 18,
          device = cairo_ps)

# Metrics
data_predictive |>
  group_by(monitor) |>
  summarise(mape = mean(abs(y - f) / y),
            mspe = mean(((y - f) / y)^2))
