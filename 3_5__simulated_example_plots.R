rm(list = ls())
library(dplyr)
library(ggplot2)
library(cowplot)
theme_set(
  theme_cowplot(font_size = 18, font_family = "Palatino") +
    background_grid() +
    theme(text = element_text(size = 18, family = "Palatino"))
)
blue <- rgb(32, 114, 184, maxColorValue = 255)
red <- rgb(237, 0, 0, maxColorValue = 255)

# Data --------------------------------------------------------------------

data_predictive <- read.csv(file = "./data/predictive__simulated_example.csv") |> 
  mutate(monitor = monitor == "True",
         which_shape = what_detected != "nothing" & what_detected != "")
glimpse(data_predictive)

y_lim <- c(min(filter(data_predictive, t > 8)$ci_lower),
           max(filter(data_predictive, t > 8)$ci_upper))

p_with <- data_predictive |> 
  filter(t > 8, monitor) |> 
  ggplot(aes(x = t, y = y)) +
  geom_point(aes(shape = which_shape, size = which_shape), 
             show.legend = FALSE) +
  geom_line(aes(y = f), col = blue, size = 1.2) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), col = "grey69",
              alpha = 0.2) +
  scale_shape_manual(values = c(19, 4)) + 
  scale_size_manual(values = c(2, 3.5)) + 
  scale_color_manual(values = c("black", "tomato1")) + 
  scale_x_continuous(breaks = scales::pretty_breaks(8)) +
  scale_y_continuous(limits = y_lim, breaks = scales::pretty_breaks(6)) +
  labs(x = "Time", y = "y") 

p_without <- data_predictive |> 
  filter(t > 8, !monitor) |> 
  ggplot(aes(x = t, y = y)) +
  geom_point(aes(shape = which_shape), show.legend = FALSE, size = 2) +
  geom_line(aes(y = f), col = blue, size = 1.2) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), col = "grey69",
              alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(8)) +
  scale_y_continuous(limits = y_lim, breaks = scales::pretty_breaks(6)) +
  labs(x = "Time", y = "y")
p_grid <- plot_grid(
  p_without, p_with, ncol = 2, labels = "AUTO", label_size = 20)
p_grid
save_plot(filename = "./figures/simulated_example_predictive.eps",
          plot = p_grid, base_height = 4, base_width = 18,
          device = cairo_ps)

# Metrics
data_predictive |>
  group_by(monitor) |>
  summarise(mape = mean(abs(y - f) / y),
            mspe = mean(((y - f) / y)^2))
