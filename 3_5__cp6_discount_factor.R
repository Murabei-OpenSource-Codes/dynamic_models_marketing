rm(list = ls())
library(RBATS)
library(dplyr)
library(ggplot2)
library(cowplot)
library(xtable)
theme_set(
  theme_cowplot(font_size = 14, font_family = "Palatino") +
    background_grid() +
    theme(text = element_text(size = 14, family = "Palatino"))
)
blue <- rgb(32, 114, 184, maxColorValue = 255)
red <- rgb(237, 0, 0, maxColorValue = 255)

# Import data -------------------------------------------------------------
data_cp6 <- read.csv("./data/cp6__west_harrison.csv")
data_cp6$time <- as.Date(data_cp6$time)
str(data_cp6)
plot(data_cp6$sales)

y <- c(data_cp6$sales)
dates <- data_cp6$time
length(y) == length(dates)

# Data --------------------------------------------------------------------
get_model_results <- function(y, dates, d_trend = 0.95, index_missing = NULL) {

  # y_missing <- y[index_missing]
  if (!is.null(index_missing)) y[index_missing] <- NA_real_

  model_object <- dlm(polynomial_order = 2,
                      discount_factors = list(polynomial = d_trend))
  fitted_model <- fit(model_object, y = y, prior_length = 13)

  # y[index_missing] <- y_missing

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
    ) |>
    mutate(delta = d_trend)

  df_predictive <- fitted_model$data_predictive %>%
    mutate(y = y, time = dates, type = "filter") %>%
    as_tibble() |>
    bind_rows(
      fitted_model$data_predictive_smooth %>%
        mutate(y = y, time = dates, type = "smooth") %>%
        as_tibble()
    ) |>
    mutate(delta = d_trend)

  y_fcast <- df_predictive |>
    filter(type == "filter") |>
    pull(mean)

  # Model metrics
  data_metrics <- data.frame(
    ll = logLik(fitted_model)[1],
    mape = mean(abs(y - y_fcast) / y, na.rm = TRUE),
    mpe = mean((y - y_fcast) / y, na.rm = TRUE),
    mae = mean(abs(y - y_fcast), na.rm = TRUE),
    mase = Metrics::mase(y, y_fcast),
    delta = d_trend
  )

  list(
    predictive = df_predictive, posterior = df_posterior,
    metrics = data_metrics
  )
}

v_delta <- c(0.70, 0.8, 0.85, 0.90, 0.95, 1)
out <- lapply(v_delta, function(x) {
  get_model_results(y = y, dates = dates, d_trend = x, index_missing = NULL)
})

data_metrics <- do.call(rbind, lapply(out, "[[", "metrics"))
data_posterior <- do.call(rbind, lapply(out, "[[", "posterior"))
data_predictive <- do.call(rbind, lapply(out, "[[", "predictive"))

print(xtable(data_metrics, digits = 4), include.rownames = FALSE)

# Plotting ----------------------------------------------------------------
list_plots <- lapply(
  unique(data_predictive$delta),
  function(chosen) {
    data_posterior |>
      filter(delta == chosen, type == "smooth", parameter == "level") |>
      slice(-(1:12)) %>%
      ggplot(aes(x = time, y = y)) +
      geom_point(size = 2) +
      # geom_line() +
      geom_line(aes(y = mean), col = blue, size = 1.2) +
      geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
                  col = "grey69", alpha = 0.2) +
      scale_x_date(breaks = scales::pretty_breaks(6), date_labels = "%b/%Y") +
      scale_y_continuous(breaks = scales::pretty_breaks(8)) +
      labs(x = "Time", y = "Sales")
  })

p_grid <- plot_grid(plotlist = list_plots[c(2, 5, 6)],
                    ncol = 3, labels = "AUTO", label_size = 20)

save_plot("./figures/cp6_discount_factor.eps",
          plot = p_grid, base_height = 4, base_width = 22,
          device = cairo_ps)
