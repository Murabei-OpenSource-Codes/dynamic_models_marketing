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
red <- rgb(237, 0, 0, maxColorValue = 255)

# Data --------------------------------------------------------------------

plot(AirPassengers)
y <- c(AirPassengers)
dates <- seq.Date(as.Date("1949-01-01"), as.Date("1960-12-01"), by = "month")
length(dates) == length(y)

# Fit model ---------------------------------------------------------------
get_model_results <- function(y, dates, lambda = 1) {
  
  n <- length(y)
  log_jacobian <- if (lambda == 0) sum(log(1 / y[-(1:12)])) else n * log(lambda) + (lambda - 1) * sum(log(y[-(1:12)]))
  y <- if (lambda == 0) log(y) else y^lambda

  model_object <- dlm(polynomial_order = 2,
                      seasonal = list(type = "fourier", period = 12, harmonics = 1:2),
                      discount_factors = list(polynomial = 0.95, seasonal = 0.98))
  fitted_model <- fit(model_object, y = y, prior_length = 13)
  
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
    mutate(lambda = lambda)
  
  df_predictive <- fitted_model$data_predictive %>% 
    mutate(y = y, time = dates, type = "filter") %>% 
    as_tibble() |> 
    bind_rows(
      fitted_model$data_predictive_smooth %>% 
        mutate(y = y, time = dates, type = "smooth") %>% 
        as_tibble()
    ) |> 
    mutate(lambda = lambda)

  g_inv <- if (lambda == 0) function(x, l) exp(x) else function(x, l) x^(1/l)
  g_inv_p <- if (lambda == 0) function(x, l) exp(x) else function(x, l) x^(1/l - 1) / l 
  g_inv_pp <- if (lambda == 0) function(x, l) exp(x) else function(x, l) x^(1/l - 2) * (1 - l) / l^2 
  
  l <- lambda
  df_predictive <- df_predictive |> 
    mutate(mean_original = g_inv(mean, l) + g_inv_pp(mean, l) * variance / 2,
           variance_original = g_inv_p(mean, l)^2 * variance / 2,
           ci_lower_original = mean_original - qnorm(1-0.025) * sqrt(variance_original),
           ci_upper_original = mean_original + qnorm(1-0.025) * sqrt(variance_original),
           y_original = g_inv(y, l))

  y_fcast <- df_predictive |> 
    filter(type == "filter") |> 
    pull(mean)
  
  data_ll <- df_predictive |> 
    filter(type == "filter") |> 
    slice(-(1:12)) |> 
    summarise(
      lpl_1 = log_jacobian + sum(log(1/sqrt(variance) * dt((y - mean)/sqrt(variance), df))),
      lpl_2 = sum(log(1/sqrt(variance_original) * dt((y_original - mean_original)/sqrt(variance_original), df)))
    )
  
  
  # Model metrics
  data_metrics <- data.frame(
    ll_1 = data_ll$lpl_1,
    ll_2 = data_ll$lpl_2,
    mape = mean(abs(y[-(1:12)] - y_fcast[-(1:12)]) / y[-(1:12)]),
    mspe = mean(((y[-(1:12)] - y_fcast[-(1:12)]) / y[-(1:12)])^2),
    lambda = lambda
  )
  
  list(
    predictive = df_predictive, posterior = df_posterior,
    metrics = data_metrics
  )
}

v_lambdas <- c(1, 1/2, 3/4, 0)
out <- lapply(v_lambdas, function(x) {
  get_model_results(y = y, dates = dates, lambda = x)
})

data_metrics <- do.call(rbind, lapply(out, "[[", "metrics"))
data_posterior <- do.call(rbind, lapply(out, "[[", "posterior"))
data_predictive <- do.call(rbind, lapply(out, "[[", "predictive"))


print(xtable(data_metrics |> 
               select(lambda, ll_1, mape, mspe), digits = 3),
      include.rownames = FALSE)

# Plotting ----------------------------------------------------------------
aux <- data_predictive |> 
  filter(lambda == 0, type == "filter") |> 
  slice(-(1:12)) |> 
  summarise(min = min(ci_lower_original),
            max = max(ci_upper_original))
ylim <- c(aux$min, aux$max)

list_plots = lapply(
  unique(data_predictive$lambda),
  function(chosen) {
    cat(chosen, "\n")
    
    if (chosen == 1) trans <- "original"
    if (chosen == 0) trans <- "log"
    if (chosen == 1/2) trans <- "sqrt"
    if (chosen == 3/4) trans <- "potência 3/4"
    
    data_predictive |> 
      filter(lambda == chosen, type == "filter") |> 
      slice(-(1:12)) %>% 
      ggplot(aes(x = time, y = y_original)) +
      geom_point(size = 2) +
      geom_line(aes(y = mean_original), col = blue, size = 1.2) +
      geom_ribbon(aes(ymin = ci_lower_original, ymax = ci_upper_original),
                  col = "grey69", alpha = 0.2) +
      scale_x_date(breaks = scales::pretty_breaks(6), date_labels = "%b/%Y") +
      scale_y_continuous(breaks = scales::pretty_breaks(6), limits = ylim) +
      labs(x = "Time", y = "Airline passengers") 
    })

p_grid <- plot_grid(plotlist = list_plots, ncol = 2, labels = "AUTO",
                    label_size = 20)
save_plot("./figures/air_passengers_transformed.eps",
          plot = p_grid, base_height = 6, base_width = 16,
          device = cairo_ps)
