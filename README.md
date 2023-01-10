### A Review of Bayesian Dynamic Forecasting Models: Applications in Marketing

This repository contains the R and Python codes used for the applications in
the paper
`A Review of Bayesian Dynamic Forecasting Models: Applications in Marketing`
by Migon, H. S., Alves, M. B., Menezes, A. F. B, and Pinheiro, E. G.

To run the codes you need an R and Python session with the following packages
installed for the R:

```
R_pkgs <- c("remotes", "dplyr", "ggplot2", "cowplot", "Metrics", "xtable")
install.packages(R_pkgs)
remotes::install_github("AndrMenezes/RBATS", build_vignettes = TRUE)
```
and for Python, they are listed in the `requirements.txt` and can be installed
as follows:
```
pip install -r requirements.txt
```

### Codes organization

The codes are organized following the paper sections order.

- [x] Section 3.4: Some examples of inference in DLM
  - [x] i) The components of a time series: `3_4__air_passenger.R`
  - [x] ii) Sequential nature of Bayesian inference: `3_4__air_passenger_sequential_learning.R`
  - [x] iii) Retrospective analysis or smoothing: `3_4__air_passenger.R`
- [x] Section 3.5: Practical Aspects of Bayesian Forecasting Models
  - [x] i) Normal model with transformed data: `3_5__air_passenger_transformed.R`
  - [x] ii) Discount factors and missing data: `3_5__cp6_discount_factor.R`
  - [x] iv) Intervention analysis: `3_5__cp6_intervention.py` and `3_5__cp6_intervention_plots.R`
  - [x] v) Bayesian monitoring and interventions: `3_5__simulated_example.py`, `3_5__simulated_example_plots.R`, `3_5__telephone_calls.py` and `3_5__telephone_calls_plots.R`

- [x] Section 4.3: Illustration: a Poisson model for quarterly sales
  - [x]  `4_3__turkey_sales_poisson.py` and `4_3__turkey_sales_poisson_plots.R`

### License

Those codes are released under the Apache License, Version 2.0. Please, see file [LICENSE.md](https://github.com/Murabei-OpenSource-Codes/dynamic_models_marketing/blob/develop/LICENSE.md).
