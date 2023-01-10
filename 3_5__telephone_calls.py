"""Monitoring in monthly telephone calls."""
import numpy as np
import pandas as pd
from pybats.dglm import dlm
from pybats_detection.monitor import Monitoring
from pybats_detection.loader import load_telephone_calls

###############################################################################
# Paths
results_path = "./modelagem/2021-10-21__paper_examples/modelos/resultados/%s"

# Load data
telephone_calls = load_telephone_calls()

###############################################################################
# Defining the model
a = np.array([350, 0])
R = np.eye(2)
np.fill_diagonal(R, val=[100])
mod = dlm(a, R, ntrend=2, deltrend=0.90)

# Fitting without the monitoring
monitor = Monitoring(mod=mod)
fit_no_monitor = monitor.fit(y=telephone_calls["average_daily_calls"],
                             h=10000, bilateral=True, prior_length=40)

# Fitting with the automatic monitoring
fit_monitor = monitor.fit(y=telephone_calls["average_daily_calls"], h=4,
                          tau=0.135,
                          discount_factors={"trend": [0.20, 0.9]},
                          bilateral=True, prior_length=40)

# Organize the results
data_predictive_no_monitor = fit_no_monitor.get(
    "filter").get("predictive").copy()
data_predictive_no_monitor["monitor"] = False
data_predictive_monitor = fit_monitor.get("filter").get("predictive").copy()
data_predictive_monitor["monitor"] = True
data_predictive = pd.concat(
    [data_predictive_no_monitor, data_predictive_monitor])
data_predictive = data_predictive.join(telephone_calls)

data_posterior_no_monitor = fit_no_monitor.get(
    "smooth").get("posterior").copy()
data_posterior_no_monitor["monitor"] = False
data_posterior_monitor = fit_monitor.get("smooth").get("posterior").copy()
data_posterior_monitor["monitor"] = True
data_posterior = pd.concat(
    [data_posterior_no_monitor, data_posterior_monitor])
data_posterior = data_posterior.join(telephone_calls)

# Exporting
data_predictive.to_csv(
    "./data/predictive__telephone_calls.csv", index=False)
data_posterior.to_csv(
    "./data/posterior__telephone_calls.csv", index=False)
