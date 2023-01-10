"""Simulated example with two level change."""
import numpy as np
import pandas as pd
from pybats.dglm import dlm
from pybats_detection.monitor import Monitoring

# Simulating the data
np.random.seed(66)
y1 = np.random.normal(loc=100, scale=0.8, size=40)
y2 = np.random.normal(loc=104, scale=0.5, size=20)
y3 = np.random.normal(loc=98, scale=0.5, size=20)
y = np.concatenate([y1, y2, y3])
t = np.arange(0, len(y)) + 1
df_simulated = pd.DataFrame({"t": t, "y": y})

# Define the model
a = np.array([100])
R = np.eye(1)
R[[0]] = 100
mod = dlm(a, R, ntrend=1, deltrend=0.95)

# Initialize monitor object
monitor = Monitoring(mod=mod)

# Fit without monitoring
fit_no_monitor = monitor.fit(y=df_simulated["y"], h=100000)
data_predictive_no_monitor = fit_no_monitor.get("filter").get("predictive")

# Fit with monitoring
fit_monitor = monitor.fit(y=df_simulated["y"],
                          bilateral=True, h=4, tau=0.135,
                          discount_factors={"trend": 0.10},
                          verbose=True)
data_predictive_monitor = fit_monitor.get("filter").get("predictive")

# Appending
data_predictive_no_monitor["monitor"] = False
data_predictive_monitor["monitor"] = True
data_predictive = pd.concat(
    [data_predictive_no_monitor, data_predictive_monitor])

# Exporting
data_predictive.to_csv(
    "./data/predictive__simulated_example.csv", index=False)
