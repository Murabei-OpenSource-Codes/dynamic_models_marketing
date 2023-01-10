"""Intervention in CP6 data."""
import numpy as np
import pandas as pd
from pybats.dglm import dlm
from pybats_detection.intervention import Intervention
from pybats_detection.loader import load_cp6

# Load data
cp6 = load_cp6()

# Define the dlm
a = np.array([600, 1])
R = np.array([[100, 0], [0, 25]])
mod = dlm(a, R, ntrend=2, deltrend=[0.90, 0.98])

# Fit without intervention
fit_no_intervention = Intervention(mod=mod, level=0.05).fit(
    y=cp6["sales"], interventions=[])

# Fit with intervention
list_interventions = [
    {"time_index": 12, "which": ["variance", "noise"],
     "parameters": [{"v_shift": "ignore"},
                    {"h_shift": np.array([0, 0]),
                     "H_shift": np.array([[1000, 25], [25, 25]])}]
     },
    {"time_index": 25, "which": ["noise", "variance"],
     "parameters": [{"h_shift": np.array([80, 0]),
                     "H_shift": np.array([[100, 0], [0, 0]])},
                    {"v_shift": "ignore"}]},
    {"time_index": 37, "which": ["subjective"],
     "parameters": [{"a_star": np.array([970, 0]),
                     "R_star": np.array([[50, 0], [0, 5]])}]}
]

fit_intervention = Intervention(mod=mod, level=0.05).fit(
    y=cp6["sales"], interventions=list_interventions)

# Organize the results
data_predictive_no_intervention = fit_no_intervention.get(
    "filter").get("predictive").copy()
data_predictive_no_intervention["intervention"] = False
data_predictive_intervention = fit_intervention.get(
    "filter").get("predictive").copy()
data_predictive_intervention["intervention"] = True
data_predictive = pd.concat(
    [data_predictive_no_intervention, data_predictive_intervention])
data_predictive = data_predictive.join(cp6)

data_posterior_no_intervention = fit_no_intervention.get(
    "smooth").get("posterior").copy()
data_posterior_no_intervention["intervention"] = False
data_posterior_intervention = fit_intervention.get(
    "smooth").get("posterior").copy()
data_posterior_intervention["intervention"] = True
data_posterior = pd.concat(
    [data_posterior_no_intervention, data_posterior_intervention])
cp6["t"] = cp6.index + 1
data_posterior = data_posterior.merge(
    cp6, on="t", how="left", validate="many_to_one")

# Exporting
data_predictive.to_csv("./data/predictive_intervention__cp6.csv", index=False)
data_posterior.to_csv("./data/posterior_intervention__cp6.csv", index=False)
