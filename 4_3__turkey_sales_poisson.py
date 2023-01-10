"""Dynamic log-linear Poisson model."""
import numpy as np
import pandas as pd
from scipy.stats import nbinom
from pybats.dglm import pois_dglm
from utils import tidy_parameters

# Import data
data_turkey = pd.read_csv("./data/turkey_sales.csv")
pd_y = data_turkey["sale"].copy()

# Model specification
a = np.array([4.4, 0.045, -0.04, 0.5, 0.05, -0.0004])
R = np.eye(6)
np.fill_diagonal(R, val=[0.1, 0.001, 0.2, 0.04, 0.2, 1])
mod = pois_dglm(a0=a, R0=R, ntrend=2, deltrend=0.95, delseas=0.70,
                seasPeriods=[4], seasHarmComponents=[[1, 2]])

# Dictionaries to keep state and predictive parameters
dict_predictive = {"t": [], "y": [], "f": [], "q": [], "r": [], "p": [],
                   "beta": []}
dict_state_parms = {
    "prior": {"a": [], "R": []},
    "posterior": {"m": [], "C": []}
}

# Perform the filtering and keep the paramters
for t in range(0, len(pd_y)):
    yt = pd_y.values[t]
    # Get mean and variance
    ft, qt = mod.forecast_marginal(k=1, state_mean_var=True)
    ft = np.ravel(ft)[0]
    qt = np.ravel(qt)[0]
    # Get the parameters of NB(alpha, beta / (1 + beta))
    alpha, beta = mod.get_conjugate_params(ft, qt, mod.param1, mod.param2)
    alpha = np.ravel(alpha)[0]
    beta = np.ravel(beta)[0]
    p = beta / (1 + beta)

    # Saving prior state parameters
    dict_state_parms["prior"]["a"].append(mod.a)
    dict_state_parms["prior"]["R"].append(mod.R)

    # Update model
    mod.update(y=yt)

    # Saving posterior state parameters
    dict_state_parms["posterior"]["m"].append(mod.m)
    dict_state_parms["posterior"]["C"].append(mod.C)

    # Saving parameters
    dict_predictive["t"].append(t + 1)
    dict_predictive["y"].append(yt)
    dict_predictive["f"].append(ft)
    dict_predictive["q"].append(qt)
    dict_predictive["r"].append(alpha)
    dict_predictive["beta"].append(beta)
    dict_predictive["p"].append(p)
    # end of loop

# Organize the predictive parameters in DataFrame
df_predictive_filter = pd.DataFrame(dict_predictive)

# Organize the posterior parameters in DataFrame
df_posterior = tidy_parameters(
    dict_parameters=dict_state_parms["posterior"],
    entry_m="m", entry_v="C",
    names_parameters=list(mod.get_coef().index),
    index_seas_parameters=mod.iseas,
    F=mod.F)
n_parms = len(df_posterior["parameter"].unique())
t_index = np.arange(0, len(df_posterior) / n_parms) + 1
df_posterior["t"] = np.repeat(t_index, n_parms)
df_posterior["t"] = df_posterior["t"].astype(int)
df_posterior.sort_values(["parameter", "t"], inplace=True)

###############################################################################
# Smoothing
T_end = len(pd_y)
F = mod.F
G = mod.G
R = dict_state_parms["prior"]["R"]
a = dict_state_parms["prior"]["a"]
C = dict_state_parms["posterior"]["C"]
m = dict_state_parms["posterior"]["m"]
# Dictionaty to save predictive and posterior parameters
ak = m[T_end-1]
Rk = C[T_end-1]
fk = dict_predictive["f"][T_end-1]
qk = dict_predictive["q"][T_end-1]
beta = dict_predictive["beta"][T_end-1]
r = dict_predictive["r"][T_end-1]
p = dict_predictive["p"][T_end-1]
dict_smooth_parms = {
    "t": [T_end], "ak": [ak], "Rk": [Rk], "f": [fk], "q": [qk],
    "p": [p], "r": [r], "beta": [beta]}

# Perform smoothing
for k in range(1, T_end):
    # B_{t-k}
    B_t_k = C[T_end-k-1] @ G.T @ np.linalg.pinv(R[T_end-k])

    # a_t(-k) and R_t(-k)
    ak = m[T_end-k-1] + B_t_k @ (ak - a[T_end-k])
    Rk = C[T_end-k-1] + B_t_k @ (Rk - R[T_end-k]) @ B_t_k.T

    # f_t(-k) and q_t(-k)
    fk = F.T @ ak
    # qk = (s[T_end - k] / s[T_end - k - 1]) * F.T @ Rk @ F
    qk = F.T @ Rk @ F

    # Get the smooth parameters of predictive distribution
    r, beta = mod.get_conjugate_params(fk, qk, r, beta)
    beta = np.ravel(beta)[0]
    p = beta / (1 + beta)
    r = np.ravel(r)[0]

    # Saving parameters
    dict_smooth_parms["ak"].append(ak)
    dict_smooth_parms["Rk"].append(Rk)
    dict_smooth_parms["f"].append(fk[0][0])
    dict_smooth_parms["q"].append(qk[0][0])
    dict_smooth_parms["r"].append(r)
    dict_smooth_parms["p"].append(p)
    dict_smooth_parms["beta"].append(beta)
    dict_smooth_parms["t"].append(T_end-k)

# Organize the posterior smooth parameters in pd.DataFrame
df_posterior_smooth = tidy_parameters(
    dict_parameters=dict_smooth_parms,
    entry_m="ak", entry_v="Rk",
    names_parameters=list(mod.get_coef().index),
    index_seas_parameters=mod.iseas,
    F=mod.F)
n_parms = len(df_posterior_smooth["parameter"].unique())
df_posterior_smooth.reset_index(inplace=True)
df_posterior_smooth = df_posterior_smooth.sort_values(
    ['parameter', 'index'])
df_posterior_smooth["t"] = np.tile(dict_smooth_parms["t"], n_parms)
df_posterior_smooth = df_posterior_smooth.sort_values(
    ["parameter", "t"]).reset_index(drop=True).drop('index', axis=1)

# Predictive smooth parameters
dict_filtered = {key: dict_smooth_parms[key] for key in (
    dict_smooth_parms.keys() & {"t", "f", "q", "p", "r", "beta"})}
df_predictive_smooth = pd.DataFrame(dict_filtered).sort_values(
    "t").reset_index(drop=True)
df_predictive_smooth = df_predictive_smooth.join(
    data_turkey[["sale"]].rename(columns={"sale": "y"}))

###############################################################################
# Organize and export results
df_predictive_filter["type"] = "filter"
df_predictive_smooth["type"] = "smooth"
cols = df_predictive_smooth.columns
df_predictive = pd.concat([df_predictive_filter[cols],
                           df_predictive_smooth[cols]])

level = 0.05
df_predictive["mean"] = (
    df_predictive["r"] * (1 - df_predictive["p"]) / df_predictive["p"])
df_predictive["mode"] = np.where(
    df_predictive["r"] > 1,
    np.floor((df_predictive["r"] - 1) *
             (1 - df_predictive["p"]) / df_predictive["p"]), 0)
df_predictive["variance"] = (
    df_predictive["r"] * (1 - df_predictive["p"]) / df_predictive["p"]**2)
df_predictive["ci_lower"] = nbinom.ppf(
    q=level/2, n=df_predictive["r"], p=df_predictive["p"])
df_predictive["ci_upper"] = nbinom.ppf(
    q=1-level/2, n=df_predictive["r"], p=df_predictive["p"])

# Append posterior parameters
df_posterior["type"] = "filter"
df_posterior_smooth["type"] = "smooth"
df_posterior_appended = pd.concat([df_posterior, df_posterior_smooth])

# Merge to get the time
df_predictive = df_predictive.join(data_turkey)
df_predictive = df_predictive[
    ['time', 'sale', 'type', 'f', 'q', 'r', 'p', 'mean', 'mode', 'variance',
     'ci_lower', 'ci_upper']].copy()

df_posterior_appended["t"] = df_posterior_appended["t"] - 1
df_posterior_appended = df_posterior_appended.merge(
    data_turkey.reset_index().rename(columns={"index": "t"}),
    how="left", on="t", validate="many_to_one")
df_posterior_appended = df_posterior_appended[[
    'time', 'type', 'sale', 'parameter', 'mean', 'variance']].copy()

# Exporting
df_predictive.to_csv("./data/turkey_sales__predictive.csv", index=False)
df_posterior_appended.to_csv("./data/turkey_sales__posterior.csv", index=False)
