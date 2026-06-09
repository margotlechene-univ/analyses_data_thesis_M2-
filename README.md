This repository contains all R scripts used for the statistical analyses presented in the master's internship report entitled "Investigating alternative reproductive tactics in wild Atlantic salmon", conducted on the La Roche stream (Oir catchment, Normandy, France) over the 2015–2025 period, with historical data extending back to 1988.
AnalysesDescriptives.Rmd
Main analysis script. Contains:
- Descriptive statistics of the population (biometrics, maturity proportions, age structure)
- Age imputation by binomial GLM stratified by year
- Length–weight allometric relationship and body condition residuals
- PIT tag selection bias assessment
- L50 estimation and interannual variability
- Effects of seasonal climatic conditions (temperature × precipitation interactions) on body size and maturation probability
- Density-dependent effects on growth and maturation (intraspecific and interspecific competition)
- Historical analyses 1988–2025 (density-growth stability, Bernoulli stochastic simulation)
- Frequency-dependence analyses (temporal autocorrelation, links with adult returns)

Arbre_propre.Rmd
Individual life trajectory reconstruction for PIT-tagged Atlantic salmon parr. Contains:
- Temporal node definition (autumn/spring electrofishing, RFID antenna detections)
- State assignment combining detection location (Roche, Oir, Aval) and maturity status
- Life history tree construction for 0+ individuals (n = 3,074) and 1+ individuals (n = 537)
- GLM analyses of smolting probability, re-encounter probability and maturity persistence between consecutive autumns

estimation_truites_laroche.Rmd
Estimation of local brown trout (Salmo trutta) densities (ind/m²) on the La Roche stream sectors using the Carle & Strub removal method (multi-pass electrofishing) and single-pass correction by mean annual capture probability.

estimations_saumons_laroche.Rmd
Estimation of local Atlantic salmon (Salmo salar) densities (ind/m²) on the La Roche stream sectors using the same methodology as estimation_truites_laroche.Rmd. Includes assessment of spatial and temporal homogeneity of capture efficiency (Kruskal-Wallis tests, coefficient of variation).

glm.R
GLM-based age attribution and stratification optimisation. Contains:
- Comparison of ordinal GLM models for age stratification (AIC-based model selection)
- Selection of the best stratification strategy (size + year vs size + year + zone)
- Binomial GLM for age imputation of individuals without known age

leslie_sceB_only.R
Preliminary Lefkovitch-type matrix model exploring the demographic consequences of different sneaker tactic frequency scenarios in the La Roche Atlantic salmon population. Simulates population dynamics under varying proportions of precocious males (0%, 25%, 100%). This script is presented as a prospect and was not finalised within the scope of this internship.

mixtools.R
Independent validation of age class separation by Gaussian mixture modelling (normalmixEM, k = 2 components, package mixtools). Fitted separately for each year on fork length distributions. Provides accuracy and AUC metrics to assess the robustness of the bimodal age separation.

Data availability
Raw data are the property of INRAE DECOD + U3E and are not publicly available. Analyses are based on:
Autumn electrofishing surveys at La Roche (1988–2025)
PIT tag individual monitoring data (2015–2025)
Environmental data from the La Roche hydrometric station
Adult abundance time series on the Oir catchment (Servanty & Prévost, 2016)
