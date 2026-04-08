# An Integrated Multi-Method Framework for Gender-Based Violence Research
### A Synthetic Data Demonstration Using Kenya Demographic and Health Survey Parameters

![R](https://img.shields.io/badge/R-%23276DC3.svg?style=for-the-badge&logo=r&logoColor=white)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Status: Under Review](https://img.shields.io/badge/Status-Under%20Review-orange)](https://github.com/Grolds-Code/gbv-multimethod-simulation)

---

## Overview

This repository contains all R code to fully reproduce the simulation study presented in:

> **"An Integrated Multi-Method Framework for Gender-Based Violence Research: A Synthetic Data Demonstration Using Kenya Demographic and Health Survey Parameters"**
> Grold Otieno Mboya, School of Health Sciences, Jaramogi Oginga Odinga University of Science and Technology (JOOUST)
> *Submitted to BMC Medical Research Methodology* (Under Review, 2026)

The study develops and demonstrates a sequential five-phase analytical framework that integrates descriptive epidemiology, machine learning (Random Forest), multivariable logistic regression, causal mediation analysis, and sensitivity analysis within Heise's ecological model of GBV. A single synthetic dataset (N = 3,000) parameterized to reflect the 2022 Kenya Demographic and Health Survey (KDHS) serves as the empirical testbed.

---

## Scientific Context

Gender-based violence (GBV) research faces a persistent **methodological singularity**: studies either predict risk using machine learning but lack causal interpretability, or estimate adjusted associations using regression but miss complex non-linear interactions. This framework bridges both approaches by applying them sequentially to the same dataset, allowing each method to inform the next.

**Key findings from the simulation:**
| Metric | Value |
|--------|-------|
| GBV prevalence (N = 3,000, seed = 42) | 27.0% |
| Random Forest accuracy | 74.75% |
| Null classifier baseline | 73.1% |
| AUC-ROC | 0.711 |
| Sensitivity / Specificity | 33.9% / 89.8% |
| Top predictor: partner alcohol use (aOR) | 6.63 (95% CI: 5.52–7.98) |
| Childhood trauma (aOR) | 1.98 (95% CI: 1.60–2.46) |
| Mediation (all 3 pathways) | Non-significant (ACME p > 0.05) |
| Ecological model fit improvement | ΔAIC = 67.5 vs. single-level model |

---

## Repository Structure
```
gbv-multimethod-simulation/
├── 00_dgp_parameters.R          # Data-generating process: KDHS-parameterized simulation
├── 01_descriptive_epidemiology.R # Phase 1: Prevalence estimation & bivariate analysis
├── 02_predictive_modeling.R      # Phase 2: Random Forest classification & variable importance (MDA)
├── 03_causal_inference_logistic_regression.R  # Phase 3: Nested logistic regression & ecological framework testing
├── 04_mediation_analysis_sem.R   # Phase 4: Causal mediation analysis (potential outcomes framework)
├── 05_final_visualization.R      # Phase 5: Evidence synthesis & integrated visualization
├── 06_sensitivity_analysis.R     # Phase 6: Sample size sensitivity (N = 500, 1000, 3000, 5000)
├── outputs/                      # Generated tables and CSV results
└── figures/                      # Generated publication-quality figures
```

---

## Analytical Framework

The five-phase pipeline is executed sequentially on a single dataset:
```
Phase 1: Descriptive Epidemiology
    ↓  (identifies priority variables)
Phase 2: Random Forest (Machine Learning)
    ↓  (top predictors inform regression specification)
Phase 3: Multivariable Logistic Regression
    ↓  (adjusted associations & ecological model testing)
Phase 4: Causal Mediation Analysis
    ↓  (tests indirect pathways via potential outcomes framework)
Phase 5: Evidence Synthesis & Sensitivity Analysis
```

---

## Reproducing the Analysis

### 1. Clone the repository
```bash
git clone https://github.com/Grolds-Code/gbv-multimethod-simulation.git
cd gbv-multimethod-simulation
```

### 2. Install R dependencies
```r
install.packages(c(
  "tidyverse",    # Data manipulation and visualization
  "simstudy",     # Synthetic data generation
  "randomForest", # Random Forest classification
  "pROC",         # ROC curve and AUC
  "caret",        # Model evaluation metrics
  "mediation",    # Causal mediation analysis
  "ggplot2",      # Publication-quality figures
  "ggforestplot", # Forest plots
  "scales",       # Plot formatting
  "dplyr"         # Explicit namespace calls
))
```

### 3. Run scripts in order
```r
setwd("path/to/gbv-multimethod-simulation")
rm(list = ls())

source("00_dgp_parameters.R")
source("01_descriptive_epidemiology.R")
source("02_predictive_modeling.R")
source("03_causal_inference_logistic_regression.R")
source("04_mediation_analysis_sem.R")
source("05_final_visualization.R")
source("06_sensitivity_analysis.R")
```

> **Important:** Scripts must be run in numerical order. Each script depends on objects created by the preceding script. All scripts use `set.seed(42)` for data generation and `set.seed(123)` for train/test splitting to ensure full reproducibility.

---

## Reproducibility Notes

- All scripts are plain ASCII-encoded for cross-platform compatibility
- R version: 4.5.2
- No `conflicted` package used; all ambiguous functions use explicit `dplyr::` namespace prefixes
- The sensitivity analysis (Script 06) re-runs the full pipeline at N = 500, 1,000, and 5,000 to assess framework stability
- Variable importance is computed using **Mean Decrease in Accuracy (MDA)** via `importance(rf_model, type = 1)`

---

## Outputs

| File | Description |
|------|-------------|
| `figures/phase2_roc_curve.png` | ROC curve for Random Forest model |
| `figures/phase2_variable_importance.png` | MDA variable importance plot |
| `figures/phase3_forest_plot.png` | Forest plot of adjusted odds ratios |
| `figures/phase5_integrated_summary.png` | Multi-phase evidence synthesis |
| `figures/phase5_pathway_network.png` | Causal pathway network diagram |
| `figures/phase6_sensitivity_analysis.png` | Supplementary sensitivity analysis |
| `outputs/phase6_rf_performance_by_n.csv` | RF metrics across sample sizes |
| `outputs/phase6_logistic_or_by_n.csv` | aOR stability across sample sizes |

---

## Ethical Statement

This study uses purely synthetic data generated via computational simulation. No human participants were recruited and no identifiable data were used. Simulation parameters were derived from aggregate, publicly available findings from the 2022 KDHS and published meta-analyses. Institutional review board (IRB) approval was not required.

---

## Citation

If you use this code, please cite:
```
Mboya, G.O. (2026). An Integrated Multi-Method Framework for Gender-Based Violence 
Research: A Synthetic Data Demonstration Using Kenya Demographic and Health Survey 
Parameters. BMC Medical Research Methodology [Under Review].
```

---

## Author

**Grold Otieno Mboya**
School of Health Sciences, Jaramogi Oginga Odinga University of Science and Technology (JOOUST), Bondo, Kenya
✉️ gmotieno@jooust.ac.ke

---

## License

This project is licensed under the MIT License. See `LICENSE` for details.
