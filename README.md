# gbv-multimethod-simulation
R code for simulating and analyzing GBV data using multi-phase ecological framework

# Multi-Method Analysis of Gender-Based Violence: Simulation Study

##  Overview
This repository contains all R code to reproduce the simulation and analyses from the study:

> **"Developing an Integrated Analytical Framework for Gender-Based Violence Research: A Simulation Study Integrating Machine Learning and Causal Inference"**  
> *Grold Otieno Mboya*

The study implements a five-phase analytical framework integrating machine learning, causal inference, and mediation analysis within an ecological model of GBV.

##  Repository Structure
- `01_descriptive_analysis.R` – Phase 1: Prevalence estimation and bivariate analysis
- `02_random_forest.R` – Phase 2: Random Forest classification and variable importance
- `03_logistic_regression.R` – Phase 3: Multivariable logistic regression and ecological model testing
- `04_mediation_analysis.R` – Phase 4: Causal mediation analysis
- `05_synthesis.R` – Phase 5: Integration and visualization of multi-phase results

##  How to Reproduce
1. Clone this repository
2. Install required R packages (see below)
3. Run scripts in numerical order
4. Outputs will be saved in `/outputs/`

##  R Dependencies
```r
install.packages(c("tidyverse", "randomForest", "mediation", "ggplot2", 
                   "pROC", "caret", "lavaan", "simstudy"))
## License
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
