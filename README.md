# Replication files for "Non-Linear Dimensionality Reduction for Macroeconomic Forecasting"
## Alessandro Ciancetta, Nicole Poynarova, Alessandro Tenderini

Replication files for the master's project "Non-Linear Dimensionality Reduction for Macroeconomic Forecasting" at the Barcelona School of Economics, Data Science Methodology program 2023.


## Contents

The repository contains three main notebooks:

 - `data_preparation.Rmd` explains the procedure for data preprocessing
 - `forecast_evaluation.Rmd` contains the code for running the forecast evaluation, saving the results into the `results` folder and automatically generate Tables 2 and 3.
 - `results_analysis.Rmd` contains the code for replicating Figures 1,2 and 3
 
 
 The folder `sourcecode/forecast_evaluation` contains:
 - the definition of the function `forecast_evaluation_fredmd.R`, the main engine for parallel execution of the forecast evaluation
 - the specification of all the forecasting algorithms and tuning procedures, in the subfolder `sourcecode/forecast_evaluation/forecast_algorithms`
