# pandemic-statespace
a Bayesian statistical support to the PoPS global pest spread model

currently using jags and the data inputs obtained for a given case study with the [PoPS Global Data Acquisition notebook](https://github.com/ncsu-landscape-dynamics/PoPS-Global/blob/master/notebooks/1_data_acquisition_format.ipynb)

# Objectives
- Expand on [statistical tests of case study viability](https://github.com/arielsaffer/PoPS-Global-Ag-CaseStudy/blob/master/exploration/StatisticalModel.ipynb) with temporal data
- Move towards Bayesian callibration of PoPS Global
- Benchmark model to compare performance
- Flexible option to test new modules, incorporate additional data sources, and more!

# Status
Working from a static binomial regression of model predictors to a dynamic state-space model

## Current (working) models:
1. Static binomial
2. Dynamic (temporal) binomial
3. Dynamic binomial with interaction term
4. Dynamic state-space (basic)  

# Workflow
1. workspace: import packages, define case study-relevant fields
2. data: import and format static and temporal data
3. models: some work, some don't (listed)! 
4. runModel: pick a model, run it, and review intermediate outputs (convergence diagnostics and summary statistics)
5. simResults: simulate and visualize results

