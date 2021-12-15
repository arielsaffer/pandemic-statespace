# Script 4: runModels.R
# Models for Pandemic State Space Model

# Predicting pest presence and introductions using the data provided to the 
# [PoPS Global model of pest and pathogen spread](https://github.com/ncsu-landscape-dynamics/PoPS-Global)

# The "regression_data.csv" is generated using the [Statistical Model notebook] 
# (https://github.com/arielsaffer/PoPS-Global-Ag-CaseStudy/blob/master/exploration/StatisticalModel.ipynb)

# You should have already run: workspace.R, data.R, models.R
# Code adapted from [Ecological Forecasting](https://github.com/EcoForecast/EF_Activities) 

dev.off() # Reset plot space
palette("default")

# Which model do you want to run?
model = Binomial_Pandemic # StateSpace_Pandemic 
# Options defined in models.R, e.g. Binomial_Pandemic, Temporal_Pandemic, 
# Temporal_Pandemic_Intx, StateSpace_Pandemic, your own!

# Did you pick a static or dynamic model?
model_type = "static" # "static" or "dynamic"

nchain = 3
nruns = 5000 
# 5000 - 15000 depending on complexity/convergence

# Select data
if (model_type =="static"){
  dat = dat_static
} else if (model_type=="dynamic"){
  dat = dat_dynamic
}

## Run model
j.model   <- jags.model (file = textConnection(model),
                         data = dat,
                         # inits = inits, # Optional
                         n.chains = nchain)

## Sample output
jags.out   <- coda.samples (model = j.model,
                            variable.names = c("b","a"), 
                            # Note: if you add model parameters, you may need to trace
                            # different variables
                            n.iter = nruns)

## Plot: Review this plot for even mixing between the chains and
## parameter convergence (e.g. not multi-modal distributions)

plot(jags.out)

## Plot: Review this plot to determine amount of burnout
## After how many samples is the diagnostic consistently under 1.05?

BGR <- gelman.plot(jags.out)

## Set and remove burn-in

burnin = 2000 
# 2000 is usually safe, but eyes are needed
# Could be 1500 - 5000  

jags.burn <- window(jags.out,start=burnin)

## Review the summary of parameter values

summary <- summary(jags.burn)
print(summary)

## Convert to matrix
var.mat <- as.matrix(jags.burn)

## Review pairwise scatter plots & correlation
pairs(var.mat)	
cor(var.mat)  

# If everything worked okay here, you can go to...

# Next: simResults.R
