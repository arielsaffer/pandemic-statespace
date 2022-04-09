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
model = Binomial_Pandemic 
# Options defined in models.R, e.g. Binomial_Pandemic, Temporal_Pandemic, 
# Temporal_Pandemic_IntX, StateSpace_Pandemic, your own!

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


if (model == Verbatim_Pandemic) {
  dat$pcap_orig <- mean(static_data[static_data$Origin==1|static_data$Destination==1,"P_Cap"])
  dat$pcap_dest <- static_data$P_Cap

  dat$sig_host <- sd(1-static_data$Host_Area)
  dat$sig_clim <- sd(1-static_data$Climate_Max)
  dat$beta <- 0.5
}

## Run model
j.model   <- jags.model (file = textConnection(model),
                         data = dat,
                         # inits = inits, # Optional
                         n.chains = nchain)

## Sample output

if (model == Verbatim_Pandemic) {
  parameters = c("alpha","lamda")
} else if (model == Binomial_Dist_Pandemic | model == StateSpace_Dist_Pandemic) {
  parameters = c("b","a", "c")
  } else{
  parameters = c("d","b","a")
}

# Note: if you add model parameters, you may need to trace
# different variables

jags.out   <- coda.samples (model = j.model,
                            variable.names = parameters, 
                            n.iter = nruns)

## Plot: Review this plot for even mixing between the chains and
## parameter convergence (e.g. not multi-modal distributions)

# plot(jags.out)

## Plot: Review this plot to determine amount of burnout
## After how many samples is the diagnostic consistently under 1.05?

# BGR <- gelman.plot(jags.out)

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
# pairs(var.mat)	
#You might not want to see this if it's very big

## Sample output
cor(var.mat)  

# Look at the credible intervals of the parameter estimates

# All parameters

MCMCplot(var.mat, 
         params = parameters, 
         ci = c(50, 90))


# In static model (*need to generalize to all models)

# Just trade

MCMCplot(var.mat, 
         params = "d", 
         ci = c(50, 90))

# Climate, host, and trade

MCMCplot(var.mat, 
         params = c("b","d"), 
         ci = c(50, 90))

# If everything worked okay here, you can go to...
# Next: simResults.R

# Additional plotting option: full distributions

data.frame(var.mat[,grepl("d",colnames(var.mat))]) %>% 
  gather(key="param", value="value") %>%
  ggplot(aes(y = param, x = value)) +
  stat_halfeye(.width = c(.90, .5)) +
  theme_bw() + 
  xlim(0,22) +
  ggtitle("Trade parameters") +
  labs(y= "", x = "Regression Parameter Estimates") + 
  scale_y_discrete(labels=c(names(dat$origin),names(dat$bridge)))


