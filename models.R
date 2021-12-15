# Script 3: models.R
# Models for Pandemic State Space Model

# Predicting pest presence and introductions using the data provided to the 
# [PoPS Global model of pest and pathogen spread](https://github.com/ncsu-landscape-dynamics/PoPS-Global)

# The "regression_data.csv" is generated using the [Statistical Model notebook] 
# (https://github.com/arielsaffer/PoPS-Global-Ag-CaseStudy/blob/master/exploration/StatisticalModel.ipynb)

# You should have already run: workspace.R, data.R

# Here are many models - some work, some don't (that is labeled). You can run all of these
# or modify as needed/create your own, then provide the model name to the run_models.R script.

#### Models that work

### Binomial_Pandemic
# Static model of presence absence

Binomial_Pandemic <- "model{
for(i in 1:n){
  logit(prob[i]) <- a + b[1]*clim[i] + b[2]*host[i] + b[3]*origin[i] + b[4]*bridge[i]  ## Process model
  presence_data[i]  ~ dbin(prob[i], 1)		        ## Data model
}

#### Priors
for (j in 1:4){
  b[j] ~ dlnorm(0,0.25) # expect all coefficients to be positive
  }
  a ~ dnorm(-3, 1.5) # baseline probability is 0, so alpha should be fairly negative
}"


### Temporal_Pandemic
# First transition to temporal - but, conflates *presence* and *introduction* - resolved in model "Pandemic_StateSpace"

Temporal_Pandemic = "
model{
  #### Process Model
  for(t in 2:nt){
    for (i in 1:n){
      ## process model
      logit(prob_intro[t,i]) <- a + b[3]*infec_trade[t,i] + b[1]*clim[i] + b[2]*host[i] 
      ## data model
      presence_data[t,i]  ~ dbin(prob_intro[t,i],1)
    }
  }
  
  #### Priors
  for (j in 1:3){
    b[j] ~ dexp(1) # dlnorm(0,0.25) # expect all coefficients to be positive
  }
  a ~ dnorm(-3, 1.5) # baseline probability is 0, so alpha should be fairly negative
}"


### Temporal_Pandemic_IntX
# Similar to Temporal_Pandemic, but treating host area and climate similarity as an interaction term

Temporal_Pandemic_IntX = "
model{
  #### Process Model
  for(t in 2:nt){
    for (i in 1:n){
      ## linear process model
      logit(prob_intro[t,i]) <- a + b[3]*infec_trade[t,i] + b[4]*host[i]*clim[i]
      presence_data[t,i]  ~ dbin(prob_intro[t,i],1)
    }
  }
  
  #### Priors
  # expect all slope coefficients to be positive
  for (j in 3:4){
    b[j] ~ dlnorm(0,0.25) # expect all coefficients to be positive
  }
  a ~ dnorm(-3, 1.5) # baseline probability is 0, so alpha should be fairly negative
}"


### StateSpace_Pandemic
# Treats presence as a permanent state, following the first introduction.

StateSpace_Pandemic = "
model{
  #### Process Model
  for(t in 2:nt){
    for (i in 1:n){
      ## process model
      logit(prob_intro[t,i]) <- a + b[3]*infec_trade[t,i] + b[4]*clim[i]*host[i]
      intro[t,i]  ~ dbin(prob_intro[t,i],1)
      
      ## data model 
      present[t,i] <- presence_data[t-1,i] + intro[t,i]*(1 - presence_data[t-1,i])
      presence_data[t,i] ~ dbin(present[t,i], 1)
    }
  }
  
  #### Priors
  # expect all slope coefficients to be positive
  for (j in 3:4){
  b[j] ~ dlnorm(0,0.25) # expect all coefficients to be positive
  }
  a ~ dnorm(-3, 1.5) # baseline probability is 0, so alpha should be fairly negative
}"


#### Models that don't work just yet
## Hello, kind visitor! If you know why they don't work, please let me know. :)

## StateSpace_Pandemic_Detect
# Trying to introduce a detection lag as the data observation model
# whereby the probability of detection increases with time since the first introduction

StateSpace_Pandemic_Detect = "
model{
  #### Process Model
  for(t in 2:nt){
    for (i in 1:n){
      ## linear process model
      logit(prob_intro[t,i]) <- alpha + b3*infec_trade[t,i] + b4*clim[i]*host[i]
      intro[t,i]  ~ dbin(prob_intro[t,i],1)
      present[t,i] <- presence_data[t-1,i] + intro[t,i]*(1 - presence_data[t-1,i])
      
      # observation model 
      presence_data[t,i] ~ dbin(present[t,i]/lag, (1 + sum(present[2:t,i])))
      ++++++++
    }
  }
  
  #### Priors
  # expect all slope coefficients to be positive
  b3 ~ dlnorm(0,0.25)
  b4 ~ dlnorm(0,0.25)
  lag ~ dunif(1,5)

  alpha ~ dnorm(-4, 1.5) # baseline probability is 0, so alpha should be fairly negative
}"

# More to come!


# Next: runModels.R