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
  logit(prob[i]) <- a + b[1]*clim[i] + b[2]*host[i] + 
        inprod(d[1,], origin[i,]) + inprod(d[2,], bridge[i,]) + e[i] ## Process model
  presence_data[i]  ~ dbin(prob[i], 1)		        ## Data model
  }

#### Priors

for(i in 1:n){
  e[i] ~ dgamma(0.1,0.1)
}

for (j in 1:2){
  b[j] ~ dlnorm(0,0.25) # host and climate: expect all coefficients to be positive
}

for(f in 1:2){
for (g in 1:k){
  d[f,g] ~ dlnorm(0,0.25) # commodities: expect all coefficients to be positive
  }
}
  a ~ dnorm(-3, 1.5) # baseline probability is 0, so alpha should be fairly negative
}"

### Binomial_Dist_Pandemic
# Including distance to infected population

Binomial_Dist_Pandemic <- "model{
for(i in 1:n){
  logit(prob[i]) <- a + b[1]*clim[i] + b[2]*host[i] + b[3]*origin[i] + b[4]*bridge[i] + c[1]*dist_bridge[i] + c[2]*dist_origin[i]
  ## Process model
  presence_data[i]  ~ dbin(prob[i], 1)		        
  ## Data model
}

#### Priors
for (j in 1:4){
  b[j] ~ dlnorm(0,0.25) # expect all coefficients to be positive
}

for (k in 1:2){
  c[k] ~ dnorm(0,0.5) # distance coefs should be negative
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
    b[j] ~ dlnorm(0,0.25) # expect all coefficients to be positive
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
      logit(prob_intro[t,i]) <- a + b[3]*infec_trade[t,i] + b[4]*clim[i]*host[i] + e[i]
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

### StateSpace_Dist_Pandemic
# Treats presence as a permanent state, following the first introduction
# Incorporating option for natural neighbor/cross-border spread

StateSpace_Dist_Pandemic = "
model{
  #### Process Model
  for(t in 2:nt){
    for (i in 1:n){
      ## process model
      logit(prob_trade[t,i]) <- a[1] + b[3]*infec_trade[t,i] + b[4]*clim[i]*host[i]
      trade_intro[t,i]  ~ dbin(prob_trade[t,i],1)
      
      logit(prob_border[t,i]) <- a[2] + c*infec_dist[t,i] + b[4]*clim[i]*host[i]
      border_intro[t,i] ~ dbin(prob_border[t,i],1)
    
      ## data model 
      present[t,i] <- presence_data[t-1,i] + max(trade_intro[t,i],border_intro[t,i])*(1 - presence_data[t-1,i])
      presence_data[t,i] ~ dbin(present[t,i], 1)
    }
  }
  
  #### Priors
  # expect all slope coefficients to be positive
  for (j in 3:4){
  b[j] ~ dlnorm(0,0.25) # expect all coefficients to be positive
  }
  c ~ dnorm(0,0.5) # should come out negative
  for (z in 1:2){
  a[z] ~ dnorm(-3, 1.5) # baseline probability is 0, so alpha should be fairly negative
  }
}"



#### Models that don't work just yet (run this code)
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
      present[t,i] <- min(sum(intro[1:t,i]), 1)
      
      # observation model 
      presence_data[t,i] ~ dbin(present[t,i]/lag, max(1, sum(present[2:t,i])))
    }
  }
  
  #### Priors
  # expect all slope coefficients to be positive
  b3 ~ dlnorm(0,0.25)
  b4 ~ dlnorm(0,0.25)
  lag ~ dunif(1,5)

  alpha ~ dnorm(-4, 1.5) # baseline probability is 0, so alpha should be fairly negative
}
"

## Verbatim_Pandemic
# Using the original model equations to try to calibrate alpha and lambda (and beta)

Verbatim_Pandemic = "
model{
  #### Process Model
  for(t in 2:nt){
    for (i in 1:n){
      ## process model
      entry[t,i] <- (1 - pcap_dest[i])*(1 - pcap_orig)*(1 - exp((-1)*lamda*infec_trade[t,i]))
      est[t,i] <- alpha*exp((-1)*beta*(((clim[i]/sig_clim)**2) + (host[i]/sig_host)**2))
      intro[t,i]  ~ dbin(entry[t,i]*est[t,i],1)
      
      ## data model 
      present[t,i] <- presence_data[t-1,i] + intro[t,i]*(1 - presence_data[t-1,i])
      presence_data[t,i] ~ dbin(present[t,i], 1)
    }
  }
  
  #### Priors
  # expect all slope coefficients to be positive
  # beta  ~ dunif(0,1)# expect all coefficients to be positive
  alpha ~ dunif(0,1)
  lamda ~ dlnorm(0,1)
  }"


# More to come!


# Next: runModels.R