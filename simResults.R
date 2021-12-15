# Script 5: simResults.R
# Models for Pandemic State Space Model

# Predicting pest presence and introductions using the data provided to the 
# [PoPS Global model of pest and pathogen spread](https://github.com/ncsu-landscape-dynamics/PoPS-Global)

# The "regression_data.csv" is generated using the [Statistical Model notebook] 
# (https://github.com/arielsaffer/PoPS-Global-Ag-CaseStudy/blob/master/exploration/StatisticalModel.ipynb)

# You should have already run: workspace.R, data.R, models.R, runModels.R

# If you use a new model or modify the process model, you will need to adapt the simulation accordingly!

# Set variables
nsim = nruns - burnin
nloc = dat$n

dev.off() # Reset plot space

## SIMULATE RESULTS and summary statistics:
# For Static models
if (model_type == "static"){
  
  # Simulate predictions from the data/model
  result <- matrix(NA, nloc, nsim)
  
  for(i in 1:nsim){
    for (n in 1:nloc){
      if (model == Binomial_Pandemic){
        prob <- invlogit(var.mat[i,"a"] + var.mat[i,"b[1]"]*dat$clim[n] + var.mat[n,"b[2]"]*dat$host[n] + 
                           var.mat[i,"b[3]"]*dat$origin[n] + var.mat[i,"b[4]"]*dat$bridge[n])
      } else {prob <- 0  # define your process model here
      } 
      result[n,i] <- rbinom(1,1,prob)		   
    }
  }

  # Extract static summary statistics
  mean_intros <- rowSums(result)/nsim
}

## SIMULATE RESULTS and summary statistics:
# For dynamic models
if (model_type == "dynamic"){
  # Set up additional variables and results array
  ntime = dat$nt
  result <- array(NA, c(ntime, nloc, nsim))
  
  # Simulate from model output parameter distributions
  for(i in 1:nsim){
    for (t in 1:ntime){
      for (n in 1:nloc){
        if (model == Temporal_Pandemic){
          prob <- invlogit(var.mat[i,"a"] + var.mat[i,"b[1]"]*dat$clim[n] + var.mat[n,"b[2]"]*dat$host[n] + 
                           var.mat[i,"b[3]"]*dat$infec_trade[t,n])
        } else if (model == Temporal_Pandemic_IntX | model== StateSpace_Pandemic) {
          prob <- invlogit(var.mat[i,"a"] + var.mat[i,"b[3]"]*dat$infec_trade[t,n] + 
                             var.mat[n,"b[4]"]*dat$host[n]*dat$clim[n])
        } else {
          prob <- 0 # Define your process model here
        }
        result[t, n, i] <- rbinom(1,1,prob)
      }
    }
  }

  # Calculate the cumulative introductions over time by location
  
  cum_intros <- array(NA, c(ntime, nloc, nsim))
  
  for (i in 1:nsim){
    for (n in 1:nloc){
      cum_intros[, n, i] <- cumsum(result[, n, i])
    }
  }
  
  # This could be used to compare the timing of predicted intro
  # to the timing of first report (haven't done that yet here though)
  first_intro <- matrix(NA, nloc, nsim)
  
  for (i in 1:nsim){
    for (n in 1:nloc){
      intros <- which(result[, n, i]==1)
      if (length(intros)==0){
        first_intro[n, i] <- 0
      }
      else {
        first_intro[n, i] <- intros[1]
      }
    }
  }
  
  # What portion of runs predicted introduction?
  
  mean_intros <- rep(NA, nloc)
  
  for (n in 1:nloc){
    mean_intros[n] = mean(first_intro[n,]>0)
  }
}

# Create results dataframe
results <- as.data.frame(cbind(ISO3 = static_data$ISO3,
                                 mean_intros = mean_intros))

### SUMMARY plot of prediction accuracy (right countries)
# Visualize the probabilities, with top 12 countries labeled
top12 = results[order(-mean_intros),][1:12,]

plot(mean_intros,col=c("lightblue","mediumpurple")[static_data$Destination + 1],
     pch=19, cex=1.5, ylim=c(0, max(mean_intros + 0.1)), ylab="% w/Intro", xlab="", xaxt="n", bty='n',
     main=paste0("Percent of Runs with Intros \n(of ", format(nsim, big.mark=',',scientific=FALSE)," runs)"))

abline(h=0.5, lty=2, col="firebrick3")
text(x=as.integer(rownames(top12)),y=as.numeric(top12$mean_intros) + 0.05, 
     labels=top12$ISO3, cex=0.9, col="#36454F")

palette("default")
palette(adjustcolor(palette(),alpha.f=0.3))

### PLOT OVER TIME
# For dynamic models, plot introductions over time for the top 12 locations

if (model_type == "dynamic"){
  
  nlines = 400
  samp_intros <- sample.int(dim(cum_intros)[3],nlines)
  
  counter <- 0 
  
  par(mfrow=c(2,3))
  for (country in as.integer(rownames(top12))){
    counter <- counter + 1
    plot(year_set, cum_intros[,country,1], ylim = c(0, max(cum_intros[nt,country,])), 
         type='l', col=8, main=static_data$ISO3[country], xlab="Year", ylab="Cumulative Intros")
    for (i in samp_intros){
      lines(year_set, (cum_intros[,country,i] + rnorm(1,0,0.2)), type='l', col=8)
    }
    lines(year_set, round(rowMeans(cum_intros[,country,1:nlines])), 
          type='l', lwd=2, col='firebrick3')
    if ((counter+3) %% 6 == 0){
      legend("topleft", c("Single run", "Mean across runs"), 
             col=c('gray', 'firebrick3'), lwd=c(1,2), bty='n')
    }
  }
}

# Table of countries with intros in > 50% of runs
kable(results[results$mean_intros > 0.5,], col.names=c("Country","% Runs with Intro"), 
      caption = "Percent of runs with introduction for \ncountries with introductions in over 50% of runs.")


# That's all for now!

