# Script 2: data.R
# Data for Pandemic State Space Model

# Predicting pest presence and introductions using the data provided to the 
# [PoPS Global model of pest and pathogen spread](https://github.com/ncsu-landscape-dynamics/PoPS-Global)
# To run the temporal model, you will need to create an "annual_agg" of the traded commodities of interest

# The "regression_data.csv" is generated using the [Statistical Model notebook] 
# (https://github.com/arielsaffer/PoPS-Global-Ag-CaseStudy/blob/master/exploration/StatisticalModel.ipynb)

# You should have already run: workspace.R

#### Data import: static 

static_data = read.csv(paste0(data_path,"regression_data.csv"))

# Set up data list

dat_static <- list(presence_data = static_data$Destination, 
            clim = static_data$Climate_Max, 
            host = static_data$Host_Area, 
            origin = static_data[,paste0("Origin_",substr(commodities,1,4))], 
            bridge = static_data[,paste0("Bridge_",substr(commodities,1,4))], 
            n = nrow(static_data))


#### Data import: temporal

## TRADE DATA
annual_files <- list.files(paste0(input_dir,'comtrade/annual_agg/',
                                  commodities), pattern="*.csv", full.names=TRUE)

# Select relevant years as defined by start_year
years <- unlist(lapply(annual_files, get_year))
year_set <- years[which(years==start_year):length(years)]
annual_files_set <- annual_files[which(years==start_year):length(years)]

# Set up structure for trade data
trade <- read.csv(annual_files_set[1])
trade <- subset(trade,select=-c(index))
trade_dim <- dim(trade)

# Create a matrix of all trade values

trade_data <- array(data=NA, dim=c(length(year_set),trade_dim))

for (i in 1:length(year_set)){
  trade <- read.csv(annual_files_set[i])
  trade <- subset(trade,select=-c(index))
  trade_data[i,,] <- data.matrix(trade)
}

# Min-max scale (min is 0)

trade_scaled <- trade_data/max(trade_data)

## PRESENCE DATA

origin_df <- read.csv(paste0(input_dir, 'origin_locations.csv'))
validation_df <- read.csv(paste0(input_dir, 'first_records_validation.csv'))

# Create structure

nt = length(year_set)
n = nrow(static_data)

presence_data <- matrix(data=NA, nt, n)

# Is it present at that time step?

for (i in 1:length(year_set)){
  year = as.integer(year_set[i])
  origins <- ifelse(static_data$ISO3 %in% origin_df$ISO3,1,0)
  intros <- ifelse(static_data$ISO3 %in% validation_df[validation_df$ObsFirstIntro<=year,]$ISO3,1,0)
  presence <- pmax(origins,intros)
  
  presence_data[i,] <- presence
}

# Create the matrix of infected trade values at each timestep

infec_trade_mat = array(NA, c(nt, n, n))
infec_trade = matrix(NA, nt, n)

infec_trade_mat[1,,] <- t(t(trade_scaled[1,,])*presence_data[1,])
infec_trade[1,] <- rowSums(infec_trade_mat[1,,])

for(t in 2:nt){
  infec_trade_mat[t,,] <- t(t(trade_scaled[t,,])*presence_data[t-1,])
  for (i in 1:n){
    infec_trade[t,i] <- sum(infec_trade_mat[t,i,])
  }
}

# Set up the data list for the temporal model

dat_dynamic <- list(presence_data = presence_data, 
            clim = static_data$Climate_Max, 
            host = static_data$Host_Area, 
            infec_trade = infec_trade, 
            n = n, 
            nt = nt)


# Next: models.R
