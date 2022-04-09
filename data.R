# Script 2: data.R
# Data for Pandemic State Space Model

# Predicting pest presence and introductions using the data provided to the 
# [PoPS Global model of pest and pathogen spread](https://github.com/ncsu-landscape-dynamics/PoPS-Global)
# To run the temporal model, you will need to create an "annual_agg" of the traded commodities of interest

# The "regression_data.csv" is generated using the [Statistical Model notebook] 
# (https://github.com/arielsaffer/PoPS-Global-Ag-CaseStudy/blob/master/exploration/StatisticalModel.ipynb)

# You should have already run: workspace.R

### Data import: world map (for plots)

world = st_read(list.files(input_dir, pattern="*.gpkg", full.names=TRUE)[1])

#### Data import: static 

static_data = read.csv(paste0(data_path,"regression_data.csv"))

# Set up data list

dat_static <- list(presence_data = static_data$Destination, 
            clim = static_data$Climate_Max, 
            host = static_data$Host_Area,
            dist_bridge = static_data$Dist_Bridge,
            dist_origin = static_data$Dist_Origin,
            n = nrow(static_data)
            )

if (is.null(commodity_list)){
  # Use all available commodities
  dat_static$origin = static_data[,grepl("Origin_", names(static_data))]
  dat_static$bridge = static_data[,grepl("Bridge_", names(static_data))]
  commodities <- str_split(names(dat_static$origin), '_', simplify=TRUE)[,2]
} else {
  # Use only listed commodities
  commodities <- commodity_list
  dat_static$origin = static_data[,paste0("Origin_",commodities)]
  dat_static$origin = static_data[,paste0("Bridge_",commodities)]
}

dat_static$k = dim(dat_static$origin)[2] # How many commodities?


#### Data import: temporal

## TRADE DATA
# NOTE: Temporal trade data currently only handles one commodity/commodity agg.
# Multi-commodity to be inorporated soon.

files_list <- list.files(paste0(input_dir,'comtrade/',timestep,'_',commodity_type,'/',
                                  commodities[1],'/'), pattern="*.csv", full.names=TRUE)

# Select relevant years as defined by start_year
times <- unlist(lapply(files_list, get_time))
times_set <- times[min(which(grepl(start_year,times))):length(times)]
files_set <- files_list[min(which(grepl(start_year,times))):length(times)]

# Set up structure for trade data
trade <- read.csv(files_set[1])
trade <- subset(trade,select=-c(index))
trade_dim <- dim(trade)

# Create a matrix of all trade values

trade_data <- array(data=NA, dim=c(length(times_set),trade_dim))

for (i in 1:length(times_set)){
  trade <- read.csv(files_set[i])
  trade <- subset(trade,select=-c(index))
  trade_data[i,,] <- data.matrix(trade)
}

# Min-max scale (min is 0)

trade_scaled <- trade_data/max(trade_data)

## DISTANCE MATRIX

np <- import("numpy")
distance <- np$load(paste0(input_dir, 'distance_matrix.npy'))
dist_scaled <- distance/max(distance)

## PRESENCE DATA

origin_df <- read.csv(paste0(input_dir, 'origin_locations.csv'))
validation_df <- read.csv(paste0(input_dir, 'first_records_validation.csv'))

# Create structure

nt = length(times_set)
n = nrow(static_data)

presence_data <- matrix(data=NA, nt, n)

# Is it present at that time step?

for (i in 1:length(times_set)){
  year = as.integer(times_set[i])
  origins <- ifelse(static_data$ISO3 %in% origin_df$ISO3,1,0)
  intros <- ifelse(static_data$ISO3 %in% validation_df[validation_df$ObsFirstIntro<=year,]$ISO3,1,0)
  presence <- pmax(origins,intros)
  
  presence_data[i,] <- presence
}

# Create the matrix of shortest infected distance values at each timestep

infec_dist_mat = array(NA, c(nt, n, n))
infec_dist = matrix(NA, nt, n)

infec_dist_mat[1,,] <- t(t(dist_scaled)*presence_data[1,])
infec_dist_mat[1,,][infec_dist_mat[1,,]==0] <- NA
infec_dist[1,] <- rowMins(infec_dist_mat[1,,], na.rm=TRUE)

for(t in 2:nt){
  infec_dist_mat[t,,] <- t(t(dist_scaled)*presence_data[t-1,])
  for (i in 1:n){
    infec_dist[t,i] <- min(infec_dist_mat[t,i,][infec_dist_mat[t,i,]>0])
  }
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
            infec_dist = infec_dist,
            n = n, 
            nt = nt)


# Next: models.R
