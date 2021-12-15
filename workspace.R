# Script 1: workspace.R
# Workspace for Pandemic State Space Model

# Predicting pest presence and introductions using the data provided to the 
# [PoPS Global model of pest and pathogen spread](https://github.com/ncsu-landscape-dynamics/PoPS-Global)

# The "regression_data.csv" is generated using the [Statistical Model notebook] 
# (https://github.com/arielsaffer/PoPS-Global-Ag-CaseStudy/blob/master/exploration/StatisticalModel.ipynb)

##### Libraries
library(rjags)
library(knitr) 

##### Data location

google_root = "Q:"
model_name = "slf_model" 
# Options: "slf_model", "mln_model", "ToBRFV_model", "tuta_absoluta_model"

data_path = paste0(google_root,"/Shared drives/Pandemic Data/",model_name,"/")

# Path to formatted model inputs
if (model_name == "slf_model") {
  input_dir = paste0(data_path, "inputs/noTWN/")
} else {
  input_dir = paste0(data_path, "inputs/")
}

#### Case study parameters

# Define the commodity aggregate, start year
if (model_name =="slf_model"){
  start_year = 2006
  commodities = "6802"
} else if (model_name == "ToBRFV_model"){
  start_year = 2015
  commodities = "120991"
} else {
  start_year = 2010 # You decide!
  commodities = "HS-CODE"
  }


#### Helper functions

# Define logit function
logit <- function(x){
  log(x/(1 - x))
}

# Define an inverse logit function to predict from binomial regressions
invlogit <- function(x){
  1/(1+exp(-x))
}

# Extracting 4-digit year from trade files
get_year <- function(fil){
  substr(fil, (nchar(fil)-7), (nchar(fil)-4))[1]
}


# Next: data.R
