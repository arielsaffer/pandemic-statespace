# Script 1: workspace.R
# Workspace for Pandemic State Space Model

# Predicting pest presence and introductions using the data provided to the 
# [PoPS Global model of pest and pathogen spread](https://github.com/ncsu-landscape-dynamics/PoPS-Global)

# The "regression_data.csv" is generated using the [Statistical Model notebook] 
# (https://github.com/arielsaffer/PoPS-Global-Ag-CaseStudy/blob/master/exploration/StatisticalModel.ipynb)

##### Libraries
library(rjags)
library(knitr)
library(fda)
library(MCMCvis)

# To handle the distance matrix (.npy)
library(reticulate)
library(matrixStats)

# To make a map
library(rgdal)
library(sf)
library(sp)

# Optional plotting
library(ggplot2)
library(tidybayes)
library(cowplot)
library(tidyverse)

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

# Define start year, whether you want to use separate ("adjusted") or 
# aggregated ("agg") commodities, and an optional commodity list

# For temporal models, define the timestep as "monthly" or "annual"

if (model_name =="slf_model"){
  start_year = 2006
  commodity_type = "adjusted"
  commodity_list = NULL # c("6802","6803")
  timestep = "monthly"
} else if (model_name == "ToBRFV_model"){
  start_year = 2015
  commodity_type = "adjusted"
  commodity_list = NULL
} else if (model_name =="tuta_absoluta_model"){ 
  start_year = 2004
  commodity_type = "agg"
  } else { 
    start_year = 2010 # You decide!
    commodity_type = "adjusted"
    commodity_list = c("HSCODE1","HSCODE2")
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
get_time <- function(file_path){
  sub_file <- str_split(basename(file_path),"_",simplify=TRUE)[2]
  strsplit(sub_file,'.',fixed=TRUE)[[1]][1]
}


# Next: data.R
