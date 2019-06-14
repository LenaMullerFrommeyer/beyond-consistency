#### bc-libraries_and_functions.r: Part of `beyond_consistency.Rmd` ####
#
# This script loads libraries and creates a number of 
# additional functions to facilitate data prep and analysis.
#
# Written by: A. Paxton (University of Connecticut)
# Date last modified: 10 June 2019
#####################################################################################

#### Load necessary packages ####

# list of required packages
required_packages = c(
  'plyr',
  'knitr',
  'dplyr',
  'lme4',
  'ggplot2',
  'pander',
  'viridis',
  'tibble',
  'RCurl',
  'lmerTest',
  'crqa',
  'gtools',
  'devtools'
)

# load required packages using pacman
pacman::p_load(required_packages, character.only=TRUE)

# intstall the developer's version of tidyr
devtools::install_github("tidyverse/tidyr")
library(tidyr)

#### Global variables ####

# set the seed
set.seed(9)

# specify width of the diagonal recurrence profile
wsz = 5 

# create frame for data
drps = data.frame()
descriptives = data.frame()

#create the values for the quadratic analyses
raw = -wsz:wsz
timeVals = data.frame(raw)
t <- poly(1:(wsz*2+1), 2)
timeVals[,paste("ot", 1:2, sep="")] <- t[timeVals$raw+(wsz+1), 1:2]


#### Prevent scientific notation ####
options(scipen=999)

# read in pander_lme
pander_lme_url = "https://raw.githubusercontent.com/a-paxton/stats-tools/2a1bf715097bbcc966ab612af3a9e0b14408d4ff/pander_lme.R"
pander_lme_file = getURL(pander_lme_url, ssl.verifypeer = FALSE)
eval(parse(text = pander_lme_file))
