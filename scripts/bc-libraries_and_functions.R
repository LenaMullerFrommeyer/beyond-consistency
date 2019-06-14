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

wsz = 5

#### Prevent scientific notation ####
options(scipen=999)

# read in pander_lme
pander_lme_url = "https://raw.githubusercontent.com/a-paxton/stats-tools/2a1bf715097bbcc966ab612af3a9e0b14408d4ff/pander_lme.R"
pander_lme_file = getURL(pander_lme_url, ssl.verifypeer = FALSE)
eval(parse(text = pander_lme_file))
