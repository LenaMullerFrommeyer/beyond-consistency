#### bc-libraries_and_functions.r: Part of `beyond_consistency.Rmd` ####
#
# This script loads libraries and creates a number of 
# additional functions to facilitate data prep and analysis.
#
# Written by: A. Paxton (University of Connecticut)
# Date last modified: 12 August 2019
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
  'tidyr',
  'simr',
  'EMAtools',
  'lsr',
  'psych',
  'Hmisc'
)

# load required packages using pacman
pacman::p_load(required_packages, character.only=TRUE)

#### Global variables ####

# set the seed
set.seed(9)

#### Prevent scientific notation ####
options(scipen=999)

# read in pander_lme
pander_lme_url = "https://raw.githubusercontent.com/a-paxton/stats-tools/2a1bf715097bbcc966ab612af3a9e0b14408d4ff/pander_lme.R"
pander_lme_file = getURL(pander_lme_url, ssl.verifypeer = FALSE)
eval(parse(text = pander_lme_file))

# read in pander_lm
pander_lm_url = "https://raw.githubusercontent.com/a-paxton/stats-tools/fb4f67ac8f4572b38bad2cd749b4e6de46d5cd23/pander_lm.R"
pander_lm_file = getURL(pander_lm_url, ssl.verifypeer = FALSE)
eval(parse(text = pander_lm_file))