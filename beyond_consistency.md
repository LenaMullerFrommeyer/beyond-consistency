---
title: "Beyond consistency: Contextual dependency of language style in monologue and
  conversation"
output:
  html_document:
    keep_md: yes
    number_sections: yes

---

This R markdown provides the data preparation for our forthcoming manuscript.

To run this from scratch, you will need the following files:

* [This is where a description of the data setup goes]
* `./scripts/bc-libraries_and_functions.r`: Loads in necessary libraries and
  creates new functions for our analyses.

**Code written by**: L. Mueller-Frommeyer (Technische Universitaet 
Braunschweig) & A. Paxton (University of Connecticut)

**Date last modified**: 10 June 2019



***

## Preliminaries


```r
# clear everything
rm(list=ls())

# load libraries and add new functions
source('./scripts/bc-libraries_and_functions.r')
```

```
## package 'zeallot' successfully unpacked and MD5 sums checked
## package 'utf8' successfully unpacked and MD5 sums checked
## package 'vctrs' successfully unpacked and MD5 sums checked
## package 'cli' successfully unpacked and MD5 sums checked
## package 'crayon' successfully unpacked and MD5 sums checked
## package 'fansi' successfully unpacked and MD5 sums checked
## package 'pillar' successfully unpacked and MD5 sums checked
## package 'purrr' successfully unpacked and MD5 sums checked
## package 'assertthat' successfully unpacked and MD5 sums checked
## package 'pkgconfig' successfully unpacked and MD5 sums checked
## package 'R6' successfully unpacked and MD5 sums checked
## package 'rlang' successfully unpacked and MD5 sums checked
## package 'tibble' successfully unpacked and MD5 sums checked
## package 'tidyselect' successfully unpacked and MD5 sums checked
## package 'BH' successfully unpacked and MD5 sums checked
## package 'plogr' successfully unpacked and MD5 sums checked
## package 'dplyr' successfully unpacked and MD5 sums checked
## 
## The downloaded binary packages are in
## 	C:\Users\Lena\AppData\Local\Temp\Rtmpy0N0gw\downloaded_packages
## package 'minqa' successfully unpacked and MD5 sums checked
## package 'nloptr' successfully unpacked and MD5 sums checked
## package 'RcppEigen' successfully unpacked and MD5 sums checked
## package 'lme4' successfully unpacked and MD5 sums checked
## 
## The downloaded binary packages are in
## 	C:\Users\Lena\AppData\Local\Temp\Rtmpy0N0gw\downloaded_packages
## package 'colorspace' successfully unpacked and MD5 sums checked
## package 'labeling' successfully unpacked and MD5 sums checked
## package 'munsell' successfully unpacked and MD5 sums checked
## package 'RColorBrewer' successfully unpacked and MD5 sums checked
## package 'gtable' successfully unpacked and MD5 sums checked
## package 'lazyeval' successfully unpacked and MD5 sums checked
## package 'plyr' successfully unpacked and MD5 sums checked
## package 'reshape2' successfully unpacked and MD5 sums checked
## package 'scales' successfully unpacked and MD5 sums checked
## package 'viridisLite' successfully unpacked and MD5 sums checked
## package 'withr' successfully unpacked and MD5 sums checked
## package 'ggplot2' successfully unpacked and MD5 sums checked
## 
## The downloaded binary packages are in
## 	C:\Users\Lena\AppData\Local\Temp\Rtmpy0N0gw\downloaded_packages
## package 'pander' successfully unpacked and MD5 sums checked
## 
## The downloaded binary packages are in
## 	C:\Users\Lena\AppData\Local\Temp\Rtmpy0N0gw\downloaded_packages
## package 'gridExtra' successfully unpacked and MD5 sums checked
## package 'viridis' successfully unpacked and MD5 sums checked
## 
## The downloaded binary packages are in
## 	C:\Users\Lena\AppData\Local\Temp\Rtmpy0N0gw\downloaded_packages
## package 'tidyr' successfully unpacked and MD5 sums checked
## 
## The downloaded binary packages are in
## 	C:\Users\Lena\AppData\Local\Temp\Rtmpy0N0gw\downloaded_packages
## package 'RCurl' successfully unpacked and MD5 sums checked
## 
## The downloaded binary packages are in
## 	C:\Users\Lena\AppData\Local\Temp\Rtmpy0N0gw\downloaded_packages
## package 'numDeriv' successfully unpacked and MD5 sums checked
## package 'lmerTest' successfully unpacked and MD5 sums checked
## 
## The downloaded binary packages are in
## 	C:\Users\Lena\AppData\Local\Temp\Rtmpy0N0gw\downloaded_packages
## package 'dotCall64' successfully unpacked and MD5 sums checked
## package 'deSolve' successfully unpacked and MD5 sums checked
## package 'spam' successfully unpacked and MD5 sums checked
## package 'maps' successfully unpacked and MD5 sums checked
## package 'misc3d' successfully unpacked and MD5 sums checked
## package 'tseriesChaos' successfully unpacked and MD5 sums checked
## package 'fields' successfully unpacked and MD5 sums checked
## package 'plot3D' successfully unpacked and MD5 sums checked
## package 'pracma' successfully unpacked and MD5 sums checked
## package 'crqa' successfully unpacked and MD5 sums checked
## 
## The downloaded binary packages are in
## 	C:\Users\Lena\AppData\Local\Temp\Rtmpy0N0gw\downloaded_packages
```

