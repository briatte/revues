#
# scraper packages
#

library(rvest)
library(stringr)

#
# plots packages
#

library(animation)
library(dplyr)
library(ggplot2)
library(scales)

#
# clusters packages
#

library(cluster)   # cluster objects
library(ggfortify) # cluster objects
library(tidyr)     # data reshaping

#
# network packages
#

library(network) # network objects
library(GGally)  # network plots
library(sna)     # unweighted degree
library(tnet)    # weighted degree

#
# folders
#

dir.create("csv", showWarnings = FALSE)
dir.create("html", showWarnings = FALSE)
dir.create("plots", showWarnings = FALSE)

#
# MAKE
#

source("data.r")
source("plots.r")
source("clusters.r")
source("networks.r")
