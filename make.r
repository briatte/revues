#
# scraper packages
#

library(readr)
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
library(ggdendro)  # dendrograms
library(ggfortify) # cluster objects
library(tidyr)     # data reshaping

#
# network packages
#

library(GGally)  # quick network plots
library(tnet)    # weighted degree (load before network)
library(igraph)  # community detection
library(network) # network objects and plots
library(sna)     # unweighted degree

#
# folders
#

dir.create("data"  , showWarnings = FALSE)
dir.create("csv"   , showWarnings = FALSE)
dir.create("html"  , showWarnings = FALSE)
dir.create("plots" , showWarnings = FALSE)

#
# MAKE
#

data = "data/revues-soc.csv"
html = "html/revues-soc-2015.html"

source("01-get-data.r")
source("02-find-clusters.r")
source("03-draw-plots.r")
source("04-draw-networks.r")
