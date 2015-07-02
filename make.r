# packages

library(dplyr)
library(readr)
library(rvest)
library(stringr)
library(texreg)
library(tidyr)

# plots

library(ggplot2)
library(scales)

# clusters

library(cluster)
library(ggfortify)

# networks

library(tnet)      # weighted network measures (load before network)
library(igraph)    # community detection
library(network)   # network objects and plots
library(sna)       # unweighted degree

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

source("01-data.r")
source("02-clusters.r")
source("03-indices.r")
source("04-networks.r")
source("05-regressions.r")
