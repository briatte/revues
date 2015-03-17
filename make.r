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
library(ggdendrogram) # dendrograms
library(ggfortify) # cluster objects
library(tidyr)     # data reshaping

#
# network packages
#

library(tnet)    # weighted degree  (load before network)
library(network) # network objects
library(GGally)  # network plots
library(sna)     # unweighted degree

#
# folders
#

dir.create("csv", showWarnings = FALSE)
dir.create("html", showWarnings = FALSE)
dir.create("plots", showWarnings = FALSE)

#
# MAKE
#

data = "revues-soc.csv"
html = "html/revues-soc-2015.html"

source("data.r")
source("clusters.r")
source("plots.r")
source("networks.r")
