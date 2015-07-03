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

# list of available disciplines

base = "http://www.cairn.info/"
uids = c(
  "all"      = 0   ,
  "arts"     = 70  ,
  "droit"    = 2   ,
  "eco"      = 1   ,
  "educ"     = 8   ,
  "general"  = 4   ,
  "geo"      = 30  ,
  "histoire" = 3   ,
  "infocom"  = 9   ,
  "lettres"  = 5   ,
  "philo"    = 6   ,
  "psycho"   = 7   ,
  "sanpub"   = 141 ,
  "soc"      = 11  ,
  "scpo"     = 10  ,
  "sport"    = 12
)

# set discipline here, or set to 'all' to download all disciplines

# disc = "geo"
stopifnot(disc %in% names(uids))

data = paste0("data/revues-", disc, ".csv")
html = paste0("html/revues-", disc, ifelse(disc == "all", "", "-"), "2015.html")
arts = paste0("data/cairn-", disc, ".csv")

# URL to sociology journals

if(!file.exists(html))
  download.file(ifelse(disc == "all", "https://www.cairn.info/listerev.php",
                       paste0(base, "discipline.php?POS=",
                              uids[ disc ], "&TITRE=ALL")),
                html, mode = "wb", quiet = TRUE)

source("01-data.r")
source("02-clusters.r")
source("03-indices.r")
source("04-networks.r")
source("05-regressions.r")
