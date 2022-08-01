# ===================================================
# CREATE THE DATA LOOKUP TABLES:
# ---------------------------------------------------
# Create the data tables for the new colourmaps:
# ==========
# TWO Pre-reqisites for this:
# 1. A starlink.Rdata dataset for the starlink colourmaps.  These have 256 values.
# 2. A function_colourmaps.R file containing the new additional colourmaps.  These will create 1000 values.
# ==========
# For pre-requisite 1:
load("colourmap_data/starlink.Rdata")
# ==========
# For pre-requisite 2:
source("function_colourmaps.R")
# ==========
colouRmaps$starlink <- starlink
# ==========
# Add a function-like wrapper for the starlink lookup tables:
lookup_colourmaps <- names(colouRmaps$starlink$maps)
colouRmaps$define_map2 <- sapply(lookup_colourmaps, function(i)  function(...) colouRmaps$general$lookup2map(..., cmapname=i) )
colouRmaps$define_map <- c(colouRmaps$define_map, colouRmaps$define_map2)
colouRmaps$define_map2 <- NULL
# ==========
# Create aliases / shortcuts:
for(cm in colouRmaps$mapnames()){
  colouRmaps[[cm]] <- colouRmaps$define_map[[cm]]
}
for(cm in names(colouRmaps$visuals)){
  colouRmaps[[cm]] <- colouRmaps$visuals[[cm]]
}
# ==========
save(colouRmaps, file="colouRmaps.Rdata")
# ==========
# Save it to the package data dir:
use_data(colouRmaps, overwrite = TRUE)

