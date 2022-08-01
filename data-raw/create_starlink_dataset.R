# ===================================================
# CREATE THE DATA LOOKUP TABLES:
# ---------------------------------------------------
# Create the STARLINK colourmaps from the FITS files:
# install.packages("plotrix")
# install.packages("https://cran.r-project.org/src/contrib/Archive/astro/astro_1.2.tar.gz", repo=NULL, type="source")
require(astro)

hdr <- "These Colour Tables are taken from the GAIA distribution of STARLINK,
where they are released under the GNU copyleft.
url: http://star-www.rl.ac.uk/
url: http://star-www.dur.ac.uk/~pdraper/
"

starlink <- list()
starlink$hdr <- hdr
ff <- Sys.glob("colourmap_data/starlink/maps/*.fits")
for(f in ff){
  pal <- gsub(".fits","", basename(f))
  aa <- as.data.frame(read.fits(f)$dat[[1]])
  names(aa) <- c('r','g','b')
  nn <- dim(aa)[1]
  aa$zval <- (0:(nn-1))/(nn-1)
  starlink[["maps"]][[pal]] <- aa
}
ff <- Sys.glob("colourmap_data/starlink/ramps/*.fits")
for(f in ff){
  pal <- gsub(".fits","", basename(f))
  aa <- as.numeric(read.fits(f)$dat[[1]])
  starlink[["ramps"]][[pal]] <- aa
}
# ---------------------------------------------------
save(starlink, file="colourmap_data/starlink.Rdata")
# ---------------------------------------------------
