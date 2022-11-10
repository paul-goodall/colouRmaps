globalVariables(c("colouRmaps"))

#' List Colourmaps
#'
#' Lists the colourmaps available via *colouRmap*
#'
#' @return A character vector of extract strings of length nchar=nc
#' @export
cmap_names <- function(){
  colouRmaps$mapnames()
}

#' Show a Colourmap
#'
#' Show a graphical representation of a particular colourmap.
#'
#' @param colourmap_name The name of one of the colourmaps shown via `cmap_names`
#' @param ncolours Number of times to sample the colourmap to create the colourbar
#' @param ... Other options to pass to the `colouRmaps$define_map` function.
#' @return Prints a ggplot object showing the details of the colourmap.
#' @export
cmap_show <- function(colourmap_name="none", ncolours=1000, ...){
  colouRmaps$show(colourmap_name=colourmap_name, ncolours=ncolours, ...)
}

#' Show All Colourmaps
#'
#' Displays all available colourmaps.
#'
#' @return Prints a ggplot object showing all the colourmaps.
#' @export
cmap_show_all <- function(){
  colouRmaps$show_all()
}

#' Create Colourmap Values
#'
#' Creates the colourmap values for your data, from a specified colourmap.
#'
#' @param colourmap_name The name of one of the colourmaps shown via `cmap_names`
#' @param zvals The data values to be colourised
#' @param normalise (logical) Whether or not to normalise the data to (0,1)
#' @param output_mode There are 3 output modes to choose from:
#' 1 ==> returns a named list
#' 2 ==> returns a detailed data frame
#' 3 ==> returns a simpler data frame
#' @param ... ...
#' @return returns an object mapping your data to colours as per `output_mode`
#' @export
cmap_create <- function(colourmap_name="rainbow5", zvals=1:1000, normalise=T, output_mode=1, ...){
  colouRmaps$define_map[[colourmap_name]](z0=zvals, normalise=normalise, output_mode=output_mode, ...)
}

#' Create continuous colour-Palette for use with ggplot
#'
#' Creates the colourmap values for your data, from a specified colourmap.
#'
#' @param colourmap_name The name of one of the colourmaps shown via `cmap_names`
#' @return returns a function to convert value=x to a hex-value colour.
#' @export
cmap_continuous <- function(colourmap_name="rainbow5"){
  z0 <- 0:100/100
  c0 <- colouRmaps$define_map[[colourmap_name]](z0=z0, normalise=T, output_mode=3)

  function(x){
    r  <- stats::approx(z0, c0$red,   xout=x)$y
    g  <- stats::approx(z0, c0$green, xout=x)$y
    b  <- stats::approx(z0, c0$blue,  xout=x)$y
    hh <- as.character(x)
    ii <- which(is.na(x))
    if(length(ii) > 0){
      hh[-ii] <- grDevices::rgb(r[-ii],g[-ii],b[-ii])
      hh[ii] <- NA
    } else {
      hh <- grDevices::rgb(r,g,b)
    }
    return (hh)
  }
}

#' Wrapper for ggplot2::continuous_scale
#'
#' Concise call to colourise a ggplot object with a colouRmap
#'
#' @param colourmap_name The name of one of the colourmaps shown via `cmap_names`
#' @param aesthetics - as per `ggplot2::continuous_scale`
#' @param scale_name - as per `ggplot2::continuous_scale`
#' @param na.value - as per `ggplot2::continuous_scale`
#' @param guide - as per `ggplot2::continuous_scale`
#' @param trans - as per `ggplot2::continuous_scale`
#' @return a `ggplot2::continuous_scale` with a colouRmap palette
#' @export
#'
colouRise <- function(colourmap_name="roygbiv2", aesthetics = "fill", scale_name="gradient", na.value = "grey50", guide = "colourbar", trans="identity"){
  ggplot2::continuous_scale(aesthetics = aesthetics, scale_name=scale_name,
                   palette=colouRmaps::cmap_continuous(colourmap_name),
                   na.value = na.value, guide = guide, trans=trans)
}



