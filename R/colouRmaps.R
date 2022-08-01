
#' List Colourmaps
#'
#' Lists the colourmaps available via *colouRmap*
#'
#' @param my_str A character vector to be cut
#' @param nc Number of characters from the left to be kept
#' @return A character vector of extract strings of length nchar=nc
#' @export
cmap_names <- function(){
  colouRmaps$mapnames()
}

#' Show Colourmap
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
#' @return returns an object mapping your data to colours as per `output_mode`
#' @export
cmap_create <- function(colourmap_name="rainbow5", zvals, normalise=T, output_mode=1, ...){
  colouRmaps$define_map[[colourmap_name]](z0=zvals, normalise=normalise, output_mode=output_mode, ...)
}


