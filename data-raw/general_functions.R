colouRmaps <- list()
# ========================================
colouRmaps$mapnames  <- function(){ return(sort(names(colouRmaps$define_map))) }
colouRmaps$rampnames <- function(){ return(names(colouRmaps$starlink$ramps)) }
# ========================================
colouRmaps$general$normalise_z <- function(z0, normalise){
  z1 <- min(z0)
  z2 <- max(z0)
  if(normalise){
    z <- (z0-z1)/(z2-z1)
  } else {
    z <- z0
    checks <- 0
    if(z1 >= 0) checks <- checks + 1
    if(z1 <= 1) checks <- checks + 1
    if(z2 >= 0) checks <- checks + 1
    if(z2 <= 1) checks <- checks + 1
    print("min_z = ", z1, "\nmax_z = ", z2,"\n")
    if(checks < 4) stop("Your input values z0 must lie in the domain (0,1).
                            If you have chosen 'normalise=F' then please ensure that your own transformation satisfies this criterion.")
  }
  return (z)
}
# ========================================
colouRmaps$general$rgb_cap_n_tail <- function(r){
  ii <- which(r < 0.0)
  r[ii] <- 0.0
  ii <- which(r > 1.0)
  r[ii] <- 1.0

  return (r)
}
# ========================================
colouRmaps$general$get_colourmap_output <- function(r,g,b,z,z0,output_mode=1){

  if(!(output_mode %in% 1:3)) print("output_mode must be 1/2/3 - defaulting to output_mode=1.")
  awesome_colours <- stats::setNames(grDevices::rgb(r,g,b), z)

  if(output_mode == 2){
    awesome_colours <- data.frame(
      'z'=z0,
      'norm_z'=z,
      'red'=r,
      'green'=g,
      'blue'=b,
      'hex'=grDevices::rgb(r,g,b)
    )
  }

  if(output_mode == 3){
    awesome_colours <- data.frame(
      'red'=r,
      'green'=g,
      'blue'=b,
      'zval'=z
    )
  }

  return (awesome_colours)
}
# ========================================
colouRmaps$visuals$show_all <- function(){
  colourmaps  <- colouRmaps$mapnames()
  colourramps <- names(colouRmaps$starlink$ramps)
  cdf <- NULL
  cdd <- NULL
  np <- 1
  plotlist <- list()
  for(cm in colourmaps){
    nn <- 1000
    #cat(cm, "\n")
    cdd <- colouRmaps[[cm]](1:nn, normalise=T, output_mode=2)
    cdd$xmin <- (0:(nn-1))/nn
    cdd$xmax <- (1:nn)/nn
    cdd$id <- paste0(cm,"_",1:nn)
    cdd$colourmap_name <- cm
    cdd$hex <- grDevices::rgb(cdd$r,cdd$g,cdd$b)
    cdd$col <- NA
    fillvals <- stats::setNames(cdd$hex, cdd$id)
    colvals  <- stats::setNames(cdd$col, cdd$id)
    myplot <- c()
    p <- ggplot2::ggplot(cdd) +
      ggplot2::geom_rect(alpha = 1, size=0.01, stat="identity", ggplot2::aes(xmin=xmin,xmax=xmax,ymin=0,ymax=1, fill=id, colour=id)) +
      ggplot2::scale_fill_manual(values = fillvals) +
      ggplot2::scale_colour_manual(values = colvals) +
      ggplot2::scale_x_continuous(expand = c(0.003, 0.003)) +
      ggplot2::theme(legend.position = "none",
            axis.text.x=ggplot2::element_blank(),
            axis.ticks.x=ggplot2::element_blank(),
            axis.text.y=ggplot2::element_blank(),
            axis.ticks.y=ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            panel.background = ggplot2::element_rect(fill = 'black'),
            plot.background = ggplot2::element_rect(fill = "#BFD5E3"),
            plot.margin = ggplot2::margin(t = 4, r = 4, b = 2, l = 4)
      ) +
      ggplot2::labs(subtitle=cm)
    plotlist[[np]] <- p
    np <- np + 1
  }

  mygrid <- get("grid.arrange", asNamespace("gridExtra"))
  do.call(mygrid, c(plotlist, ncol=3))
}
# =====================================
colouRmaps$visuals$show <- function(colourmap_name="none", ncolours=1000, ...){

  if(!(colourmap_name %in% colouRmaps$mapnames())){
    print("Colour maps:")
    print(colouRmaps$mapnames())
    print("Colour ramps:")
    print(colouRmaps$rampnames())
    stop("Above are a list of included colourmaps and ramps.  You can view the set with colouRmaps$show_all()")
  }
  my_args <- c(as.list(environment()), list(...))
  arg_string <- NULL
  for(nm in names(my_args)){
    arg_string <- paste0(arg_string, nm, " = ", my_args[nm], ", ")
  }

  awesome_colours <- colouRmaps$define_map[[colourmap_name]](1:ncolours, output_mode=2, ...)
  dd <- awesome_colours
  ee <- data.frame('hex'=dd$hex)
  dx <- dd$norm_z[2] - dd$norm_z[1]
  ee$xmin <- dd$norm_z - 0.5*dx
  ee$xmax <- dd$norm_z + 0.5*dx
  ee$fill=as.character(dd$norm_z)
  awesome_colours <- colouRmaps$define_map[[colourmap_name]](1:ncolours, output_mode=1, ...)
  dd$z <- dd$norm_z
  dd <- dd[,-which(names(dd) == "hex")]
  dd <- reshape2::melt(dd, id=c("z"))
  y1 <- 1.05
  y2 <- 1.2
  ee$ymin <- y1
  ee$ymax <- y2
  cc <- data.frame('hex'="#000000",'xmin'=min(ee$xmin),'xmax'=max(ee$xmax),'fill'="border",'colour'="border")
  colourbar_border_width <- 0.01
  ee$ymin <- y1
  ee$ymax <- y2
  cc$xmin <- cc$xmin - colourbar_border_width
  cc$xmax <- cc$xmax + colourbar_border_width
  cc$ymin <- y1 - colourbar_border_width
  cc$ymax <- y2 + colourbar_border_width
  ee$colour <- NA
  ee <- rbind(cc,ee)
  awesome_colours <- c('border'="#000000",
                       'norm_z'="#000000",
                       'red'='#FF0000',
                       'green'='#00FF00',
                       'blue'="#0000FF",
                       awesome_colours)

  ggplot2::ggplot(data=dd) +
    ggplot2::geom_line(stat="identity", size=1, ggplot2::aes(x=z, y=value, colour=variable))+
    ggplot2::geom_rect(data=ee, alpha = 1, size=0.01, stat="identity", ggplot2::aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax, fill=fill, colour=colour)) +
    ggplot2::scale_colour_manual(
      values = awesome_colours,
      aesthetics = c("colour", "fill")
    ) +
    ggplot2::scale_y_continuous(limits = c(0, cc$ymax), breaks = seq(0, cc$ymax, by = 0.1)) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(subtitle=paste0("colouRmaps$show(", arg_string,")")) +
    ggplot2::xlab("input value") +
    ggplot2::ylab("output colour r/g/b")
}
# =====================================
colouRmaps$general$lookup2map <- function(z0, output_mode=1, normalise=T, cmapname="rainbow"){

  z <- colouRmaps$general$normalise_z(z0, normalise)
  r=g=b <- z*0

  # ~~~~~~~~~~~~~~~~~~~~
  # Colour mapping here! :)

  cdata <- colouRmaps$starlink$maps[[cmapname]]
  r <- stats::approx(cdata$zval, cdata$r, xout=z)$y
  g <- stats::approx(cdata$zval, cdata$g, xout=z)$y
  b <- stats::approx(cdata$zval, cdata$b, xout=z)$y

  # ~~~~~~~~~~~~~~~~~~~~

  # Cap and Tail:
  r <- colouRmaps$general$rgb_cap_n_tail(r)
  g <- colouRmaps$general$rgb_cap_n_tail(g)
  b <- colouRmaps$general$rgb_cap_n_tail(b)

  awesome_colours <- colouRmaps$general$get_colourmap_output(r,g,b,z,z0,output_mode)

  return (awesome_colours)
}



