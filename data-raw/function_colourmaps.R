# =====================================
source("general_functions.R")
# =====================================
colouRmaps$define_map$rainbow5 <- function(z0, normalise=T, output_mode=1){

  z <- colouRmaps$general$normalise_z(z0, normalise)
  r=g=b <- z*0

  r1 = 104/255
  r2 = 119/255
  r3 = 134/255
  r4 = 164/255

  g1 = 30/255
  g2 = 80/255
  g3 = 130/255
  g4 = 180/255
  g5 = 220/255
  g6 = 270/255

  b1 = 30/255
  b2 = 75/255
  b3 = 105/255
  b4 = 195/255
  b5 = 220/255

  # RED
  ii <- which(z > r1)
  r[ii] = 0.5 - 0.5*sin(pi*(z[ii]-r1)/(r3-r1)+0.5*pi)
  ii <- which(z > r3)
  r[ii] = 0.99411 + 0.00588*cos(2*pi*((z[ii]-r3)/(r4-r3)))
  ii <- which(z > r4)
  r[ii] = 1.0

  # GREEN
  ii <- which(z > g1)
  g[ii] = sin(2*pi*(z[ii]-g1)/(200/255))
  ii <- which(z > g2)
  g[ii] = 1.0
  ii <- which(z > g3)
  g[ii] = sin(2*pi*((z[ii]-g3)/(200/255))+0.5*pi)
  ii <- which(z > g4)
  g[ii] = 0.0
  ii <- which(z > g5)
  g[ii] = sin(2*pi*(z[ii]-g5)/(200/255))
  ii <- which(z > g6)
  g[ii] = 1.0

  # BLUE
  ii <- which(z <= b1)
  b[ii] = 0.5 - 0.5*sin(pi*(z[ii]-b1)/(b1)-0.5*pi)
  ii <- which(z > b1)
  b[ii] = 1.0
  ii <- which(z > b2)
  b[ii] = sin(2*pi*((z[ii]-b2)/(120/255))+0.5*pi)
  ii <- which(z > b3)
  b[ii] = 0.0
  ii <- which(z > b4)
  b[ii] = sin(2*pi*((z[ii]-b4)/(120/255)))
  ii <- which(z > b5)
  b[ii] <- 1.0

  # Cap and Tail:
  r <- colouRmaps$general$rgb_cap_n_tail(r)
  g <- colouRmaps$general$rgb_cap_n_tail(g)
  b <- colouRmaps$general$rgb_cap_n_tail(b)

  awesome_colours <- colouRmaps$general$get_colourmap_output(r,g,b,z,z0,output_mode)

  return (awesome_colours)
}
# =====================================
colouRmaps$define_map$gle_redblue <- function(z0, normalise=T, output_mode=1){

  z <- colouRmaps$general$normalise_z(z0, normalise)
  r=g=b <- z*0

  z1 = 0.25
  z2 = 0.50
  z3 = 0.75

  b <- 132/255+(123/255)*4*z

  ii <- which(z > z1)
  r[ii] <- (z[ii]-z1)*4
  g[ii] <- r[ii]
  b[ii] <- 1

  ii <- which(z > z2)
  r[ii] <- 1
  g[ii] <- 1-4*(z[ii]-z2)
  b[ii] <- g[ii]

  ii <- which(z > z3)
  r[ii] <- 1-(123/255)*4*(z[ii]-z3)
  g[ii] <- 0
  b[ii] <- 0

  # Cap and Tail:
  r <- colouRmaps$general$rgb_cap_n_tail(r)
  g <- colouRmaps$general$rgb_cap_n_tail(g)
  b <- colouRmaps$general$rgb_cap_n_tail(b)

  awesome_colours <- colouRmaps$general$get_colourmap_output(r,g,b,z,z0,output_mode)

  return (awesome_colours)
}
# =====================================
colouRmaps$define_map$smooth_redblue <- function(z0, normalise=T, output_mode=1){

  z <- colouRmaps$general$normalise_z(z0, normalise)
  r=g=b <- z*0

  b1 <- 0.0
  b2 <- 0.2
  b3 <- 0.5
  b4 <- 0.9

  g1 <- 0.0
  g2 <- 0.1
  g3 <- 0.4

  r1 <- 0.0
  r2 <- 0.15
  r3 <- 0.5
  r4 <- 0.7

  # Blue:
  b <- sin(0.5*z*pi/(b2-b1))
  ii <- which(z >= b2)
  b[ii] <- 1
  ii <- which(z >= b3)
  b[ii] <- cos((z[ii]-b3)*pi)
  b[ii] <- 0.5*(1 + cos(pi*(z[ii]-b3)/(b4-b3)))
  ii <- which(z >= b4)
  b[ii] <- 0

  # Green:
  ii <- which(z > g2)
  g[ii] <- 0.5*(1 - cos(pi*(z[ii]-g2)/(g3-g2)))
  ii <- which(z > g3)
  g[ii] <- b[ii]

  # Red:
  ii <- which(z > r2)
  r[ii] <- 0.5*(1 - cos(pi*(z[ii]-r2)/(r3-r2)))*0.5*(1 - cos(pi*(z[ii]-r2)/(r3-r2)))
  ii <- which(z > r3)
  r[ii] <- 1
  ii <- which(z > r4)
  r[ii] <- cos((z[ii]-r4)*pi*1.25)


  # Cap and Tail:
  r <- colouRmaps$general$rgb_cap_n_tail(r)
  g <- colouRmaps$general$rgb_cap_n_tail(g)
  b <- colouRmaps$general$rgb_cap_n_tail(b)

  awesome_colours <- colouRmaps$general$get_colourmap_output(r,g,b,z,z0,output_mode)

  return (awesome_colours)
}
# =====================================
colouRmaps$define_map$rgb_harmonics <- function(z0, normalise=T, output_mode=1, freq_r=0.5,freq_g=1.5,freq_b=2.5, phase_r=0, phase_g=0, phase_b=0){

  z <- colouRmaps$general$normalise_z(z0, normalise)
  r=g=b <- z*0

  r <- 0.5*(1 - cos(z*freq_r*2*pi + phase_r))
  g <- 0.5*(1 - cos(z*freq_g*2*pi + phase_g))
  b <- 0.5*(1 - cos(z*freq_b*2*pi + phase_b))

  # Cap and Tail:
  r <- colouRmaps$general$rgb_cap_n_tail(r)
  g <- colouRmaps$general$rgb_cap_n_tail(g)
  b <- colouRmaps$general$rgb_cap_n_tail(b)

  awesome_colours <- colouRmaps$general$get_colourmap_output(r,g,b,z,z0,output_mode)

  return (awesome_colours)
}

# =====================================
colouRmaps$define_map$great_barrier_reef <- function(z0, normalise=T, output_mode=1){

  z <- colouRmaps$general$normalise_z(z0, normalise)
  r=g=b <- z*0

  z1 = 0.25
  z2 = 0.50
  z3 = 0.6
  z4 = 0.75
  z5 = 0.85

  b1 <- 0.0
  b2 <- 0.2
  b3 <- 0.5

  g1 <- 0.0
  g2 <- 0.1
  g3 <- 0.4

  # Blue:
  b <- sin(0.5*z*pi/(b2-b1))
  ii <- which(z >= b2)
  b[ii] <- 1
  ii <- which(z >= b3)
  b[ii] <- cos((z[ii]-b3)*pi)

  # Green:
  ii <- which(z > g2)
  g[ii] <- 0.5*(1 - cos(pi*(z[ii]-g2)/(g3-g2)))

  ii <- which(z > z1)
  r[ii] <- (z[ii]-z1)*4

  ii <- which(z > z2)
  r[ii] <- 1

  ii <- which(z > z4)
  r[ii] <- 1-(123/255)*4*(z[ii]-z4)

  ii <- which(z > z1)
  r[ii] <- 0.5*(1 - cos((z[ii]-z1)*pi/(1-z1)))

  # Cap and Tail:
  r <- colouRmaps$general$rgb_cap_n_tail(r)
  g <- colouRmaps$general$rgb_cap_n_tail(g)
  b <- colouRmaps$general$rgb_cap_n_tail(b)

  awesome_colours <- colouRmaps$general$get_colourmap_output(r,g,b,z,z0,output_mode)

  return (awesome_colours)
}

# =====================================
colouRmaps$define_map$bruise_colours <- function(z0, normalise=T, output_mode=1){

  z <- colouRmaps$general$normalise_z(z0, normalise)
  r=g=b <- z*0

  z1 = 0.25
  z2 = 0.50
  z3 = 0.75


  # Blue:
  b <- 0.25 + 0.75*sin((z/z2)*pi)
  ii <- which(z > z1)
  b[ii] <- 1
  ii <- which(z > z2)
  b[ii] <- cos((z[ii]-0.5)*2*pi)
  ii <- which(z > z3)
  b[ii] <- 0

  # Green:
  ii <- which(z > z1)
  g[ii] <- 0.35*(1 - cos((z[ii]-z1)*2*pi/(1-z1)))


  ii <- which(z > z1)
  r[ii] <- (z[ii]-z1)*4

  ii <- which(z > z2)
  r[ii] <- 1

  ii <- which(z > z3)
  r[ii] <- 0.9 + 0.1*cos(z[ii]*8*pi)



  # Cap and Tail:
  r <- colouRmaps$general$rgb_cap_n_tail(r)
  g <- colouRmaps$general$rgb_cap_n_tail(g)
  b <- colouRmaps$general$rgb_cap_n_tail(b)

  awesome_colours <- colouRmaps$general$get_colourmap_output(r,g,b,z,z0,output_mode)

  return (awesome_colours)
}
# =====================================
colouRmaps$define_map$blue_purple_red1 <- function(z0, normalise=T, output_mode=1){

  z <- colouRmaps$general$normalise_z(z0, normalise)
  r=g=b <- z*0

  # ~~~~~~~~~~~~~~~~~~~~
  # Colour mapping here! :)
  r <- z^0.5
  g <- 0
  b <- (1-z^2)^0.5

  # ~~~~~~~~~~~~~~~~~~~~

  # Cap and Tail:
  r <- colouRmaps$general$rgb_cap_n_tail(r)
  g <- colouRmaps$general$rgb_cap_n_tail(g)
  b <- colouRmaps$general$rgb_cap_n_tail(b)

  awesome_colours <- colouRmaps$general$get_colourmap_output(r,g,b,z,z0,output_mode)

  return (awesome_colours)
}
# =====================================
colouRmaps$define_map$blue_purple_red2 <- function(z0, normalise=T, output_mode=1){

  z <- colouRmaps$general$normalise_z(z0, normalise)
  r=g=b <- z*0

  # ~~~~~~~~~~~~~~~~~~~~
  # Colour mapping here! :)

  r <- z^0.5
  g <- 0
  b <- (1-z)^0.5

  # ~~~~~~~~~~~~~~~~~~~~

  # Cap and Tail:
  r <- colouRmaps$general$rgb_cap_n_tail(r)
  g <- colouRmaps$general$rgb_cap_n_tail(g)
  b <- colouRmaps$general$rgb_cap_n_tail(b)

  awesome_colours <- colouRmaps$general$get_colourmap_output(r,g,b,z,z0,output_mode)

  return (awesome_colours)
}
# =====================================
colouRmaps$define_map$blue_purple_red3 <- function(z0, normalise=T, output_mode=1){

  z <- colouRmaps$general$normalise_z(z0, normalise)
  r=g=b <- z*0

  # ~~~~~~~~~~~~~~~~~~~~
  # Colour mapping here! :)

  r <- (1-(z-1)^2)^0.5
  g <- 0
  b <- (1-z^2)^0.5

  # ~~~~~~~~~~~~~~~~~~~~

  # Cap and Tail:
  r <- colouRmaps$general$rgb_cap_n_tail(r)
  g <- colouRmaps$general$rgb_cap_n_tail(g)
  b <- colouRmaps$general$rgb_cap_n_tail(b)

  awesome_colours <- colouRmaps$general$get_colourmap_output(r,g,b,z,z0,output_mode)

  return (awesome_colours)
}
# =====================================
colouRmaps$define_map$blue_purple_red4 <- function(z0, normalise=T, output_mode=1){

  z <- colouRmaps$general$normalise_z(z0, normalise)
  r=g=b <- z*0

  # ~~~~~~~~~~~~~~~~~~~~
  # Colour mapping here! :)

  r <- (0.5*(1-cos(z*pi)))^0.5
  g <- 0
  b <- (0.5*(1+cos(z*pi)))^0.5

  # ~~~~~~~~~~~~~~~~~~~~

  # Cap and Tail:
  r <- colouRmaps$general$rgb_cap_n_tail(r)
  g <- colouRmaps$general$rgb_cap_n_tail(g)
  b <- colouRmaps$general$rgb_cap_n_tail(b)

  awesome_colours <- colouRmaps$general$get_colourmap_output(r,g,b,z,z0,output_mode)

  return (awesome_colours)
}
# =====================================
colouRmaps$define_map$roygbiv1 <- function(z0, normalise=T, output_mode=1){

  z <- colouRmaps$general$normalise_z(z0, normalise)
  r=g=b <- z*0

  # ~~~~~~~~~~~~~~~~~~~~
  # Colour mapping here! :)

  #colour-states:
  rr <- c(0, 125/255, 0, 0, 0, 1, 1, 1, 1, 1)
  gg <- c(0, 38/255, 0, 1, 1, 1, 165/255, 0, 0, 1)
  bb <- c(0, 205/255, 1, 1, 0, 0, 0, 0, 1, 1)
  nn <- length(rr)
  for(i in 1:(nn-1)){
    z1 <- (i-1)/(nn-1)
    z2 <- i/(nn-1)
    ii <- which((z > z1)*(z <= z2) == 1)
    zz <- (z[ii] - z1)*(nn-1)
    transition_in  <- zz
    transition_out <- 1-transition_in
    r[ii] <- rr[i]*transition_out + rr[i+1]*transition_in
    g[ii] <- gg[i]*transition_out + gg[i+1]*transition_in
    b[ii] <- bb[i]*transition_out + bb[i+1]*transition_in
  }

  #print(data.frame('r'=r,'g'=g,'b'=b))

  # ~~~~~~~~~~~~~~~~~~~~

  # Cap and Tail:
  r <- colouRmaps$general$rgb_cap_n_tail(r)
  g <- colouRmaps$general$rgb_cap_n_tail(g)
  b <- colouRmaps$general$rgb_cap_n_tail(b)

  awesome_colours <- colouRmaps$general$get_colourmap_output(r,g,b,z,z0,output_mode)

  return (awesome_colours)
}
# =====================================
colouRmaps$define_map$roygbiv2 <- function(z0, normalise=T, output_mode=1){

  z <- colouRmaps$general$normalise_z(z0, normalise)
  r=g=b <- z*0

  # ~~~~~~~~~~~~~~~~~~~~
  # Colour mapping here! :)

  #colour-states:
  rr <- c(0, 125/255, 0, 0, 0, 1, 1, 1, 1, 1)
  gg <- c(0, 38/255, 0, 1, 1, 1, 165/255, 0, 0, 1)
  bb <- c(0, 205/255, 1, 1, 0, 0, 0, 0, 1, 1)
  nn <- length(rr)
  for(i in 1:(nn-1)){
    z1 <- (i-1)/(nn-1)
    z2 <- i/(nn-1)
    ii <- which((z > z1)*(z <= z2) == 1)
    zz <- (z[ii] - z1)*(nn-1)
    transition_in  <- (1-(zz-1)^2)^0.5
    transition_out <- 1-transition_in
    r[ii] <- rr[i]*transition_out + rr[i+1]*transition_in
    g[ii] <- gg[i]*transition_out + gg[i+1]*transition_in
    b[ii] <- bb[i]*transition_out + bb[i+1]*transition_in
  }

  # ~~~~~~~~~~~~~~~~~~~~

  # Cap and Tail:
  r <- colouRmaps$general$rgb_cap_n_tail(r)
  g <- colouRmaps$general$rgb_cap_n_tail(g)
  b <- colouRmaps$general$rgb_cap_n_tail(b)

  awesome_colours <- colouRmaps$general$get_colourmap_output(r,g,b,z,z0,output_mode)

  return (awesome_colours)
}
# =====================================
colouRmaps$define_map$roygbiv3 <- function(z0, normalise=T, output_mode=1){

  z <- colouRmaps$general$normalise_z(z0, normalise)
  r=g=b <- z*0

  # ~~~~~~~~~~~~~~~~~~~~
  # Colour mapping here! :)

  #colour-states:
  rr <- c(0, 125/255, 0, 0, 0, 1, 1, 1, 1, 1)
  gg <- c(0, 38/255, 0, 1, 1, 1, 165/255, 0, 0, 1)
  bb <- c(0, 205/255, 1, 1, 0, 0, 0, 0, 1, 1)
  nn <- length(rr)
  for(i in 1:(nn-1)){
    z1 <- (i-1)/(nn-1)
    z2 <- i/(nn-1)
    ii <- which((z > z1)*(z <= z2) == 1)
    zz <- (z[ii] - z1)*(nn-1)
    transition_in  <- (0.5*(1-cos(zz*pi)))
    transition_out <- 1-transition_in
    r[ii] <- rr[i]*transition_out + rr[i+1]*transition_in
    g[ii] <- gg[i]*transition_out + gg[i+1]*transition_in
    b[ii] <- bb[i]*transition_out + bb[i+1]*transition_in
  }

  # ~~~~~~~~~~~~~~~~~~~~

  # Cap and Tail:
  r <- colouRmaps$general$rgb_cap_n_tail(r)
  g <- colouRmaps$general$rgb_cap_n_tail(g)
  b <- colouRmaps$general$rgb_cap_n_tail(b)

  awesome_colours <- colouRmaps$general$get_colourmap_output(r,g,b,z,z0,output_mode)

  return (awesome_colours)
}
# =====================================
colouRmaps$define_map$paints1 <- function(z0, normalise=T, output_mode=1, nbreaks=10){

  z <- colouRmaps$general$normalise_z(z0, normalise)
  r=g=b <- z*0

  if(nbreaks < 10){
    nbreaks <- 10
    print("nbreaks must be >= 10. Setting nbreaks=10.")
  }

  # ~~~~~~~~~~~~~~~~~~~~
  # Colour mapping here! :)

  #colour-states:
  black   <- c(0,0,0)
  purple  <- c(125/255, 38/255, 205/255)
  blue    <- c(0, 0, 1)
  cyan    <- c(0, 1, 1)
  green   <- c(0, 1, 0)
  yellow  <- c(1, 1, 0)
  orange  <- c(1, 165/255, 0)
  red     <- c(1, 0, 0)
  magenta <- c(1, 0, 1)
  white   <- c(1, 1, 1)
  my_colours <- data.frame('r'=NA,'g'=NA,'b'=NA)[-1,]
  my_colours <- rbind(my_colours, black, purple, blue, cyan, green, yellow, orange, red, magenta, white)
  names(my_colours) <- c('r','g','b')

  rr0 <- as.character(my_colours$r)
  gg0 <- as.character(my_colours$g)
  bb0 <- as.character(my_colours$b)
  nn0 <- length(rr0)
  zz0 <- (0:(nn0-1))/(nn0-1)
  rr <- stats::approx(zz0, rr0, n=nbreaks)$y
  gg <- stats::approx(zz0, gg0, n=nbreaks)$y
  bb <- stats::approx(zz0, bb0, n=nbreaks)$y
  nn <- length(rr)

  for(i in 1:(nn-1)){
    z1 <- (i-1)/(nn-1)
    z2 <- i/(nn-1)
    ii <- which((z > z1)*(z <= z2) == 1)
    zz <- (z[ii] - z1)*(nn-1)
    transition_in  <- abs(1-(1-zz)^2)^0.5
    transition_out <- abs(1-zz^2)^0.5
    r[ii] <- rr[i]*transition_out + rr[i+1]*transition_in
    g[ii] <- gg[i]*transition_out + gg[i+1]*transition_in
    b[ii] <- bb[i]*transition_out + bb[i+1]*transition_in
  }

  x <- r
  x <- (x - min(x))/(max(x) - min(x))
  r <- x
  x <- g
  x <- (x - min(x))/(max(x) - min(x))
  g <- x
  x <- b
  x <- (x - min(x))/(max(x) - min(x))
  b <- x


  #print(summary(data.frame('r'=r,'g'=g,'b'=b)))

  # ~~~~~~~~~~~~~~~~~~~~

  # Cap and Tail:
  r <- colouRmaps$general$rgb_cap_n_tail(r)
  g <- colouRmaps$general$rgb_cap_n_tail(g)
  b <- colouRmaps$general$rgb_cap_n_tail(b)

  awesome_colours <- colouRmaps$general$get_colourmap_output(r,g,b,z,z0,output_mode)

  return (awesome_colours)
}
# =====================================
colouRmaps$define_map$paints2 <- function(z0, normalise=T, output_mode=1, nbreaks=10){

  z <- colouRmaps$general$normalise_z(z0, normalise)
  r=g=b <- z*0

  if(nbreaks < 10){
    nbreaks <- 10
    print("nbreaks must be >= 10. Setting nbreaks=10.")
  }

  # ~~~~~~~~~~~~~~~~~~~~
  # Colour mapping here! :)

  #colour-states:
  black   <- c(0,0,0)
  purple  <- c(125/255, 38/255, 205/255)
  blue    <- c(0, 0, 1)
  cyan    <- c(0, 1, 1)
  green   <- c(0, 1, 0)
  yellow  <- c(1, 1, 0)
  orange  <- c(1, 165/255, 0)
  red     <- c(1, 0, 0)
  magenta <- c(1, 0, 1)
  white   <- c(1, 1, 1)
  my_colours <- data.frame('r'=NA,'g'=NA,'b'=NA)[-1,]
  my_colours <- rbind(my_colours, black, purple, blue, cyan, green, yellow, orange, red, magenta, white)
  names(my_colours) <- c('r','g','b')

  rr0 <- as.character(my_colours$r)
  gg0 <- as.character(my_colours$g)
  bb0 <- as.character(my_colours$b)
  nn0 <- length(rr0)
  zz0 <- (0:(nn0-1))/(nn0-1)
  rr <- approx(zz0, rr0, n=nbreaks)$y
  gg <- approx(zz0, gg0, n=nbreaks)$y
  bb <- approx(zz0, bb0, n=nbreaks)$y
  nn <- length(rr)

  for(i in 1:(nn-1)){
    z1 <- (i-1)/(nn-1)
    z2 <- i/(nn-1)
    ii <- which((z > z1)*(z <= z2) == 1)
    zz <- (z[ii] - z1)*(nn-1)
    transition_in  <- abs(1-(1-zz)^2)^0.5
    transition_out <- abs(1-zz^2)^0.5
    rbit <- rr[i]*transition_out + rr[i+1]*transition_in
    gbit <- gg[i]*transition_out + gg[i+1]*transition_in
    bbit <- bb[i]*transition_out + bb[i+1]*transition_in

    maxval <- max(max(rbit),max(gbit),max(bbit))

    r[ii] <- rbit/maxval
    g[ii] <- gbit/maxval
    b[ii] <- bbit/maxval

  }


  #print(summary(data.frame('r'=r,'g'=g,'b'=b)))

  # ~~~~~~~~~~~~~~~~~~~~

  # Cap and Tail:
  r <- colouRmaps$general$rgb_cap_n_tail(r)
  g <- colouRmaps$general$rgb_cap_n_tail(g)
  b <- colouRmaps$general$rgb_cap_n_tail(b)

  awesome_colours <- colouRmaps$general$get_colourmap_output(r,g,b,z,z0,output_mode)

  return (awesome_colours)
}
# =====================================
colouRmaps$define_map$roygbiv4 <- function(z0, normalise=T, output_mode=1, nbreaks=30){

  z <- colouRmaps$general$normalise_z(z0, normalise)
  r=g=b <- z*0

  if(nbreaks < 30){
    nbreaks <- 30
    print("nbreaks must be >= 30. Setting nbreaks=30.")
  }

  # ~~~~~~~~~~~~~~~~~~~~
  # Colour mapping here! :)

  #colour-states:
  black   <- c(0,0,0)
  purple  <- c(125/255, 38/255, 205/255)
  blue    <- c(0, 0, 1)
  cyan    <- c(0, 1, 1)
  green   <- c(0, 1, 0)
  yellow  <- c(1, 1, 0)
  orange  <- c(1, 165/255, 0)
  red     <- c(1, 0, 0)
  magenta <- c(1, 0, 1)
  white   <- c(1, 1, 1)
  my_colours <- data.frame('r'=NA,'g'=NA,'b'=NA)[-1,]
  my_colours <- rbind(my_colours, black, purple, blue, cyan, green, yellow, orange, red, magenta, white)
  names(my_colours) <- c('r','g','b')

  rr0 <- as.character(my_colours$r)
  gg0 <- as.character(my_colours$g)
  bb0 <- as.character(my_colours$b)
  nn0 <- length(rr0)
  zz0 <- (0:(nn0-1))/(nn0-1)
  zz <- stats::approx(zz0, rr0, n=nbreaks)$x
  rr <- stats::approx(zz0, rr0, n=nbreaks)$y
  gg <- stats::approx(zz0, gg0, n=nbreaks)$y
  bb <- stats::approx(zz0, bb0, n=nbreaks)$y
  nn <- length(rr)

  # now evaluate these at the actual z-values:
  r <- stats::approx(zz, rr, xout=z)$y
  g <- stats::approx(zz, gg, xout=z)$y
  b <- stats::approx(zz, bb, xout=z)$y

  #print(summary(data.frame('r'=r,'g'=g,'b'=b)))

  # ~~~~~~~~~~~~~~~~~~~~

  # Cap and Tail:
  r <- colouRmaps$general$rgb_cap_n_tail(r)
  g <- colouRmaps$general$rgb_cap_n_tail(g)
  b <- colouRmaps$general$rgb_cap_n_tail(b)

  awesome_colours <- colouRmaps$general$get_colourmap_output(r,g,b,z,z0,output_mode)

  return (awesome_colours)
}
# =====================================
colouRmaps$define_map$roygbiv5 <- function(z0, normalise=T, output_mode=1, nbreaks=30, dn=8){

  z <- colouRmaps$general$normalise_z(z0, normalise)
  r=g=b <- z*0

  if(nbreaks < 30){
    nbreaks <- 30
    print("nbreaks must be >= 30. Setting nbreaks=30.")
  }

  # ~~~~~~~~~~~~~~~~~~~~
  # Colour mapping here! :)

  #colour-states:
  black   <- c(0,0,0)
  purple  <- c(125/255, 38/255, 205/255)
  blue    <- c(0, 0, 1)
  cyan    <- c(0, 1, 1)
  green   <- c(0, 1, 0)
  yellow  <- c(1, 1, 0)
  orange  <- c(1, 165/255, 0)
  red     <- c(1, 0, 0)
  magenta <- c(1, 0, 1)
  white   <- c(1, 1, 1)
  my_colours <- data.frame('r'=NA,'g'=NA,'b'=NA)[-1,]
  my_colours <- rbind(my_colours, black, purple, blue, cyan, green, yellow, orange, red, magenta, white)
  names(my_colours) <- c('r','g','b')

  rr0 <- as.character(my_colours$r)
  gg0 <- as.character(my_colours$g)
  bb0 <- as.character(my_colours$b)
  nn0 <- length(rr0)
  zz0 <- (0:(nn0-1))/(nn0-1)
  zz <- stats::approx(zz0, rr0, n=nbreaks)$x
  rr <- stats::approx(zz0, rr0, n=nbreaks)$y
  gg <- stats::approx(zz0, gg0, n=nbreaks)$y
  bb <- stats::approx(zz0, bb0, n=nbreaks)$y
  nn <- length(rr)

  # Create the discrete map values:
  dw <- 5
  sigma <- dn/nn
  r1=g1=b1 <- zz*0
  for(i in zz){
    ww <- exp(-(zz-i)^2/sigma)^2
    ww <- ww/sum(ww)
    r1 <- r1 + rr*ww
    g1 <- g1 + gg*ww
    b1 <- b1 + bb*ww
  }
  # now evaluate these at the actual z-values:
  r <- stats::approx(zz, r1, xout=z)$y
  g <- stats::approx(zz, g1, xout=z)$y
  b <- stats::approx(zz, b1, xout=z)$y

  #print(summary(data.frame('r'=r,'g'=g,'b'=b)))

  # ~~~~~~~~~~~~~~~~~~~~

  # Cap and Tail:
  r <- colouRmaps$general$rgb_cap_n_tail(r)
  g <- colouRmaps$general$rgb_cap_n_tail(g)
  b <- colouRmaps$general$rgb_cap_n_tail(b)

  awesome_colours <- colouRmaps$general$get_colourmap_output(r,g,b,z,z0,output_mode)

  return (awesome_colours)
}
# =====================================
