setwd("/Volumes/Abyss/Dropbox/my_DataScience/R_PackageDev/colouRmaps")

#install.packages("hexSticker")
#remotes::install_github("GuangchuangYu/hexSticker")
#install.packages("showtext")

library(hexSticker)
library(ggplot2)

xvals <- function(nx, ny, adj=0.5){
  matrix(rep(1:nx, ny), nrow = nx, ncol = ny) - 1 + adj
}

yvals <- function(nx,ny, adj=0.5){
  matrix(rep(1:ny, each=nx), nrow = nx, ncol = ny) - 1 + adj
}

pvals_x <- function(nx,ny, adj=0.5){
  matrix(1:(nx*ny), nrow = nx, ncol = ny) - 1 + adj
}

pvals_y <- function(nx,ny, adj=0.5){
  t(matrix(1:(nx*ny), nrow = ny, ncol = nx)  - 1 + adj)
}

zeroes <- function(nx,ny){
  matrix(1:(nx*ny), nrow = nx, ncol = ny)*0
}

rvals <- function(nx, ny, adj=0.5, origin=NULL){
  if(is.null(origin)) origin <- c(0.5*nx, 0.5*ny)
  xx <- xvals(nx,ny,adj) - origin[1]
  yy <- yvals(nx,ny,adj) - origin[2]
  rr <-sqrt(xx^2 + yy^2)
  rr
}

svals <- function(nx, ny, adj=0.5, origin=NULL){
  if(is.null(origin)) origin <- c(0.5*nx, 0.5*ny)
  xx <- xvals(nx,ny,adj) - origin[1]
  yy <- yvals(nx,ny,adj) - origin[2]
  rr <-sqrt(xx^2 + yy^2)
  ang <- atan2(yy,xx)
  ii <- which(ang < 0)
  ang[ii] <- 2*pi + ang[ii]
  ang <- ang/(2*pi)
  ax <- abs(xx)
  ay <- abs(yy)
  ac <- ax
  ii <- which(ay > ax)
  ac[ii] <- ay[ii]
  ss <- ac + ang
  ss
}






aa <-  xvals(10,10)
bb <-  yvals(10,10)

aa*bb

nx <- 11
ny <- 11
image(xvals(nx,ny), col = cmap_create("rainbow5"))
image(yvals(nx,ny), col = cmap_create("rainbow5"))
image(pvals_x(nx,ny), col = cmap_create("rainbow5"))
image(pvals_y(nx,ny), col = cmap_create("rainbow5"))
image(rvals(nx,ny), col = cmap_create("rainbow5"))
image(svals(nx,ny), col = cmap_create("rainbow5"))

n <- 10
xx <- rep(1:n,n)
yy <- rep(1:n,each=n)

d <- ggplot(diamonds, aes(carat, price))
d <- d + geom_hex(binwidth = c(.1, 500))
d <- d + continuous_scale(aesthetics = "fill", scale_name="gradient",
                   palette=cmap_continuous("roygbiv2"),
                   na.value = "grey50", guide = "colourbar", trans="log10")
d <- d + theme_void() + theme_transparent() + theme(legend.position="none")


# Centred Hexagonal Numbers:
hex_centered_total_n <- function(n){
  nn <- 3*n^2 - 3*n + 1
  if(n == 0) nn <- 0
  nn
}
hex_centered_layer_n <- function(n){
  nn <- hex_centered_total_n(n) - hex_centered_total_n(n-1)
  if(n == 0) nn <- 0
  nn
}

hexagon_layer_coords <- function(layer_n=2, hex_dr=1, theta_offset=0){
  if(layer_n == 1){
    xx <- c(0)
    yy <- c(0)
    v_theta <- c(0)
  } else {
    nn <- hex_centered_layer_n(layer_n)
    v_theta <- (0:(nn-1)) * (2*pi/nn) + theta_offset
    layer_radius <- (layer_n-1)*2*hex_dr
    xx <- layer_radius*cos(v_theta)
    yy <- layer_radius*sin(v_theta)
  }
  hex <- data.frame('hex_x'=xx,'hex_y'=yy,'hex_theta'=v_theta,'layer'=layer_n)
  hex
}

centred_hex_array_coords <- function(nlayers=4,hex_dr=1, theta_offset=0){
  for(i in 1:nlayers){
    if(i == 1){
      hex <- hexagon_layer_coords(i,hex_dr=1, theta_offset=0)
    } else {
      hex <- rbind(hex,hexagon_layer_coords(i,hex_dr=1, theta_offset=0))
    }
  }
  hex
}




nl <- 4
dd <- centred_hex_array_coords(nl, theta_offset=pi/6)
ax <- abs(dd$hex_x)
ay <- abs(dd$hex_y)
ac <- ax
ii <- which(ay > ax)
ac[ii] <- ay[ii]
dd$val1 <- dd$layer + dd$hex_theta/(2*pi)
dd$val2 <- sqrt(dd$hex_x^2 + dd$hex_y^2)
dd$val3 <- ac + dd$hex_theta/(2*pi)
dd$rank <- 1:dim(dd)[1]
dd$inverse_rank <- dim(dd)[1]:1

nfac <- 1000
df <- as.data.frame(lapply(dd, rep, nfac*dd$inverse_rank))
nn <- dim(df)[1]
df$x <- df$hex_x + rnorm(nn,0,2)
df$y <- df$hex_y + rnorm(nn,0,2)

d <- ggplot(df, aes(x, y))
d <- d + geom_hex(binwidth=c(2,2))
d <- d + continuous_scale(aesthetics = "fill", scale_name="gradient",
                          palette=cmap_continuous("blue_purple_red3"),
                          na.value = "grey50", guide = "colourbar", trans="identity")
d <- d + theme_void() + theme_transparent() + theme(legend.position="none")
d


sticker(d, package="colouRmaps", p_size=8, s_x=1, s_y=1, s_width=2, s_height=2)

library(showtext)
## Loading Google fonts (http://www.google.com/fonts)

font_add_google("Gochi Hand", "gochi")
## Automatically use showtext to render text for future devices
showtext_auto()


library(colouRmaps)
d <- ggplot(diamonds, aes(carat, price)) + geom_hex(binwidth = c(.1, 500))
d1 <- ggplot_build(d)$data[[1]]

my_cols <- colouRmaps::cmap_create(colourmap_name="rainbow5",
                                   zvals=d1$count,
                                   normalise=T,
                                   output_mode=1)

d + geom_hex(binwidth = c(.1, 500)) +
  scale_fill_manual(values=as.factor(pal))

d + geom_hex(binwidth = c(.1, 500)) + scale_fill_gradient(low="lightblue1",high="darkblue",trans="log10")


testpal <- function(...){
  function(x){
    aa <- scales::gradient_n_pal(...)(x)
    print(x)
    print(aa)
    aa
  }
}

d + geom_hex(binwidth = c(.1, 500)) +
  continuous_scale(aesthetics = "fill", scale_name="gradient",
                   palette=testpal(colours=c("lightblue1","darkblue")),
                   na.value = "grey50", guide = "colourbar", trans="log10")

d + geom_hex(binwidth = c(.1, 500)) +
  continuous_scale(aesthetics = "fill", scale_name="gradient",
                   palette=cmap_continuous("smooth_redblue"),
                   na.value = "grey50", guide = "colourbar", trans="log10")

d + geom_hex(binwidth = c(.1, 500)) +
  continuous_scale(aesthetics = "fill", scale_name="gradient",
                   palette=cmap_continuous("roygbiv2"),
                   na.value = "grey50", guide = "colourbar", trans="log10")



out <- switch(type,
              continuous = (grDevices::colorRampPalette(pal))(n),
              discrete = pal[1:n])
structure(out, class = "palette", name = name)

stats::setNames(numeric_time_of_week, labels_time_of_week)

pal <- wes_palette("Zissou1", 21, type = "continuous")
image(volcano, col = pal)

pal2 <- colouRmaps::cmap_create("rainbow5")

d <- ggplot(diamonds, aes(carat, price)) +
  geom_hex(binwidth = c(.1, 500)) +
  continuous_scale(aesthetics = "fill", scale_name="gradient",
                   palette=cmap_continuous("roygbiv2"),
                   na.value = "grey50", guide = "colourbar", trans="log10")

nn <- 300
df <- data.frame('x'=(0:nn/nn),'y'=1)
ggplot(df, aes(x, y, fill=x)) +
  geom_tile() +
  continuous_scale(aesthetics = "fill", scale_name="gradient",
                   palette=cmap_continuous("roygbiv2"),
                   na.value = "grey50", guide = "colourbar", trans="identity") +
  ggplot2::theme(legend.position = "none",
                 axis.text.x=ggplot2::element_blank(),
                 axis.ticks.x=ggplot2::element_blank(),
                 axis.text.y=ggplot2::element_blank(),
                 axis.ticks.y=ggplot2::element_blank(),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.background = ggplot2::element_rect(fill = 'black'),
                 plot.background = ggplot2::element_rect(fill = "#BFD5E3"),
                 plot.margin = ggplot2::margin(t = 2, r = 2, b = 1, l = 2)
  ) + xlab(NULL) + ylab(NULL) +
  geom_label(
    label=cm,
    x=0.5,
    y=1,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.35,
    size = 10,
    color = "black",
    fill="#ffffff"
  )


pp <- list()
pp[[1]] <- grid.raster(redGradient)
pp[[2]] <- grid.raster(redGradient)

grid.arrange(pp, nrow=2)

redGradient <- matrix(hcl(0, 80, seq(50, 80, 10)),
                      nrow=4, ncol=5)

redGradient[1,4] <- "#ffffff"


gg <- image(matrix(1:100), col = cmap_create("rainbow5"))

## use the ggplot2 example
sticker(p, package="hexSticker", p_size=9, s_x=1, s_y=.75, s_width=1.3, s_height=1,
        p_family = "gochi", filename="figures/ggplot2-google-font.svg")
