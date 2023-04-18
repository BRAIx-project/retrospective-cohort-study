gg_zoom <- function(g, xrange, yrange) {
  g + 
    scale_x_continuous(limits = xrange) +
    scale_y_continuous(limits = yrange)  
}
