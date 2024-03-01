# transet create points perpendicular to a line with set distance
# npts is # of points on one side in addition to the center point
transect <- function(tpts, tlen, npts = 1){
  
  tpts$thetaT = tpts$theta+pi/2
  
  dx = tlen*cos(tpts$thetaT)
  dy = tlen*sin(tpts$thetaT)
  
  x = tpts$x
  y = tpts$y
  
  x.inner <-  do.call(cbind, lapply(npts:1, function(i) {return(x + i*dx)}))
  y.inner <-  do.call(cbind, lapply(npts:1, function(i) {return(y + i*dy)}))
  x.inner.names <- do.call(c, lapply(npts:1, function(i) { return(paste0("-", (i)))}))
  y.inner.names <- do.call(c, lapply(npts:1, function(i) { return(paste0("-", (i)))}))

  x.outer <-  do.call(cbind, lapply(0:npts, function(i) {return(x - i*dx)}))
  y.outer <-  do.call(cbind, lapply(0:npts, function(i) {return(y - i*dy)}))
  x.outer.names <- do.call(c, lapply(0:npts, function(i) { return(paste0("+", i))}))
  y.outer.names <- do.call(c, lapply(0:npts, function(i) { return(paste0("+", i))}))
  
  xx  <- as.data.frame(cbind(x.inner,x.outer))
  yy <- as.data.frame(cbind(y.inner, y.outer))
  names(xx) <- c(x.inner.names, x.outer.names)
  names(yy) <- c(y.inner.names, y.outer.names)
  xx$transectID <- seq(1:nrow(xx))
  yy$transectID <- seq(1:nrow(yy))
  
  
  xy <- cbind(pivot_longer(xx, cols = 1:(2*npts+1), names_to = "point_position"), 
        pivot_longer(yy, cols = 1:(2*npts+1), names_to = "point_position"))[,c(1:3,6)] %>% 
        mutate(point_position = as.numeric(point_position)) %>%
        arrange (transectID, point_position) 
  names(xy)[3:4] <- c("x","y")
  
  return(xy)
}
