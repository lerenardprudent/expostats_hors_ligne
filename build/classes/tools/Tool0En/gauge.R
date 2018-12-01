library(colorRamps)
#library(RColorBrewer)
library(colorspace)

# function to create a circle
circle <- function(center=c(0,0), radius=1, npoints=100)
{
  r = radius
  tt = seq(0, 2*pi, length=npoints)
  xx = center[1] + r * cos(tt)
  yy = center[1] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

logScaleFactor <- function(p) {
  factor <- log10((p*100)+10) - 1
  factor <- ifelse(factor > 1, 1, factor)  
  return (factor)
}

prob2rads <- function(p0)
{
    p <- p0
    p <- logScaleFactor(p)
    #p <- p0
    rad <- (5*pi/4) - (3 * p * pi/2)
    return(rad)
}

rads2xy <- function(r, radius) {
  return (list(x = radius * cos(r), y = radius * sin(r)))
}

# function to get slices
slice2xy <- function(p, radius) 
{
  r = prob2rads(p)
  return (rads2xy(r, radius))
}

# function to get major and minor tick marks
ticks <- function(center=c(0,0), from=0, to=2*pi, radius=0.9, npoints=5)
{
  r = radius
  tt = seq(from, to, length=npoints)
  xx = center[1] + r * cos(tt)
  yy = center[1] + r * sin(tt)
  heatColors <- heat.colors(npoints, .5)
  return(data.frame(x = xx, y = yy, clr = heatColors))
}

dessinerRisqueMetre.G <- function(actualProb, minProbUnacceptable=0.05, minProbUnsafe=0.05, colorProb="darkblue", actualProb2=NULL, colorProb2="#4863A0")
{
  # external circle (this will be used for the black border)
  border_cir = circle(c(0,0), radius=1, npoints = 100)
  
  # gray border circle
  external_cir = circle(c(0,0), radius=0.97, npoints = 100)
  numSlices <- 20000
  
  from <- 5*pi/4
  toUnsafe <- prob2rads(minProbUnsafe)
  to <- prob2rads(minProbUnacceptable)
  gradient_safe_out2 = ticks(c(0,0), from=from, to=toUnsafe, radius=0.89, numSlices)
  gradient_safe_in2 = ticks(c(0,0), from=from, to=toUnsafe, radius=0.1, numSlices)
  
  gradient_unsafe_out = ticks(c(0,0), from=toUnsafe, to=to, radius=0.89, numSlices)
  gradient_unsafe_in = ticks(c(0,0), from=toUnsafe, to=to, radius=0.1, numSlices)
  
  gradient_unacceptable_out = ticks(c(0,0), from=to, to=-pi/4, radius=0.89, numSlices)
  gradient_unacceptable_in = ticks(c(0,0), from=to, to=-pi/4, radius=0.1, numSlices)
  
  # label to be displayed
  par(ps = 7, cex = 1, cex.main = 1)
  label = ""
  
  plot(border_cir$x, border_cir$y, type="n", asp=1, axes=FALSE,
       xlim=c(-1.1,1.1), ylim=c(-1.1,1.1),
       xlab="", ylab="", xaxs="i", yaxs="i")
  
   # red slice
  #polygon(c(Sred$x, 0), c(Sred$y, 0),
  #        border = "#DC3912", col = "#DC3912", lty = NULL)
  
  # add gray border
  lines(external_cir$x, external_cir$y, col="gray85", lwd=20)
  
  # add external border
  lines(border_cir$x, border_cir$y, col="gray20", lwd=2)
  
  #gradient
  arrows(x0=gradient_unacceptable_out$x, y0=gradient_unacceptable_out$y,
         x1=gradient_unacceptable_in$x, y1=gradient_unacceptable_in$y, length=0, lwd=1,
         col=colorRampPalette(c("coral", "red"))(dim(gradient_unacceptable_out)[1]))
  
  
  #gradient
  arrows(x0=gradient_unsafe_out$x, y0=gradient_unsafe_out$y,
         x1=gradient_unsafe_in$x, y1=gradient_unsafe_in$y, length=0, lwd=1,
         col=colorRampPalette(c("yellow", "orange"))(dim(gradient_unsafe_out)[1]))
  
    #gradient
  arrows(x0=gradient_safe_out2$x, y0=gradient_safe_out2$y,
         x1=gradient_safe_in2$x, y1=gradient_safe_in2$y, length=0, lwd=1,
         col=colorRampPalette(c("green", "lightgreen"))(dim(gradient_safe_out2)[1]))
  
  
  tio <- ticks2(0.5, 1, 26, .89)
  tii <- ticks2(0.5, 1, 26, .85)
  arrows(x0=tio$x, y0=tio$y, x1=tii$x, y1=tii$y, length=0, lwd=1, col="black")
  
  tio2 <- ticks2(0, .5, 51, .89)
  tii2 <- ticks2(0, .5, 51, .85)
  arrows(x0=tio2$x, y0=tio2$y, x1=tii2$x, y1=tii2$y, length=0, lwd=1, col="black")

  tio3 <- ticks2(0, .5, 11, .89)
  tii3 <- ticks2(0, .5, 11, .82)
  arrows(x0=tio3$x, y0=tio3$y, x1=tii3$x, y1=tii3$y, length=0, lwd=1, col="black")
  
  # add value
  #windowsFonts(
  #  A=windowsFont("Arial Black"),
  #  B=windowsFont("Bookman Old Style"),
  #  C=windowsFont("Comic Sans MS"),
  #  D=windowsFont("Symbol")
  #)
  #text(0, -0.65, paste("p =", signif(actualProb*100,3), "%") , cex=2.5)
  # add label of variable
  #text(0, 0.43, label, cex=3.8)
  
  #add acceptable limit
  #axy <- slice2xy(minProbUnacceptable, .89)
  #arrows(0, 0, axy$x, axy$y, col="white", lwd=3, lty=1, length=0)
  #arrows(0, 0, axy$x, axy$y, col="black", lwd=1, lty=3, length=0)
  
  # add needle
  needleRadius <- .89
  arrows(0, 0, slice2xy(actualProb, needleRadius)$x, slice2xy(actualProb, needleRadius)$y, col=colorProb, lwd=5, length=0)
  p1 <- signif(actualProb*100,3)
  text(0, -0.45, if ( is.null(actualProb2) ) bquote(paste(italic(p)*' = ', .(p1), "%")) else bquote(paste(italic(p)['1']*' = ', .(p1), "%")), col=colorProb, cex=2)
  
  if ( !is.null(actualProb2) ) {
    arrows(0, 0, slice2xy(actualProb2, needleRadius)$x, slice2xy(actualProb2, needleRadius)$y, col=colorProb2, lwd=5, length=0)
    p2 <- signif(actualProb2*100,3)
    text(0, -0.65, bquote(paste(italic(p)['2']*' = ', .(p2), "%")),col=colorProb2, cex=2)
  }
  
  seuilTriDelta <- 0.03
  rc <- prob2rads(minProbUnacceptable)
  zc <- slice2xy(minProbUnacceptable, 1.05)
  zcl <- rads2xy(rc-seuilTriDelta, 1.20)
  zcr <- rads2xy(rc+seuilTriDelta, 1.20)
  triangleSeuilX <- c(zc$x, zcl$x, zcr$x, zc$x)
  triangleSeuilY <- c(zc$y, zcl$y, zcr$y, zc$y)
  
  #textRad <- 0.15
  #flipLabel <- rc <= (pi/2)
  #text(slice2xy(minProbUnacceptable, textRad)$x, slice2xy(minProbUnacceptable, textRad)$y, labels=paste("SEUIL", "=", minProbUnacceptable), col="black", cex=1.2, adj=ifelse(flipLabel,0,1), srt=57.2958*(rc+ifelse(flipLabel,0,pi) ))
  
  polygon(triangleSeuilX, triangleSeuilY,
          border = "black", col = "red", lwd=1, lty = 1)
  
  # add central blue point
  points(0, 0, col="#2e9ef3", pch=19, cex=3)
  
  sapply(seq(0, 100, 20), plotSpeed)
  sapply(c(5,10,15,25,30,50,70), function(x) { plotSpeed(x, .83, 1)})
}

# add values 0 and 100
# coordinates of min and max values (0, 100)
plotSpeed <- function(sp, radi=.78, lwid=2) {
  endPoint <- sp %% 100 == 0
  xy <- slice2xy(sp/100, ifelse(endPoint, .7, .7))
  mto <- slice2xy(sp/100, .9)
  mti <- slice2xy(sp/100, radi) 
  arrows(x0=mto$x, y0=mto$y,
         x1=mti$x, y1=mti$y, length=0, lwd=lwid, col='black')  
  text(xy$x, xy$y, labels=paste0(sp, "%"), col="black", cex=ifelse(endPoint, 2, 1.5))
  return(xy)
}

ticks2 <- function(pFrom, pTo, npoints=5, radius=0.9)
{
  r <- radius
  ps <- seq(pFrom, pTo, length=npoints)
  xx = sapply(ps, function(p) { return (slice2xy(p, radius)$x)})
  yy = sapply(ps, function(p) { return (slice2xy(p, radius)$y)})
  return(data.frame(x = xx, y = yy))
}