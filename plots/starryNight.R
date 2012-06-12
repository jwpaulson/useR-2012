library(grid)
plot.new()
pushViewport(viewport(xscale=c(0, 1), yscale=c(0.5, 1),
             clip=TRUE))
             
res <- 50
for (i in 1:res)
  grid.rect(y=1 - (i-1)/res, just="top",
            gp=gpar(col=NULL, fill=grey(0.5*i/res)))

moon <- function(x, y, size) {
  angle <- seq(-90, 90, length=50)/180*pi
  x1 <- x + size*cos(angle)
  y1 <- y + size*sin(angle)
  mod <- 0.8
  x2 <- x + mod*(x1 - x)
  grid.polygon(c(x1, rev(x2)), c(y1, rev(y1)),
               default.unit="native",
               gp=gpar(col=NULL, fill="white"))
}

moon(.1, .9, .03)

star <- function(x, y, size) {
  x1 <- c(x,           x + size*.1, x + size*.5, x + size*.1,
          x,           x - size*.1, x - size*.5, x - size*.1) + .05
  y1 <- c(y - size,    y - size*.1, y,           y + size*.1,
          y + size*.7, y + size*.1, y,           y - size*.1) + .05
  grid.polygon(x1, y1, 
               default.unit="native",
               gp=gpar(col=NULL, fill="white"))
}

star(.5, .7, .02)
star(.8, .9, .02)
star(.72, .74, .02)
star(.62, .88, .02)

grid.circle(runif(20, .2, 1), runif(20, .6, 1), r=.002,
            default.unit="native",
            gp=gpar(col=NULL, fill="white"))

hill <- function(height=0.1, col="black") {
  n <- 100
  x <- seq(0, 1, length=n)
  y1 <- sin(runif(1) + x*2*pi)
  y2 <- sin(runif(1) + x*4*pi)
  y3 <- sin(runif(1) + x*8*pi)
  y <- 0.6 + height*((y1 + y2 + y3)/3)
  grid.polygon(c(x, rev(x)), c(y, rep(0, n)),
               default.unit="native",
               gp=gpar(col=NULL, fill=col))
}

hill()

rdir <- function(n) {
  sample(seq(-45, 45, length=10), n)/180*pi
}

grid.text("Once upon a time ...",
          x=.15, y=.51, just="bottom",
          default.unit="native",
          gp=gpar(col="white", fontface="italic", fontsize=10))

popViewport()

#grid.rect()
