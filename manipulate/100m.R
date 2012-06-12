setwd("~/Desktop/useR2012/manipulate/")
library(manipulate)
library(mosaic)
source("mLineFit.R")

swim <- fetchData("swim100m.csv")
# View(swim)
summary(swim)

mLineFit(time~year, swim)
title(main="Men and Women's 100 Meter Freestyle\n(World Records)")
legend(1970, 95, legend=c("Women", "Men"), col=c("blue", "red"), lwd=2, bty="n")
