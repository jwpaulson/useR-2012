library(mosaic)
library(mosaicManip)

# using mFit
data(KidsFeet)
mFit(width ~ length, data=KidsFeet)

# using mGrad
mGrad(x*y~x&y)
mGrad(sin(x*y)~x&y)
mGrad(sin(cos(x)+y)~x&y)
mGrad(cos(x)*sin(y)*y~x&y)

