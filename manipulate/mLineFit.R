# Interactive line-fitting application
.mLineFitHelper = function(formula, data=NULL,...){
  mod = lm( formula, data=data, ... )
  response.name = deparse(mod$terms[[2L]])
  best.slope=coef(mod)[2]
  best.inter=coef(mod)[1]
  mat = model.matrix(mod)
  independ.index = min(ncol(mat), 2)
  response = fitted(mod) + resid(mod)
  explanatory = mat[,independ.index]
  vecnames = colnames(mat)
  mx = mean(explanatory)
  my = mean(response)
  
  show.resids = FALSE
  show.squares = FALSE
  intercept.range = sort(sd(response)*c(-2,2))
  cur.intercept = mean(intercept.range)
  slope.range=sort(coef(mod)[2]*c(-1,1.5))
  cur.slope = 1;
  
  show.plot = function(){
    model.vals = cur.slope*(explanatory-mx) + (cur.intercept+my)
    plot( explanatory, response,
      ylab=response.name,
      xlab=vecnames[independ.index],
      pch=20,col="blue",
      main="Men and Women's 100 Meter Freestyle\n(World Records)")
    legend(1970, 95, legend=c("Women", "Men"), col=c("blue", "red"), lwd=2, bty="n")
    
    if( show.resids ) {
      for( k in 1:length(explanatory) ) {
        this.color = c("red","blue","blue")[sign(response[k]-model.vals[k])+2]
        lines( c(0,0)+explanatory[k], c(response[k],model.vals[k]), col=this.color)
      }
    }
    if( show.squares )  {
      foo = par("usr")
      goo = par("pin")
      text(foo[1],foo[4]-.05*(foo[4]-foo[3]),paste("Sum Sq. Resids=",signif(sum((response-model.vals)^2),3)),pos=4)
      hscale = (goo[2]/goo[1])*(foo[2]-foo[1])/(foo[4]-foo[3]) # scale horizontal appropriately
      for( k in 1:length(explanatory) ) {
        hlength = hscale*abs(response[k] - model.vals[k])
        polygon( explanatory[k]+c(0,0,hlength,hlength),
           c(response[k],model.vals[k],model.vals[k],response[k]),
           col=rgb(1,0,0,.1), border=NA)
        }
     } 
     
     abline(cur.intercept+my-cur.slope*mx, cur.slope,col=rgb(0,0,0,.3),lwd=2)
     points( explanatory, model.vals, pch=10, col="black") 
     points( explanatory, response, pch=20, col="blue")
     
        
  }
  
  do.plot = function(slope,inter,resids,squares) {
    cur.slope <<- slope*best.slope
    cur.intercept <<- inter
    show.resids <<- resids
    show.squares <<- squares
    show.plot()
  }
  return(do.plot)
}
  
  
mLineFit = function(form, data){  
 f = .mLineFitHelper( form, data)
 manipulate( f(slope,inter,resids,squares), 
   inter=slider(-3,3,initial=0,step=.05,ticks=FALSE,label="Intercept Offset"),
   slope=slider(-1,2,initial=1,step=.05,ticks=FALSE,label="Slope Multiplier"),
   resids=checkbox(FALSE,label="Show Residuals"),
   squares=checkbox(FALSE,label="Square resids"))
}
