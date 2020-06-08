#' @param sdp.out results of sdp_farm function
#' @param farm.yrly results of sdp_df function
#' @return a plot showing strategy choice given wealth class, abline for mean wealth, and contours showing fitness levels at time step

plot.wealth <- function(sdp.out, farm.yrly){
  f1<-sdp.out$FarmWealth
  bestpatch <- sdp.out$OptimalStrategy
  wealthclasses=0:ncol(f1[,-1])
  timesteps = 0:nrow(bestpatch)
  
  
  w=0:ncol(sdp.out$FarmWealth[,-1])
  t=1:nrow(sdp.out$OptimalStrategy)
  par(xpd = T, mar = par()$mar + c(0,0,0,12))
  
  image(x=timesteps,y=wealthclasses[-1],z=bestpatch,ylab="Wealth",xlab="Time",zlim=c(0,5), main="Best Strategy",col=c("black",topo.colors(4))) 
  box() 
  
  contour(x=1:parms$t_max,y=wealthclasses,z=f1,add=TRUE, col = "white")
  
  # abline(h = mean(farm.yrly$avg.wealth), col="red", lwd=3, lty=2)
  legend(55, 25, fill=c("black",topo.colors(4), "red"), legend=c("destitue","wheat & 50% use rate","wheat & 75% use rate","barley & 50% use rate", "barley & 75% use rate", "mean wealth"))
  ablineclip(h = mean(farm.yrly$avg.wealth), x1 = 0,x2 = 49, lwd=3,lty = 2, col = "red")
  
}