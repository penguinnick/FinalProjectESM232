#' @param times sequence of time steps
#' @parms a list with model parameters
#' @cropparms a list summarizing yield per hectare for crops considered
#' @stockparms a list containing arguments passed to evolve_pop_wofftake.R
#' @return a list containing wealth, surplus, and strategy choice for each timestep for each wealth class

source("./R/wealth.R")
source("./R/wheat_yield.R")
source("./R/barley_yield.R")
source("./R/nnfix.R")
source("./R/resetp0.R")

sdp_farm <- function(parms, crop.parms, stock.parms, hsr=36, max_store=20, PrS=0.85, PrGood=0.8, D.good=0.75, D.bad=0.85){
  PrBad = 1-PrGood
  with(as.list(c(crop.parms, stock.parms, parms)),{
    # set model parameters
    
    times=seq(1:t_max-1)
    x_class=x_crit:x_max
    # # initialize outputs
    f <- matrix(nrow=t_max,ncol=nclass ,0) # optimal fitness
    f[t_max,] <- hsr
    s <- matrix(nrow=parms$t_max,ncol=nclass ,0) # surplus
    s[t_max,] <- max_store
    best.strategy <- matrix(nrow=t_max-1,ncol=nclass-1,0) # best patch
    V <- vector(length = sum(ncrops,nstocks))
    # to store population at each time step
    pops <- matrix(nrow = length(fertility), ncol=max(times),0.0)
    for(t in rev(times)){
      for (x in x_class[-1]){
        
        # crops
        # wheatgain=wheat_yield(yields = wheatY)
        wheatgain=wheatY[t,2]
        # barleygain=barley_yield(yields = barleyY)
        barleygain=barleyY[t,2]
        cropgains <- c(as.integer(wheatgain), as.integer(barleygain)) # variable crop gain 
        cropcost <- c(cropgains/10) # crop cost is proportional to yield
        # p.crops <- c(ppois(wheatgain, lambda = wheatgain), ppois(barleygain, lambda = barleygain))
        p.crops <- c(ppois(wheatgain, lambda = wl), ppois(barleygain, lambda = bl))
        
        # livestock
        L <- lapply(offtake, function(o){L=evolve_pop_wofftake(fertility, survivability, initialpop = p0, nstep = 3, offtake=o);  return(L)})
        G.stock.products <- make.vector(L)
        G.new.stock <-c(as.vector(unlist(L[[1]][5])), as.vector(unlist(L[[2]][5]))) 
        G.new.stock<-nnfix(G.new.stock)
        
        gains <- c(PrS*(PrGood*(p.crops[1]*(x - cropcost[1] + cropgains[1] + G.new.stock[1] - G.stock.products[1] + sum(p0) - D.good*sum(p0))) + PrBad*(p.crops[1]*(x - cropcost[1] + cropgains[1] + G.new.stock[1] - G.stock.products[1] + sum(p0) - D.bad*sum(p0)))), 
                   PrS*(PrGood*(p.crops[1]*(x - cropcost[1] + cropgains[1] + G.new.stock[2] + G.stock.products[2] + sum(p0) - D.good*sum(p0))) + PrBad*(p.crops[1]*(x - cropcost[1] + cropgains[1] + G.new.stock[2] - G.stock.products[2] + sum(p0) - D.bad*sum(p0)))), 
                   PrS*(PrGood*(p.crops[2]*(x - cropcost[2] + cropgains[2] + G.new.stock[1] + G.stock.products[1] + sum(p0) - D.good*sum(p0))) + PrBad*(p.crops[2]*(x - cropcost[2] + cropgains[2] + G.new.stock[1] - G.stock.products[1] + sum(p0) - D.bad*sum(p0)))),
                   PrS*(PrGood*(p.crops[2]*(x - cropcost[2] + cropgains[2] + G.new.stock[2] + G.stock.products[2] + sum(p0) - D.good*sum(p0))) + PrBad*(p.crops[2]*(x - cropcost[2] + cropgains[2] + G.new.stock[2] - G.stock.products[2] + sum(p0) - D.bad*sum(p0)))))
        # consider gains after meeting hsr
        V <- wealth(gains) #
        
        # record best strategy
        best <- which.max(V)
        best <- ifelse(length(best)==0,5,best)
        best.strategy[t-1,x] <- best
        
        # identify the gains from the best strategy
        bestV <- max(V)
        
        # use surplus, if necessary
        bestV <- ifelse(bestV-hsr<0, bestV+s[t-1,x]*0.95, bestV) # if max gains do not meet hsr, add last year's surplus, minus a 5% decay rate
        bestV <- ifelse(bestV-hsr<0, 0, bestV-hsr) # if added surplus still doesnt meet hsr, then the household is destitute
        f[t,x-x_crit+1] <- bestV # record household wealth for year t
        f[t,x-x_crit+1] <- ifelse(f[t,x-x_crit+1]>maxwealth, maxwealth,f[t,x-x_crit+1])
        s[t,x] <- ifelse(bestV+hsr>hsr, bestV-hsr+s[t-1,x]*0.95, s[t-1,x]*0.95) # if added surplus helped meet hsr, store whatever is left over from this year and last year minus a 5% decay rate. Otherwise, carry over last year's surplus minus a 5% decay rate otherwise
        s[t,x] <- ifelse(s[t,x]>max_store, max_store, s[t,x]) # if surplus is maxed out, set to max, otherwise keep the value
        s[t,x] <- ifelse(s[t,x]<0, 0, s[t,x]) # if surplus is in the red, set to 0, otherwise keep the value
  
        # reset goat herd size
        poplist<-list(a=L[[1]][1], b=L[[2]][1])
        p0 <- reset.p0(Livestock.list = poplist, bestV = best)
        pops[,t] <- p0
        
      }
    }
    return(list(FarmWealth=f, Surplus=s, OptimalStrategy=best.strategy, goatpop=pops))
  }) 
}

# } 