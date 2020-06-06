#' @param times sequence of time steps
#' @parms a list with model parameters
#' @cropparms a list summarizing yield per hectare for crops considered
#' @stockparms a list containing arguments passed to evolve_pop_wofftake.R
#' @return a list containing wealth, surplus, age-structured goat population and strategy choice for each timestep for each wealth class
source("./R/sdp_farm_v3.R")
source("./R/wealth.R")
source("./R/wheat_yield.R")
source("./R/barley_yield.R")
source("./R/nnfix.r")
source("./R/resetp0.R")
source("./R/goat_array.R")
source("./R/makevector.R")

sdp_farm <- function(parms, crop.parms, stock.parms,hsr=36,max_store=20,PrS=0.85,PrGood=0.8,D.good=0.75,D.bad=0.85){
PrBad = 1-PrGood
# t_max=parms$t_max
# ncrops=2
# p0=stock.parms$initialpop
# wheatY=crop.parms$wheatY
# barleyY=crop.parms$barleyY
# wl=crop.parms$wl
# bl=crop.parms$bl
# offtake=stock.parms$offtake
# x_crit=parms$x_crit
# maxwealth=parms$maxwealth
# t=50
# x=1
# x_max=parms$x_max
  with(as.list(c(crop.parms, stock.parms, parms)),{

    # set model parameters
    times=seq(1:(parms$t_max-1))
    x_class=parms$x_crit:parms$x_max
    nclass=length(x_class) # parms$x_max
    # initialize outputs
    f <- matrix(nrow=parms$t_max,ncol=nclass ,0) # optimal fitness
    f[parms$t_max,] <- hsr
    s <- matrix(nrow=parms$t_max,ncol=nclass ,0) # surplus
    s[parms$t_max,] <- max_store
    best.strategy <- matrix(nrow=t_max-1,ncol=nclass-1,0) # best patch
    gp <- matrix(nrow=parms$t_max,ncol=nclass ,0) # surplus
    V <- vector(length = sum(crop.parms$ncrops,stock.parms$nstocks))
    # create array for storing goat population at each timestep for each wealth class
    # goat.pop=goat.array(times,x_class,agecats)
    # goat.pop=goat.array(x_class[-1],times,agecats[-1])
    goat.pop=goat.array(parms$x_max,parms$t_max,agecats[-1])
    # goat.pop=goat.array(x_class[-1],t_max,agecats[-1])
    goat.pop[,,]=0.0
    # set final population size
    # goat.pop[t_max,1,1:9]=p0
    for(i in x_class[-1]){
      # goat.pop[i,t_max-1,]=p0
      goat.pop[i,t_max,]=p0
      
    }
    # goat.pop[,t_max-1,]=p0
    
    for(t in rev(times)){
      
      # crops
      wheatgain=wheatY[t,2]
      barleygain=barleyY[t,2]
      cropgains <- c(as.integer(wheatgain), as.integer(barleygain)) # variable crop gain 
      cropcost <- c(cropgains/10) # crop cost is proportional to yield
      p.crops <- c(ppois(wheatgain, lambda = wl), ppois(barleygain, lambda = bl))
      for (x in x_class[-1]){
        p0=goat.pop[x,t+1,]
        # p0=goat.pop[x,t,]
        L <- lapply(offtake, function(o){L=evolve_pop_wofftake(fertility, survivability, initialpop = p0, nstep = 3, offtake=o, K=maxwealth, minHerd = x);  return(L)})
        G.stock.products <- make.vector(L)
        # G.new.stock <-c(as.vector(unlist(L[[1]][5])), as.vector(unlist(L[[2]][5]))) 
        G.new.stock <-c(L[[1]]$newstock[2], L[[2]]$newstock[2])
        # G.new.stock<-nnfix(G.new.stock)
        
        gains <- c(PrS*(PrGood*(p.crops[1]*(x - cropcost[1] + cropgains[1] + G.new.stock[1] - G.stock.products[1] + sum(p0) - D.good*sum(p0))) + PrBad*(p.crops[1]*(x - cropcost[1] + cropgains[1] + G.new.stock[1] - G.stock.products[1] + sum(p0) - D.bad*sum(p0)))), 
                   PrS*(PrGood*(p.crops[1]*(x - cropcost[1] + cropgains[1] + G.new.stock[2] + G.stock.products[2] + sum(p0) - D.good*sum(p0))) + PrBad*(p.crops[1]*(x - cropcost[1] + cropgains[1] + G.new.stock[2] - G.stock.products[2] + sum(p0) - D.bad*sum(p0)))), 
                   PrS*(PrGood*(p.crops[2]*(x - cropcost[2] + cropgains[2] + G.new.stock[1] + G.stock.products[1] + sum(p0) - D.good*sum(p0))) + PrBad*(p.crops[2]*(x - cropcost[2] + cropgains[2] + G.new.stock[1] - G.stock.products[1] + sum(p0) - D.bad*sum(p0)))),
                   PrS*(PrGood*(p.crops[2]*(x - cropcost[2] + cropgains[2] + G.new.stock[2] + G.stock.products[2] + sum(p0) - D.good*sum(p0))) + PrBad*(p.crops[2]*(x - cropcost[2] + cropgains[2] + G.new.stock[2] - G.stock.products[2] + sum(p0) - D.bad*sum(p0)))))
        
        # consider gains after meeting hsr
        V <- wealth(gains) 
        
        # record best strategy as that chosen in the preceeding time step
        best <- which.max(V)
        best <- ifelse(length(best)==0,5,best)
        best.strategy[t-1,x] = best
        
        # identify the gains from the best strategy
        bestV <- max(V)
        
        # use surplus, if necessary
        bestV = ifelse(bestV-hsr<0, bestV+s[t-1,x]*0.95, bestV) # if max gains do not meet hsr, add last year's surplus, minus a 5% decay rate
        bestV <- ifelse(bestV-hsr<0, 0, bestV-hsr) # if added surplus still doesnt meet hsr, then the household is destitute
        f[t,x-x_crit+1] <- bestV # record household wealth for year t
        f[t,x-x_crit+1] <- ifelse(f[t,x-x_crit+1]>maxwealth, maxwealth,f[t,x-x_crit+1])
        s[t,x] = ifelse(bestV+hsr>hsr, bestV-hsr+s[t-1,x]*0.95, s[t-1,x]*0.95) # if added surplus helped meet hsr, store whatever is left over from this year and last year minus a 5% decay rate. Otherwise, carry over last year's surplus minus a 5% decay rate otherwise
        s[t,x] <- ifelse(s[t,x]>max_store, max_store, s[t,x]) # if surplus is maxed out, set to max, otherwise keep the value
        s[t,x] <- ifelse(s[t,x]<0, 0, s[t,x]) # if surplus is in the red, set to 0, otherwise keep the value
        
        # reset goat herd size
        poplist<-list(a=L[[1]]$popbyage[,2], b=L[[2]]$popbyage[,2])
        p1=reset.p0(Livestock.list = poplist, bestV = best)
        # for(i in p1){
        #   p1[i] <- ifelse(p1[i]<0,0,p1[i])
        # }
        # record goat pop for current time step and wealth class
        goat.pop[x,t,]=p1
        # gp[t+1,x] <- sum(p1)
        
        # goat.pop[t,x,]=p1
        
      }
    }
    return(list(FarmWealth=f, Surplus=s, OptimalStrategy=best.strategy, goatpop=goat.pop))
    # return(list(FarmWealth=f, Surplus=s, OptimalStrategy=best.strategy, goatpop=gp))
  }) 
}
