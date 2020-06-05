## function computes optimal strategy and combination of strategies 

#' @param x_class sequence of wealth classes from 0 to maximum wealth
#' @param PrS probability of cultivation
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

sdp_farm <- function(f,s,b=best.strategy, parms, crop.parms, stock.parms, x_class, times, hsr=36, max_store=20, PrS=c(0.85), HI.w=0.4, HI.b=0.6){
  
  with(as.list(c(crop.parms, stock.parms, parms)),{
    
    # # initialize outputs
    f[t_max,] <- hsr
    s[t_max,] <- max_store
    V <- vector(length = sum(ncrops,nstocks))
    # to store population at each time step
    pops <- matrix(nrow = length(fertility), ncol=max(times),0.0)
    
    for(t in rev(times)){
      wheatgains=sapply(x_class, function(s){wheat_yield(yields = wheatY, area = s)}) 
      barleygains=sapply(x_class, function(s){barley_yield(yields = barleyY, area = s)})
      for (x in x_class[-1]){
        # crops
        wheatgain=wheatgains[x]*HI.w
        barleygain=barleygains[x]*HI.b
        cropgains <- c(as.integer(wheatgain), as.integer(barleygain)) # variable crop gain 
        cropcost <- c(cropgains/10) # crop cost is proportional to yield
        p.crops <- c(ppois(wheatgain, lambda = wheatgain), ppois(barleygain, lambda = barleygain))
        
        # livestock
        offtake.t <- lapply(offtake, function(o){L=evolve_pop_wofftake(fertility, survivability, initialpop = p0, nstep = 3, offtake=o);  return(L)})
        tmp<-lapply(offtake, function(o){tmp=evolve_pop_wofftake(fertility, survivability, p0, nstep = 3, offtake = o, K=maxwealth, minHerd = x); return(tmp)})
        off <- compute.offtake.metrics(tmp)
        G.stock.products <- off[2,]
        # offtake.t <- c(offtake.t[[1]][4],offtake.t[[2]][4])
        # G.stock.products <- as.vector(unlist(offtake.t)) #c(unlist(livestock$goatoff50$offtot), unlist(livestock$goatoff75$offtot ))
        # G.stock.products <- nnfix(G.stock.products)
        ns.t <- lapply(offtake, function(o){L=evolve_pop_wofftake(fertility, survivability, initialpop = p0, nstep = 3, offtake=o);  return(L)})
        # ns.t <- lapply(offtake, function(o){L=evolve_pop_wofftake(fertility, survivability, initialpop = p0, nstep = 3, offtake=o); Lt=L$newstock[3]; return(Lt)})
        # n50 <- ns.t[[1]][5]
        # n50 <- n50[3]
        G.new.stock <- c(tmp[[1]]$newstock[3], tmp[[2]]$newstock[3])
        # G.new.stock <-c(as.vector(unlist(ns.t[[1]][5])), as.vector(unlist(ns.t[[2]][5]))) # ns.t[[1]]$newstock[3], ns.t[[2]]$newstock[3])
        # G.new.stock <-G.new.stock[c(3,6)]
        # G.new.stock<-nnfix(G.new.stock)
        # G.new.stock <- c(livestock$goatoff50$newstock[3],livestock$goatoff75$newstock[3])
        # G.new.stock <- as.integer(G.new.stock)
        # B.stock.products <- as.vector(unlist(lapply(offtake, function(o){tmp=evolve_pop_wofftake(fertility, (1-survivability), initialpop = p0, nstep = 3, offtake=o); return(sum(tmp$offtakes[,2]))})))
        B.livestock <- lapply(offtake, function(o){tmp=evolve_pop_wofftake(fertility, (1-survivability), initialpop = p0, nstep = 3, offtake=o); return(tmp)})
        B.stock.products <- c(B.livestock[[1]][4],B.livestock[[2]][4])
        B.stock.products <- as.vector(unlist(B.stock.products))
        # B.stock.products <- c(B.livestock$goatoff50$offtot, B.livestock$goatoff75$offtot )
        # B.stock.products <- as.integer(B.stock.products)
        # B.new.stock <- as.vector(unlist(lapply(offtake, function(o){tmp=evolve_pop_wofftake(fertility, (1-survivability), initialpop = p0, nstep = 3, offtake=o); return(tmp$newstock[3])})))
        B.new.stock<-c(as.vector(unlist(B.livestock[[1]][5])), as.vector(unlist(B.livestock[[2]][5])))
        # B.new.stock <- c(B.livestock$goatoff50$newstock[3],B.livestock$goatoff75$newstock[3])
        B.new.stock <- B.new.stock[c(3,6)]
        # np.t <- lapply(offtake, function(o){L=evolve_pop_wofftake(fertility, survivability, initialpop = p0, nstep = 3, offtake=o); Lt=L$popbyage[,2]; return(Lt)})
        g50<-ns.t[[1]][1]
        # g50<-as.vector(unlist(ns.t$goatoff50$popbyage[,3]))
        # g75<-ns.t$goatoff75$popbyage[,3]
        g50.df <- as.data.frame(g50)
        g50p <- g50.df$popbyage.3
        
        g75<-ns.t[[2]][1]
        g75.df <- as.data.frame(g75)
        g75p <- g75.df$popbyage.3
        potentialp0s<- list(g50=g50p, g75=g75p)
        # g75p <- as.vector(g75.df[,3])
        # newp<- list(g50=g50.df[,3], g75=g75.df[,3]) #  c(as.vector(unlist(np.t[1])), as.vector(unlist(np.t[2])))
        # newp <- c(livestock$goatoff50$popbyage[,2], livestock$goatoff75$popbyage[,2])
        # newp <- c(livestock$goatoff50$popbyage[,2], livestock$goatoff75$popbyage[,2])
        # lapply(offtake, function(o){tmp=evolve_pop_wofftake(fertility, survivability, initialpop = p0, nstep = 3, offtake=o); return(tmp$popbyage[,2])})
        # op.t <- lapply(offtake, function(o){L=evolve_pop_wofftake(fertility, survivability, initialpop = p0, nstep = 3, offtake=o); Lt=L$popbyage[,1]; return(sum(Lt))})
        # oldp<- c(as.vector(unlist(op.t[1])), as.vector(unlist(op.t[2])))
        # oldp <- c(livestock$goatoff50$popbyage[,1], livestock$goatoff75$popbyage[,1])
        # lapply(offtake, function(o){tmp=evolve_pop_wofftake(fertility, survivability, initialpop = p0, nstep = 3, offtake=o); return(tmp$popbyage[,1])})
        # current.herd <- rep(sum(p0),2)
        # hsrcost <- rep(hsr,2)
        # expectedstockgain <- PrS*((wealth(x+current.herd-G.stock.products,t+1)) + (wealth(x+current.herd-B.stock.products,t+1)))
        # expectedstockgain <- PrS*(wealth(current.herd-as.numeric(G.stock.products)-hsrcost+as.numeric(G.new.stock),t+1)) + (1-PrS)*(wealth(current.herd-as.numeric(B.stock.products)-hsrcost+as.numeric(B.new.stock),t+1))
        # expectedstockgain <- PrS*((x+current.herd-G.stock.products) + (x+current.herd-B.stock.products))
        # compare all gains
        
        # gains <- c(expectedcropgain[1]+expectedstockgain[1], expectedcropgain[1]+expectedstockgain[2], expectedcropgain[2]+expectedstockgain[1], expectedcropgain[2]+expectedstockgain[2])
        
        gains <- c(PrS*(p.crops[1]*(x-cropcost[1] + cropgains[1]+G.new.stock[1]+(G.stock.products[1]-hsr))), 
                   PrS*(p.crops[1]*(x-cropcost[1]+cropgains[1]+G.new.stock[2]+(G.stock.products[2]-hsr))), 
                   PrS*(p.crops[2]*(x-cropcost[2]+cropgains[2]+G.new.stock[1]+(G.stock.products[1]-hsr))), 
                   PrS*(p.crops[2]*(x-cropcost[2]+cropgains[2]+G.new.stock[2]+(G.stock.products[2]-hsr))))
        V <- wealth(gains)
        # Vs <- ifelse(V<0, V+(s[t-1,x]*0.95), V)
        # Vs[Vs<0] <- 0 # destitute
        # f[t,x-x_crit+1] <- max(Vs) # optimal fitness
        f[t,x-x_crit+1] <- max(V) # optimal fitness
        # best strategy at previous timestep
        best <- which.max(V)
        best <- ifelse(length(best)==0,5,best)
        
        # best.strategy[t-1,x] <- best
        
        best.strategy[t-1,x] <- best
        
        # add to surplus
        s[t,x] <- ifelse(V[best]-hsr>0,V[best]-hsr,0)
        s[t,x] <- ifelse(s[t,x]>max_store, max_store, s[t,x])
        # reset goat herd size
        # p0 <- ifelse(which.max(V)%%2==0, as.vector(newp[[2]]), as.vector(newp[[1]]))
        # p0 <- ifelse(which.max(V)%%2==0, g75p, g50p)
        p.ind <- ifelse(which.max(V)%%2==0, 2, 1)
        p0<- as.vector(unlist(potentialp0s[p.ind]))
        pops[,t]<-p0
      }
    }#)
    return(list(FarmWealth=f, Surplus=s, OptimalStrategy=best.strategy, goatpop=pops))
  }) 
}
# return(list(FarmWealth=f, Surplus=s, OptimalStrategy=best.strategy))
# } 

