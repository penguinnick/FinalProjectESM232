#' @param x_class sequence of wealth classes from 0 to maximum wealth
#' @param PrS probabilities of good and bad year affecting livestock strategy choice
#' @parms a list with model parameters
#' @cropparms a list summarizing yield per hectare for crops considered
#' @stockparms a list containing arguments passed to evolve_pop_wofftake.R


source("./R/wealth.R")
source("./R/wheat_yield.R")
source("./R/barley_yield.R")

sdp_farm <- function(f,s,parms, crop.parms, stock.parms, x_class, times, max_store=20, PrS=c(0.85, 0.65)){
  # x_max=parms$x_max
  # x_crit=parms$x_crit
  with(as.list(c(crop.parms, stock.parms, parms)),{
    
    # # initialize outputs
    # times=seq(1:t_max)
    # nclass=length(x_class)
    # f <- matrix(nrow=t_max,ncol=nclass ,0) # optimal fitness
    # s <- matrix(nrow=t_max,ncol=nclass ,0) # surplus
    # best.strategy <- matrix(nrow=t_max-1,ncol=nclass-1,0) # best patch
    f[t_max,] <- hsr
    s[t_max,] <- max_store
    V <- vector(length = sum(ncrops,nstocks))
  sapply(rev(times), function(t){
    # for(t in rev(times)){
      for (x in x_class[-1]){
        # crops
        wheatgain=wheat_yield(yields = wheatY)
        barleygain=barley_yield(yields = barleyY)
        cropgains <- c(as.integer(wheatgain), as.integer(barleygain)) # variable crop gain 
      cropcost <- c(cropgains/10) # crop cost is proportional to yield
        p.crops <- c(ppois(wheatgain, lambda = wheatgain), ppois(barleygain, lambda = barleygain))
        # expectedcropgain <- p.crops*(wealth(x-cropcost+cropgains,t+1) + ((1-p.crops)*(wealth(x-cropcost+cropgains,t+1))))
        expectedcropgain <- p.crops*(wealth(x-cropcost+cropgains,t+1) + ((1-p.crops)*(wealth(x-cropcost+cropgains,t+1))))
        # expectedcropgain <- p.crops*(x-cropcost+cropgains) + (1-p.crops)*(x-cropcost+cropgains)
        
        
        # livestock
        # livestock <- lapply(offtake, function(o){L=evolve_pop_wofftake(fertility, survivability, initialpop = p0, nstep = 3, offtake=o); Lt=L$offtot; return(Lt)})
        offtake.t <- lapply(offtake, function(o){L=evolve_pop_wofftake(fertility, survivability, initialpop = p0, nstep = 3, offtake=o); Lt=L$offtot; return(Lt)})
        G.stock.products <- as.vector(unlist(offtake.t)) #c(unlist(livestock$goatoff50$offtot), unlist(livestock$goatoff75$offtot ))
        # G.stock.products <- c(unlist(livestock$goatoff50$offtot), unlist(livestock$goatoff75$offtot ))
        # G.stock.products <- as.vector(unlist(lapply(offtake, function(o){tmp=evolve_pop_wofftake(fertility, survivability, initialpop = p0, nstep = 3, offtake=o); return(sum(tmp$offtakes[,2]))})))
        # G.stock.products <- as.vector(unlist(lapply(offtake, function(o){tmp=evolve_pop_wofftake(fertility, survivability, initialpop = p0, nstep = 3, offtake=o); return(sum(tmp$offtot[3]))})))
        # G.stock.products <- as.integer(G.stock.products)
        # G.new.stock <- as.vector(unlist(lapply(offtake, function(o){tmp=evolve_pop_wofftake(fertility, survivability, initialpop = p0, nstep = 3, offtake=o); return(tmp$newstock[3])})))
        ns.t <- lapply(offtake, function(o){L=evolve_pop_wofftake(fertility, survivability, initialpop = p0, nstep = 3, offtake=o); Lt=L$newstock[3]; return(Lt)})
        G.new.stock <-as.vector(unlist(ns.t))
        # G.new.stock <- c(livestock$goatoff50$newstock[3],livestock$goatoff75$newstock[3])
        # G.new.stock <- as.integer(G.new.stock)
        # B.stock.products <- as.vector(unlist(lapply(offtake, function(o){tmp=evolve_pop_wofftake(fertility, (1-survivability), initialpop = p0, nstep = 3, offtake=o); return(sum(tmp$offtakes[,2]))})))
        B.livestock <- lapply(offtake, function(o){tmp=evolve_pop_wofftake(fertility, (1-survivability), initialpop = p0, nstep = 3, offtake=o); return(tmp)})
        B.stock.products <- c(B.livestock$goatoff50$offtot, B.livestock$goatoff75$offtot )
        # B.stock.products <- as.integer(B.stock.products)
        # B.new.stock <- as.vector(unlist(lapply(offtake, function(o){tmp=evolve_pop_wofftake(fertility, (1-survivability), initialpop = p0, nstep = 3, offtake=o); return(tmp$newstock[3])})))
        B.new.stock <- c(B.livestock$goatoff50$newstock[3],B.livestock$goatoff75$newstock[3])
        # B.new.stock <- as.integer(B.new.stock)
        np.t <- lapply(offtake, function(o){L=evolve_pop_wofftake(fertility, survivability, initialpop = p0, nstep = 3, offtake=o); Lt=L$popbyage[,2]; return(Lt)})
        newp<- c(as.vector(unlist(np.t[1])), as.vector(unlist(np.t[2])))
        # newp <- c(livestock$goatoff50$popbyage[,2], livestock$goatoff75$popbyage[,2])
        # newp <- c(livestock$goatoff50$popbyage[,2], livestock$goatoff75$popbyage[,2])
          # lapply(offtake, function(o){tmp=evolve_pop_wofftake(fertility, survivability, initialpop = p0, nstep = 3, offtake=o); return(tmp$popbyage[,2])})
        op.t <- lapply(offtake, function(o){L=evolve_pop_wofftake(fertility, survivability, initialpop = p0, nstep = 3, offtake=o); Lt=L$popbyage[,1]; return(Lt)})
        oldp<- c(as.vector(unlist(op.t[1])), as.vector(unlist(op.t[2])))
        # oldp <- c(livestock$goatoff50$popbyage[,1], livestock$goatoff75$popbyage[,1])
        # lapply(offtake, function(o){tmp=evolve_pop_wofftake(fertility, survivability, initialpop = p0, nstep = 3, offtake=o); return(tmp$popbyage[,1])})
        current.herd <- as.vector(unlist(lapply(oldp, function(n){sum(n)})))
        # expectedstockgain <- PrS*((wealth(x+current.herd-G.stock.products,t+1)) + (wealth(x+current.herd-B.stock.products,t+1)))
        expectedstockgain <- PrS*((wealth(x+current.herd-G.stock.products,t)) + (wealth(x+current.herd-B.stock.products,t)))
        # expectedstockgain <- PrS*((x+current.herd-G.stock.products) + (x+current.herd-B.stock.products))
        # compare all gains
        gains <- c(expectedcropgain[1]+expectedstockgain[1], expectedcropgain[1]+expectedstockgain[2], expectedcropgain[2]+expectedstockgain[1], expectedcropgain[2]+expectedstockgain[2])
        V <- gains-hsr
        # Vs <- ifelse(V<0, V+(s[t-1,x]*0.95), V)
        # Vs[Vs<0] <- 0 # destitute
        # f[t,x-x_crit+1] <- max(Vs) # optimal fitness
        f[t,x-x_crit+1] <- max(V) # optimal fitness
        # best strategy at previous timestep
        best <- which.max(V)

        # best.strategy[t-1,x] <- best

        best.strategy[t-1,x] <- best

        # add to surplus
        s[t,x] <- ifelse(V[best]-hsr>0,V[best]-hsr,0)
        s[t,x] <- ifelse(s[t,x]>max_store, max_store, s[t,x])
        # reset goat herd size
        p0 <- ifelse(which.max(V)%%2==0, as.vector(newp[[2]]), as.vector(newp[[1]]))
      }
    })
  })
}
  # return(list(FarmWealth=f, Surplus=s, OptimalStrategy=best.strategy))
# } 

