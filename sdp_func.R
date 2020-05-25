



parms= list(x_crit=0, # critical mass to survive
            x_max=160, # maximal mass
            x_rep=4, # critical mass for reproduction
            t_max=50, # number of time steps
            npatch=4) 
            
rates=list(wheatY=data.frame(year=seq(1:50), mean_yield=rpois(n=50, lambda = 30)),
           # wheat_yield(wheatY) # generates wheat yield
           barleyY=data.frame(year=seq(1:50), mean_yield=rpois(n=50, lambda = 45)),
           # barley_yield(barleyY) # generates barley yield
           npatch=4,
           good.b=c(0, 0, 1.20, 1.80), # birthrates in good  year; cattle goats (current fitness)
           good.d=c(0, 0, 0.95, 0.85), # death rates in bad year; cattle goats (curent fitness)
           bad.b=c(0, 0, 1, 1.10),
           bad.d=c(0, 0, 0.65, 0.75),
           # pop.r.g <- c(0, 0, 1.14, 1.53) # population growth good year (expected gains)
           # pop.r.b <- c(0.65, 0.825)
           goodyear=c(0.25, 0.25, 0.8, 0.8)) #,
           # repr=c(0,0,1,5)) #,
           # pgoodyr.stock <- c(0.8, 0.8) # cows goats
           # pbadyr.stock <- c(0.2, 0.2)
           # pgoodyr.crop <- c(0.25, 0.25) # barley wheat
           # pbadyr.crop <- c(0.75, 0.75)
           # hsr=36)
x_class=parms$x_crit:parms$x_max
nmass=length(x_class)
times=1:(parms$t_max-1)

# initialize 
f <- matrix(nrow=parms$t_max,ncol=nmass ,0) # optimal fitness
bestpatch <- matrix(nrow=parms$t_max-1,ncol=nmass-1,0) # best patch
V <- vector(length=parms$npatch) # current fitness
f[parms$t_max,] <- 36

fitness <- function(x,t)
{
  xx <- pmin(x ,parms$x_max)
  xx <- pmax(xx,parms$x_crit)
  fitness <- f[t,xx-parms$x_crit+1]
}

sdp <- function(theta, hsr, parms, rates){
  with(as.list(c(parms,rates)),{

    # f[t_max,] <- hsr
    # theta=8
    # x_rep=4
    # hsr=36
    for(t in rev(times)){
      for (x in x_class[-1]){
        wheatgain=wheat_yield(yields = wheatY)
        barleygain=barley_yield(yields = barleyY)
        cropgains <- c(wheatgain, barleygain, 0, 0) # variable crop gain 
        cropcost <- c(cropgains/10) # crop cost is proportional to yield
        # p.0 <- c(x, x*theta) # cows, theta goats equals one cow
        #-- sets initial herd size/composition
        i <- seq(0,x/theta)
        for(y in 1: length(i)){
          ncattle <- y
          ngoats <- x-y
          if(ncattle+ngoats>=hsr){
            d=0
          }
         # if(ncattle+ngoats<hsr){
           else { d=hsr-(ncattle+ngoats)
          }
          stock.t <- c(0, 0, ncattle, ngoats)
          repr <- cropgains + stock.t
          dfit <- pmax(0,pmin(x-hsr,repr)) 
          expectedgain <- goodyear*(fitness( hsr- (good.b*stock.t) - (good.d*stock.t) + cropgains - cropcost , t+1)) + (1-goodyear)*(fitness(hsr- (bad.b*stock.t) - (bad.d*stock.t) + cropgains - cropcost - dfit, t+1))
        }
        V <- expectedgain + dfit
        V[expectedgain == 0] <- 0 # destitute
        f[t,x-x_crit+1] <- max(V) # optimal fitness
        bestpatch[t,x-x_crit] <- which.max(V) # best patch
      }
    }
  })
  return(list(wealth=f, strategy=bestpatch))
}

# t <- seq(1:10)
# hsr <- seq(1:50)
# a <- sapply(hsr, function(hsr){sdp(theta = 8, hsr = hsr, parms = parms, rates = rates, times = times)})

source("./R/wheat_yield.R")
source("./R/barley_yield.R")
a<-sdp(theta = 8, hsr = 36, parms = parms, rates = rates)
a$wealth
