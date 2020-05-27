



parms= list(x_crit=0, # critical mass to survive
            x_max=160, # maximal mass
            x_rep=4, # critical mass for reproduction
            t_max=50, # number of time steps
            npatch=2,
            hsr=36) 


goatoff=list(goatoff50 = c(10.8, 1.1, 0.9, 0.3, 0.3, 0.6, 1.1, 1.5 , 1.5),  # at 50%
             goatoff75 = c(16.2, 1.7, 1.4, 0.5, 0.4, 0.9, 1.6, 2.2 , 2.2)) # at 75% use rate

offtake= lapply(goatoff, function(o){o/p0}) # create offtake rates as % of initial population

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
s <- matrix(nrow=parms$t_max,ncol=nmass ,0) # surplus
pr.A = 0.25
pr.B = 0.8
best.strategy= list(crops=matrix(nrow=parms$t_max-1,ncol=nmass-1 ,0) ,stock=matrix(nrow=parms$t_max-1,ncol=nmass-1 ,0))
# bestpatch <- matrix(nrow=t_max-1,ncol=nmass-1,0) # best patch
Vs <- vector(length=parms$npatch) # current fitness
Vc <- vector(length=parms$npatch) # current fitness

# bestpatch <- matrix(nrow=parms$t_max-1,ncol=nmass-1,0) # best patch
# V <- vector(length=parms$npatch) # current fitness
f[parms$t_max,] <- 36
s[parms$t_max,] <- 20

# fitness <- function(x,t)
# {
#   xx <- pmin(x ,parms$x_max)
#   xx <- pmax(xx,parms$x_crit)
#   # fitness <- f[t,xx-parms$x_crit+1]
#   f[t,xx-parms$x_crit+1]
# }

deficit.surplus <- function(hsr, gains, maxwealth, x, t){
  # gains<- cropgains
  # maxwealth=160
  # hsr=36
  xx <- pmin(gains, maxwealth)
  d <- ifelse(xx-hsr>0, 0, xx-hsr)
  # d.index <- which.max(d)
  deficit <- d
  s <- ifelse(d==0, xx-hsr, 0)
  # s.index <- which.max(s)
  surplus <- s
  return(list(deficit=deficit, surplus=surplus))
  # f[t-1,x+1] <- surplus+x+1
}
p0 = c(48, 23, 19, 15, 13, 12, 9, 6, 3)
# sdp <- function(theta, hsr, parms, rates){
#   with(as.list(c(parms,rates)),{

    # f[t_max,] <- hsr
    # theta=8
    # x_rep=4
    hsr=36
    

    for(t in rev(times)){
      
      for (x in x_class[-1]){
        # crops
        wheatgain=wheat_yield(yields = wheatY)
        barleygain=barley_yield(yields = barleyY)
        cropgains <- c(wheatgain, barleygain) # variable crop gain 
        cropcost <- c(cropgains/10) # crop cost is proportional to yield
        dscrop <- deficit.surplus(hsr = hsr, gains = cropgains, maxwealth = parms$x_max, x, t)
        expectedcropgain <- pr.A*(x-cropcost + cropgains + dscrop$deficit) + (1-pr.A)*(x-cropcost + cropgains + dscrop$deficit)
        Vc <- expectedcropgain + dscrop$deficit
        best.strategy$crops[t-1,x]<- which.max(Vc)
        c <- best.strategy$crops[t,x]
        f[t-1,x] <- max(Vc)-hsr
        s[t-1,x] <- dscrop$surplus[which.max(Vc)]
        
        # livestock
        livestock <- lapply(offtake, function(o){tmp=evolve_pop_wofftake(fertility, survivability, initialpop = p0, nstep = 3, offtake=o); return(tmp)})
        stock.products <- c(livestock$goatoff50$offtot[2], livestock$goatoff75$offtot[2])
        new.stock <- c(livestock$goatoff50$newstock[3], livestock$goatoff75$newstock[3])
        dsstock <- deficit.surplus(hsr = hsr, gains = stock.products, maxwealth = parms$x_max, x, t)
        expectedstockgain <- pr.B*(stock.products + new.stock + dsstock$deficit) + (1-pr.B)*(stock.products + new.stock + dsstock$deficit)
        Vs <- expectedstockgain + dsstock$deficit
        best.strategy$stock[t-1,x]<- which.max(Vs)
        m <- best.strategy$stock[t,x]
        p1 <- livestock[[m]]$popbyage[,2]
        f[t-1,x] <- f[t-1,x] + Vs[m] + Vc[c]
        f[t,x] <- ifelse(f[t-1,x]<sum(hsr,s[t-1,x]), 0, x+sum(p1)) 
        p0 = p1
      }
    }
  # })
# }

#         
#         V <- expectedcropgain + expectedstockgain - hsr
#         best.strategy$crops[t,x]<- which.max(Vc) 
#         
#         f[t-1,x] <- max(Vc)
#         
#         Vs <- expectedstockgain - hsr
#         best.strategy$stock[t,x] <- which.max(Vs) 
#         
#         
#         
#         s[t,x] <- s[t,x] + dsstock$surplus[which.max(Vc)]
#         
#         f[t-1,x] <- max(Vc) + max(Vs)
#         
#         
#         s[t,x] <- s[t,x] + dsstock$surplus
#        
#         # p.0 <- c(x, x*theta) # cows, theta goats equals one cow
#         #-- sets initial herd size/composition
#         i <- seq(0,x/theta)
#         for(y in 1: length(i)){
#           ncattle <- y
#           ngoats <- x-y
#           if(ncattle+ngoats>=hsr){ # add wheat and barley gain
#             d=0
#           }
#          # if(ncattle+ngoats<hsr){
#            else { d=hsr-(ncattle+ngoats) # get deficit. if goats and crop gains are still less than hsr, then the deficit is the difference
#           }
#           stock.t <- c(10, 10, ncattle, ngoats)
#           repr <- cropgains + stock.t
#           dfit <- pmax(0,pmin(x-hsr,repr)) # modify to reflect the hsr
#           expectedgain <- goodyear*(fitness( hsr- (good.b*stock.t) - (good.d*stock.t) + cropgains - cropcost , t+1)) + (1-goodyear)*(fitness(hsr- (bad.b*stock.t) - (bad.d*stock.t) + cropgains - cropcost - dfit, t+1))
#         }
#         V <- expectedgain + dfit
#         V[expectedgain == 0] <- 0 # destitute
#         f[t,x-x_crit+1] <- max(V) # optimal fitness
#         bestpatch[t,x-x_crit] <- which.max(V) # best patch
#       }
#     }
#   })
#   return(list(wealth=f, strategy=bestpatch))
# }

# t <- seq(1:10)
# hsr <- seq(1:50)
# a <- sapply(hsr, function(hsr){sdp(theta = 8, hsr = hsr, parms = parms, rates = rates, times = times)})

source("./R/wheat_yield.R")
source("./R/barley_yield.R")
a<-sdp(theta = 8, hsr = 36, parms = parms, rates = rates)
a$wealth
tail(a$wealth)
